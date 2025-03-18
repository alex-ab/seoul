/*
 * \brief  Block interface
 * \author Markus Partheymueller
 * \author Alexander Boettcher
 * \date   2012-09-15
 */

/*
 * Copyright (C) 2012      Intel Corporation
 * Copyright (C) 2013-2024 Genode Labs GmbH
 * Copyright (C) 2026      Alexander Boettcher
 *
 * This file is distributed under the terms of the GNU General Public License
 * version 2.
 *
 * The code is partially based on the Seoul VMM, which is distributed
 * under the terms of the GNU General Public License version 2.
 *
 * Modifications by Intel Corporation are contributed under the terms and
 * conditions of the GNU General Public License version 2.
 */

/* Genode includes */
#include <base/log.h>
#include <base/heap.h>

/* local includes */
#include "disk.h"

static Genode::Heap * disk_heap(Genode::Ram_allocator *ram = nullptr,
                                Genode::Env::Local_rm *rm = nullptr)
{
	static Genode::Heap heap(ram, rm);
	return &heap;
}


Seoul::Disk::Disk(Genode::Env &env, Motherboard &mb,
                  char * backing_store_base, Genode::size_t backing_store_size,
                  Genode::Node const &config)
:
	_env(env),
	_mb(mb),
	_backing_store_base(backing_store_base),
	_backing_store_size(backing_store_size)
{
	/* initialize disk heap */
	disk_heap(&env.ram(), &env.rm());

	mb.bus_disk.add(this, receive_static<MessageDisk>);

	config.for_each_sub_node("disk", [&](auto const &d) {
		auto disk_id      = d.attribute_value("id", 0u);
		bool allowpartial = d.attribute_value("partial", false);
		bool nosync       = d.attribute_value("nosync",  false);

		if (disk_id < MAX_DISKS) {
			auto &disk        = _disks[disk_id];
			disk.allowpartial = allowpartial;
			disk.nosync       = nosync;
		}
	});
}


void Seoul::Disk_signal::_signal() { _obj.handle_disk(_id); }


void Seoul::Disk::handle_disk(unsigned disknr)
{
	bool        wakeup  =  false;
	auto        &disk   =  _disks[disknr];
	auto        &source = *disk.blk_con->tx();
	auto const blk_size =  disk.info.block_size;

	disk.mutex.acquire();

	while (source.ack_avail())
	{
		auto const packet      = source.get_acked_packet();
		auto       disk_answer = MessageDisk::DISK_OK;

		switch (packet.operation()) {
		case Block::Packet_descriptor::Opcode::END:
		case Block::Packet_descriptor::Opcode::TRIM:
		case Block::Packet_descriptor::Opcode::SYNC:
			source.release_packet(packet);
			continue;

		case Block::Packet_descriptor::Opcode::WRITE:
		{
			bool partial = disk.with_pending(packet.tag(), [&](auto &write) {

				if (write.pkg.size() <= write.consumed + packet.size())
					return false;

				typedef Block::Packet_descriptor Pkg;

				write.consumed += packet.size();

				Pkg p0(packet.offset()  + packet.size(),
				       write.pkg.size() - write.consumed);
				Pkg p1(p0, Pkg::WRITE,
				       packet.block_number()   + packet.block_count(),
				       write.pkg.block_count() - (write.consumed / blk_size),
				       packet.tag());

				wakeup = true;

				bool ok = source.try_submit_packet(p1);

				if (!ok) {
					Genode::log("partial write delayed");

					write.consumed -= packet.size();

					return true;
				}

				return true;
			});

			if (partial)
				continue;

			break;
		}

		case Block::Packet_descriptor::Opcode::READ:
		{
			bool partial = disk.with_pending(packet.tag(), [&](auto &read) {

				bool partial_read = false;

				bool ok = for_each_dma_desc(read, packet, source.packet_content(packet),
				                            [&](auto dma_addr, auto src, auto count) {
					if (count)
						memcpy(dma_addr, src, count);
				}, [&](auto dma_addr, auto src, auto count) {
					if (count)
						memcpy(dma_addr, src, count);
					partial_read = true;
				});

				if (ok) {
					destroy(disk_heap(), read.dma);
					read.dma = { };
					return false;
				}

				if (!partial_read) {
					Genode::warning("unknown read state");
					return true;
				}

				typedef Block::Packet_descriptor Pkg;

				Pkg p(packet, Pkg::READ,
				      packet.block_number()  + packet.block_count(),
				      read.pkg.block_count() - (read.consumed / blk_size),
				      packet.tag());

				wakeup = true;

				ok = source.try_submit_packet(p);

				if (!disk.allowpartial &&
				    disk.max_size > packet.block_count() * blk_size)
				{
					Genode::warning("disc ", disknr, ": "
					                "reduce transfer size per packet permanently - ",
					                disk.max_size, " -> ",
					                packet.block_count() * blk_size);
					disk.max_size = packet.block_count() * blk_size;
				}

				if (!ok)
					Genode::log("partial read delayed");

				return true;
			});

			if (partial)
				continue;

			break;
		}
		default:
			Genode::error("unknown block operation ", int(packet.operation()));
			continue;
		}

		if (!packet.succeeded()) {
			disk_answer = MessageDisk::DISK_STATUS_BUSY;
			Genode::warning("packet not successful !? - operation=",
			                int(packet.operation()));
		}

		disk.with_pending(packet.tag(), [&](auto &pend) {

			disk.mutex.release();

			MessageDiskCommit mdc(disknr, pend.usertag, disk_answer);
			_mb.bus_diskcommit.send(mdc);

			disk.mutex.acquire();

			/* release correct size of original packet allocation */
			source.release_packet(pend.pkg);

			disk.free_pending(pend);

			return true;
		});
	}

	if (disk.resume_execution) {
		disk.resume_execution = false;

		disk.mutex.release();

		MessageDiskCommit msg(disknr, ~0U, MessageDisk::DISK_STATUS_RESUME);
		_mb.bus_diskcommit.send(msg);

		disk.mutex.acquire();

		if (msg.status != MessageDisk::DISK_OK) {
			disk.resume_execution = true;
		}
	}

	disk.mutex.release();

	if (wakeup)
		source.wakeup();
}


bool Seoul::Disk::receive(MessageDisk &msg)
{
	if (msg.disknr >= MAX_DISKS)
		Logging::panic("You configured more disks than supported.\n");

	auto &disk = _disks[msg.disknr];

	if (!disk.info.block_size) {
		Genode::String<16> label("disk", msg.disknr);
		/*
		 * If we receive a message for this disk the first time, create the
		 * structure for it.
		 */
		try {
			Genode::Allocator_avl * block_alloc =
				new (disk_heap()) Genode::Allocator_avl(disk_heap());

			disk.blk_con =
				new (disk_heap()) Block::Connection<>(_env, block_alloc,
				                                      block_packetstream_size,
				                                      label.string());
			disk.signal =
				new (disk_heap()) Seoul::Disk_signal(_env.ep(), *this,
				                                     *disk.blk_con, msg.disknr);
		} catch (...) {
			/* there is none. */
			return false;
		}

		disk.info = disk.blk_con->info();

		if (!disk.info.block_size || disk.info.block_size != 512 ||
		     disk.info.align_log2 > 31)
			Logging::panic("unsupported block size %d", disk.info.align_log2);

		auto &tx      = *disk.blk_con->tx();
		disk.max_size = tx.bulk_buffer_size() / 512 * 512;

		Genode::log("disk ", msg.disknr, ": ",
		            disk.nosync ? " without" : " with", " simple SYNC support",
		            !disk.allowpartial ? ", avoiding partial block packet requests " : "");
	}

	msg.error = MessageDisk::DISK_OK;

	switch (msg.type) {
	case MessageDisk::DISK_GET_PARAMS:
	{
		Genode::String<16> label("disk", msg.disknr);

		if (disk.info.block_count >= 1ull << 32 ||
		    disk.info.block_size  >= 1ull << 32)
			Logging::panic("disk: too many blocks");

		msg.params->flags           = DiskParameter::FLAG_HARDDISK;
		msg.params->sectors         = disk.info.block_count;
		msg.params->sectorsize      = unsigned(disk.info.block_size);
		msg.params->maxrequestcount = unsigned(disk.info.block_count);

		memcpy(msg.params->name, label.string(), label.length());

		return true;
	}
	case MessageDisk::DISK_ALL_REQ_DONE:
	{
		Genode::Mutex::Guard guard(disk.mutex);

		disk.blk_con->tx()->wakeup();

		return true;
	}
	case MessageDisk::DISK_WRITE:
		/* don't write on read only medium */
		if (!disk.info.writeable) {
			/* nevertheless confirm that commit got processed */
			Genode::warning("write denied to r/o disk", msg.disknr);
			MessageDiskCommit ro(msg.disknr, msg.usertag, MessageDisk::DISK_OK);
			_mb.bus_diskcommit.send(ro);
			return true;
		}

		[[fallthrough]];

	case MessageDisk::DISK_READ:
		/* read and write handling */
		return execute(msg.type == MessageDisk::DISK_WRITE, disk, msg);
	default:
		Genode::warning("unknown disk operation ", unsigned(msg.type));
		return false;
	}
}


bool Seoul::Disk::execute(bool const write, Disk_session &disk,
                          MessageDisk &msg)
{
	auto const  total    = DmaDescriptor::sum_length(msg.dmacount, msg.dma);
	auto const  blk_size = disk.info.block_size;
	auto const  blocks   = total / blk_size + ((total % blk_size) ? 1 : 0);
	auto       &tx       = *disk.blk_con->tx();

	if (total % blk_size)
		Genode::warning("unsupported data size ", total, "/", blk_size);

	if (total > disk.max_size)
	{
		/* notify model to retry with decreased amount */
		msg.more  = disk.max_size;
		msg.error = MessageDisk::DISK_STATUS_DMA_TOO_LARGE;

		Genode::warning("disc ", msg.disknr, ": ",
		                "request model to limit max transfer size - ",
		                total, " -> ", disk.max_size, " - ",
		                write ? "write" : "read");

		return true;
	}

	Genode::Mutex::Guard guard(disk.mutex);

	if (disk.resume_execution) {
		Genode::warning("unexpected resume state");
		return false;
	}

	auto const & fn_resume = [&]() {
		disk.resume_execution = true;
		msg.error             = MessageDisk::DISK_STATUS_BUSY;

		tx.wakeup();
	};

	/* sync when read/write operation changed */
	if (!disk.nosync && disk.last_op_write != write) {

		if (!tx.ready_to_submit()) {
			fn_resume();
			return true;
		}

		auto sync_packet = tx.alloc_packet_attempt(0);
		auto sync_result = sync_packet.convert<bool>([&](auto const &p)
		{
			typedef Block::Packet_descriptor Pkg;

			Pkg packet(p, Pkg::SYNC, 0);

			auto ok = tx.try_submit_packet(packet);

			if (ok)
				disk.last_op_write = write;
			else
				tx.release_packet(p);

			return ok;
		}, [&] (auto /* temporary insufficient space in packet stream */) {
			return false;
		});

		if (!sync_result) {
			fn_resume();
			return true;
		}
	}

	if (!tx.ready_to_submit()) {
		fn_resume();
		return true;
	}

	auto result = tx.alloc_packet_attempt(blocks * blk_size,
	                                      unsigned(disk.info.align_log2));

	auto res = result.convert<bool>([&](auto const &p) {
		typedef Block::Packet_descriptor Pkg;

		bool success = write ? _execute_write(tx, p, blocks, disk, msg)
		                     : _execute_read (tx, p, blocks, disk, msg);

		if (!success) {
			tx.release_packet(p);
			fn_resume();
		}

		return true;
	}, [&](auto /* temporary insufficient space in packet stream */) {
		fn_resume();
		return true;
	});

	return res;
}


bool Seoul::Disk::_execute_read(Block::Session::Tx::Source       &tx,
                                Block::Packet_descriptor   const &desc,
                                unsigned long              const  blocks,
                                Disk_session                     &disk,
                                MessageDisk                const &msg)
{
	/* if false, temporarily effect, more space if some read requests are processed */
	return disk.alloc_pending([&](auto &pend, auto const tag) {

		typedef Block::Packet_descriptor Pkg;

		Pkg packet(desc, Pkg::READ, msg.sector, blocks, tag);

		pend = { .usertag    = msg.usertag,
		         .physoffset = msg.physoffset,
		         .consumed   = 0,
		         .dma        = new (disk_heap()) DmaDescriptor[msg.dmacount],
		         .dmacount   = msg.dmacount,
		         .pkg        = packet };

		for (unsigned i = 0; i < pend.dmacount; i++)
			pend.dma[i] = msg.dma[i];

		bool ok = tx.try_submit_packet(packet);

		if (!ok) {
			Genode::destroy(disk_heap(), pend.dma);
			disk.free_pending(pend);
		}

		return ok;
	});
}


bool Seoul::Disk::_execute_write(Block::Session::Tx::Source       &tx,
                                 Block::Packet_descriptor   const &desc,
                                 unsigned long              const  blocks,
                                 Disk_session                     &disk,
                                 MessageDisk                const &msg)
{
	return disk.alloc_pending([&](auto &pend, auto const tag) {

		typedef Block::Packet_descriptor Pkg;

		Pkg packet(desc, Pkg::WRITE, msg.sector, blocks, tag);

		pend = { .usertag    = msg.usertag,
		            .physoffset = msg.physoffset,
		            .consumed   = 0,
		            .dma        = nullptr,
		            .dmacount   = msg.dmacount, /* dummy to mark as used */
		            .pkg        = packet };

		bool ok = for_each_dma_desc(msg, packet, tx.packet_content(packet),
		                            [&](auto dma_addr, auto src, auto count) {
			memcpy(src, dma_addr, count);
		});

		if (!ok) {
			disk.free_pending(pend);
			return ok;
		}

		ok = tx.try_submit_packet(packet);

		if (!ok)
			disk.free_pending(pend);

		return ok;
	});
}
