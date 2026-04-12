/*
 * \brief  Block interface
 * \author Markus Partheymueller
 * \author Alexander Boettcher
 * \date   2012-09-15
 */

/*
 * Copyright (C) 2012 Intel Corporation
 * Copyright (C) 2013-2024 Genode Labs GmbH
 *
 * This file is distributed under the terms of the GNU General Public License
 * version 2.
 *
 * The code is partially based on the Vancouver VMM, which is distributed
 * under the terms of the GNU General Public License version 2.
 *
 * Modifications by Intel Corporation are contributed under the terms and
 * conditions of the GNU General Public License version 2.
 */

#pragma once

/* Genode includes */
#include <block_session/connection.h>

/* Seoul includes */
#include <nul/motherboard.h>
#include <host/dma.h>

namespace Seoul {
	class Disk;
	class Disk_signal;
}

class Seoul::Disk_signal
{
	private:

		Disk     &_obj;
		unsigned  _id;

		void _signal();

	public:

		Genode::Signal_handler<Disk_signal> const sigh;

		Disk_signal(Genode::Entrypoint &ep, Disk &obj,
		            Block::Connection<> &block, unsigned disk_nr)
		:
		  _obj(obj), _id(disk_nr),
		  sigh(ep, *this, &Disk_signal::_signal)
		{
			block.tx_channel()->sigh_ack_avail(sigh);
			block.tx_channel()->sigh_ready_to_submit(sigh);
		}
};


class Seoul::Disk : public StaticReceiver<Seoul::Disk>
{
	private:

		struct Pending {
			unsigned long  usertag;
			unsigned long  physoffset;
			unsigned long  consumed;
			DmaDescriptor *dma;
			unsigned       dmacount;

			Block::Packet_descriptor pkg;
		};

		/* SATA model has max 33 and IDE 1 as max outstanding requests atm */
		enum { MAX_DISKS = 6, MAX_OUTSTANDING = 64 };

		struct Disk_session {
			Block::Connection<> * blk_con;
			Block::Session::Info  info;
			Disk_signal         * signal;
			Genode::Mutex         mutex;
			unsigned long         max_size;
			struct Pending        pending[MAX_OUTSTANDING] { };
			bool                  resume_execution;
			bool                  last_op_write;
			bool                  nosync;
			bool                  allowpartial;

			bool alloc_pending(auto const &fn)
			{
				for (unsigned i = 0; i < MAX_OUTSTANDING; i++) {
					auto & p = pending[i];

					if (p.dmacount)
						continue;

					return fn(p, Block::Request::Tag { i });
				}

				return false;
			}

			void free_pending(Pending &p) { p = { }; }

			bool with_pending(Block::Packet_descriptor::Tag const &tag, auto const &fn)
			{
				if (tag.value >= MAX_OUTSTANDING)
					Logging::panic("in-sane outstanding tag write");

				return fn(pending[tag.value]);
			}
		};


		Genode::Env         &_env;
		Motherboard         &_mb;

		struct Disk_session  _disks [MAX_DISKS] { };

		char        * const _backing_store_base;
		size_t        const _backing_store_size;

		/*
		 * Noncopyable
		 */
		Disk             (Disk const &);
		Disk &operator = (Disk const &);

		bool execute(bool, Disk_session &, MessageDisk &);

		bool _execute_write(Block::Session::Tx::Source       &,
		                    Block::Packet_descriptor   const &,
		                    unsigned long              const  ,
		                    Disk_session                     &,
		                    MessageDisk                const &);

		bool _execute_read (Block::Session::Tx::Source       &,
		                    Block::Packet_descriptor   const &,
		                    unsigned long              const  ,
		                    Disk_session                     &,
		                    MessageDisk                const &);

		bool for_each_dma_desc(auto         &msg,
		                       auto   const &packet,
		                       char * const  source,
		                       auto   const &fn)
		{
			unsigned long offset = 0;

			if (!packet.size())
				return false;

			/* check bounds for read and write operations */
			for (unsigned i = 0; i < msg.dmacount; i++) {
				char * const dma_addr = _backing_store_base +
				                        msg.dma[i].byteoffset +
				                        msg.physoffset;
				auto const bytecount  = msg.dma[i].bytecount;

				/* check for bounds */
				if (dma_addr >= _backing_store_base + _backing_store_size ||
				    dma_addr < _backing_store_base) {

					Genode::error(__func__, ":", __LINE__, " out-of-bound access",
					              (void *)dma_addr,
					              " ", (void *)_backing_store_base,
				                  "+", Genode::Hex(_backing_store_size));
					return false;
				}

				if (bytecount > packet.size() - offset) {
					Genode::error(__func__, ":", __LINE__, " insufficient space  -",
					              "packet.size()=", packet.size(), " ",
					              bytecount, " > ", packet.size(), "-", offset);
					return false;
				}

				fn(dma_addr, source + offset, bytecount);

				offset += bytecount;
			}

			return true;
		}

		bool for_each_dma_desc(auto         &msg,
		                       auto   const &packet,
		                       char * const  source,
		                       auto   const &fn,
		                       auto   const &fn_partial)
		{
			unsigned long offset   = 0;
			unsigned long consumed = 0;

			if (!packet.size())
				return false;

			/* check bounds for read and write operations */
			for (unsigned i = 0; i < msg.dmacount; i++) {
				char * const dma_addr = _backing_store_base +
				                        msg.dma[i].byteoffset +
				                        msg.physoffset;
				auto const bytecount  = msg.dma[i].bytecount;


				/* check for bounds */
				if (dma_addr >= _backing_store_base + _backing_store_size ||
				    dma_addr < _backing_store_base) {

					Genode::error(__func__, ":", __LINE__, " out-of-bound access",
					              (void *)dma_addr,
					              " ", (void *)_backing_store_base,
				                  "+", Genode::Hex(_backing_store_size));
					return false;
				}

				if (consumed < msg.consumed) {
					if (consumed + bytecount <= msg.consumed) {
						consumed += bytecount;
						continue;
					}

					auto con  = msg.consumed - consumed;
					auto size = Genode::min(packet.size(), bytecount - con);

					if (packet.size() < bytecount - con) {
						msg.consumed += packet.size();

						fn_partial(dma_addr + con, source, size);
						return false;
					}

					fn(dma_addr + con, source, size);

					consumed += con;
					offset    = size;

					continue;
				}

				if (bytecount > packet.size() - offset) {
					msg.consumed += packet.size();

					fn_partial(dma_addr, source + offset, packet.size() - offset);
					return false;
				}

				fn(dma_addr, source + offset, bytecount);

				offset += bytecount;
			}

			return true;
		}

	public:

		static constexpr unsigned block_packetstream_size = 4096 * 1024
	                                                      + 64 * 1024;

		Disk(Genode::Env &, Motherboard &, char *, Genode::size_t, Genode::Node const &);

		void handle_disk(unsigned);

		bool receive(MessageDisk &msg);
};
