/**
 * Virtio filesystem device
 *
 * Copyright (C) 2024, Alexander Boettcher
 *
 * This file is part of Seoul.
 *
 * Seoul is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 as
 * published by the Free Software Foundation.
 *
 * Seoul is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License version 2 for more details.
 */

#include "nul/motherboard.h"
#include "executor/bios.h"
#include "model/pci.h"
#include "service/lock.h"

#include "virtio_pci.h"


/**********************
 * Structures of FUSE *
 **********************/

enum fuse_opcode {
	FUSE_INIT = 26,
};

struct fuse_in_header {
	uint32 len;
	uint32 opcode;
	uint64 unique;
	uint64 nodeid;
	uint32 uid;
	uint32 gid;
	uint32 pid;
	uint16 total_extlen; /* length of extensions in 8byte units */
	uint16 padding;
} __attribute__((packed));

struct fuse_out_header {
	uint32 len;
	int32  error;
	uint64 unique;
} __attribute__((packed));


struct fuse_init_in {
	uint32 major;
	uint32 minor;
	uint32 max_readahead;
	uint32 flags;
	uint32 flags2;
	uint32 unused[11];
} __attribute__((packed));


/* Virtio spec 1.2, 5.11 File system, 5.11.4 Device configuration layout */
struct Virtio_fs_config
{
	bool use_notify_queue { };

	unsigned read(unsigned const off) const
	{
		char const tag[36] = "mytag";

		auto const word = off / 4;

		switch (word) {
		case 0 ... 36 / 4 - 1: /* tag name */ /* XXX garbage behind tag */
			return *(unsigned *)(tag + word * 4);
		case 36 / 4:
			return  1u; /* number of queues */
		case 40 / 4:
			return 64u; /* notify_buf_size */
		default:
			Logging::panic("unknown fs config read %u\n", off);
			return 0;
		}
	}

	void write(unsigned const, unsigned const) {
		/* not allowed according to spec - nop */ }
};


class Virtio_fs: public StaticReceiver<Virtio_fs>, Virtio::Device
{
	private:

//		unsigned    const  _device { 0x10002 };
		Seoul::Lock      _lock      { };
		Virtio_fs_config _fs_config { };

		~Virtio_fs();

	public:

		Virtio_fs(DBus<MessageIrqLines>  &bus_irqlines,
		          DBus<MessageMemRegion> &bus_memregion,
		          uint64 const bar_addr,
		          uint8  const irq_pin,
		          uint8  const irq_line,
		          uint16 const bdf)
		:
			Virtio::Device(bus_irqlines, bus_memregion, irq_pin, irq_line, bdf,
			               26 /* virtio type */,
			               0x01080001, /* pci class code (mass storage), sub class (other), prog if, rev. id */
			               bar_addr,
			               3 /* queues */)
		{
			_verbose = false;
		}

		bool receive(MessageBios &msg)
		{
			switch (msg.irq) {
			case BiosCommon::BIOS_RESET_VECTOR:

				Seoul::Lock::Guard guard(_lock);

				_fs_config = { };
				reset();
			};

			return false;
		}

		bool receive(MessagePciConfig &msg)
		{
			if (msg.bdf != _bdf)
				return false;

			return sync_and_irq(_lock, [&]() {
				return Virtio::Device::receive(msg); });
		}

		bool receive(MessageMem &msg)
		{
			if (msg.phys < _phys_bar_base || _phys_bar_base + PHYS_BAR_SIZE <= msg.phys)
				return false;

			return sync_and_irq(_lock, [&]() {

				unsigned const offset = unsigned(msg.phys - _phys_bar_base);

				switch (offset) {
				case BAR_OFFSET_CONFIG ... BAR_OFFSET_CONFIG + RANGE_SIZE - 1:
					if (msg.read)
						*msg.ptr = _fs_config.read(offset - BAR_OFFSET_CONFIG);
					else
						_fs_config.write(offset - BAR_OFFSET_CONFIG, *msg.ptr);

					return true;
				default:
					return Virtio::Device::receive(msg);
				}
			});
		}

		void notify (unsigned queue) override
		{
			/*
			 * queue 0 (hipro)
			 * queue 1 (notification) - iif VIRTIO_FS_F_NOTIFICATION negotiated
			 * queue 2 (request)
			 */
			Logging::printf("virtio_fs: notify %u %s", queue,
			                queue == 0 ? "HIPRO" :
			                queue == 1 && _fs_config.use_notify_queue ? "NOTIFY" : "REQUEST");

			auto &used_queue = _queues[queue].queue;

			bool inject = used_queue.consume([&] (auto const descriptor, auto) {
				auto const request = vmm_address(descriptor.addr, descriptor.len);
				auto const request_size = descriptor.len;

				Logging::printf("request_size %u in_header=%lu out_header=%lu\n",
				                request_size, sizeof(fuse_in_header),
				                sizeof(fuse_out_header));

				if (!request || request_size < sizeof(fuse_in_header)) {
					Logging::printf("virtio_fs, invalid request\n");
					return 0U;
				}

				auto const & in = *reinterpret_cast<fuse_in_header *>(request);
				auto const unique = in.unique;

				Logging::printf("opcode %u len=%u unique=%llx\n",
				                in.opcode, in.len, unique);

				auto desc1 = used_queue.next_desc(descriptor);

				switch (in.opcode) {
				case FUSE_INIT:
					fuse_opcode_init(desc1);
					break;
				default:
					Logging::printf("virtio_fs: unknown opcode %u len=%u unique=%llx\n",
					                in.opcode, in.len, unique);
					break;
				}

				auto & out = *reinterpret_cast<fuse_out_header *>(request);
				out.len    = sizeof(out);
				out.error  = 0; /* XXX which ? */
				out.unique = unique;

				return request_size;
			});

			if (inject)
				inject_irq();

			if (inject)
				Logging::printf("virtio_fs:  inject !\n");
		}

		enum { VIRTIO_FS_F_NOTIFICATION = 1 };

		uint32 dev_feature (unsigned const sel) override
		{
			if (sel == 0) return VIRTIO_FS_F_NOTIFICATION;

			return 0u;
		}

		void   drv_feature_ack (unsigned sel, uint32 value) override {
			Logging::printf("%s sel=%u value=%x\n", __func__, sel, value);
			if (sel == 0)
				_fs_config.use_notify_queue = value & VIRTIO_FS_F_NOTIFICATION;
		}

		uint32 drv_feature     (unsigned)         override {
			Logging::panic("%s\n", __func__);
			return 0u;
		}

		void notify_power(unsigned value) override
		{
			Logging::panic("virtio_fs:%s implement me value=%x", __func__, value);
#if 0
			Logging::printf("virtio_fs: power change %x\n", value);

			if ((value & 3) != 0)
				return;

			_fs_config = { };
			reset();
#endif
		}
#if 1
		bool fuse_opcode_init(auto const &desc)
		{
			if (!desc.len)
				return false;

			auto desc1_addr = vmm_address(desc.addr, desc.len);
			auto desc1_size = desc.len;

			Logging::printf("next desc1 %u", desc.len);

			if (desc1_size < sizeof(fuse_init_in))
				return false;

			auto const &init_in = *reinterpret_cast<fuse_init_in *>(desc1_addr);

			Logging::printf("next major %u %u", init_in.major, init_in.minor);

			return true;
		}
#endif
};


PARAM_HANDLER(virtio_fs,
	      "virtio_fs:mem,bdf - attach an virtio fs to the PCI bus",
	      "Example: 'virtio_fs:0xe0200000'",
	      "If no bdf is given a free one is used.")
{
	unsigned const bdf = PciHelper::find_free_bdf(mb.bus_pcicfg, unsigned(argv[1]));
	if (bdf >= 1u << 16)
		Logging::panic("virtio_fs: invalid bdf\n");

	auto const irq_pin  =  3; /* PCI INTC# */
	auto const irq_line = 11; /* defined by acpicontroller dsdt for INTC# */
	auto const bar_base = argv[0];

	if (argv[0] == ~0UL)
		Logging::panic("virtio_fs: missing bar address");

	auto dev = new Virtio_fs(mb.bus_irqlines, mb.bus_memregion,
	                         bar_base, irq_pin, irq_line, uint16(bdf));

	mb.bus_pcicfg.add(dev, Virtio_fs::receive_static<MessagePciConfig>);
	mb.bus_mem   .add(dev, Virtio_fs::receive_static<MessageMem>);
	mb.bus_bios  .add(dev, Virtio_fs::receive_static<MessageBios>);

	Logging::printf("Virtio Filesystem\n");
}
