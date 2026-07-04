/*
 * \brief  Genode <-> VMM PCI adapter
 * \author Alexander Boettcher
 * \date   2025-07-04
 */

/*
 * Copyright (C) 2026 Alexander Boettcher
 *
 * This file is part of Seoul, which is distributed
 * under the terms of the GNU General Public License version 2.
 */

#pragma once


#include <base/attached_io_mem_dataspace.h>
#include <base/env.h>

#include <irq_session/connection.h>
#include <io_port_session/connection.h>


namespace Seoul {
	class Pci;
}


class Seoul::Pci : public StaticReceiver<Seoul::Pci>
{
	private:

		class Device {

				typedef Genode::Constructible<Irq_connection>     Genode_irq;
				typedef Genode::Constructible<Io_mem_connection>  Genode_iom;
				typedef Genode::Constructible<Io_port_connection> Genode_iop;

				struct Range { unsigned base; unsigned size; };

			public:

				Genode::Signal_handler<Device> handler;

				DBus<MessageIrq> &bus_irq;
				Genode::Mutex    &mutex;
				Genode_irq        irq          { };
				Genode_iom        iomem        { };
				Genode_iop        ioport       { };
				Range             ioport_range { };
				char              irq_nr       { };

				Device(Genode::Entrypoint &ep, DBus<MessageIrq> &bus,
				       Genode::Mutex &mux)
				:
					handler(ep, *this, &Device::handle_irq),
					bus_irq(bus), mutex(mux)
				{ }

				void handle_irq()
				{
					Genode::Mutex::Guard guard(mutex);

					error("irq triggered ", irq_nr);

#if 0
					MessageIrq msg(shared ? MessageIrq::ASSERT_NOTIFY : MessageIrq::ASSERT_IRQ, utcb->msg[1] & 0xff);
#endif
					MessageIrq msg(MessageIrq::ASSERT_NOTIFY, irq_nr);
					bool ok = bus_irq.send(msg);

					if (!ok)
						error(" irq could not be handled");
				}
		};

		Genode::Env           &_env;
		Motherboard           &_mb;
		Genode::Heap          &_heap;
		Seoul::Guest_memory   &_gmem;
		Genode::Mutex          _mutex  { };
		Device                 _device { _env.ep(), _mb.bus_hostirq, _mutex };

		unsigned const _bdf_base { 0xb0000000u }; /* XXX Qemu */

		void _dump_msg(MessageHwPciConfig const &msg) const
		{
			using namespace Genode;

			log("hwpci:",
			    " ", Hex((msg.bdf >> 8) & 0xff, Hex::OMIT_PREFIX),
			    ":", Hex((msg.bdf >> 3) & 0x1f, Hex::OMIT_PREFIX),
			    ".",     (msg.bdf >> 0) & 0x07, " ",
			    msg.type == MessageHwPciConfig::Type::TYPE_READ  ? "read"  :
			                MessageHwPciConfig::Type::TYPE_WRITE ? "write" :
			                MessageHwPciConfig::Type::TYPE_PTR   ? "ptr"
			                                                     : "unknown",
			    " ", Hex(msg.dword), " value=", Hex(msg.value));
		}

	public:

		Pci(Genode::Env &env, Motherboard &mb, Genode::Heap &heap,
		    Seoul::Guest_memory &memory)
		: _env(env), _mb(mb), _heap(heap), _gmem(memory)
		{
			_mb.bus_hwpcicfg .add(this, receive_static<MessageHwPciConfig>);
			_mb.bus_hostop   .add(this, receive_static<MessageHostOp>);
			_mb.bus_acpi     .add(this, receive_static<MessageAcpi>);
			_mb.bus_irqnotify.add(this, receive_static<MessageIrqNotify>);
			_mb.bus_hwioin   .add(this, receive_static<MessageHwIOIn>);
			_mb.bus_hwioout  .add(this, receive_static<MessageHwIOOut>);
		}

		bool receive(MessageIOIn &msg)
		{
			if (!_device.ioport.constructed())
				return false;

			if (!in_range(msg.port, _device.ioport_range.base,
			                        _device.ioport_range.size))
				return false;

			if (msg.count) {
				/* have to use msg.ptr instead of msg.value then */
				error("hwioin ", Hex(msg.port), "+", Hex(msg.count), " rep required ...");
				return false;
			}

			switch (msg.type) {
			case MessageIOIn::TYPE_INB:
				msg.value = _device.ioport->inb(uint16(msg.port));
				break;
			case MessageIOIn::TYPE_INW:
				msg.value = _device.ioport->inw(uint16(msg.port));
				break;
			case MessageIOIn::TYPE_INL:
				msg.value = _device.ioport->inl(uint16(msg.port));
				break;
			};

			return true;
		}

		bool receive(MessageIOOut &msg)
		{
			if (!_device.ioport.constructed())
				return false;

			if (!in_range(msg.port, _device.ioport_range.base,
			                        _device.ioport_range.size))
				return false;

			if (msg.count) {
				/* have to use msg.ptr instead of msg.value then */
				error("hwioout ", Hex(msg.port), "+", Hex(msg.count), " rep required ...");
				return false;
			}

			switch (msg.type) {
			case MessageIOOut::TYPE_OUTB:
				_device.ioport->outb(uint16(msg.port), uint8(msg.value));
				break;
			case MessageIOOut::TYPE_OUTW:
				_device.ioport->outw(uint16(msg.port), uint16(msg.value));
				break;
			case MessageIOOut::TYPE_OUTL:
				_device.ioport->outl(uint16(msg.port), uint32(msg.value));
				break;
			};

			return true;
		}

		bool receive(MessageIrqNotify &msg)
		{
			Genode::Mutex::Guard guard(_mutex);

			if ((msg.mask & 0x20) || (msg.mask & (1 << 10)))
				error("---- ", msg.baseirq, " mask=", Hex(msg.mask), " ", Thread::myself()->name);

			if (msg.mask & (1 << 5)) {
				if (_device.irq.constructed())
					_device.irq->ack_irq();

				return true;
			}

			return false;
		}

		bool receive(MessageHwPciConfig &msg)
		{
			using namespace Genode;

			auto const offset   = ((msg.bdf >> 8) & 0xff) * 256
			                    + ((msg.bdf >> 3) & 0x1f) * 8
			                    + ((msg.bdf >> 0) & 0x07);
			auto const cfg_addr = _bdf_base + 0x1000u * offset;

			if (msg.type != MessageHwPciConfig::Type::TYPE_READ &&
			    msg.type != MessageHwPciConfig::Type::TYPE_WRITE) {

				_dump_msg(msg);
				error("hwpci: unsupported operation by now ",
				      msg.type == MessageHwPciConfig::Type::TYPE_READ  ? "read"  :
				                  MessageHwPciConfig::Type::TYPE_WRITE ? "write" :
				                  MessageHwPciConfig::Type::TYPE_PTR   ? "ptr"
				                                                       : "unknown");
				return true;
			}

			if (msg.dword >= 1024)
				return false;

			/* XXX only let through bdf 0:1.0 - network device in Qemu */
			if (msg.bdf != 8)
				return false;

			Genode::Mutex::Guard guard(_mutex);
		
			try {
				Attached_io_mem_dataspace cfg(_env, cfg_addr, 0x1000 /* size */);

				auto const access_cfg = cfg.local_addr<char>() + msg.dword * 4;
				if (msg.type == MessageHwPciConfig::Type::TYPE_READ)
					msg.value = *reinterpret_cast<unsigned *>(access_cfg);
				else
				if (msg.type == MessageHwPciConfig::Type::TYPE_WRITE)
					*reinterpret_cast<unsigned *>(access_cfg) = msg.value;

//				if (msg.type == MessageHwPciConfig::Type::TYPE_READ && msg.dword == 0xf)
//					msg.value = (msg.value & 0xffff0000) | (1u << 8) | 5;

				if (msg.value != ~0u)
					_dump_msg(msg);
			} catch (...) {
				_dump_msg(msg);
				error("exception ...");
				return false;
			}

			return true;
		}

		bool receive(MessageHostOp &msg)
		{
			using namespace Genode;

			switch (msg.type) {
			default: return false;
			case MessageHostOp::OP_ASSIGN_PCI:
			case MessageHostOp::OP_ATTACH_IRQ:
			case MessageHostOp::OP_ALLOC_IOIO_REGION:
			case MessageHostOp::OP_ATTACH_PCI_IOMEM:
				break;
			}

			Genode::Mutex::Guard guard(_mutex);

			switch (msg.type) {
			default: return false;
			case MessageHostOp::OP_ASSIGN_PCI:
				error("op assign pci bdf=", Hex(msg.value), " parent=", Hex(msg.len));
				break;
			case MessageHostOp::OP_ATTACH_IRQ: {
				error("op assign irq ", Hex(msg.value), " lock=", msg.len, " cpu=", Hex(msg.cpu));
				_device.irq_nr = char(msg.value);
				_device.irq.construct(_env, msg.value);
				_device.irq->sigh(_device.handler);
				break;
			}
			case MessageHostOp::OP_ALLOC_IOIO_REGION: {
				_device.ioport_range = { .base = uint32(0xffffu & (msg.value >> 8)),
				                         .size = uint32(0x00ffu &  msg.value) };
				error("op attach alloc ioio region ", Hex(msg.value),
				      " port=", Hex(_device.ioport_range.base),
				      " size=", Hex(_device.ioport_range.size));
				_device.ioport.construct(_env, _device.ioport_range.base,
				                               _device.ioport_range.size);
				break;
			}
			case MessageHostOp::OP_ATTACH_PCI_IOMEM:
				error("op assign iomem host=", Hex(msg.value), " len=", msg.len, " -> guest=", Hex(msg.cpu));

				if (_device.iomem.constructed())
					break;

				addr_t attach_at { };

				/*
				 * Remove previously allocated RAM dataspace of pcidirect.
				 */
				auto ok = _gmem.remove_region(msg.value, [&](auto region) {
					attach_at = region->_local_addr;
					_env.rm().detach(attach_at);
					_env.ram().free(static_cap_cast<Ram_dataspace>(region->_ds));
					destroy(_heap, region);
				});

				if (!ok) {
					error("hwpci: removing previous range failed");
					return false;
				}

				/*
				 * Get access to host I/O memory and attach to guest space and
				 * attach to VMM space at <attach_at>.
				 */
				_device.iomem.construct(_env, msg.value, msg.len);

				auto const ds = _device.iomem->dataspace();

				if (!ds.valid()) {
					error("hwpci: attach iomem failed, invalid range");
					return false;
				}

				auto guest_phys = msg.cpu; /* XXX fix naming, e.g. union */

				/* add to guest memory tracking */
				_gmem.add_region(_heap, guest_phys, msg.value, ds, msg.len);

				/* re-attach at same VMM virtual address, to avoid trouble */
				_env.rm().attach(ds, {
					.size       = { },   .offset    = { },
					.use_at     = true,  .at        = attach_at,
					.executable = { },   .writeable = true,
				}).with_result(
					[&] (Env::Local_rm::Attachment &a) {
						a.deallocate = false; },
					[&] (Region_map::Attach_error) {
						error("io mem attach failed"); });

				break;
			}

			return true;
		}

		bool receive(MessageAcpi &msg)
		{
			using namespace Genode;

//			Genode::Mutex::Guard guard(_mutex);

//			error("MessageAcpi - implement me - type=", unsigned(msg.type), " ", msg.pin);

//			msg.gsi = 5; /* XXX */

			return false;
		}

};
