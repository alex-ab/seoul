/**
 * Virtio template device
 *
 * - make a copy and extend it for your case, look for XXX marks
 *
 * Copyright (C) 2026, Alexander Boettcher
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

struct Virtio_template_config
{
	unsigned event { };

	unsigned read(unsigned const off) const
	{
		switch (off) {
		case 0x0:
		case 0x8:
		case 0xc:
		default:
			Logging::printf("unknown virtio pci config read %x\n", off);
			return 0;
		}
	}

	void write(unsigned const off, unsigned const value)
	{
		switch (off) {
		case 0x4:
		default:
			Logging::printf("unknown virtio pci config write %x value=%x\n",
			                off, value);
			break;
		}
	}
};


class Virtio_template: public StaticReceiver<Virtio_template>, Virtio::Device
{
	private:

		unsigned    const      _device { 0x100ff }; /* choose a device number disjunct of the already existing virtio_* XXX */
		Seoul::Lock            _lock { };
		Virtio_template_config _pci_config { };

		~Virtio_template();

	public:

		Virtio_template(DBus<MessageIrqLines>  &bus_irqlines,
		                DBus<MessageMem>       &bus_mem,
		                DBus<MessageMemRegion> &bus_memregion,
		                uint64 const bar_addr,
		                uint8  const irq_pin,
		                uint8  const irq_line,
		                uint16 const bdf,
		                bool   const msix,
		                bool   const verbose)
		:
			Virtio::Device(bus_irqlines, bus_mem, bus_memregion,
			               irq_pin, irq_line, bdf,
			               18 /* XXX virtio type */,
			               0x09020001, /* XXX set pci class, e.g. code, sub class, prog if, rev. id */
			               bar_addr,
			               2 /* number queues to be set XXX */,
			               msix)
		{ }

		bool receive(MessageBios &msg)
		{
			switch (msg.irq) {
			case BiosCommon::BIOS_RESET_VECTOR:

				Seoul::Lock::Guard guard(_lock);

				_pci_config = { };
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
						*msg.ptr = _pci_config.read(offset - BAR_OFFSET_CONFIG);
					else
						_pci_config.write(offset - BAR_OFFSET_CONFIG, *msg.ptr);

					return true;
				default:
					return Virtio::Device::receive(msg);
				}
			});
		}

		void notify (unsigned queue) override
		{
			Logging::printf("virtio_template: notify via queue %u", queue);
		}

		uint32 dev_feature     (unsigned)         override { return 0u; }
		void   drv_feature_ack (unsigned, uint32) override { }
		uint32 drv_feature     (unsigned)         override { return 0u; }

		void notify_power(unsigned value) override
		{
			Logging::printf("virtio_template: power change %x\n", value);

			/*
			 *  e.g.:
			 * _pci_config = { };
			 * reset();
			 */
		}
};

PARAM_HANDLER(virtio_template,
              "virtio_template:mem,bar,arg2,arg3,msix, verbose - attach an virtio template to the PCI bus",
              "Example: 'virtio_template:0xe0200000,4096,4096'",
              "If no bdf is given a free one is used.")
{
	unsigned const bdf = PciHelper::find_free_bdf(mb.bus_pcicfg, unsigned(argv[1]));
	if (bdf >= 1u << 16)
		Logging::panic("virtio_template: invalid bdf\n");

	auto const irq_pin  =  3; /* PCI INTC# */
	auto const irq_line = 11; /* defined by acpicontroller dsdt for INTC# */
	auto const bar_base = argv[0];

	bool const msix    = (argv[4] == ~0UL) ? true  : !!argv[4];
	bool const verbose = (argv[5] == ~0UL) ? false : !!argv[5];

	if (argv[0] == ~0UL)
		Logging::panic("virtio_template: missing bar address");

	auto dev = new Virtio_template(mb.bus_irqlines, mb.bus_mem,
	                               mb.bus_memregion,
	                               bar_base, irq_pin, irq_line,
	                               uint16(bdf), msix, verbose);

	mb.bus_pcicfg.add(dev, Virtio_template::receive_static<MessagePciConfig>);
	mb.bus_mem   .add(dev, Virtio_template::receive_static<MessageMem>);
	mb.bus_bios  .add(dev, Virtio_template::receive_static<MessageBios>);

	/* argv[...] for more arguments */

	Logging::printf("virtio template up\n");
}
