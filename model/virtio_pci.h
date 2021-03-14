/**
 * Virtio PCI device
 *
 * Copyright (C) 2021-2022, Alexander Boettcher
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

#pragma once

#include "nul/motherboard.h"
#include "model/pci.h"

#include "virtio_queue.h"


namespace Virtio {
	struct Pci_cap;
	class  Device;
}

struct Virtio::Pci_cap
{
	struct layout
	{
		enum { VENDOR = 9 };
		enum Types { COMMON_CFG = 1, NOTIFY_CFG = 2, ISR_CFG = 3, DEVICE_CFG = 4 };

		union {
			struct {
				uint8 id;
				uint8 next;
				uint8 len;
				uint8 type;
			};
			struct { uint32 value; };
		};
	};

	unsigned bar;
	unsigned offset;
	unsigned length;
	layout   cap { };

	Pci_cap(uint8 next_cap, layout::Types type, unsigned bar,
	        unsigned offset, unsigned length)
	:
		bar(bar), offset(offset), length(length)
	{
		cap.id   = layout::VENDOR;
		cap.next = next_cap;
		cap.len  = 4 * 4; /* e.g. [0x40 - 0x50) */
		cap.type = type;
	 }

	unsigned read(unsigned const off) const
	{
		switch (off) {
		case 0:
			return cap.value;
		case 4:
			return bar;
		case 8:
			return offset;
		case 12:
			return length;
		default:
			return ~0U;
		}
	}
};

class Virtio::Device
{
	protected:

		DBus<MessageIrqLines>  &_bus_irqlines;
		DBus<MessageMemRegion> &_bus_memregion;

		/* isr, config, notify structures are part of the first BAR MEM by now */
		enum {
			BAR_ID                = 0,
			BAR_OFFSET_ISR        = 0x200,
			BAR_OFFSET_CONFIG     = 0x400,
			BAR_OFFSET_NOTIFY     = 0x600,
			RANGE_SIZE            = 0x200,
			NOTIFY_OFF_MULTIPLIER = 8, /* for 64bit wide access */
		};

		uint8  const _irq;
		uint8  const _device_type;
		uint16 const _bdf;

		uint32       _control   { 0 };
		uint32       _status    { 1u << 20 /* cap list support */ };

		uint64 const _phys_bar_base;
		uint16 const _phys_bar_size { 0x1000u };

		bool         _bar0_size  { false };
		bool         _all_notify { false };
		bool         _verbose    { false };

		uint32       _feature_select  { 0 };
		uint32       _device_status   { 0 };
		uint32       _queue_select    { 0 };

		enum { QUEUES_NUM = 2, VIRTIO_QUEUE_SIZE = 512 };

		struct Queue {
			unsigned queue_size    { VIRTIO_QUEUE_SIZE };
			uint32   phys_addr [6] { };

			uint64 descriptor_area() {
				return  uint64(phys_addr[0]) |
				       (uint64(phys_addr[1]) << 32); }

			uint64 driver_area() {
				return  uint64(phys_addr[2]) |
				       (uint64(phys_addr[3]) << 32); }

			uint64 device_area() {
				return  uint64(phys_addr[4]) |
				       (uint64(phys_addr[5]) << 32); }

			Virtio::Queue queue { };

		} _queues[QUEUES_NUM];

		Queue &queue () { return _queues[_queue_select]; }

		void enable_queue()
		{
			uintptr_t des_base = vmm_address(queue().descriptor_area(),
			                                 2 * queue().queue_size);
			uintptr_t dev_base = vmm_address(queue().device_area(),
			                                 6 + 8 * queue().queue_size);
			uintptr_t drv_base = vmm_address(queue().driver_area(),
			                                 6 + 2 * queue().queue_size);
			if (!des_base || !dev_base || !drv_base) {
				Logging::panic("virtio queue construction failed\n");
			}

			queue().queue.init(des_base, dev_base, drv_base, queue().queue_size);
		}

		uintptr_t vmm_address(uint64 const req_addr, size_t const req_size)
		{
			MessageMemRegion mem_region(req_addr >> 12);
			if (!_bus_memregion.send(mem_region, false) || !mem_region.ptr) {
				Logging::printf("Invalid guest physical address %llx+%lx\n",
				                req_addr, req_size);
				return 0;
			}

			uint64 const guest_base = mem_region.start_page << 12;
			size_t const guest_size = mem_region.count << 12;

			if ((req_addr <  guest_base) ||
			    (req_addr >= guest_base + guest_size) ||
			    req_size == 0 || req_addr + req_size >= guest_base + guest_size) {
				Logging::printf("Invalid guest physical address %llx+%lx\n",
				                req_addr, req_size);
				return 0;
			}

			return uintptr_t(mem_region.ptr) + (req_addr - guest_base);
		}

		virtual void notify (unsigned) = 0;

		~Device();

	public:

		Device(DBus<MessageIrqLines>  &bus_irqlines,
		       DBus<MessageMemRegion> &bus_memregion,
		       unsigned char irq, unsigned short bdf,
		       uint8 device_type, uint64 phys_bar_base)
		:
			_bus_irqlines(bus_irqlines),
			_bus_memregion(bus_memregion),
			_irq(irq), _device_type(device_type), _bdf(bdf),
			_phys_bar_base(phys_bar_base)
		{ }

		void reset()
		{
			_control = 0u;
			_status  = 1u << 20 /* cap list support */;

			_bar0_size  = false;
			_all_notify = false;

			_feature_select = 0u;
			_device_status  = 0u;
			_queue_select   = 0u;

			for (unsigned i = 0; i < QUEUES_NUM; i++) {
				_queues[i].queue_size = VIRTIO_QUEUE_SIZE;
				for (unsigned j = 0; j < sizeof(_queues)/sizeof(_queues[0]); j++)
					_queues[i].phys_addr[j] = 0u;
				_queues[i].queue = { };
			}
		}

		bool receive(MessagePciConfig &msg)
		{
			if (msg.bdf != _bdf) return false;

			bool show = _verbose;

			switch (msg.type) {
			case MessagePciConfig::TYPE_READ:
				switch (msg.dword*4) {
				case 0x00: /* device & vendor (redhat) */
					msg.value = ((0x1040u + _device_type) << 16) | 0x1AF4u;
					break;
				case 0x04: /* status & control */
					msg.value = _status | _control;
					break;
				case 0x08: /* class code (input), sub class (mouse), prog if, rev. id */
					msg.value = 0x09020000;
					break;
				case 0x0c: /* bist, header type, lat timer, cache */
					msg.value = 0u;
					break;
				case 0x10: /* BAR 0 */
					if (_bar0_size) {
						msg.value = _phys_bar_size;
						_bar0_size = false;
					} else
						msg.value = _phys_bar_base;
					break;
				case 0x34: /* capability pointer */
					msg.value = 0x40;
					break;
				case 0x3c: /* max lat, min grant, intr pin, intr line */
					msg.value = 0x100u | unsigned(_irq);
					break;
				case 0x40 ... 0x4c:
				{
					Pci_cap cap(0x50, Pci_cap::layout::Types::COMMON_CFG,
					            BAR_ID, 0, RANGE_SIZE);
					msg.value = cap.read(msg.dword * 4 - 0x40);
					break;
				}
				case 0x50 ... 0x5c:
				{
					Pci_cap cap(0x60, Pci_cap::layout::Types::ISR_CFG,
					            BAR_ID, BAR_OFFSET_ISR, RANGE_SIZE);
					msg.value = cap.read(msg.dword * 4 - 0x50);
					break;
				}
				case 0x60 ... 0x6c:
				{
					Pci_cap cap(0x70, Pci_cap::layout::Types::DEVICE_CFG,
					            BAR_ID, BAR_OFFSET_CONFIG, RANGE_SIZE);
					msg.value = cap.read(msg.dword * 4 - 0x60);
					break;
				}
				case 0x70 ... 0x7c:
				{
					Pci_cap cap(0x00, Pci_cap::layout::Types::NOTIFY_CFG,
					            BAR_ID, BAR_OFFSET_NOTIFY, RANGE_SIZE);
					msg.value = cap.read(msg.dword * 4 - 0x70);
					break;
				}
				case 0x80: /* part of NOTIFY_CFG -> notify_off_multiplier */
					msg.value = _all_notify ? 0 : NOTIFY_OFF_MULTIPLIER;
					break;
				default:
					break;
				}

				if (show)
					Logging::printf("virtio PCI read  %x -> %x\n",
					                msg.dword * 4, msg.value);
				break;
			case MessagePciConfig::TYPE_WRITE:
				switch (msg.dword*4) {
				case 0x4: /* status & control */
					_control = msg.value & ((1u << 11) - 1);
					break;
				case 0x10: /* BAR0 */
					if (msg.value == ~0U)
						_bar0_size = true;
					break;
				}

				if (show)
					Logging::printf("virtio PCI write %x <- %x\n",
					                msg.dword * 4, msg.value);
				break;
			case MessagePciConfig::TYPE_PTR:
				Logging::printf("virtio PCI ptr unsupported !\n");
				break;
			}
			return true;
		}

		bool receive(MessageMem &msg)
		{
			if (msg.phys < _phys_bar_base || _phys_bar_base + _phys_bar_size <= msg.phys)
				return false;

			unsigned const offset = msg.phys - _phys_bar_base;

			/* debug start */
			bool show = _verbose;

			switch (offset) {
			case BAR_OFFSET_CONFIG ... BAR_OFFSET_CONFIG + RANGE_SIZE - 1:
				show = false;
				break;
			}
			/* debug end */

			if (!msg.read && show)
				Logging::printf("Messagemem %lx status %x %s msg.value=%x\n",
				                msg.phys, _control & 2,
				                msg.read ? "read" : "write", *msg.ptr);

			/* ignore memory requests if disabled */
			if (!(_control & 2))
				return false;

			if (!msg.ptr) {
				Logging::printf("%s invalid pointer\n", __FILE__);
				return false;
			}

			switch (offset) {
			case 0x0: /* feature select */
				if (msg.read)
					*msg.ptr = _feature_select;
				else
					_feature_select = *msg.ptr;
				break;
			case 0x4: /* features */
				if (msg.read) {
					if (_feature_select == 1)
						*msg.ptr = 1; /* VIRTIO_F_VERSION_1 (32) */
					else
						*msg.ptr = 0;
				}
				break;
			case 0x10: /* msix config, num queues */
				if (msg.read)
					*msg.ptr = unsigned(QUEUES_NUM) << 16;
				break;
			case 0x14: /* device status (1), config gen (1), queue select (2) */
				if (msg.read)
					*msg.ptr = _device_status | (_queue_select << 16);
				else {
					if ((*msg.ptr >> 16) < QUEUES_NUM) {
						_device_status = *msg.ptr & 0xff;
						_queue_select  = *msg.ptr >> 16;
					} else
						Logging::printf("virtio, queue select out of range");
				}
				break;
			case 0x18: /* queue size, queue msix vector */
				if (msg.read)
					*msg.ptr = queue().queue_size;
				else {
					if (*msg.ptr && ((*msg.ptr & 0xfffful) <= VIRTIO_QUEUE_SIZE))
						queue().queue_size = *msg.ptr & 0xfffful;
				}
				break;
			case 0x1c: /* queue enable, queue notify offset */
				if (msg.read) {
					unsigned notify_offset = ((_queue_select + 1) << 16);
					/* (notify_offset >> 16) * notify_off_multiplier -> notify PCI exit */
					*msg.ptr = notify_offset | (queue().queue.enabled() ? 1u : 0u);
				} else {
					bool enable = !!(*msg.ptr & 0x1);
					if (!enable && queue().queue.enabled())
						Logging::printf("unsupported queue disable !!!");

					if (enable && !queue().queue.enabled())
						enable_queue();
				}
				break;
			case 0x20 ... 0x34: /* descriptor/driver/device area */
				if (msg.read)
					*msg.ptr = queue().phys_addr[(offset - 0x20) / 4];
				else
					queue().phys_addr[(offset - 0x20) / 4] = *msg.ptr;
				break;
			/* notify_offset intentional start with 1 and not 0, so not using -1 here for the range */
			case BAR_OFFSET_NOTIFY ... BAR_OFFSET_NOTIFY + QUEUES_NUM * NOTIFY_OFF_MULTIPLIER:
			{
				if (msg.read) {
					*msg.ptr = 0;
					break;
				}

				/* index = [0 - QUEUES_NUM] - 0 is invalid, used as index - 1 */
				unsigned queue_index = (offset - BAR_OFFSET_NOTIFY) / NOTIFY_OFF_MULTIPLIER;

				if ((queue_index == 0) || queue_index > QUEUES_NUM) {
					for (unsigned i = 0; i < QUEUES_NUM; i++) {
						if (_queues[i].queue.enabled())
							notify(i);
					}
				} else
					if (_queues[queue_index - 1].queue.enabled())
						notify(queue_index - 1);
				break;
			}
			case BAR_OFFSET_ISR:
				if (msg.read) {
					if (_status & (1 << 3)) {
						/* 0x1 - queue interrupt, 0x2 configuration change (not supported by now) */
						*msg.ptr = 0x1; /* read clears & deassert IRQ */
						_status &= ~(1u << 3);

						MessageIrqLines msg(MessageIrq::DEASSERT_IRQ, _irq);
						_bus_irqlines.send(msg);
					}
					else
						*msg.ptr = 0;
				}
				break;
			}

			if (msg.read && show)
				Logging::printf("Messagemem %lx status %x %s msg.value=%x\n",
				                msg.phys, _control & 2,
				                msg.read ? "read " : "write", *msg.ptr);
			return true;
		}

		void inject_irq()
		{
			_status |= 1u << 3; /* not required for MSI-X XXX */

			MessageIrqLines msg_irq(MessageIrq::ASSERT_IRQ, _irq);
			_bus_irqlines.send(msg_irq);
		}
};
