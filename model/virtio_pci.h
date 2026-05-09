/**
 * Virtio PCI device
 *
 * Copyright (C) 2021-2026, Alexander Boettcher
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
	struct Vendor_cap;
	struct Power_cap;
	struct Msix_cap;
	class  Device;
}

struct Virtio::Vendor_cap
{
	struct Layout
	{
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

	unsigned const bar;
	unsigned const offset;
	unsigned const length;
	Layout         cap { };

	Vendor_cap(uint8 next_cap, Layout::Types type,
	           unsigned bar, unsigned offset, unsigned length)
	:
		bar(bar), offset(offset), length(length)
	{
		enum { VENDOR = 9 };

		cap.id   = VENDOR;
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

struct Virtio::Power_cap
{
	struct {
		union {
			struct {
				uint8 id;
				uint8 next;
				uint8 len;
				uint8 unused;
			};
			struct { uint32 value; };
		};
	} cap { };

	unsigned rest { };

	Power_cap(uint8 next_cap)
	{
		cap.id   = 1; /* POWER */;
		cap.next = next_cap;
		cap.len  = 8;
	}

	unsigned read(unsigned const off) const
	{
		switch (off) {
		case 0:
			return cap.value;
		case 4:
			return rest;
		default:
			return ~0U;
		}
	}

	void write(unsigned const off, unsigned value)
	{
		switch (off) {
		case 4:
			rest = value; break;
		default:
			break;
		}
	}
};

struct Virtio::Msix_cap
{
	struct {
		union {
			struct {
				uint8  id;
				uint8  next;
				union {
					struct {
						uint16 table_size : 11;
						uint16 unused     :  3;
						uint16 func_mask  :  1;
						uint16 enable     :  1;
					};
					struct { uint16 val; };
				};
			};
			struct { uint32 value; };
		};
	} cap { };

	uint32 const table_bar;
	uint32 const table_offset;
	uint32 const pending_bar;
	uint32 const pending_offset;

	enum   { MAX_MSIX = 6 };
	struct { uint32 low, high, data, ctrl; } msix_table [MAX_MSIX] { };

	Msix_cap(uint8 next_cap, uint32 table_bar, uint32 table_offset,
	         uint32 pending_bar, uint32 pending_offset, uint8 max_queues)
	:
	  table_bar(table_bar), table_offset(table_offset),
	  pending_bar(pending_bar), pending_offset(pending_offset)
	{
		cap.id         = 0x11 /* MSIX */;
		cap.next       = next_cap;
		cap.table_size = max_queues > MAX_MSIX
		               ? uint8(MAX_MSIX - 1) : uint8(max_queues - 1);

		static_assert(sizeof(cap) == 4, "cap size mismatch");
	}

	unsigned read(unsigned const off) const
	{
		switch (off) {
		case 0:
			return cap.value;
		case 4:
			return (table_offset   & ~0x7u) | (table_bar & 0x7);
		case 8:
			return (pending_offset & ~0x7u) | (pending_bar & 0x7);
		default:
			return ~0U;
		}
	}

	void write(unsigned const off, unsigned const value)
	{
		switch (off) {
		case 0:
			cap.value |= value & (0x3u << 30);
			break;
		default:
			Logging::printf("msix ? write off %u <- %x\n", off, value);
			break;
		}
	}
};

class Virtio::Device
{
	protected:

		DBus<MessageIrqLines>  &_bus_irqlines;
		DBus<MessageMem>       &_bus_mem;
		DBus<MessageMemRegion> &_bus_memregion;

		/* isr, config, notify structures are part of the first BAR MEM by now */
		enum {
			BAR_ID                = 0,
			BAR_OFFSET_ISR        = 0x200,
			BAR_OFFSET_CONFIG     = 0x400,
			BAR_OFFSET_NOTIFY     = 0x600,

			BAR_OFFSET_MSIX_TABLE   = 0x800,
			BAR_OFFSET_MSIX_PENDING = 0x900,

			RANGE_SIZE            = 0x200,
			NOTIFY_OFF_MULTIPLIER = 8, /* for 64bit wide access */

			PHYS_BAR_SIZE = 0x1000,

			QUEUES_MAX = 4,

			VIRTIO_QUEUE_SIZE = 512
		};

		uint8  const _pin;
		uint8  const _irq;
		uint8  const _device_type;
		uint8  const _queues_count;
		uint32 const _pci_type; /* class code, sub class, prog if, rev. id */

		uint64       _phys_bar_base;

		uint16 const _bdf;
		bool         _bar0_size      { false };
		bool         _all_notify     { false };
		bool         _verbose        { false };
		bool         _config_changed { false };

		bool         _msi_support;
		bool         _assert_irq   { };
		bool         _deassert_irq { };

		uint32       _control   { 0 };
		uint32       _status    { 1u << 4 /* cap list support */ };

		uint32       _dev_feature_word  { 0 };
		uint32       _drv_feature_word  { 0 };
		uint8        _device_status     { 0 };
		uint8        _config_generation { 0 };
		uint16       _queue_select      { 0 };

		Power_cap    _power      { 0x50 };
		Msix_cap     _msix_state { 0x00, BAR_ID, BAR_OFFSET_MSIX_TABLE,
		                                 BAR_ID, BAR_OFFSET_MSIX_PENDING,
		                                 _queues_count };

		struct Queue {
			uint16 queue_size         { VIRTIO_QUEUE_SIZE };
			uint16 msix_vector_config { };
			uint16 msix_vector_queue  { };
			uint32 phys_addr [6]      { };

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

		} _queues[QUEUES_MAX];

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
			MessageMemRegion mem_region(uintptr_t(req_addr >> 12));
			if (!_bus_memregion.send(mem_region, false) || !mem_region.ptr) {
				Logging::printf("Invalid guest physical address A %llx+%lx\n",
				                req_addr, req_size);
				return 0;
			}

			uint64 const guest_base = mem_region.start_page << 12;
			size_t const guest_size = mem_region.count << 12;

			if ((req_addr <  guest_base) ||
			    (req_addr >= guest_base + guest_size) ||
			    req_size == 0 || req_addr + req_size >= guest_base + guest_size) {
				Logging::printf("Invalid guest physical address B %llx+%lx\n",
				                req_addr, req_size);
				return 0;
			}

			return uintptr_t(mem_region.ptr) + uintptr_t(req_addr - guest_base);
		}

		virtual void notify      (unsigned) = 0;
		virtual void notify_power(unsigned) = 0;

		virtual uint32 dev_feature     (unsigned)         = 0;
		virtual void   drv_feature_ack (unsigned, uint32) = 0;
		virtual uint32 drv_feature     (unsigned)         = 0;

		~Device() { }

		uint8 config_generation()
		{

			if (_config_changed) {
				_config_generation ++;
				_config_changed = false;
			}

			return _config_generation;
		}

		void config_changed() { _config_changed = true; }

		bool sync_and_irq(auto &lock, auto const &fn)
		{
			bool assert_irq   = false;
			bool deassert_irq = false;
			bool result       = false;

			{
				Seoul::Lock::Guard guard(lock);

				result = fn();

				assert_irq   = _assert_irq;
				deassert_irq = _deassert_irq;

				_assert_irq = _deassert_irq = false;
			}

			assert_irqs(assert_irq, deassert_irq);

			return result;
		}

		bool intx_disabled() const { return !!(_control & (1 << 10)); }

	public:

		Device(DBus<MessageIrqLines>  &bus_irqlines,
		       DBus<MessageMem>       &bus_mem,
		       DBus<MessageMemRegion> &bus_memregion,
		       uint8  const pin,           uint8  const irq, uint16 const bdf,
		       uint8  const device_type,   uint32 const pci_class,
		       uint64 const phys_bar_base, uint8  const queues_count,
		       bool   const msi)
		:
			_bus_irqlines(bus_irqlines), _bus_mem(bus_mem),
			_bus_memregion(bus_memregion),
			_pin(pin), _irq(irq), _device_type(device_type),
			_queues_count(queues_count), _pci_type(pci_class),
			_phys_bar_base(phys_bar_base), _bdf(bdf), _msi_support(msi)
		{ }

		void reset()
		{
			_control = 0u;
			_status  = 1u << 4 /* cap list support */;

			_bar0_size      = false;
			_all_notify     = false;
			_config_changed = false;

			_assert_irq     = false;
			_deassert_irq   = false;

			_dev_feature_word  = 0u;
			_drv_feature_word  = 0u;
			_device_status     = 0u;
			_queue_select      = 0u;
			_config_generation = 0u;

			reset_queues();
		}

		void reset_queues()
		{
			for (auto &queue : _queues) {
				queue.queue_size = VIRTIO_QUEUE_SIZE;
				for (auto &phys : queue.phys_addr) phys = 0u;
				queue.queue = { };
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
					msg.value = (_status << 16) | _control;
					break;
				case 0x08: /* class code, sub class, prog if, rev. id */
					msg.value = _pci_type;
					break;
				case 0x0c: /* bist, header type, lat timer, cache */
					msg.value = 0u;
					break;
				case 0x10: /* BAR 0 */
					if (_bar0_size) {
						msg.value = PHYS_BAR_SIZE;
						_bar0_size = false;
					} else {
						if (_phys_bar_base >= (1ull << 32))
							Logging::panic("phys bar addr too large\n");
						msg.value = unsigned(_phys_bar_base);
					}
					break;
				case 0x2c: /* subsystem ID, system vendor ID */
					msg.value = 0x11101AF4u;
					break;
				case 0x34: /* capability pointer */
					msg.value = 0x40;
					break;
				case 0x3c: /* max lat, min grant, intr pin, intr line */
					msg.value = (unsigned(_pin) << 8) | unsigned(_irq);
					break;
				case 0x40 ... 0x44:
					msg.value = _power.read(msg.dword * 4 - 0x40);
					break;
				case 0x50 ... 0x5c:
				{
					Vendor_cap cap(0x60, Vendor_cap::Layout::Types::COMMON_CFG,
					               BAR_ID, 0, RANGE_SIZE);
					msg.value = cap.read(msg.dword * 4 - 0x50);
					break;
				}
				case 0x60 ... 0x6c:
				{
					Vendor_cap cap(0x70, Vendor_cap::Layout::Types::ISR_CFG,
					               BAR_ID, BAR_OFFSET_ISR, RANGE_SIZE);
					msg.value = cap.read(msg.dword * 4 - 0x60);
					break;
				}
				case 0x70 ... 0x7c:
				{
					Vendor_cap cap(0x80, Vendor_cap::Layout::Types::DEVICE_CFG,
					               BAR_ID, BAR_OFFSET_CONFIG, RANGE_SIZE);
					msg.value = cap.read(msg.dword * 4 - 0x70);
					break;
				}
				case 0x80 ... 0x8c:
				{
					Vendor_cap cap(_msi_support ? 0xa0 : 0x00,
					               Vendor_cap::Layout::Types::NOTIFY_CFG,
					               BAR_ID, BAR_OFFSET_NOTIFY, RANGE_SIZE);
					msg.value = cap.read(msg.dword * 4 - 0x80);
					break;
				}
				case 0x90: /* part of NOTIFY_CFG -> notify_off_multiplier */
					msg.value = _all_notify ? 0 : NOTIFY_OFF_MULTIPLIER;
					break;
				case 0xa0 ... 0xbc:
					msg.value = _msix_state.read(msg.dword * 4 - 0xa0);
					break;
				default:
					msg.value = 0;
					break;
				}

				if (show)
					Logging::printf("%x:%x.%u virtio PCI read  %x -> %x\n",
					                (_bdf >> 8) & 0xff, (_bdf >> 3) & 0x1f,
					                _bdf & 0x7, msg.dword * 4, msg.value);
				break;
			case MessagePciConfig::TYPE_WRITE:
				switch (msg.dword*4) {
				case 0x4: /* status & control */
					_control = msg.value & ((1u << 11) - 1);
					break;
				case 0x10: /* BAR0 */
					if (msg.value == ~0U)
						_bar0_size = true;
					else
						_phys_bar_base = msg.value;
					break;
				case 0x44: /* PCI power cap, 2nd half */
					_power.write(4, msg.value);
					notify_power(_power.read(4));
					break;
				case 0xa0 ... 0xbc:
					_msix_state.write(msg.dword * 4 - 0xa0, msg.value);
					break;
				default:
					break;
				}

				if (show)
					Logging::printf("%x:%x.%u virtio PCI write %x <- %x\n",
					                (_bdf >> 8) & 0xff, (_bdf >> 3) & 0x1f,
					                _bdf & 0x7, msg.dword * 4, msg.value);
				break;
			case MessagePciConfig::TYPE_PTR:
				Logging::printf("virtio PCI ptr unsupported !\n");
				break;
			}
			return true;
		}

		bool receive(MessageMem &msg)
		{
			if (msg.phys < _phys_bar_base || _phys_bar_base + PHYS_BAR_SIZE <= msg.phys)
				return false;

			auto const offset = msg.phys - _phys_bar_base;

			/* debug start */
			bool show = _verbose;

			switch (offset) {
			case BAR_OFFSET_CONFIG ... BAR_OFFSET_CONFIG + RANGE_SIZE - 1:
				show = false;
				break;
			}
			/* debug end */

			if (!msg.read && show)
				Logging::printf("Messagemem %llx status %x %s msg.value=%x\n",
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
			case 0x0: /* device feature word select */
				if (msg.read)
					*msg.ptr = _dev_feature_word;
				else
					_dev_feature_word = *msg.ptr;
				break;
			case 0x4: /* device feature word value */
				if (msg.read) {
					*msg.ptr = dev_feature(_dev_feature_word);
					if (_dev_feature_word == 1)
						*msg.ptr |= 1u; /* VIRTIO_F_VERSION_1 (32) */
				}
				break;
			case 0x8: /* driver feature word select */
				if (msg.read)
					*msg.ptr = _drv_feature_word;
				else
					_drv_feature_word = *msg.ptr;
				break;
			case 0xc: /* driver feature word value */
				if (msg.read)
					*msg.ptr = drv_feature(_drv_feature_word);
				else
					drv_feature_ack(_drv_feature_word, *msg.ptr);
				break;
			case 0x10: /* msix config, num queues */
				if (msg.read)
					*msg.ptr = (unsigned(_queues_count) << 16)
					         | (unsigned(queue().msix_vector_config));
				else
					queue().msix_vector_config = uint16(*msg.ptr);
				break;
			case 0x14: /* device status (1), config gen (1), queue select (2) */
				if (msg.read)
					*msg.ptr = (unsigned(_device_status)) |
					           (unsigned(config_generation()) << 8) |
					           (unsigned(_queue_select) << 16);
				else {
					if ((*msg.ptr >> 16) < _queues_count) {
						_device_status = *msg.ptr & 0xff;
						_queue_select  = uint16(*msg.ptr >> 16);
					} else
						Logging::printf("virtio, queue select out of range\n");
				}
				break;
			case 0x18: /* queue size, queue msix vector */
				if (msg.read)
					*msg.ptr = (uint32(queue().msix_vector_queue) << 16)
					         | queue().queue_size;
				else {
					if (*msg.ptr) {
						if ((*msg.ptr & 0xfffful) <= VIRTIO_QUEUE_SIZE)
							queue().queue_size = *msg.ptr & 0xfffful;
						queue().msix_vector_queue = uint16(*msg.ptr >> 16);
					}
				}
				break;
			case 0x1c: /* queue enable, queue notify offset */
				if (msg.read) {
					unsigned notify_offset = ((unsigned(_queue_select) + 1) << 16);
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
			case BAR_OFFSET_NOTIFY ... BAR_OFFSET_NOTIFY + QUEUES_MAX * NOTIFY_OFF_MULTIPLIER:
			{
				if (msg.read) {
					*msg.ptr = 0;
					break;
				}

				/* index = [0 - QUEUES_MAX] - 0 is invalid, used as index - 1 */
				unsigned queue_index = unsigned((offset - BAR_OFFSET_NOTIFY) / NOTIFY_OFF_MULTIPLIER);

				if ((queue_index == 0) || queue_index > _queues_count) {
					for (unsigned i = 0; i < _queues_count; i++) {
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
						/* 0x1 - queue interrupt, 0x2 configuration change */
						*msg.ptr = (_config_changed ? 0x2 : 0x1);

						/* read clears & deassert IRQ */
						_status &= ~(1u << 3);

						_deassert_irq = true;
					}
					else
						*msg.ptr = 0;
				}
				break;
			case BAR_OFFSET_MSIX_TABLE ... BAR_OFFSET_MSIX_TABLE +
			                               (RANGE_SIZE / 2) - 4:
			{
				auto const row = (offset - BAR_OFFSET_MSIX_TABLE) / (4*4);
				auto const off = (offset - BAR_OFFSET_MSIX_TABLE) % (4*4);
				if (row >= Msix_cap::MAX_MSIX) {
					*msg.ptr = ~0u;
					break;
				}

				if (msg.read) {
					switch (off) {
					case  0: *msg.ptr = _msix_state.msix_table[row].low;  break;
					case  4: *msg.ptr = _msix_state.msix_table[row].high; break;
					case  8: *msg.ptr = _msix_state.msix_table[row].data; break;
					case 12: *msg.ptr = _msix_state.msix_table[row].ctrl; break;
					default: *msg.ptr = ~0u; break;
					}
				} else {
					switch (off) {
					case  0: _msix_state.msix_table[row].low  = *msg.ptr; break;
					case  4: _msix_state.msix_table[row].high = *msg.ptr; break;
					case  8: _msix_state.msix_table[row].data = *msg.ptr; break;
					case 12: _msix_state.msix_table[row].ctrl = *msg.ptr; break;
					default: break;
					}
				}
				if (_verbose)
					Logging::printf("msix offset bar %x row=%u value=%x- %s\n",
					                offset, row, *msg.ptr, msg.read ? "read" : "write");
				break;
			}
			case BAR_OFFSET_MSIX_PENDING ... BAR_OFFSET_MSIX_PENDING +
			                                 (RANGE_SIZE / 2) - 4:
				Logging::printf("msix pending offset bar %x %s\n", offset,
				                msg.read ? "read" : "write");
			}

			if (msg.read && show)
				Logging::printf("%x:%x.%u Messagemem %llx status %x %s msg.value=%x\n",
				                (_bdf >> 8) & 0xff, (_bdf >> 3) & 0x1f,
				                _bdf & 0x7, msg.phys, _control & 2,
				                msg.read ? "read " : "write", *msg.ptr);
			return true;
		}

		void inject_irq()
		{
			_status |= 1u << 3;

			_assert_irq = true;
		}

		void assert_irqs(bool assert_irq, bool deassert_irq)
		{
			if (_msix_state.cap.enable) {
				if (!assert_irq && !deassert_irq)
					return;

				if (queue().msix_vector_queue >= Msix_cap::MAX_MSIX)
					return;

				auto &entry = _msix_state.msix_table[queue().msix_vector_queue];
				auto  addr  = (uint64(entry.high) << 32) | uint64(entry.low);

				if (!(entry.ctrl & 0x1)) {
					MessageMem msg(false /* write */, addr, &entry.data);
					_bus_mem.send(msg);
				}
				return;
			}

			if (intx_disabled())
				return;

			if (deassert_irq) {
				MessageIrqLines msg(MessageIrq::DEASSERT_IRQ, _irq);
				_bus_irqlines.send(msg);
			}

			if (assert_irq) {
				MessageIrqLines msg_irq(MessageIrq::ASSERT_IRQ, _irq);
				_bus_irqlines.send(msg_irq);
			}
		}
};
