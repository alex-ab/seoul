/**
 * Virtio input device
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

#include "nul/motherboard.h"
#include "executor/bios.h"
#include "model/pci.h"

#include "virtio_pci.h"

/*
 * include/uapi/linux/input-event-codes.h
 */
enum { EV_SYNC = 0, EV_KEY = 1, EV_REL = 2, EV_ABS = 3 };

enum { REL_WHEEL = 8,        EV_REL_FEATURES =  1u << REL_WHEEL };
enum { ABS_X = 0, ABS_Y = 1, EV_ABS_FEATURES = (1u << ABS_X) | (1u << ABS_Y) };

enum { BUTTON_LEFT = 272, BUTTON_RIGHT = 273, BUTTON_MIDDLE = 274 };

struct Virtio_input_config
{
	struct reg_0
	{
		enum {
			VIRTIO_INPUT_CFG_UNSET     = 0x00,
			VIRTIO_INPUT_CFG_ID_NAME   = 0x01,
			VIRTIO_INPUT_CFG_ID_SERIAL = 0x02,
			VIRTIO_INPUT_CFG_ID_DEVIDS = 0x03,
			VIRTIO_INPUT_CFG_PROP_BITS = 0x10,
			VIRTIO_INPUT_CFG_EV_BITS   = 0x11,
			VIRTIO_INPUT_CFG_ABS_INFO  = 0x12,
		};

		union {
			struct {
				uint8 select;
				uint8 subsel;
				uint8 size;
				uint8 reserved;
			};
			struct { uint32 value; };
		};
	};

	reg_0 reg { };

	unsigned read(unsigned const off, unsigned const width,
	              unsigned const height) const
	{
		char const device_name   [] = "Virtio mouse";
		char const device_serial [] = "serial 34";
		char const device_id     [] = "42";

		switch (off) {
		case 0: {
			reg_0 result = reg;
			switch (result.select) {
			case reg_0::VIRTIO_INPUT_CFG_UNSET:
				result.size = 0; /* ? */
				break;
			case reg_0::VIRTIO_INPUT_CFG_ID_NAME:
				result.size = sizeof(device_name);
				break;
			case reg_0::VIRTIO_INPUT_CFG_ID_SERIAL:
				result.size = sizeof(device_serial);
				break;
			case reg_0::VIRTIO_INPUT_CFG_ID_DEVIDS:
				result.size = sizeof(device_id);
				break;
			case reg_0::VIRTIO_INPUT_CFG_PROP_BITS:
				/* https://www.kernel.org/doc/Documentation/input/event-codes.txt */
				result.size = 0; /* unsupported, INPUT_PROP_* - maybe _DIRECT XXX ? */
				break;
			case reg_0::VIRTIO_INPUT_CFG_EV_BITS:
				switch (reg.subsel) {
				case EV_KEY:
					result.size = 36; /* 36 byte - bitmap */
					break;
				case EV_REL:
					result.size =  2; /* 2 byte - bitmap */
					break;
				case EV_ABS:
					result.size =  1; /* 1 byte - bitmap */
					break;
				default:
					result.size =  0; /* no support */
					break;
				}
				break;
			case reg_0::VIRTIO_INPUT_CFG_ABS_INFO:
				result.size = 5 * 4; /* abs info struct */
				break;
			default:
				Logging::printf("unknown input config read %x select=%x\n",
				                off, result.select);
				result.size = 0;
				break;
			}
			return result.value;
		}
		case 8 ... 36 + 8 - 1:
		{
			unsigned const pos  = off - 8;
			unsigned len_string = 0;
			char const * string = nullptr;

			switch (reg.select) {
			case reg_0::VIRTIO_INPUT_CFG_ID_NAME:
				string     = device_name;
				len_string = sizeof(device_name);
				break;
			case reg_0::VIRTIO_INPUT_CFG_ID_SERIAL:
				string     = device_serial;
				len_string = sizeof(device_serial);
				break;
			case reg_0::VIRTIO_INPUT_CFG_ID_DEVIDS:
				string     = device_id;
				len_string = sizeof(device_id);
				break;
			case reg_0::VIRTIO_INPUT_CFG_EV_BITS:
				if (reg.subsel == EV_ABS && pos == 0)
					return EV_ABS_FEATURES; /* bitmap for ABS_* - 0 (ABS_X), 1 (ABS_Y) */
				if (reg.subsel == EV_REL && pos == 0)
					return EV_REL_FEATURES; /* bitmap for REL_* */
				if (reg.subsel == EV_KEY) {
					if (pos == BUTTON_LEFT / 32 * 4)
						return 1u << (BUTTON_LEFT   % 32) |
						       1u << (BUTTON_MIDDLE % 32) |
						       1u << (BUTTON_RIGHT  % 32);
				}
				return 0;
			case reg_0::VIRTIO_INPUT_CFG_ABS_INFO:
				if (reg.subsel == ABS_X) {
					switch (pos) {
					case 0: /* min */
						return 0;
					case 4: /* max */
						return width;
					case 8: /* fuzz */
						return 0;
					case 12: /* flat */
						return 0;
					case 16: /* res */
						return 0;
					}
				} else
				if (reg.subsel == ABS_Y) {
					switch (pos) {
					case 0: /* min */
						return 0;
					case 4: /* max */
						return height;
					case 8: /* fuzz */
						return 0;
					case 12: /* flat */
						return 0;
					case 16: /* res */
						return 0;
					}
				} else
					Logging::printf("unknown abs info %x pos=%x\n",
					                reg.subsel, pos);

				return 0;
			}

			if (len_string) {
				if (pos >= len_string)
					return 0;

				unsigned ret = 0;
				if (pos + 4 > len_string) {
					memcpy(&ret, string + pos, len_string - pos);
				} else
					memcpy(&ret, string + pos, 4);
				return ret;
			} else
				Logging::printf("unknown input config read II %x\n", off);

			return 0;
		}
		default:
			return 0;
		}
	}

	void write(unsigned const off, unsigned const value)
	{
		switch (off) {
		case 0:
			reg.value = value;
			break;
		default:
			Logging::printf("unknown input config write %x value=%x\n",
			                off, value);
			break;
		}
	}
};

class Virtio_input: public StaticReceiver<Virtio_input>, Virtio::Device
{
	private:

		unsigned const  _device { 0x10002 };

		Virtio_input_config _input_config { };

		~Virtio_input();

	public:

		unsigned width  { };
		unsigned height { };

		Virtio_input(DBus<MessageIrqLines>  &bus_irqlines,
		             DBus<MessageMemRegion> &bus_memregion,
		             unsigned char irq, unsigned short bdf)
		:
			Virtio::Device(bus_irqlines, bus_memregion, irq, bdf,
			               18, 0xf7a00000ull /* phys bar address XXX */)
		{ }

		bool receive(MessageBios &msg) {
			switch(msg.irq) {
			case BiosCommon::BIOS_RESET_VECTOR:
				_input_config = { };
				reset();
			};

			return false;
		}

		bool receive(MessagePciConfig &msg) {
			return Virtio::Device::receive(msg); }

		bool receive(MessageMem &msg)
		{
			if (msg.phys < _phys_bar_base || _phys_bar_base + _phys_bar_size <= msg.phys)
				return false;

			unsigned const offset = msg.phys - _phys_bar_base;

			switch (offset) {
			case BAR_OFFSET_CONFIG ... BAR_OFFSET_CONFIG + RANGE_SIZE - 1:
				if (msg.read)
					*msg.ptr = _input_config.read(offset - BAR_OFFSET_CONFIG,
					                              width, height);
				else
					_input_config.write(offset - BAR_OFFSET_CONFIG, *msg.ptr);

				return true;
			default:
				return Virtio::Device::receive(msg);
			}
		}

		bool button_click(MessageInput &msg)
		{
			bool const button_left   = msg.data  & (1u << 31);
			bool const button_middle = msg.data  & (1u << 30);
			bool const button_right  = msg.data  & (1u << 29);

			if (!button_left && !button_middle && !button_right)
				return false;

			enum {
				EVENT_KEY, EVENT_SYNC, EVENT_DONE
			} event_state { EVENT_KEY };

			auto button = [&] (auto const descriptor, auto)
			{
				auto const data = vmm_address(descriptor.addr, descriptor.len);
				auto const size = descriptor.len;

				if (!data || size != 8) {
					Logging::printf("could not send button click\n");
					return 0;
				}

				unsigned *event = reinterpret_cast<unsigned *>(data);

				switch (event_state) {
				case EVENT_KEY: {
					unsigned key = button_left   ? BUTTON_LEFT   :
					               button_middle ? BUTTON_MIDDLE :
					               button_right  ? BUTTON_RIGHT  : 0;
					unsigned value = button_left   ? msg.data2 & 0x1 :
					                 button_middle ? msg.data2 & 0x1 :
					                 button_right  ? msg.data2 & 0x1 : 0;
					*event = (key << 16) | unsigned (EV_KEY);
					event++;
					*event = value;
					event_state = EVENT_SYNC;
					break;
				}
				case EVENT_SYNC:
					*event = EV_SYNC;
					event++;
					*event = 0;
					event_state = EVENT_DONE;
					break;
				case EVENT_DONE:
					return 0;
				}
				return 8;
			};

			bool const inject = _queues[0].queue.consume(button);
			if (inject)
				inject_irq();

			return true;
		}

		bool scroll_wheel(MessageInput &msg)
		{
			bool const valid = msg.data & (1u << 28);

			if (!valid)
				return false;

			enum {
				EVENT_KEY, EVENT_SYNC, EVENT_DONE
			} event_state { EVENT_KEY };

			auto wheel = [&] (auto const descriptor, auto)
			{
				auto const data = vmm_address(descriptor.addr, descriptor.len);
				auto const size = descriptor.len;

				if (!data || size != 8) {
					Logging::printf("could not send wheel\n");
					return 0;
				}

				unsigned *event = reinterpret_cast<unsigned *>(data);

				switch (event_state) {
				case EVENT_KEY: {
					*event = (unsigned(REL_WHEEL) << 16) | unsigned (EV_REL);
					event++;
					*event = msg.data2;
					event_state = EVENT_SYNC;
					break;
				}
				case EVENT_SYNC:
					*event = EV_SYNC;
					event++;
					*event = 0;
					event_state = EVENT_DONE;
					break;
				case EVENT_DONE:
					return 0;
				}
				return 8;
			};

			bool const inject = _queues[0].queue.consume(wheel);
			if (inject)
				inject_irq();

			return true;
		}

		bool receive(MessageInput &msg)
		{
			if (msg.device != _device)
				return false;

			auto &queue = _queues[0];

			if (!queue.queue.enabled())
				return false;

			if (button_click(msg))
				return true;

			if (scroll_wheel(msg))
				return true;

			enum {
				EVENT_X, EVENT_Y, EVENT_SYNC, EVENT_DONE
			} event_state { EVENT_X };

			auto read_x = [&] (auto const descriptor, auto)
			{
				auto const data = vmm_address(descriptor.addr, descriptor.len);
				auto const size = descriptor.len;

				if (!data || size != 8 || event_state == EVENT_DONE) {
					if (event_state != EVENT_DONE)
						Logging::printf("could not send all data\n");
					return 0;
				}

				unsigned *event = (unsigned *)data;

				switch (event_state) {
				case EVENT_X:
					*event = (unsigned(ABS_X) << 16) | unsigned (EV_ABS);
					event++;
					*event = msg.data;
					event_state = EVENT_Y;
					break;
				case EVENT_Y:
					*event = (unsigned(ABS_Y) << 16) | unsigned(EV_ABS);
					event++;
					*event = msg.data2;
					event_state = EVENT_SYNC;
					break;
				case EVENT_SYNC:
					*event = EV_SYNC;
					event++;
					*event = 0;
					event_state = EVENT_DONE;
					break;
				case EVENT_DONE:
					return 0;
				}

				return 8;
			};

			bool const inject = queue.queue.consume(read_x);
			if (inject)
				inject_irq();

			return true;
		}

		void notify (unsigned queue) override
		{
			/*
			 * queue 0 (eventq)  - seems to be ack notify of sent events
			 * queue 1 (statusq) - feedback such as keyboard LED updates - unsupported
			 */
			if (queue != 0)
				Logging::printf("virtio_input: notify %u", queue);
		}
};

PARAM_HANDLER(virtio_input,
	      "virtio_input:bdf,irq - attach an virtio input to the PCI bus",
	      "Example: 'virtio_input:,11'.",
	      "If no bdf is given a free one is used.")
{
	Virtio_input *dev = new Virtio_input(mb.bus_irqlines, mb.bus_memregion,
	                                     argv[1],
	                                     PciHelper::find_free_bdf(mb.bus_pcicfg, argv[0]));

	mb.bus_pcicfg.add(dev, Virtio_input::receive_static<MessagePciConfig>);
	mb.bus_mem   .add(dev, Virtio_input::receive_static<MessageMem>);
	mb.bus_input .add(dev, Virtio_input::receive_static<MessageInput>);
	mb.bus_bios  .add(dev, Virtio_input::receive_static<MessageBios>);

	ConsoleModeInfo info { };
	MessageConsole  msg(MessageConsole::TYPE_GET_MODEINFO, /* XXX VESA mode */ 0);
	msg.index = 1; /* XXX */
	msg.info  = &info;
	mb.bus_console.send(msg);

	dev->width  = info.resolution[0];
	dev->height = info.resolution[1];

	Logging::printf("virtio input width/height %ux%u\n", dev->width, dev->height);
}
