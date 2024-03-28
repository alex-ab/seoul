/**
 * ACPI controller model
 *
 * Copyright (C) 2013 Jacek Galowicz, Intel Corporation.
 * Copyright (C) 2024 Alexander Boettcher
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


#include <nul/timer.h>
#include <service/time.h>

#include "nul/motherboard.h"
#include "executor/bios.h"
#include "service/lock.h"

#define CMD_ACPI_ENABLE 0xab
#define CMD_ACPI_DISABLE 0xba

#define PORT_SMI_CMD        0xaeae

/* The pm1 event register group is somewhat complicated.
 * port numbers follow a partition rule of the register block.
 * see ACPI spec 4.7.3.1
 */
#define PM1_EVT_LEN         4
#define PORT_PM1A_EVENT_BLK     0xaea6
#define PORT_PM1B_EVENT_BLK     0xaeaa
#define PORT_PM1A_EVENT_STATUS  (PORT_PM1A_EVENT_BLK)
#define PORT_PM1A_EVENT_ENABLE  (PORT_PM1A_EVENT_BLK + (PM1_EVT_LEN) / 2) // 0xa6 + 4/2 = 0xa8
#define PORT_PM1B_EVENT_STATUS  (PORT_PM1B_EVENT_BLK)
#define PORT_PM1B_EVENT_ENABLE  (PORT_PM1B_EVENT_BLK + (PM1_EVT_LEN) / 2) // 0xaa + 4/2 = 0xac

#define PM1_CNT_LEN         2
#define PORT_PM1A_CONTROL   0xaeb0
#define PORT_PM1B_CONTROL   0xaeb2

#define GPE0_BLK_LEN        4
#define GPE1_BLK_LEN        4

#define PORT_GPE0_STATUS    0xaeb4
#define PORT_GPE0_ENABLE    (PORT_GPE0_STATUS + 2)
#define PORT_GPE1_STATUS    (PORT_GPE0_STATUS + GPE0_BLK_LEN)
#define PORT_GPE1_ENABLE    (PORT_GPE1_STATUS + 2)

#define PORT_PCIU   0xae00
#define PORT_PCID   0xae04
#define PORT_B0EJ   0xae08


class AcpiController : public StaticReceiver<AcpiController>, public BiosCommon
{
	private:

		struct {
			uint16 _pm1a_status  { };
			uint16 _pm1a_enable  { };
			uint16 _pm1a_control { };

			uint16 _pm1b_status  { };
			uint16 _pm1b_enable  { };
			uint16 _pm1b_control { };

			uint8  _gpe0_sts { };
			uint8  _gpe0_en  { };

			uint8  _gpe1_sts { };
			uint8  _gpe1_en  { };

			uint32 _pciu { }; // read-only, REFRESH register (card plugged in)
			uint32 _pcid { }; // read-only, DETACH register (card to be unplugged)

			bool _irq_asserted { };
		} _state { };

		Seoul::Lock _lock { };

		StopWatch _watch { _mb.clock() };

		void _trigger_irq()
		{
			MessageIrqLines msg(MessageIrq::ASSERT_IRQ, 9);
			_state._irq_asserted = _mb.bus_irqlines.send(msg);
		}

		bool _trigger_gpe(unsigned const event_nr)
		{
			Logging::printf("trigger gpe %u\n", event_nr);

			// Activate this event in the appropriate register
			_state._gpe0_sts |= static_cast<unsigned char>( 0x00ff & (1 << event_nr));
			_state._gpe1_sts |= static_cast<unsigned char>((0xff00 & (1 << event_nr)) >> 8);

			// If this event is masked by the guest, then just ignore it
			if (!(_state._gpe0_sts & _state._gpe0_en) && !(_state._gpe1_sts & _state._gpe1_en))
				return false;

			Logging::printf("trigger gpe %u -> irq\n", event_nr);

			return true;
		}

	public:

		AcpiController(Motherboard &mb) : BiosCommon(mb) { }

		bool  receive(MessageDiscovery &);
		bool  receive(MessageAcpiEvent const &);
		bool  receive(MessageIOIn  &);
		bool  receive(MessageIOOut &);
		bool _receive(MessageIOOut &);

		bool receive(MessageBios &msg)
		{
			switch(msg.irq) {
			case BiosCommon::BIOS_RESET_VECTOR:

				Seoul::Lock::Guard guard(_lock);

				_state = { };
			};

			return false;
		}
};

bool AcpiController::receive(MessageDiscovery &msg)
{
	if (msg.type != MessageDiscovery::DISCOVERY)
		return false;

	/* The following FADT entries will tell the guest kernel
	 * how to interact with the system when receiving
	 * System Control Interrupts (SCI).
	 * Only the GPE part is important for hot plugging, but
	 * all the PM-stuff is mandatory for event management
	 * to work.
	 */

	discovery_write_dw("FACP", 56, PORT_PM1A_EVENT_BLK);
	discovery_write_dw("FACP", 60, PORT_PM1B_EVENT_BLK);
	discovery_write_dw("FACP", 64, PORT_PM1A_CONTROL);
	discovery_write_dw("FACP", 68, PORT_PM1B_CONTROL);
	discovery_write_dw("FACP", 88, PM1_EVT_LEN, 1);
	discovery_write_dw("FACP", 89, PM1_CNT_LEN, 1);
	discovery_write_dw("FACP", 80, PORT_GPE0_STATUS, 4); // GPE0_BLK
	discovery_write_dw("FACP", 84, PORT_GPE1_STATUS, 4); // GPE1_BLK

	discovery_write_dw("FACP", 92, GPE0_BLK_LEN, 1);
	discovery_write_dw("FACP", 93, GPE1_BLK_LEN, 1);
	discovery_write_dw("FACP", 94, 16, 1); // GPE1_BASE (offset)

	/* This is used at boot once. Linux will write
	 * CMD_ACPI_ENABLE via system IO using port PORT_SMI_CMD
	 * to tell the mainboard it wants to use ACPI.
	 * If CMD_ACPI_ENABLE was defined as 0x00, the guest kernel
	 * would think that ACPI was always on. Therefore, this is
	 * optional and one could just erase the next three lines.
	 */
	discovery_write_dw("FACP", 48, PORT_SMI_CMD);
	discovery_write_dw("FACP", 52, CMD_ACPI_ENABLE, 1);
	discovery_write_dw("FACP", 53, CMD_ACPI_DISABLE, 1);

	return true;
}

bool AcpiController::receive(MessageAcpiEvent const &msg)
{
	if (msg.type != MessageAcpiEvent::ACPI_EVENT_GP &&
	    msg.type != MessageAcpiEvent::ACPI_EVENT_HOT_REPLUG &&
	    msg.type != MessageAcpiEvent::ACPI_EVENT_HOT_UNPLUG &&
	    msg.type != MessageAcpiEvent::ACPI_EVENT_FIXED)
		return false;

	bool irq = false;

	{
		Seoul::Lock::Guard guard(_lock);

		switch (msg.type) {
		case MessageAcpiEvent::ACPI_EVENT_GP:
			irq = _trigger_gpe(msg.num);
			break;
		case MessageAcpiEvent::ACPI_EVENT_HOT_REPLUG:
			_state._pciu |= (1u << msg.num);
			irq = _trigger_gpe(1);
			break;
		case MessageAcpiEvent::ACPI_EVENT_HOT_UNPLUG:
			_watch.start();
			_state._pcid |= (1 << msg.num);

			irq = _trigger_gpe(1);
			break;
		case MessageAcpiEvent::ACPI_EVENT_FIXED:

			_state._pm1a_status |= uint16(msg.num);

			irq = !!(_state._pm1a_status & _state._pm1a_enable);
			break;
		default:
			return false;
		}
	}

	if (irq)
		_trigger_irq();

	return true;
}

bool AcpiController::receive(MessageIOIn &msg)
{
	if (!in_range(msg.port, PORT_PM1A_EVENT_BLK, PM1_EVT_LEN)  &&
	    !in_range(msg.port, PORT_PM1B_EVENT_BLK, PM1_EVT_LEN)  &&
	    !in_range(msg.port, PORT_PM1A_CONTROL  , PM1_CNT_LEN)  &&
	    !in_range(msg.port, PORT_PM1B_CONTROL  , PM1_CNT_LEN)  &&
	    !in_range(msg.port, PORT_GPE0_STATUS   , GPE0_BLK_LEN) &&
	    !in_range(msg.port, PORT_GPE1_STATUS   , GPE1_BLK_LEN) &&
	    !in_range(msg.port, PORT_PCIU          , 4) &&
	    !in_range(msg.port, PORT_PCID          , 4))
		return false;

	Seoul::Lock::Guard guard(_lock);

	switch (msg.port) {
	default: return false;
	case PORT_PM1A_EVENT_STATUS: msg.value = _state._pm1a_status;  break;
	case PORT_PM1A_EVENT_ENABLE: msg.value = _state._pm1a_enable;  break;
	case PORT_PM1B_EVENT_STATUS: msg.value = _state._pm1b_status;  break;
	case PORT_PM1B_EVENT_ENABLE: msg.value = _state._pm1b_enable;  break;
	case PORT_PM1A_CONTROL:      msg.value = _state._pm1a_control; break;
	case PORT_PM1B_CONTROL:      msg.value = _state._pm1b_control; break;
	case PORT_GPE0_STATUS:       msg.value = _state._gpe0_sts;     break;
	case PORT_GPE0_ENABLE:       msg.value = _state._gpe0_en;      break;
	case PORT_GPE1_STATUS:       msg.value = _state._gpe1_sts;     break;
	case PORT_GPE1_ENABLE:       msg.value = _state._gpe1_en;      break;
	case PORT_PCIU:              msg.value = _state._pciu;         break;
	case PORT_PCID:              msg.value = _state._pcid;         break;
	}

	switch (msg.type) {
	case MessageIOIn::TYPE_INB: msg.value &= 0x000000ffu; break;
	case MessageIOIn::TYPE_INW: msg.value &= 0x0000ffffu; break;
	case MessageIOIn::TYPE_INL: msg.value &= 0xffffffffu; break;
	}

	if (false)
		Logging::printf("acpi %s  %s -> %x\n",
		                msg.type == MessageIOIn::TYPE_INB ? "inb" :
		                msg.type == MessageIOIn::TYPE_INW ? "inw" :
		                msg.type == MessageIOIn::TYPE_INL ? "inl" : "und",
		                msg.port == PORT_PM1A_EVENT_STATUS ? "PM1A_STATUS" :
		                msg.port == PORT_PM1A_EVENT_ENABLE ? "PM1A_ENABLE" :
		                msg.port == PORT_PM1B_EVENT_STATUS ? "PM1B_STATUS" :
		                msg.port == PORT_PM1B_EVENT_ENABLE ? "PM1B_ENABLE" :
		                msg.port == PORT_PM1A_CONTROL      ? "PM1A_CONTROL" :
		                msg.port == PORT_PM1B_CONTROL      ? "PM1B_CONTROL" :
		                msg.port == PORT_GPE0_STATUS       ? "GPE0_STATUS" :
		                msg.port == PORT_GPE0_ENABLE       ? "GPE0_ENABLE" :
		                msg.port == PORT_GPE1_STATUS       ? "GPE1_STATUS" :
		                msg.port == PORT_GPE1_ENABLE       ? "GPE1_ENABLE" :
		                msg.port == PORT_PCIU              ? "PORT_PCIU" :
		                msg.port == PORT_PCID              ? "PORT_PCID" :
		                "unknown",
		                msg.value);

	return true;
}

bool AcpiController::receive(MessageIOOut &msg)
{
	if (!in_range(msg.port, PORT_SMI_CMD       , 4)            &&
	    !in_range(msg.port, PORT_PM1A_EVENT_BLK, PM1_EVT_LEN)  &&
	    !in_range(msg.port, PORT_PM1B_EVENT_BLK, PM1_EVT_LEN)  &&
	    !in_range(msg.port, PORT_PM1A_CONTROL  , PM1_CNT_LEN)  &&
	    !in_range(msg.port, PORT_PM1B_CONTROL  , PM1_CNT_LEN)  &&
	    !in_range(msg.port, PORT_GPE0_STATUS   , GPE0_BLK_LEN) &&
	    !in_range(msg.port, PORT_GPE1_STATUS   , GPE1_BLK_LEN) &&
	    !in_range(msg.port, PORT_B0EJ          , 4))
		return false;

	bool   result = false;
	bool   assert = false;
	bool deassert = false;

	{
		Seoul::Lock::Guard guard(_lock);

		result = _receive(msg);

		if (!result)
			return result;

		if (((_state._pm1a_status & _state._pm1a_enable) ||
		     (_state._pm1b_status & _state._pm1b_enable) ||
		     (_state._gpe0_sts    & _state._gpe0_en)     ||
		     (_state._gpe1_sts    & _state._gpe1_en)))
			assert = true;
		else
		if (_state._irq_asserted && !(_state._pm1a_status & _state._pm1a_enable) &&
		                            !(_state._pm1b_status & _state._pm1b_enable) &&
		                            !(_state._gpe0_sts    & _state._gpe0_en)     &&
		                            !(_state._gpe1_sts    & _state._gpe1_en)) {
			deassert = true;
			_state._irq_asserted = false;
		}
	}

	if (assert)
		_trigger_irq();

	if (deassert) {
		MessageIrqLines msg(MessageIrq::DEASSERT_IRQ, 9);
		_mb.bus_irqlines.send(msg);
	}

	return result;
}

bool AcpiController::_receive(MessageIOOut &msg)
{
	unsigned result = 0;

	auto const previous = msg.port == PORT_PM1A_CONTROL      ? _state._pm1a_control :
	                      msg.port == PORT_PM1B_CONTROL      ? _state._pm1b_control :
	                      msg.port == PORT_PM1A_EVENT_STATUS ? _state._pm1a_status  :
	                      msg.port == PORT_PM1A_EVENT_ENABLE ? _state._pm1a_enable  :
	                      msg.port == PORT_PM1B_EVENT_STATUS ? _state._pm1b_status  :
	                      msg.port == PORT_PM1B_EVENT_ENABLE ? _state._pm1b_enable  :
	                      msg.port == PORT_GPE0_STATUS       ? _state._gpe0_sts     :
	                      msg.port == PORT_GPE0_ENABLE       ? _state._gpe0_en      :
	                      msg.port == PORT_GPE1_STATUS       ? _state._gpe1_sts     :
	                      msg.port == PORT_GPE1_ENABLE       ? _state._gpe1_en      :
	                      0u;

	switch (msg.type) {
	case MessageIOOut::TYPE_OUTB: msg.value &= 0x000000ffu; break;
	case MessageIOOut::TYPE_OUTW: msg.value &= 0x0000ffffu; break;
	case MessageIOOut::TYPE_OUTL: msg.value &= 0xffffffffu; break;
	}

	auto constexpr SCI_EN = 1u << 0;

	switch (msg.port) {
	default: return false;

	case PORT_SMI_CMD:

		if (msg.value == CMD_ACPI_ENABLE)
			_state._pm1a_control |= SCI_EN;
		else
		if (msg.value == CMD_ACPI_DISABLE)
		    _state._pm1a_control &= uint16(~SCI_EN);

		return true;

	case PORT_PM1A_CONTROL: {

		auto const power_off_s5 = (5u << 10) | 1;
		if ((msg.value & power_off_s5) == power_off_s5) {
			Logging::printf("acpi S5 detected - power off\n");
			MessageHostOp msg(MessageHostOp::OP_VM_OFF, 0);
			_mb.bus_hostop.send(msg);
		}
		else
			Logging::printf("acpi: pm1a control unsupported %x\n", msg.value);

		return true;
	}

	case PORT_PM1B_CONTROL:
		Logging::printf("acpi: pm1b control unsupported %x\n", msg.value);
		return true;

	case PORT_B0EJ:
		_watch.stop();
		Logging::printf("PCI hot-unplug confirmed by guest "
		                "(Output on B0EJ: %x) after %llu ms\n",
		                msg.value, _watch.delta());
		_state._pcid &= ~msg.value;
		//Logging::printf("PCIU: %x, PCID: %x\n", _state._pciu, _state._pcid);
		return true;

	case PORT_PM1A_EVENT_STATUS: result = _state._pm1a_status &= ~uint16(msg.value); break;
	case PORT_PM1A_EVENT_ENABLE: result = _state._pm1a_enable  =  uint16(msg.value); break;
	case PORT_PM1B_EVENT_STATUS: result = _state._pm1b_status &= ~uint16(msg.value); break;
	case PORT_PM1B_EVENT_ENABLE: result = _state._pm1b_enable  =  uint16(msg.value); break;
	case PORT_GPE0_STATUS:       result = _state._gpe0_sts    &= ~uint8 (msg.value); break;
	case PORT_GPE0_ENABLE:       result = _state._gpe0_en      =  uint8 (msg.value); break;
	case PORT_GPE1_STATUS:       result = _state._gpe1_sts    &= ~uint8 (msg.value); break;
	case PORT_GPE1_ENABLE:       result = _state._gpe1_en      =  uint8 (msg.value); break;
	}

	if (false) {
		Logging::printf("acpi %s %s: %x = %x <- %x\n",
		                msg.type == MessageIOOut::TYPE_OUTB ? "outb" :
		                msg.type == MessageIOOut::TYPE_OUTW ? "outw" :
		                msg.type == MessageIOOut::TYPE_OUTL ? "outl" : "unde",
		                msg.port == PORT_PM1A_EVENT_STATUS  ? "PM1A_STATUS" :
		                msg.port == PORT_PM1A_EVENT_ENABLE  ? "PM1A_ENABLE" :
		                msg.port == PORT_PM1B_EVENT_STATUS  ? "PM1B_STATUS" :
		                msg.port == PORT_PM1B_EVENT_ENABLE  ? "PM1B_ENABLE" :
		                msg.port == PORT_PM1A_CONTROL       ? "PM1A_CONTROL" :
		                msg.port == PORT_PM1B_CONTROL       ? "PM1B_CONTROL" :
		                msg.port == PORT_GPE0_STATUS        ? "GPE0_STATUS" :
		                msg.port == PORT_GPE0_ENABLE        ? "GPE0_ENABLE" :
		                msg.port == PORT_GPE1_STATUS        ? "GPE1_STATUS" :
		                msg.port == PORT_GPE1_ENABLE        ? "GPE1_ENABLE" :
		                msg.port == PORT_PCIU               ? "PORT_PCIU" :
		                msg.port == PORT_PCID               ? "PORT_PCID" :
		                "unknown",
		                result, previous, msg.value);
	}

	return true;
}

PARAM_HANDLER(acpimodel, "acpimodel - Capable of issuing ACPI events to the guest.")
{
	AcpiController * dev = new AcpiController(mb);
	mb.bus_discovery .add(dev, AcpiController::receive_static<MessageDiscovery>);
	mb.bus_ioin      .add(dev, AcpiController::receive_static<MessageIOIn>);
	mb.bus_ioout     .add(dev, AcpiController::receive_static<MessageIOOut>);
	mb.bus_acpi_event.add(dev, AcpiController::receive_static<MessageAcpiEvent>);
	mb.bus_bios      .add(dev, AcpiController::receive_static<MessageBios>);
}
