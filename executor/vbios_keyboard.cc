/** @file
 * Virtual Bios keyboard routines.
 *
 * Copyright (C) 2009-2010, Bernhard Kauer <bk@vmmon.org>
 * Economic rights: Technische Universitaet Dresden (Germany)
 *
 * This file is part of Vancouver.
 *
 * Vancouver is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2 as
 * published by the Free Software Foundation.
 *
 * Vancouver is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License version 2 for more details.
 */

#include "nul/motherboard.h"
#include "executor/bios.h"
#include "host/keyboard.h"

/**
 * Virtual Bios keyboard routines.
 * Features: keybuffer
 * Missing: shift state in bda.
 */
class VirtualBiosKeyboard : public StaticReceiver<VirtualBiosKeyboard>, public BiosCommon
{
  unsigned _hostkeyboard;

  /**
   * Converts our internal keycode format into the BIOS one.
   */
  static unsigned keycode2bios(unsigned value)
  {
    static struct {
      unsigned short code;
      unsigned keycode;
    } bios_key_map[] = {
      { 72 << 8,  KBFLAG_EXTEND0 | 0x75}, // up
      { 80 << 8,  KBFLAG_EXTEND0 | 0x72}, // down
      { 77 << 8,  KBFLAG_EXTEND0 | 0x74}, // right
      { 75 << 8,  KBFLAG_EXTEND0 | 0x6b}, // left
      { 59 << 8,  0x05}, // F1
      { 60 << 8,  0x06}, // F2
      { 61 << 8,  0x04}, // F3
      { 62 << 8,  0x0c}, // F4
      { 63 << 8,  0x03}, // F5
      { 64 << 8,  0x0b}, // F6
      { 65 << 8,  0x83}, // F7
      { 66 << 8,  0x0a}, // F8
      { 67 << 8,  0x01}, // F9
      { 68 << 8,  0x09}, // F10
      {133 << 8,  0x78}, // F11
      {134 << 8,  0x07}, // F12
      { 71 << 8,  KBFLAG_EXTEND0 | 0x6c}, // home
      { 82 << 8,  KBFLAG_EXTEND0 | 0x70}, // insert
      { 83 << 8,  KBFLAG_EXTEND0 | 0x71}, // delete
      { 79 << 8,  KBFLAG_EXTEND0 | 0x69}, // end
      { 73 << 8,  KBFLAG_EXTEND0 | 0x7d}, // pgup
      { 81 << 8,  KBFLAG_EXTEND0 | 0x7a}, // pgdown
      {  0x011b,   0x76},                 // esc (grub-core/term/i386/pc/console.c)
      {       8,   0x66},                 // backspace
      {  0x1c0d,   0x5a},                 // enter (grub-core/term/i386/pc/console.c)
    };

    value = value & ~KBFLAG_NUM;

    // handle both shifts the same
    if (value & KBFLAG_RSHIFT) value = (value & ~KBFLAG_RSHIFT) | KBFLAG_LSHIFT;
    for (unsigned i=0; i < sizeof(bios_key_map) / sizeof(bios_key_map[0]); i++)
      if (bios_key_map[i].keycode == value)
	return bios_key_map[i].code;
    unsigned *ascii_map = GenericKeyboard::get_ascii_map();
    for (unsigned i=0; i<128; i++)
      if (ascii_map[i] == value)
	return (value << 8) | i;
    return 0;
  }

  void check_key(unsigned &status, unsigned key, unsigned bit, unsigned keycode) {
    if ((key & (0xff | KBFLAG_EXTEND0 | KBFLAG_EXTEND1)) == keycode) {
      if (~key & KBFLAG_RELEASE)  status |=  (1 << bit);
      else                        status &= ~(1 << bit);
    }
  }


  void update_status(unsigned key) {
    unsigned status = read_bda<unsigned>(0x17) & ~0x2f;
    if (key & KBFLAG_RSHIFT)                 status |= 1 << 0;
    if (key & KBFLAG_LSHIFT)                 status |= 1 << 1;
    if (key & (KBFLAG_LCTRL | KBFLAG_RCTRL)) status |= (1 << 2) | (1 << 8);
    if (key & (KBFLAG_LALT | KBFLAG_RALT))   status |= (1 << 3) | (1 << 9);
    if (key & KBFLAG_NUM)                    status |= 1 << 5;
    if ((key & (0xff | KBFLAG_RELEASE)) == KBCODE_SCROLL) status ^= 1 << 4;
    if ((key & (0xff | KBFLAG_RELEASE)) == KBCODE_CAPS)   status ^= 1 << 6;
    if ((key & (0xff | KBFLAG_RELEASE)) == KBCODE_NUM)    status ^= 1 << 7;
    check_key(status, key, 10, KBCODE_SYSREQ);
    check_key(status, key, 11, KBCODE_PAUSE);
    check_key(status, key, 12, KBCODE_SCROLL);
    check_key(status, key, 13, KBCODE_NUM);
    check_key(status, key, 14, KBCODE_CAPS);
    check_key(status, key, 15, KBCODE_INSERT);
    write_bda(0x17, status, 2);
  }


  /**
   * Handle the Keyboard IRQ.
   */
  bool handle_int09(CpuState *cpu)
  {
    return false;
  }


  /**
   * Keyboard INT handler.
   */
  bool handle_int16(MessageBios &msg)
  {
    CpuState *cpu = msg.cpu;
    COUNTER_INC("int16");
    auto next  = read_bda<unsigned short>(0x1a);
    auto first = read_bda<unsigned short>(0x1c);
    auto start = read_bda<unsigned short>(0x80);
    auto end   = read_bda<unsigned short>(0x82);

    switch (cpu->ah)
      {
      case 0x10: // get extended keystroke
      case 0x00: // get keystroke
        {
          // XXX For AH=0x00 we need to discard extended keystrokes.
          if (first != next)
            {
              cpu->ax = read_bda<unsigned short>(next);
              next += 2;
              if (next > end)
                next = start;
              write_bda(0x1a, next, 2);
            }
          else
            // we should block here until the next IRQ arives, but we return a zero keycode instead
            cpu->ax = 0;
        }
        break;
      case 0x11: // check extended keystroke
      case 0x01: // check keystroke
        // XXX For AH=0x01 we need to discard extended keystrokes.
        cpu->efl |= 1U << 6;
        if (first != next)
          {
            cpu->efl &= ~(1U << 6);
            cpu->ax = read_bda<unsigned short>(next);
          }
        break;
      case 0x12: // get pressed state of various keys
        /* not supported, but there the status bits should be */
        cpu->ah = read_bda<unsigned char>(0x18);
        /* fall through */
      case 0x02: // get enable state of various keys
        cpu->al = read_bda<unsigned char>(0x17);
        break;
      case 0x03: // set typematic
        // ignored
        break;
      default:
        DEBUG(cpu);
      }
    msg.mtr_out |= MTD_RFLAGS | MTD_GPR_ACDB;
    return true;
  }

public:
  /**
   * Handle messages from the keyboard host driver.
   */
  bool  receive(MessageInput &msg)
  {
    if (msg.device == _hostkeyboard) {
      update_status(msg.data);
      unsigned value = keycode2bios(msg.data);
      auto const next  = read_bda<unsigned short>(0x1a);
      auto       first = read_bda<unsigned short>(0x1c);
      auto const start = read_bda<unsigned short>(0x80);
      auto const end   = read_bda<unsigned short>(0x82);

      first += 0x2;
      if (first >= end)   first = start;
      if (value && first != next)
        {
          write_bda(read_bda<unsigned short>(0x1c), value, 2);
          write_bda(0x1c, first, 2);
        }
      return true;
    }
    return false;
  }


  /**
   * Answer HostRequests from DummyHostDevices.
   */
  bool  receive(MessageHostOp &msg)
  {
    switch (msg.type)
      {
      case MessageHostOp::OP_ALLOC_IOIO_REGION:
      case MessageHostOp::OP_ALLOC_IOMEM:
      case MessageHostOp::OP_ATTACH_IRQ:
        // we have all ports and irqs
        return true;
      case MessageHostOp::OP_ASSIGN_PCI:
      case MessageHostOp::OP_ATTACH_MSI:
        return false;
      case MessageHostOp::OP_NOTIFY_IRQ:
      case MessageHostOp::OP_VIRT_TO_PHYS:
      case MessageHostOp::OP_GUEST_MEM:
      case MessageHostOp::OP_GET_MODULE:
      case MessageHostOp::OP_GET_MAC:
      case MessageHostOp::OP_VCPU_CREATE_BACKEND:
      case MessageHostOp::OP_VCPU_BLOCK:
      case MessageHostOp::OP_VCPU_RELEASE:
      case MessageHostOp::OP_ALLOC_SEMAPHORE:
      case MessageHostOp::OP_ALLOC_SERVICE_THREAD:
      case MessageHostOp::OP_ALLOC_SERVICE_PORTAL:
      case MessageHostOp::OP_WAIT_CHILD:
      default:
        Logging::panic("%s - unimplemented operation %x", __PRETTY_FUNCTION__, msg.type);
      }
  };

  /**
   * Forward IO messages to the device models and vice-versa.
   */
  bool  receive(MessageIOIn &msg)  { return _mb.bus_ioin.send(msg); }
  bool  receive(MessageIOOut &msg) { return _mb.bus_ioout.send(msg); }

  bool  receive(MessageBios &msg) {
    switch(msg.irq) {
    case 0x09:  return handle_int09(msg.cpu);
    case 0x16:  return handle_int16(msg);
    default:    return false;
    }
  }
  bool  receive(MessageDiscovery &msg) {
    if (msg.type != MessageDiscovery::DISCOVERY) return false;

    unsigned start = 0x1e001e;
    unsigned end   = 0x2f001e;
    MessageDiscovery msg1("bda", 0x1a, &start, 4);
    MessageDiscovery msg2("bda", 0x80, &end,   4);
    _mb.bus_discovery.send(msg1);
    _mb.bus_discovery.send(msg2);
    return true;
  }


  VirtualBiosKeyboard(Motherboard &mb, unsigned hk) : BiosCommon(mb), _hostkeyboard(hk) {
    _mb.bus_input.add(this,   receive_static<MessageInput>);
    _mb.bus_bios        .add(this, receive_static<MessageBios>);
    _mb.bus_discovery   .add(this, receive_static<MessageDiscovery>);
  }
};

PARAM_HANDLER(vbios_keyboard,
              "vbios_keyboard:hostkeyboard - provide keyboard related virtual BIOS functions. Gets input from the given hostkeyboard.",
              "Example: 'vbios_keyboard:0x17'")
{
  new VirtualBiosKeyboard(mb, unsigned(argv[0]));
}
