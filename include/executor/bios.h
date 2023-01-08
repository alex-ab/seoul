/** @file
 * Common BIOS code.
 *
 * Copyright (C) 2009, Bernhard Kauer <bk@vmmon.org>
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
#pragma once
#include "nul/vcpu.h"

#define DEBUG(cpu)   Logging::printf("\t%s eax %x ebx %x ecx %x edx %x eip %x efl %x\n", __func__, cpu->eax, cpu->ebx, cpu->ecx, cpu->edx, cpu->eip, cpu->efl)

#define BIOS_BASE 0xf0000


class BiosCommon : public DiscoveryHelper<BiosCommon>
{
public:
  Motherboard &_mb;

  enum {
    BIOS_RESET_VECTOR = 0x100,
    BIOS_MAX_VECTOR
  };

protected:

  /**
   * Write bios data helper.
   */
  void write_bda(unsigned short offset, unsigned value, unsigned len)
  {
    assert(len <= sizeof(value));
    unsigned x = read_bda<unsigned>(offset);
    Cpu::move(&x, &value, len >> 1);

    MessageMem msg(false, 0x400 + offset, &x);
    _mb.bus_mem.send(msg);
  }


  /**
   * Read bios data helper.
   */
  template <typename T>
  T read_bda(size_t offset)
  {
    unsigned res;
    MessageMem msg(true, 0x400 + offset, &res);
    _mb.bus_mem.send(msg);
    return static_cast<T>(res);
  }

  /**
   * Jump to another realmode INT handler.
   */
  bool jmp_int(MessageBios &msg, unsigned char number)
  {
    unsigned short v[2];
    if (!msg.vcpu->copy_in(number*4, v, 4)) return false;

    msg.cpu->cs.sel  = v[1];
    msg.cpu->cs.base = v[1] << 4;
    msg.cpu->eip = v[0];
    msg.mtr_out |= MTD_RIP_LEN | MTD_CS_SS;
    return true;
  }

  /**
   * Set the usual error indication.
   */
  void error(MessageBios &msg, unsigned char errorcode)
  {
    msg.cpu->efl |= 1;
    msg.cpu->ah = errorcode;
    msg.mtr_out |= MTD_RFLAGS | MTD_GPR_ACDB;
  }


  /**
   * Out to IO-port.
   */
  void outb(unsigned short port, unsigned value)
  {
    MessageIOOut msg(MessageIOOut::TYPE_OUTB, port, value);
    _mb.bus_ioout.send(msg);
  }

 BiosCommon(Motherboard &mb) : _mb(mb) {}
};
