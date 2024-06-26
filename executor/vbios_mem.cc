/** @file
 * Virtual Bios memory routines.
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

/**
 * Virtual Bios mem routines.
 * Features: int15
 * Missing:
 */
class VirtualBiosMem : public StaticReceiver<VirtualBiosMem>, public BiosCommon
{
  DBus<MessageHostOp>    &_bus_hostop;
  DBus<MessageMemRegion> &_bus_memregion;

  /**
   * Return the size of guest physical memory.
   */
  size_t memsize()
  {
    MessageHostOp msg1(MessageHostOp::OP_GUEST_MEM, 0UL);
    if (!_bus_hostop.send(msg1))
      Logging::panic("%s can't get physical memory size", __PRETTY_FUNCTION__);
    return msg1.len;
  }

  /**
   * Memory+PS2 INT.
   */
  bool handle_int15(MessageBios &msg)
  {
    CpuState *cpu = msg.cpu;
    COUNTER_INC("int15");
    // default is to clear CF
    cpu->efl &= ~1;
    switch (cpu->ax)
      {
      case 0x2400: // disable A20
      case 0x2401: // enable  A20
	{
	  MessageLegacy msg1(MessageLegacy::GATE_A20, cpu->al);
	  if (_mb.bus_legacy.send(msg1))
	    cpu->ax = 0;
	  else
	    error(msg, 0x24);
	}
	break;
      case 0xc201:            // reset mouse
	{
	  cpu->ax = 0x0001;
	  cpu->bx = 0xfaaa;
	}
	break;
      case 0xe820:           // get memory map
	{
	  if ((cpu->edx == 0x534D4150 && cpu->ecx >= 20))
	    {
	      struct mmap{
		unsigned long long base;
		unsigned long long size;
		unsigned type;
	      } mmap;
	      mmap.type = 1;
	      switch (cpu->ebx)
		{
		case 0:
		  mmap.base = 0;
		  mmap.size = read_bda<unsigned>(0x13) << 10;
		  cpu->ebx++;
		  break;
		case 1:
		  mmap.base = read_bda<unsigned>(0x13) << 10;
		  mmap.size = 0xa0000 - mmap.base;
		  mmap.type = 2;

		  [[fallthrough]];
		case 2:
		{
		  MessageMemRegion msg2((1 << 20) >> 12); /*XXX read out somehow ? */
		  bool const ok = _mb.bus_memregion.send(msg2);

		  /* fallthrough case */
		  if (cpu->ebx == 1) {
		    if (ok)
		      cpu->ebx++;   /* more */
		    else
		      cpu->ebx = 0; /* end */

		    break;
		  }

		  assert(ok);

		  mmap.base = msg2.start_page << 12;
		  mmap.size = msg2.count      << 12;

		  [[fallthrough]];
		}
		case 3:
		{
		  MessageMemRegion msg2((1ull << 32) >> 12); /*XXX read out somehow ? */
		  bool const ok = _mb.bus_memregion.send(msg2);

		  /* fallthrough case */
		  if (cpu->ebx == 2) {
		    if (ok)
		      cpu->ebx++;   /* more */
		    else
		      cpu->ebx = 0; /* end */

		    break;
		  }

		  assert(ok);

		  mmap.base = msg2.start_page << 12;
		  mmap.size = msg2.count      << 12;
		  cpu->ebx = 0; /* end */
		  break;
		}
		default:
		  mmap.type = 0;
		  break;
		}

	      if (mmap.type)
		{
		  msg.vcpu->copy_out(cpu->es.base + cpu->di, &mmap, 20);
		  cpu->eax = cpu->edx;
		  cpu->ecx = 20;
		  cpu->edx = 0;
		  break;
		}
	    }
	  goto unsupported;
	}
      case 0x8800 ... 0x88ff: // get extended memory
        {
          auto const mem_kb = (memsize() - (1<<20)) / 1024;
          // Cap at 15MB for legacy compatibility.
          cpu->ax = (mem_kb > (15*1024)) ? 15*1024 : static_cast<unsigned short>(mem_kb);
          break;
        }
      case 0x00c0:            // get rom configtable
      case 0x5300 ... 0x53ff: // apm installation check
      case 0xc000 ... 0xc0ff: // get configuration
      case 0xc100 ... 0xc1ff: // get ebda
      case 0xe801:            // get memsize
      case 0xe980:            // get intel speedstep information
      unsupported:
	// unsupported
	DEBUG(cpu);
	error(msg, 0x86);
	break;
      default:
	DEBUG(cpu);
      }
    msg.mtr_out |= MTD_GPR_ACDB | MTD_RFLAGS;
    return true;
  }

public:

  bool  receive(MessageBios &msg) {
    switch(msg.irq) {
    case 0x11: // BIOS equipment word
      msg.cpu->ax = 0x34; // 80x25, ps2-mouse, no-floppy
      msg.mtr_out |= MTD_GPR_ACDB;
      return true;
    case 0x12: // get low memory
      msg.cpu->ax = read_bda<unsigned short>(0x13);
      msg.mtr_out |= MTD_GPR_ACDB;
      return true;
    case 0x15:  return handle_int15(msg);
    case 0x17:  // printer
      error(msg, msg.cpu->ah);
      return true;
    default:    return false;
    }
  }

  VirtualBiosMem(Motherboard &mb)
  : BiosCommon(mb), _bus_hostop(mb.bus_hostop), _bus_memregion(mb.bus_memregion)
  { }
};

PARAM_HANDLER(vbios_mem,
	      "vbios_mem - provide memory related virtual BIOS functions.")
{
  mb.bus_bios.add(new VirtualBiosMem(mb), VirtualBiosMem::receive_static<MessageBios>);
}

