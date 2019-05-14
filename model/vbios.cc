/** @file
 * VCPU to VBios bridge.
 *
 * Copyright (C) 2010, Bernhard Kauer <bk@vmmon.org>
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
#include "nul/vcpu.h"
#include "executor/bios.h"

#define BIOS_CODE_OFFSET 0x200
#define BIOS_SHMEM_BASE (BIOS_BASE + 0x1000)
extern char start_vbios, end_vbios;

class VBios : public BiosCommon, public StaticReceiver<VBios>
{
private:
  VCpu &_vcpu;
  struct StackFrame {
    union {
      struct {
	unsigned edi, esi, ebp, esp;
	unsigned ebx, edx, ecx, eax;
      };
      unsigned rgpr[8];
    };
    unsigned short es, ds, ncs, ss;
    unsigned short irq;
    unsigned short res[2];
    unsigned short ueip;
    unsigned short ucs;
    unsigned short uefl;
  };
  CpuState _shadowcpu {};

public:

  bool receive(CpuMessage &msg) {
    if (msg.type != CpuMessage::TYPE_SINGLE_STEP) return false;
    CpuState *cpu = msg.cpu;
    if ((cpu->pm() && !cpu->v86())
	|| !in_range(cpu->cs.base + cpu->eip, BIOS_BASE, BIOS_MAX_VECTOR)
	|| cpu->inj_info & 0x80000000) return false;

    COUNTER_INC("VB");
    unsigned irq =  (cpu->cs.base + cpu->eip) - BIOS_BASE;

    // make sure we take the normal copy-in path
    _vcpu.params_used = 0;

    MessageBios msg1(&_vcpu, cpu, irq);
    _mb.bus_bios.send(msg1, irq != BIOS_RESET_VECTOR);
    // we assume we have handled the BIOS call
    msg.mtr_out |= msg1.mtr_out;

    //if nobody changed the IP or CS we emulate an iret and directly return to the user...
    if (~msg.mtr_out & MTD_RIP_LEN  && ~msg.mtr_out & MTD_CS_SS) {
      if (cpu->v86()) {
	// jmp to IRET at the end of the VBIOS code
	cpu->eip = BIOS_CODE_OFFSET + &end_vbios - &start_vbios - 1;
	msg.mtr_out |= MTD_RIP_LEN;
      }
      else {
        unsigned short iret_frame[3];
	unsigned stack_address = cpu->esp;
	if (((cpu->ss.ar >> 10) & 1)==0) stack_address &= 0xffff;
	if (!_vcpu.copy_in(cpu->ss.base + stack_address, iret_frame, sizeof(iret_frame)))
	  Logging::panic("can not copy in iret frame");
	cpu->cs.sel = iret_frame[1];
	cpu->cs.base= cpu->cs.sel << 4;
	cpu->cs.ar  = cpu->v86() ? 0xf3 : 0x93;
	cpu->eip    = iret_frame[0];
	cpu->esp   += 6;
	// we take some flags from the user-level
	cpu->efl    = (cpu->efl & ~0x300) | (iret_frame[2]  & 0x300);
	msg.mtr_out |= MTD_RFLAGS | MTD_RSP | MTD_RIP_LEN | MTD_CS_SS;
      }
    }
    return true;
  }

  /**
   * The memory routine.
   */
  bool receive(MessageMem &msg)
  {
    // we trigger on the read of the params->count
    if (msg.read && in_range(msg.phys, BIOS_SHMEM_BASE, sizeof(_vcpu.shmem.params)) && !((msg.phys - BIOS_SHMEM_BASE) % sizeof(*_vcpu.shmem.params))) {
      unsigned number = (msg.phys - BIOS_SHMEM_BASE) / sizeof(*_vcpu.shmem.params);
      if (!number) {
	// initial read -> clear all other copy-parameters
	memset(_vcpu.shmem.params + 1, 0, sizeof(_vcpu.shmem.params) - sizeof(*_vcpu.shmem.params));
	_vcpu.params_used = 1;
      }
      else if (number == _vcpu.params_used) {
	StackFrame *frame = reinterpret_cast<StackFrame *>(_vcpu.shmem.bytes + (_vcpu.shmem.params[0].dst.seg << 4) + _vcpu.shmem.params[0].dst.ofs);
	MessageBios msg1(&_vcpu, &_shadowcpu, frame->irq - 1);
	//Logging::printf("BIOS(%x) num %d ofs %x:%x eax %x ecx %x\n", frame->irq, number, _vcpu.shmem.params[0].dst.seg, _vcpu.shmem.params[0].dst.ofs, frame->eax, frame->ecx);

	// prepare CPU registers for BIOS handling
	for (unsigned i = 0; i<8; i++) _shadowcpu.gpr[i] = frame->rgpr[7-i];
	_shadowcpu.efl      = frame->uefl;
	_shadowcpu.es.base  = frame->es << 4;
	_shadowcpu.es.sel   = frame->es;
	_shadowcpu.ds.base  = frame->ds << 4;
	_shadowcpu.ds.sel   = frame->ds;

	if (_mb.bus_bios.send(msg1, true)) {
	  // copy GPRs and eflags out
	  for (unsigned i = 0; i<8; i++) frame->rgpr[7-i] = _shadowcpu.gpr[i];
	  frame->uefl = _shadowcpu.efl;
	  _vcpu.copy_out((_vcpu.shmem.params[0].src.seg << 4) + _vcpu.shmem.params[0].src.ofs, frame, sizeof(StackFrame));

	  // we are done
	  _vcpu.params_used = 0;
	}

	// it looks like we failed the request or it was bogus
	if (_vcpu.params_used == number) _vcpu.params_used = 0;
      }
    }

    // make the shmem-area visible
    if (in_range(msg.phys, BIOS_SHMEM_BASE, sizeof(_vcpu.shmem) - 3)) {
      if (msg.read)
	Cpu::move<2>(msg.ptr, _vcpu.shmem.bytes + msg.phys -  BIOS_SHMEM_BASE);
      else
	Cpu::move<2>(_vcpu.shmem.bytes + msg.phys -  BIOS_SHMEM_BASE, msg.ptr);
      // if (in_range(msg.phys, BIOS_SHMEM_BASE, 0x1000))
      // 	Logging::printf("vbios::%s(%x, %x)\n", msg.read ? "read" : "write", msg.phys, msg.ptr[0]);
      return true;
    }


    // 16bytes below 4G -> forward requests down to the BIOS at 0xffff0
    if (!msg.read || !in_range(msg.phys, 0xfffffff0, 0x10))  return false;
    MessageMem msg2(msg.read, msg.phys & 0xfffff, msg.ptr);
    return _mb.bus_mem.send(msg2);
  }


  bool  receive(MessageDiscovery &msg) {
    if (msg.type != MessageDiscovery::DISCOVERY) return false;

    // initialize realmode idt
    unsigned value = (BIOS_BASE >> 4) << 16;
    for (unsigned i=0; i < 256; i++) {
      // the int3 instructions to demultiplex the vectors
      discovery_write_dw("bios", i, 0xcc, 1);

      // XXX init only whats needed and done on compatible BIOSes
      if (i != 0x43) {
	discovery_write_dw("realmode idt", i*4, value, 4);
      }

      // the int3 vector
      if (i == 3) {
	discovery_write_dw("realmode idt", i*4,((BIOS_BASE >> 4) << 16) + BIOS_CODE_OFFSET, 4);
	// write the BIOS code
	discovery_write_st("bios", BIOS_CODE_OFFSET, &start_vbios, &end_vbios - &start_vbios);
      }
      value++;
    }
    return true;
  }


  VBios(Motherboard &mb, VCpu &vcpu) : BiosCommon(mb), _vcpu(vcpu) {
    _vcpu.executor.add(this,   VBios::receive_static<CpuMessage>);
    _vcpu.mem.add(this,        VBios::receive_static<MessageMem>);
    _mb.bus_discovery.add(this, VBios::receive_static<MessageDiscovery>);
  }

};

#define stringify(X) tostr(X)
#define tostr(X) #X

/**
 * BIOS stub to make emulators happy.
 * Stack layout:
 *      user IRET frame (EIP, CS, EFLAGS) - 3 words
 *      int3 IRET frame (IRQ-nr, 0xf000, EFLAGS) - 3 words
 *      pushal registers - 8 dwords
 *      es, ds, ss, cs - 2 words
 */
asm ("; .code16"
	 "; start_vbios:"
	 // build stack frame
	 "; pushw %ss"
	 "; pushw %cs"
	 "; pushw %ds"
	 "; pushw %es"
	 "; pushal"

	 // make the registers accessible
	 // we start with a copy-in from the stack
	 "; movw %ss, %ax"
	 "; movw %cs, %bx"
 	 "; movl $" stringify(BIOS_SHMEM_BASE - BIOS_BASE) ", %edx"
	 "; movw $52, %cs:0(%edx)"
	 "; movw %ax, %cs:2(%edx)"
	 "; movw %sp, %cs:4(%edx)"
	 "; movw %bx, %cs:6(%edx)"
	 "; movw $(" stringify(BIOS_SHMEM_BASE - BIOS_BASE + NUM_VCPU_PARAMETER*SIZEOF_VCPU_PARAMETER) "), %cs:8(%edx)"

	 // copy in/out loop
	 "; 1:"
	 "; mov %cs:0(%edx), %cx"
	 "; test %cx, %cx"
	 "; jz 2f"
	 "; mov %cs:2(%edx), %ax"
	 "; mov %cs:4(%edx), %si"
	 "; mov %cs:6(%edx), %bx"
	 "; mov %cs:8(%edx), %di"
	 "; add $" stringify(SIZEOF_VCPU_PARAMETER) ", %dx"
	 "; mov %ax, %ds"
	 "; mov %bx, %es"
	 "; rep movsb %ds:(%si), %es:(%di)"
	 "; jmp 1b"
	 "; 2:"

	 // return back
	 "; popal"
	 "; pop %es"
	 "; pop %ds"
	 "; add $10, %esp"
	 "; iret"
	 "; end_vbios:"
#ifdef __x86_64__
	 "; .code64"
#else
	 "; .code32"
#endif
	);

PARAM_HANDLER(vbios,
	      "vbios - create a bridge between VCPU and the BIOS bus.")
{
  if (!mb.last_vcpu) Logging::panic("no VCPU for this VBIOS");
  new VBios(mb, *mb.last_vcpu);
}

