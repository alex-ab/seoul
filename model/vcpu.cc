/** @file
 * Virtual CPU.
 *
 * Copyright (C) 2010, Bernhard Kauer <bk@vmmon.org>
 * Economic rights: Technische Universitaet Dresden (Germany)
 *
 * Copyright (C) 2013 Jacek Galowicz, Intel Corporation.
 * Copyright (C) 2013 Markus Partheymueller, Intel Corporation.
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

#ifndef VMM_REGBASE
class VirtualCpu : public VCpu, public StaticReceiver<VirtualCpu>
{
#define VMM_REGBASE "../model/vcpu.cc"
#include "model/reg.h"

  uintptr_t _hostop_id { 0 };
  Motherboard &_mb;
  long long _reset_tsc_off { 0 };

  volatile unsigned _event;
  volatile unsigned _sipi;
  unsigned long _intr_hint { 0 };

  #define DEBUG_IOEXITS 0

  #if DEBUG_IOEXITS
  unsigned char debugioin[8192];
  unsigned char debugioout[8192];
  #endif

  bool _amd   { };
  bool _intel { };

  void GP0(CpuMessage &msg) {
    msg.cpu->inj_info = 0x80000b0d;
    msg.cpu->inj_error = 0;
    msg.mtr_out |= MTD_INJ;
  }


  bool handle_cpuid(CpuMessage &msg) {
    bool res = true;
    unsigned reg;
    if (msg.cpuid_index & 0x80000000u && msg.cpuid_index <= CPUID_EAX80)
      reg = (msg.cpuid_index << 4) | 0x80000000u;
    else {
      reg = msg.cpuid_index << 4;
      if (msg.cpuid_index > CPUID_EAX0) {
        reg = CPUID_EAX0 << 4;
        res = false;
      }
    }
//    Logging::printf("%s %x->%x\n", __func__, msg.cpuid_index, reg);

    if (!CPUID_read(reg | 0, msg.cpu->eax)) msg.cpu->eax = 0;
    if (!CPUID_read(reg | 1, msg.cpu->ebx)) msg.cpu->ebx = 0;
    if (!CPUID_read(reg | 2, msg.cpu->ecx)) msg.cpu->ecx = 0;
    if (!CPUID_read(reg | 3, msg.cpu->edx)) msg.cpu->edx = 0;

/*
	Logging::printf("%s %x->%x %x:%x:%x:%x\n",
	                __func__, msg.cpuid_index, reg,
	                msg.cpu->eax,
	                msg.cpu->ebx,
	                msg.cpu->ecx,
	                msg.cpu->edx);
*/

    msg.mtr_out |= MTD_GPR_ACDB;
    return res;
  }

  /**
   * Return current TSC offset. Works around destroyed value in
   * msg.cpu->tsc_off when we want to change the TSC offset.
   */
  long long get_tsc_off(CpuMessage &msg)
  {
    assert(msg.mtr_in & MTD_TSC);

    return (msg.mtr_out & MTD_TSC) ? msg.current_tsc_off : msg.cpu->tsc_off;
  }



  void handle_rdtsc(CpuMessage &msg) {
    assert((msg.mtr_in & MTD_TSC) and (msg.mtr_in & MTD_GPR_ACDB));
    msg.cpu->edx_eax(get_tsc_off(msg) + Cpu::rdtsc());
    msg.mtr_out |= MTD_GPR_ACDB;
  }

  enum {
    IA32_PLATFORM_ID     = 0x17,
    IA32_FEATURE_CONTROL = 0x3a,
    IA32_BIOS_SIGN_ID    = 0x8b,
    /* should be solely available if CPUID.0AH: EAX[15:8] > PMC_nr (0, 1, ..) */
    IA32_PMC0            = 0xc1,
    IA32_PMC1            = 0xc2,
    /* Linux seems to not care about PCM0/1 detection ? */
    IA32_MTRRCAP         = 0xfe,
    MISC_FEATURE_ENABLES = 0x140,
    IA32_PERFEVTSEL0     = 0x186,
    IA32_PERFEVTSEL1     = 0x187,
    IA32_MISC_ENABLE     = 0x1a0, /* Intel */

    IA32_SPEC_CTRL       = 0x48,
  };

	enum AMD {
	    MSR_PRED_CMD_IBPB    = 0x49, /* AMD indirect branch prediction barrier */

	    MSR_K7_EVNTSEL0      = 0xc0010000,
	    MSR_K7_PERFCTR0      = 0xc0010004,
	    MSR_K7_EVNTSEL1      = 0xc0010001,
	    MSR_K7_PERFCTR1      = 0xc0010005,
	    MSR_K7_EVNTSEL2      = 0xc0010002,
	    MSR_K7_PERFCTR2      = 0xc0010006,
	    MSR_K7_EVNTSEL3      = 0xc0010003,
	    MSR_K7_PERFCTR3      = 0xc0010007,

	    AMD64_NB_CFG           = 0xc001001f,
	    AMD_CORE_ENERGY_STATUS = 0xc001029a,
	    AMD_PKG_ENERGY_STATUS  = 0xc001029b,
	    AMD64_DE_CFG           = 0xc0011029,
	};

	enum Intel {
		MSR_SMI_COUNT               = 0x34,
		MSR_UNC_CBO_CONFIG          = 0x396,
		MSR_PKG_C3_RESIDENCY        = 0x3f8,
		MSR_PKG_C6_RESIDENCY        = 0x3f9,
		MSR_PKG_C7_RESIDENCY        = 0x3fa,
		MSR_CORE_C3_RESIDENCY       = 0x3fc,
		MSR_CORE_C6_RESIDENCY       = 0x3fd,
		MSR_CORE_C7_RESIDENCY       = 0x3fe,
		MSR_RAPL_POWER_UNIT         = 0x606,
		MSR_PKG_C2_RESIDENCY        = 0x60d,
		MSR_PKG_ENERGY_STATUS       = 0x611,
		MSR_DRAM_ENERGY_STATUS      = 0x619,
		MSR_PKG_C8_RESIDENCY        = 0x630,
		MSR_PKG_C9_RESIDENCY        = 0x631,
		MSR_PKG_C10_RESIDENCY       = 0x632,
		MSR_PP0_ENERGY_STATUS       = 0x639,
		MSR_PP1_ENERGY_STATUS       = 0x641,
		MSR_PLATFORM_ENERGY_COUNTER = 0x64d,
		MSR_PPERF                   = 0x64e,
		MSR_CORE_C1_RESIDENCY       = 0x660,
		MSR_UNC_PERF_GLOBAL_CTRL    = 0xe01,
		ADL_UNC_PERF_GLOBAL_CTL     = 0x2ff0,
	};

	bool _handle_rdmsr_intel(CpuMessage const &msg)
	{
		switch (msg.cpu->ecx) {
		case MSR_SMI_COUNT:
		case MSR_UNC_CBO_CONFIG:
		case MSR_PKG_C2_RESIDENCY:
		case MSR_PKG_C3_RESIDENCY:
		case MSR_PKG_C6_RESIDENCY:
		case MSR_PKG_C7_RESIDENCY:
		case MSR_CORE_C3_RESIDENCY:
		case MSR_CORE_C6_RESIDENCY:
		case MSR_CORE_C7_RESIDENCY:
		case MSR_RAPL_POWER_UNIT:
		case MSR_PKG_ENERGY_STATUS:
		case MSR_DRAM_ENERGY_STATUS:
		case MSR_PKG_C8_RESIDENCY:
		case MSR_PKG_C9_RESIDENCY:
		case MSR_PKG_C10_RESIDENCY:
		case MSR_PP0_ENERGY_STATUS:
		case MSR_PP1_ENERGY_STATUS:
		case MSR_PLATFORM_ENERGY_COUNTER:
		case MSR_PPERF:
		case MSR_CORE_C1_RESIDENCY:
			msg.cpu->edx_eax(0);
			return true;
		default:
			break;
		}
		return false;
	}

	bool _handle_rdmsr_amd(CpuMessage const &msg)
	{
		switch (msg.cpu->ecx) {
		case AMD_PKG_ENERGY_STATUS:
		case AMD_CORE_ENERGY_STATUS:
		case AMD64_NB_CFG:
		case AMD64_DE_CFG:
		case MSR_K7_EVNTSEL0:
		case MSR_K7_PERFCTR0:
		case MSR_K7_EVNTSEL1:
		case MSR_K7_PERFCTR1:
		case MSR_K7_EVNTSEL2:
		case MSR_K7_PERFCTR2:
		case MSR_K7_EVNTSEL3:
		case MSR_K7_PERFCTR3:
			msg.cpu->edx_eax(0);
			return true;
		default:
			break;
		}
		return false;
	}

	void handle_rdmsr(CpuMessage &msg)
	{
		bool handled = false;

		if (_intel) handled = _handle_rdmsr_intel(msg);
		if (_amd)   handled = _handle_rdmsr_amd(msg);

		if (handled) {
			msg.mtr_out |= MTD_GPR_ACDB;
			return;
		}

    switch (msg.cpu->ecx) {
    case 0x10:
      handle_rdtsc(msg);
      break;
    case 0x174 ... 0x176:
      assert(msg.mtr_in & MTD_SYSENTER);
      msg.cpu->edx_eax((&msg.cpu->sysenter_cs)[msg.cpu->ecx - 0x174]);
      break;
    case IA32_PLATFORM_ID: /* tinycore 64bit */
    case IA32_SPEC_CTRL:
    case IA32_PMC0:
    case IA32_PMC1:
    case 0xce: /* MSR_PLATFORM_INFO */
    case IA32_MTRRCAP:
    case 0x122: /* IA32_TSX_CTRL */
    case MISC_FEATURE_ENABLES: /* user mode monitor+mwait - not supported */
    case 0x179: /* MCG CAP */
    case IA32_PERFEVTSEL0:
    case IA32_PERFEVTSEL1:
    case IA32_MISC_ENABLE:
    case 0x250:
    case 0x258:
    case 0x259:
    case 0x268 ... 0x26f:
    case 0x2ff:
      msg.cpu->edx_eax(0);
      break;
    case 0x277:
      Logging::printf("[%u] IA32_PAT_MSR rdmsr %x at %x\n",
                      CPUID_EDXb, msg.cpu->ecx, msg.cpu->eip);
      msg.cpu->edx_eax(0x0007040600070406ull);
      break;
    case IA32_BIOS_SIGN_ID:
      msg.cpu->edx_eax(~0ull);
      break;
    case IA32_FEATURE_CONTROL: 
      msg.cpu->edx_eax(1 /* lock bit set -> further wrmsr not permitted */);
      break;
#ifdef __x86_64__
    case 0xc0000080:
      assert(msg.mtr_in & MTD_EFER);
      msg.cpu->edx_eax(msg.cpu->efer);
      break;
    case 0xc0000100:
      assert(msg.mtr_in & MTD_FS_GS);
      msg.cpu->edx_eax(msg.cpu->fs.base);
      break;
    case 0xc0000101:
      assert(msg.mtr_in & MTD_FS_GS);
      msg.cpu->edx_eax(msg.cpu->gs.base);
      break;
    case 0xc0000102:
      assert(msg.mtr_in & MTD_SYSCALL_SWAPGS);
      msg.cpu->edx_eax(msg.cpu->kernel_gs);
      break;
#endif
    default:
      Logging::printf("[%u] unsupported rdmsr %x at %x\n",
                      CPUID_EDXb, msg.cpu->ecx, msg.cpu->eip);
      msg.cpu->edx_eax(0);
      //GP0(msg);
    }
    msg.mtr_out |= MTD_GPR_ACDB;
  }

	bool _handle_wrmsr_intel(CpuMessage const &msg) const
	{
		switch (msg.cpu->ecx) {
		case MSR_UNC_PERF_GLOBAL_CTRL:
		case ADL_UNC_PERF_GLOBAL_CTL:
			return true;
		default:
			break;
		}
		return false;
	}

	bool _handle_wrmsr_amd(CpuMessage const &msg) const
	{
		switch (msg.cpu->ecx) {
		case AMD64_NB_CFG:
		case AMD64_DE_CFG:
		case MSR_K7_EVNTSEL0:
		case MSR_K7_PERFCTR0:
		case MSR_K7_EVNTSEL1:
		case MSR_K7_PERFCTR1:
		case MSR_K7_EVNTSEL2:
		case MSR_K7_PERFCTR2:
		case MSR_K7_EVNTSEL3:
		case MSR_K7_PERFCTR3:
			return true;
		default:
			break;
		}
		return false;
	}

	void handle_wrmsr(CpuMessage &msg)
	{
		bool handled = false;

		if (_intel) handled = _handle_wrmsr_intel(msg);
		if (_amd)   handled = _handle_wrmsr_amd(msg);

		if (handled)
			return;

		CpuState *cpu = msg.cpu;

    switch (cpu->ecx)
      {
      case 0x10:
        assert(msg.mtr_in & MTD_TSC);
        {
          long long offset    = get_tsc_off(msg);

          msg.current_tsc_off = - Cpu::rdtsc()        + cpu->edx_eax();
          cpu->tsc_off        =   msg.current_tsc_off - offset;
        }
        msg.mtr_out |= MTD_TSC;
        break;
      case IA32_BIOS_SIGN_ID:
        break;
      case IA32_PMC0:
      case IA32_PMC1:
        break;
      case MISC_FEATURE_ENABLES: /* user mode monitor+mwait - not supported */
        break;
      case MSR_PRED_CMD_IBPB:
        break;

      case 0x174 ... 0x176:
        (&cpu->sysenter_cs)[cpu->ecx - 0x174] = uintptr_t(cpu->edx_eax());
        msg.mtr_out |= MTD_SYSENTER;
        break;
      case 0x1d9: /* debug ctl - unsupported */
        Logging::printf("[%u] unsupported wrmsr debug ctl\n", CPUID_EDXb);
        break;
#ifdef __x86_64__
      case 0xc0000080:
        /* Bit 1-7 reserved, Bit 9 reserved */
        /* Intel: 12-63 bit reserved, AMD: 16-63 bit reserved */
        /* non masking out leads to SMP Linux early bootstrap issue */
        cpu->efer    = msg.cpu->edx_eax();
        if (_amd)
          cpu->efer &= 0xfd01ull;
        else
          cpu->efer &= 0x0d01ull;

        msg.mtr_out |= MTD_EFER;
        break;
      case 0xc0000081:
        cpu->star    = msg.cpu->edx_eax();
        msg.mtr_out |= MTD_SYSCALL_SWAPGS;
        assert(msg.mtr_in & MTD_SYSCALL_SWAPGS);
        break;
      case 0xc0000082:
        cpu->lstar   = msg.cpu->edx_eax();
        msg.mtr_out |= MTD_SYSCALL_SWAPGS;
        assert(msg.mtr_in & MTD_SYSCALL_SWAPGS);
        break;
      case 0xc0000083:
        cpu->cstar   = msg.cpu->edx_eax();
        msg.mtr_out |= MTD_SYSCALL_SWAPGS;
        assert(msg.mtr_in & MTD_SYSCALL_SWAPGS);
        break;
      case 0xc0000084:
        cpu->fmask   = msg.cpu->edx_eax();
        msg.mtr_out |= MTD_SYSCALL_SWAPGS;
        assert(msg.mtr_in & MTD_SYSCALL_SWAPGS);
        break;
      case 0xc0000100:
        cpu->fs.base  = msg.cpu->edx_eax();
        msg.mtr_out  |= MTD_FS_GS;
        assert(msg.mtr_in & MTD_FS_GS);
        break;
      case 0xc0000101:
        cpu->gs.base  = msg.cpu->edx_eax();
        msg.mtr_out  |= MTD_FS_GS;
        assert(msg.mtr_in & MTD_FS_GS);
        break;
      case 0xc0000102:
        cpu->kernel_gs  = msg.cpu->edx_eax();
        msg.mtr_out    |= MTD_SYSCALL_SWAPGS;
        assert(msg.mtr_in & MTD_SYSCALL_SWAPGS);
        break;
#endif
      default:
        Logging::printf("[%u] unsupported wrmsr %x <-(%x:%x) at %lx\n",
                        CPUID_EDXb, cpu->ecx, cpu->edx, cpu->eax, cpu->rip);
        //GP0(msg);
      }
  }

  bool _cpu(char const * const cpu_string) const
  {
    unsigned const reg { };
    unsigned eax { }, ebx { }, ecx { }, edx { };

    CPUID_read(reg | 0, eax);
    CPUID_read(reg | 1, ebx);
    CPUID_read(reg | 2, ecx);
    CPUID_read(reg | 3, edx);

    if (*reinterpret_cast<uint32 const *>(cpu_string + 0) == ebx &&
        *reinterpret_cast<uint32 const *>(cpu_string + 4) == edx &&
        *reinterpret_cast<uint32 const *>(cpu_string + 8) == ecx)
        return true;

    return false;
  }

  void handle_cpu_init(CpuMessage &msg, bool reset) {
    CpuState *cpu = msg.cpu;

    _amd   = _cpu("AuthenticAMD");
    _intel = _cpu("GenuineIntel");

    // http://www.sandpile.org/ia32/initial.htm
    // XXX Review initial settings of {tr,ld,gd,id}.ar

    // Set all register values to zero. This also clears inj_info.
    cpu->clear();

    cpu->efl      = 2;
    cpu->eip      = 0xfff0;
    cpu->cr0      = 0x10;
    cpu->cs.ar    = 0x93;
    cpu->cs.limit = 0xffff;
    cpu->cs.base  = 0xffff0000;
    cpu->cs.sel   = 0xf000;
    cpu->ss.ar    = 0x93;
    cpu->edx      = 0x600;
    cpu->ds.ar    = cpu->es.ar = cpu->fs.ar = cpu->gs.ar = cpu->ss.ar;
    cpu->ld.ar    = 0x1000;
    cpu->tr.ar    = 0x8b;
    cpu->ss.limit = cpu->ds.limit = cpu->es.limit = cpu->fs.limit = cpu->gs.limit = cpu->cs.limit;
    cpu->tr.limit = cpu->ld.limit = cpu->gd.limit = cpu->id.limit = 0xffff;
    /*cpu->dr6      = 0xffff0ff0;*/
    // cpu->_dr = {0, 0, 0, 0};
    cpu->dr7      = 0x400;

#ifdef __x86_64__
    if (_amd) {
      /*
       * AMD-V spec. 15.5.1 Basic Operation -> Canonicalization and Consistency Checks
       * SVME must be set
       */
      cpu->efer = 1UL << 12;
    }
#endif

    msg.mtr_out  |= MTD_ALL & ~(MTD_R8_R15 | MTD_SYSCALL_SWAPGS);
    if (reset) {
      if (false)
        Logging::printf("[%u] reset CPU from %x mtr_in %x\n",
                        CPUID_EDXb, msg.type, msg.mtr_in);

      #if DEBUG_IOEXITS
      memset(debugioin , 0, sizeof(debugioin));
      memset(debugioout, 0, sizeof(debugioout));
      #endif
      // XXX reset TSC
      // XXX floating point
      // XXX MXCSR
      // XXX MTRR
      // XXX PERF
    }


    // send LAPIC init
    LapicEvent msg2(reset ? LapicEvent::RESET : LapicEvent::INIT);
    bus_lapic.send(msg2, true);
  }


  /**
   * Prioritize different events.
   * Returns the events to clear.
   */
  void prioritize_events(CpuMessage &msg) {
    CpuState *cpu = msg.cpu;
    unsigned old_event = _event;

    assert(msg.mtr_in & MTD_STATE);
    assert(msg.mtr_in & MTD_INJ);
    assert(msg.mtr_in & MTD_RFLAGS);
    msg.mtr_out |= MTD_STATE | MTD_INJ;

    if (!old_event)  return;

    if (old_event & EVENT_RESUME) {
        Cpu::atomic_and<volatile unsigned>(&_event, ~(old_event & EVENT_RESUME));
        cpu->actv_state = 0;
    }

    if (old_event & (EVENT_DEBUG | EVENT_HOST)) {
      if (old_event & EVENT_DEBUG)
        Logging::printf("[%u] state %x event %8x eip %8x eax %x ebx %x edx %x esi %x\n",
                        CPUID_EDXb, cpu->actv_state, old_event, cpu->eip, cpu->eax, cpu->ebx, cpu->edx, cpu->esi);
      else
        if (cpu->actv_state == 1) cpu->actv_state = 0; //if cpu is in hlt wake it up
      Cpu::atomic_and<volatile unsigned>(&_event, ~(old_event & (EVENT_DEBUG | EVENT_HOST)));
      return;
    }

    if (old_event & EVENT_RESET) {
      Cpu::atomic_and<volatile unsigned>(&_event, ~VCpu::EVENT_RESET);
      handle_cpu_init(msg, true);

      // fall through as we could have got an INIT or SIPI already
    }


    // INIT
    if (old_event & EVENT_INIT) {
      Cpu::atomic_and<volatile unsigned>(&_event, ~VCpu::EVENT_INIT);
      handle_cpu_init(msg, false);
      cpu->actv_state = 3;
      // fall through as we could have got an SIPI already
    }


    // SIPI received?
    if (old_event & EVENT_SIPI) {
      cpu->rip          = 0;
      cpu->cs.sel       = _sipi & 0xff00;
      cpu->cs.base      = cpu->cs.sel << 4;
      cpu->actv_state   = 0;
      msg.mtr_out      |= MTD_CS_SS;
      Cpu::atomic_and<volatile unsigned>(&_event, ~VCpu::EVENT_SIPI);
      return;
    }

    // do block all other IRQs until we got a SIPI
    if (cpu->actv_state == 3)  return;

    // SMI
    if (old_event & EVENT_SMI && ~cpu->intr_state & 4) {
      Logging::printf("[%u] SMI received\n", CPUID_EDXb);
      Cpu::atomic_and<volatile unsigned>(&_event, ~VCpu::EVENT_SMI);
      cpu->actv_state = 0;
      // fall trough
    }

    // if we have injections pending - return
    if (cpu->inj_info & 0x80000000) { cpu->actv_state = 0; return; }

    // NMI
    if (old_event & EVENT_NMI && ~cpu->intr_state & 8 && !(cpu->intr_state & 3)) {
      Logging::printf("[%u] inject NMI %x\n", CPUID_EDXb, old_event);
      cpu->inj_info = 0x80000202;
      cpu->actv_state = 0;
      Cpu::atomic_and<volatile unsigned>(&_event, ~VCpu::EVENT_NMI);
      return;
    }

    // if we can not inject interrupts or if we are in shutdown state return
    if (cpu->intr_state & 0x3 || ~cpu->efl & 0x200 || cpu->actv_state == 2) return;

    unsigned long intr = _intr_hint;

    LapicEvent msg2(LapicEvent::INTA);
    if (old_event & EVENT_EXTINT) {
      // EXTINT IRQ via MSI or IPI: INTA directly from the PIC
      Cpu::atomic_and<volatile unsigned>(&_event, ~VCpu::EVENT_EXTINT);
      LapicEvent check(LapicEvent::CHECK_INTR);
      check.value = 0;
      if (receive(check) && check.value)
        receive(msg2);
      else {
        return;
      }
    }
    else if (intr & 1) {
      // interrupt from the APIC or directly via INTR line - INTA via LAPIC
      // do not clear EVENT_INTR here, as the PIC or the LAPIC will do this for us
      LapicEvent check(LapicEvent::CHECK_INTR);
      check.value = 0;
      if (bus_lapic.send(check, true) && check.value) {
        bus_lapic.send(msg2, true);
      } else {
        Cpu::cmpxchg8b(&_intr_hint, intr, (intr + 4) & ~1ULL);
        Cpu::atomic_and<volatile unsigned>(&_event, ~EVENT_INTR);
        return;
      }
    } else return;

    cpu->inj_info = msg2.value | 0x80000000;
    cpu->actv_state = 0;
  }

  void handle_ioin(CpuMessage &msg) {
    MessageIOIn msg2(MessageIOIn::Type(msg.io_order), msg.port);
    bool res = _mb.bus_ioin.send(msg2);

    Cpu::move(msg.dst, &msg2.value, msg.io_order);
    msg.mtr_out |= MTD_GPR_ACDB;

    #if DEBUG_IOEXITS
    if (!res && ~debugioin[msg.port >> 3] & (1u << (msg.port & 7))) {
      debugioin[msg.port >> 3] |= uint8(1u << (msg.port & 7));
      Logging::printf("[%u] could not read from ioport %x eip %x cs %x-%x\n",
                      CPUID_EDXb, msg.port, msg.cpu->eip, msg.cpu->cs.base, msg.cpu->cs.ar);
    } else msg.consumed = 1;
    #else
    msg.consumed = 1;
    #endif
  }


  void handle_ioout(CpuMessage &msg) {
    MessageIOOut msg2(MessageIOOut::Type(msg.io_order), msg.port, 0);
    Cpu::move(&msg2.value, msg.dst, msg.io_order);

    bool res = _mb.bus_ioout.send(msg2);

    #if DEBUG_IOEXITS
    if (!res && ~debugioout[msg.port >> 3] & (1 << (msg.port & 7))) {
      debugioout[msg.port >> 3] |= uint8(1u << (msg.port & 7));
      Logging::printf("[%u] could not write %x to ioport %x eip %x\n",
                      CPUID_EDXb, msg.cpu->eax, msg.port, msg.cpu->eip);
    } else msg.consumed = 1;
    #else
    msg.consumed = 1;
    #endif
  }

  /**
   * We received an asynchronous event. As code runs in many
   * threads, state updates have to be atomic!
   */
  void got_event(unsigned value) {
    COUNTER_INC("EVENT");

    Cpu::atomic_xadd<unsigned long, unsigned>(&_intr_hint, 4);
    if (value & EVENT_INTR) Cpu::atomic_or<unsigned long>(&_intr_hint, 1);

    /* Avoid delayed DEASS messages. The event loop clears INTR itself.
    if (value & DEASS_INTR) Cpu::atomic_and<volatile unsigned>(&_event, ~EVENT_INTR);*/
    if (!((~(_event & ~EVENT_INTR) & value) & (EVENT_MASK | EVENT_DEBUG | EVENT_HOST))) return;

    // INIT or AP RESET - go to the wait-for-sipi state
    if ((value & EVENT_MASK) == EVENT_INIT)
      _sipi = 0;


    if ((value & EVENT_MASK) == EVENT_SIPI) {
      /**
       * try to claim the SIPI field, if it is empty, we are waiting
       * for a SIPI. If it fails, somebody else was faster and we do
       * not wakeup the client.
       */
      if (Cpu::cmpxchg4b(&_sipi, 0, value))
        return;
    }

    Cpu::atomic_or<volatile unsigned>(&_event, STATE_WAKEUP | (value & (EVENT_MASK | EVENT_DEBUG | EVENT_HOST | EVENT_RESUME)));


    MessageHostOp msg(MessageHostOp::OP_VCPU_RELEASE, _hostop_id, _event & STATE_BLOCK);
    _mb.bus_hostop.send(msg);
  }

public:
  /**
   * Forward MEM requests to the motherboard.
   */
  bool claim(MessageMem &msg) { /* The entire vCPU subsystem should be bypassing */ return true; }
  bool receive(MessageMem &msg) { return _mb.bus_mem.send(msg, true); }
  bool receive(MessageMemRegion &msg) { return _mb.bus_memregion.send(msg, true); }


  bool receive(CpuEvent &msg) { got_event(msg.value); return true; }
  bool receive(MessageLegacy &msg) {
    if (msg.type == MessageLegacy::RESET) {
      got_event(EVENT_RESET);
      return true;
    }

    if (msg.type == MessageLegacy::UNLOCK) {
        got_event(EVENT_RESUME);
        return true;
    }

    // BSP receives only legacy signals if the LAPIC is disabled
    if (is_ap() || CPUID_EDX1 & (1 << 9)) return false;

    if (msg.type == MessageLegacy::INTR)
      got_event(EVENT_INTR);
    else if (msg.type == MessageLegacy::DEASS_INTR)
      got_event(DEASS_INTR);
    else if (msg.type == MessageLegacy::NMI)
      got_event(EVENT_NMI);
    else if (msg.type == MessageLegacy::INIT)
      got_event(EVENT_INIT);
    else return false;
    return true;
  }

  /**
   * Handle the INTA ourself in the case that there is no LAPIC or it
   * is HW disabled.
   */
  bool  receive(LapicEvent &msg) {
    if (msg.type == LapicEvent::INTA) {
      MessageLegacy msg2(MessageLegacy::INTA, msg.value);
      if (_mb.bus_legacy.send(msg2))
	msg.value = msg2.value;
      return true;
    }
    if (msg.type == LapicEvent::CHECK_INTR) {
      MessageLegacy check(MessageLegacy::CHECK_INTR);
      _mb.bus_legacy.send(check);
      msg.value = (check.value & 0xff00);
      return true;
    }
    return false;
  }

  bool claim(CpuMessage &msg) { /* Entire vCPU subsystem should be bypassing */ return true; }
  bool receive(CpuMessage &msg) {

    if (msg.type == CpuMessage::TYPE_ADD_TSC_OFF) {
        _reset_tsc_off += msg.current_tsc_off;
        return true;
    }

    // TSC drift compensation.
    if (msg.type != CpuMessage::TYPE_CPUID_WRITE && msg.mtr_in & MTD_TSC && ~msg.mtr_out & MTD_TSC) {
      COUNTER_INC("tsc adoption");
      msg.current_tsc_off = _reset_tsc_off;
      msg.cpu->tsc_off    = msg.current_tsc_off - msg.cpu->tsc_off;
      msg.mtr_out |= MTD_TSC;
    }


    switch (msg.type) {
    case CpuMessage::TYPE_CPUID:    return handle_cpuid(msg);
    case CpuMessage::TYPE_CPUID_WRITE:
      {
	unsigned reg = (msg.nr << 4) | msg.reg | (msg.nr & 0x80000000);
	unsigned old;
	if (CPUID_read(reg, old) && CPUID_write(reg, (old & msg.mask) | msg.value)) {
	  CPUID_read(reg, old);
	  return true;
	}
	return false;
      };
    case CpuMessage::TYPE_RDTSC:
      handle_rdtsc(msg);
      return true;
    case CpuMessage::TYPE_RDMSR:
      handle_rdmsr(msg);
      return true;
    case CpuMessage::TYPE_WRMSR:
      handle_wrmsr(msg);
      return true;
    case CpuMessage::TYPE_IOIN:
      handle_ioin(msg);
      return true;
    case CpuMessage::TYPE_IOOUT:
      handle_ioout(msg);
      return true;
    case CpuMessage::TYPE_INIT:
      got_event(EVENT_INIT);
      return true;
    case CpuMessage::TYPE_TRIPLE:
      msg.cpu->actv_state = 2;
      if (!is_ap())
	got_event(EVENT_RESET);
      break;
    case CpuMessage::TYPE_HLT:
      assert(!msg.cpu->actv_state);
      msg.cpu->actv_state = 1;
      break;
    case CpuMessage::TYPE_CHECK_IRQ:
      // we handle it later on
      break;
    case CpuMessage::TYPE_CALC_IRQWINDOW:
      {
	assert(msg.mtr_out & MTD_INJ);
	unsigned new_event = _event;
	msg.cpu->inj_info &= ~INJ_WIN;
	if (new_event & EVENT_INTR)                    msg.cpu->inj_info |= INJ_IRQWIN;
	if (new_event & EVENT_NMI)                     msg.cpu->inj_info |= INJ_NMIWIN;
      }
      return true;
    case CpuMessage::TYPE_SINGLE_STEP:
    case CpuMessage::TYPE_WBINVD:
    case CpuMessage::TYPE_INVD:
    case CpuMessage::TYPE_ADD_TSC_OFF:
    default:
      return false;
    }

    // handle IRQ injection
    for (prioritize_events(msg); msg.cpu->actv_state & 0x3; prioritize_events(msg)) {
      MessageHostOp msg2(MessageHostOp::OP_VCPU_BLOCK, _hostop_id);
      Cpu::atomic_or<volatile unsigned>(&_event, STATE_BLOCK);
      if (~_event & STATE_WAKEUP) _mb.bus_hostop.send(msg2);
      Cpu::atomic_and<volatile unsigned>(&_event, ~(STATE_BLOCK | STATE_WAKEUP));
    }
    return true;
  }

  VirtualCpu(VCpu *_last, Motherboard &mb) : VCpu(_last), _mb(mb), _event(0), _sipi(~0u) {
    MessageHostOp msg(this);
    if (!mb.bus_hostop.send(msg)) Logging::panic("could not create VCpu backend.");
    _hostop_id = msg.value;
    _reset_tsc_off = -Cpu::rdtsc();

    // add to the busses
    executor. add(this, VirtualCpu::receive_static<CpuMessage>);
    executor.add_iothread_callback(this, VirtualCpu::claim_static<CpuMessage>);
    bus_event.add(this, VirtualCpu::receive_static<CpuEvent>);
    mem.      add(this, VirtualCpu::receive_static<MessageMem>);
    mem.add_iothread_callback(this, VirtualCpu::claim_static<MessageMem>);
    memregion.add(this, VirtualCpu::receive_static<MessageMemRegion>);
    mb.bus_legacy.add(this, VirtualCpu::receive_static<MessageLegacy>);
    bus_lapic.add(this, VirtualCpu::receive_static<LapicEvent>);

    CPUID_reset();
  }
};

PARAM_HANDLER(vcpu,
	      "vcpu - create a new VCPU")
{
  mb.last_vcpu = new VirtualCpu(mb.last_vcpu, mb);
}
#else
VMM_REGSET(CPUID,
       VMM_REG_RW(CPUID_EAX0,  0x00, 2, ~0u,)
       VMM_REG_RW(CPUID_EBX0,  0x01, 0, ~0u,)
       VMM_REG_RW(CPUID_ECX0,  0x02, 0, ~0u,)
       VMM_REG_RW(CPUID_EDX0,  0x03, 0, ~0u,)
       VMM_REG_RW(CPUID_EAX1,  0x10, 0x673, ~0u,)
       VMM_REG_RW(CPUID_EBX1,  0x11, 1u << 16, ~0u,) /* max of logical CPU ids - disable SMT */
       VMM_REG_RW(CPUID_ECX1,  0x12, 0, ~0u,)
       VMM_REG_RW(CPUID_EDX1,  0x13, 0, ~0u,)
       VMM_REG_RW(CPUID_EDXb,  0xb3, 0, ~0u,)
       VMM_REG_RW(CPUID_EAX80, 0x80000000, 0x80000004, ~0u,)
       VMM_REG_RW(CPUID_EBX80, 0x80000001, 0, ~0u,)
       VMM_REG_RW(CPUID_ECX80, 0x80000002, 0, ~0u,)
       VMM_REG_RW(CPUID_EDX80, 0x80000003, 0, ~0u,)
       VMM_REG_RW(CPUID_EAX81, 0x80000010, 0, ~0u,)
       VMM_REG_RW(CPUID_EBX81, 0x80000011, 0, ~0u,)
       VMM_REG_RW(CPUID_ECX81, 0x80000012, 0, ~0u,)
       VMM_REG_RW(CPUID_EDX81, 0x80000013, 0, ~0u,)
       VMM_REG_RW(CPUID_EAX82, 0x80000020, 0, ~0u,)
       VMM_REG_RW(CPUID_EBX82, 0x80000021, 0, ~0u,)
       VMM_REG_RW(CPUID_ECX82, 0x80000022, 0, ~0u,)
       VMM_REG_RW(CPUID_EDX82, 0x80000023, 0, ~0u,)
       VMM_REG_RW(CPUID_EAX83, 0x80000030, 0, ~0u,)
       VMM_REG_RW(CPUID_EBX83, 0x80000031, 0, ~0u,)
       VMM_REG_RW(CPUID_ECX83, 0x80000032, 0, ~0u,)
       VMM_REG_RW(CPUID_EDX83, 0x80000033, 0, ~0u,)
       VMM_REG_RW(CPUID_EAX84, 0x80000040, 0, ~0u,)
       VMM_REG_RW(CPUID_EBX84, 0x80000041, 0, ~0u,)
       VMM_REG_RW(CPUID_ECX84, 0x80000042, 0, ~0u,)
       VMM_REG_RW(CPUID_EDX84, 0x80000043, 0, ~0u,))
#endif
