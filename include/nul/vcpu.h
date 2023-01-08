/** @file
 * External Virtual CPU interface.
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
#pragma once
#include "bus.h"
#include "executor/cpustate.h"
#include "message.h"

/**
 * The size in bytes of the VCPU parameters.
 *
 * We need 5 (register-in, param-in, param-out, register-out, empty).
 */
#define SIZEOF_VCPU_PARAMETER  12
#define NUM_VCPU_PARAMETER     5

struct CpuMessage {
  enum Type {
    TYPE_CPUID_WRITE,
    TYPE_CPUID,
    TYPE_RDTSC,
    TYPE_RDMSR,
    TYPE_WRMSR,
    TYPE_IOIN,
    TYPE_IOOUT,
    TYPE_TRIPLE,
    TYPE_INIT,
    TYPE_HLT,
    TYPE_INVD,
    TYPE_WBINVD,
    TYPE_CHECK_IRQ,
    TYPE_CALC_IRQWINDOW,
    TYPE_SINGLE_STEP,
    TYPE_ADD_TSC_OFF,
  } type;
  union {
    struct {
      CpuState *cpu;
      union {
        unsigned  cpuid_index;
        struct {
          unsigned  io_order;
          unsigned  short port;
          void     *dst;
        };
      };
    };
    struct {
      unsigned nr;
      unsigned reg;
      unsigned mask;
      unsigned value;
    };
  };
  unsigned mtr_in;
  unsigned mtr_out { 0 };
  unsigned consumed { 0 }; //info whether a model consumed this event

  // When TSC offset is adjusted, the current absolute offset is kept
  // here, as the vcpu structure will then only contain the adjustment
  // the kernel is to apply. This member is only valid, iff mtr_out &
  // MTD_TSC is true;
  long long current_tsc_off { 0 };

  CpuMessage(Type _type, CpuState *_cpu, unsigned _mtr_in) : type(_type), cpu(_cpu), mtr_in(_mtr_in) { if (type == TYPE_CPUID) cpuid_index = cpu->eax; }
  CpuMessage(unsigned _nr, unsigned _reg, unsigned _mask, unsigned _value) : type(TYPE_CPUID_WRITE), nr(_nr), reg(_reg), mask(_mask), value(_value), mtr_in(0) {}
  CpuMessage(bool is_in, CpuState *_cpu, unsigned _io_order, unsigned _port, void *_dst, unsigned _mtr_in)
  : type(is_in ? TYPE_IOIN : TYPE_IOOUT), cpu(_cpu), io_order(_io_order), port(static_cast<unsigned short>(_port)), dst(_dst), mtr_in(_mtr_in) {}
};


struct CpuEvent {
  unsigned value;
  CpuEvent(unsigned _value) : value(_value) {}
};


struct LapicEvent {
  enum Type{
    INTA,
    RESET,
    INIT,
    CHECK_INTR
  } type;
  unsigned value;
  LapicEvent(Type _type) : type(_type), value((type == INTA) ? ~0u : 0) {}
};


class VCpu
{
  VCpu *_last;
public:
  DBus<CpuMessage>       executor  { };
  DBus<CpuEvent>         bus_event { };
  DBus<LapicEvent>       bus_lapic { };
  DBus<MessageMem>       mem       { };
  DBus<MessageMemRegion> memregion { };

  VCpu *get_last() { return _last; }
  bool is_ap()     { return _last; }

  bool set_cpuid(unsigned nr, unsigned reg, unsigned value, unsigned mask=~0) {  CpuMessage msg(nr, reg, ~mask, value & mask); return executor.send(msg); }
  enum {
    EVENT_INTR   = 1 <<  0,
    EVENT_FIXED  = 1 <<  0,
    EVENT_LOWEST = 1 <<  1,
    EVENT_SMI    = 1 <<  2,
    EVENT_RRD    = 1 <<  3,
    EVENT_RESET  = 1 <<  3,
    EVENT_NMI    = 1 <<  4,
    EVENT_INIT   = 1 <<  5,
    EVENT_SIPI   = 1 <<  6,
    EVENT_EXTINT = 1 <<  7,
    EVENT_MASK   =   0x0ff,
    // SIPI vector: bits 8-15
    DEASS_INTR   = 1 << 16,
    EVENT_DEBUG  = 1 << 17,
    STATE_BLOCK  = 1 << 18,
    STATE_WAKEUP = 1 << 19,
    EVENT_HOST   = 1 << 20,
    EVENT_RESUME = 1 << 21
  };

  struct GuestPtr {
    unsigned short seg;
    unsigned short ofs;
  };
  // the parameters for the copy-in and out loop
  struct Parameter {
    volatile unsigned short count;
    struct GuestPtr src;
    struct GuestPtr dst;
    unsigned short dummy;  // align the whole structure to dwords
  };

  // the shared-memory area
  union {
    struct Parameter params[NUM_VCPU_PARAMETER];
    char bytes[4096];
  } shmem { };
  unsigned params_used { 0 };

  /**
   * Return a pointer on the free part of the shmem area.
   */
  unsigned long get_shmem_ptr() {
    unsigned long res = (shmem.params[0].dst.seg << 4)  + shmem.params[0].dst.ofs;
    for (unsigned i=0; i < params_used; i++)  res += shmem.params[i].count;
    return res;
  }

  /**
   * Add a copy-in/out request to the parameters.
   */
  void add_param(unsigned long address, unsigned short count, bool read) {
    assert (params_used < (sizeof(shmem.params) / sizeof(Parameter)));
    GuestPtr &src = read ? shmem.params[params_used].src : shmem.params[params_used].dst;
    GuestPtr &dst = read ? shmem.params[params_used].dst : shmem.params[params_used].src;
    shmem.params[params_used].count = count;
    src.seg = uint16(address >> 4);
    src.ofs = uint16(address - (src.seg << 4));
    unsigned long shmem_ptr = get_shmem_ptr();
    dst.seg = uint16(shmem_ptr >> 4);
    dst.ofs = uint16(shmem_ptr - (dst.seg << 4));
    params_used++;
  }

  /**
   * Check whether some copyin-parameters are already available and change address accordingly.  Add a copy-in/out
   * request otherwise.
   */
  bool check_params(unsigned long &address, unsigned short count, bool read) {
    if (!params_used)
      return true;
    if (read) {
      for (unsigned i=0; i < params_used; i++) {
        unsigned long src_adr = (shmem.params[i].src.seg << 4) + shmem.params[i].src.ofs;
        if (address == src_adr && shmem.params[i].count == count) {
          address = (shmem.params[i].dst.seg << 4) + shmem.params[i].dst.ofs;
          return true;
        }
      }
      add_param(address, count, true);
      return false;
    }
    else {
      unsigned long naddress = get_shmem_ptr();
      add_param(address, count, false);
      address = naddress;
      return true;
    }
  }

  /**
   * Make VCPU-local guest memory available to a model.  This includes the LAPIC and the Shmem-BIOS area.
   */
  bool copy_inout(unsigned long address, void *ptr, unsigned short count, bool read)
  {
//    Logging::printf("copy_%s(%lx, %x) %d\n", read ? "in" : "out", address, count, params_used);
    if (!check_params(address, count, read)) return false;

    MessageMemRegion msg(address >> 12);
    if (!memregion.send(msg) || !msg.ptr || ((address + count) > ((msg.start_page + msg.count) << 12))) {
      char *p = reinterpret_cast<char *>(ptr);

      if (address & 3) {
	unsigned value;
	MessageMem msg(true, address & ~3, &value);
	if (!mem.send(msg, true)) return false;
	unsigned short l = 4 - (address & 3);
	if (l > count) l = count;
	memcpy(reinterpret_cast<char *>(&value) + (address & 3), p, l);
	if (!read) {
	  msg.read = false;
	  if (!mem.send(msg, true)) return false;
	}
	p       += l;
	address += l;
	count   -= l;
      }
      while (count >= 4) {
	MessageMem msg(false, address, reinterpret_cast<unsigned *>(p));
	if (!mem.send(msg, read)) return false;
	address += 4;
	p       += 4;
	count   -= 4;
      }
      if (count) {
	unsigned value;
	MessageMem msg(true, address, &value);
	if (!mem.send(msg, true)) return false;
	memcpy(&value, p, count);
	if (!read) {
	  msg.read = false;
	  if (!mem.send(msg, true)) return false;
	}
      }
      return true;
    }
    if (read)
      memcpy(ptr, msg.ptr + (address - (msg.start_page << 12)), count);
    else
      memcpy(msg.ptr + (address - (msg.start_page << 12)), ptr, count);
    return true;
  }
  bool copy_in(unsigned long address, void *ptr, unsigned short count)  { return copy_inout(address, ptr, count, true);  }
  bool copy_out(unsigned long address, void *ptr, unsigned short count) { return copy_inout(address, ptr, count, false); }

  unsigned long long inj_count;
  VCpu (VCpu *last) : _last(last), inj_count(0) {}
};
