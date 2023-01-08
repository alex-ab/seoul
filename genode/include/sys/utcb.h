/*
 * User Thread Control Block (UTCB)
 *
 * Copyright (C) 2008, Udo Steinberg <udo@hypervisor.org>
 * Copyright (C) 2008-2010, Bernhard Kauer <bk@vmmon.org>
 * Copyright (C) 2011-2023, Alexander Boettcher
 * Economic rights: Technische Universitaet Dresden (Germany)
 *
 * This file is part of NUL.
 *
 * NUL is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 as
 * published by the Free Software Foundation.
 *
 * NUL is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
 * License version 2 for more details.
 */

#pragma once
#include "desc.h"
#include "service/cpu.h"
#include "service/helper.h"
#include "service/string.h"
#include "service/logging.h"

#define GREG(NAME)             \
  union {                      \
    struct {                   \
      unsigned char   NAME##l; \
      unsigned char   NAME##h; \
    };                         \
    unsigned short    NAME##x; \
    unsigned       e##NAME##x; \
    mword          r##NAME##x; \
  }

#define GREG16(NAME)           \
  union {                      \
    unsigned short    NAME;    \
    unsigned       e##NAME;    \
    mword          r##NAME;    \
  }


struct Utcb
{
  typedef struct Descriptor
  {
    uint16 sel, ar;
    uint32 limit;
    union {
      uint64 : 64;
      mword base;
    };

    void set(unsigned short _sel, unsigned _base, unsigned _limit, unsigned short _ar) {
      sel = _sel; base = _base; limit = _limit; ar = _ar; };

    /* 1 - 16bit, 2 - 32bit, 3 - 64bit */
    unsigned size_type() const
    {
      if (ar & (1u << 9)) return 3;
      return (ar & (1u << 10)) ? 2 : 1;
    }

    template<typename T>
    T clamp_to_size_type(T address)
    {
      auto const type = size_type();
      if (type == 1) return address & 0xfffflu;
      if (type == 2) return address & 0xfffffffflu;
      return address;
    }

    mword limit_type()
    {
      auto const type = size_type();
      if (type == 3) return ~0UL;
      return limit;
    }
  } Descriptor;

  struct head {
    union {
      struct {
        uint16 untyped;
        uint16 typed;
      };
      mword mtr;
    };
    mword crd_translate;
    mword crd;
    mword cpuid;
  } head;
  union {
    struct {
      mword     mtd;
      mword     inst_len;
      GREG16(ip); GREG16(fl);
      uint32     intr_state, actv_state, inj_info, inj_error;
      union {
	struct {
	  GREG(a);    GREG(c);    GREG(d);    GREG(b);
	  GREG16(sp); GREG16(bp); GREG16(si); GREG16(di);
#ifdef __x86_64__
          mword r8, r9, r10, r11, r12, r13, r14, r15;
#endif
	};
#ifdef __x86_64__
        mword gpr[16];
#else
        mword gpr[8];
#endif
      };
      unsigned long long qual[2];
      unsigned     ctrl[2];
      long long reserved;
      mword    cr0, cr2, cr3, cr4;
      mword pdpte[4];
#ifdef __x86_64__
      mword        cr8;
      mword        efer;
      mword        star;
      mword        lstar;
      mword        cstar;
      mword        fmask;
      mword        kernel_gs;
      uint32       : 32;          // reserved (tpr)
      uint32       : 32;          // reserved (tpr_threshold)
#endif
      mword     dr7, sysenter_cs, sysenter_esp, sysenter_eip;
      Descriptor   es, cs, ss, ds, fs, gs;
      Descriptor   ld, tr, gd, id;
      unsigned long long tsc_value, tsc_off;
    };
    unsigned msg[(4096 - sizeof(struct head)) / sizeof(unsigned)];
  };

  bool fs_or_gs(Descriptor &desc) const {
    return (&desc == &fs) || (&desc == &gs); }
};
