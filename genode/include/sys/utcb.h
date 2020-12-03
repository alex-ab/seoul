/*
 * User Thread Control Block (UTCB)
 *
 * Copyright (C) 2008, Udo Steinberg <udo@hypervisor.org>
 * Copyright (C) 2008-2010, Bernhard Kauer <bk@vmmon.org>
 * Copyright (C) 2011, Alexander Boettcher <ab764283@os.inf.tu-dresden.de>
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

#define GREG(NAME)                                  \
  union {                                           \
    struct {                                        \
      unsigned char           NAME##l, NAME##h;     \
    };                                              \
    unsigned short          NAME##x;                \
    unsigned           e##NAME##x;                  \
    mword              r##NAME##x;                  \
  }
#define GREG16(NAME)                        \
  union {                                   \
    unsigned short          NAME;           \
    unsigned           e##NAME;             \
    mword              r##NAME##x;          \
  }


struct Utcb
{
  enum {
    STACK_START = 512,          ///< Index where we store a "frame pointer" to the top of the stack
  };

  typedef struct Descriptor
  {
    uint16 sel, ar;
    uint32 limit;
    union {
      uint64 : 64;
      mword base;
    };
    void set(unsigned short _sel, unsigned _base, unsigned _limit, unsigned short _ar) { sel = _sel; base = _base; limit = _limit; ar = _ar; };
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
    mword nul_cpunr;
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
      mword        : 64;          // reserved (efer)
      mword        : 64;          // reserved (star)
      mword        : 64;          // reserved (lstar)
      mword        : 64;          // reserved (cstar)
      mword        : 64;          // reserved (fmask)
      mword        : 64;          // reserved (kernel_gs_base)
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

  // XXX
  enum { MINSHIFT = 12 };
  enum {
    HEADER_SIZE = sizeof(struct head),
    MAX_DATA_WORDS = sizeof(msg) / sizeof(msg[0]),
    MAX_FRAME_WORDS = MAX_DATA_WORDS - STACK_START - 1,
  };

  /**
   * Returns the number of words needed for storing the current UTCB
   * content to a UTCB frame as implemented in add_frame().
   */
  unsigned frame_words() { return HEADER_SIZE/sizeof(msg[0]) + head.untyped + 2*head.typed + 1; }

  unsigned *item_start() { return reinterpret_cast<unsigned *>(this + 1) - 2*head.typed; }

  // Optional check to avoid IPCs where receiver will reject
  // the message because of validate_recv_bounds() result.
  bool validate_send_bounds() {
    return
      (head.untyped <= STACK_START) and
      (head.typed*2 <= MAX_DATA_WORDS - STACK_START - 1) and
      (frame_words() <= MAX_DATA_WORDS - STACK_START - 1);
  }

  // Check whether the UTCB is empty (e.g. after receiving a message
  // through a portal) and does not violate our message size
  // constraints.
  bool validate_recv_bounds()
  {
    return
      (msg[STACK_START] == 0) and
      (head.untyped <= STACK_START) and
      (head.typed*2 <= MAX_DATA_WORDS - STACK_START - 1) and
      (frame_words() <= MAX_DATA_WORDS - STACK_START - 1);
  }

  /**
   * Add mappings to a UTCB.
   *
   * @param addr Start of the memory area to be delegated/translated
   * @param size Size of the memory area to be delegated/translated
   * @param hotspot Zero for translation or hotspot | flags | MAP_MAP for delegation.
   * @param rights Permission mask and type Crd type bits.
   * @param frame Set this to true if the receiver uses Utcb::Frame
   * and you want him to pass bound checks.
   * @param max_items The maximum number of typed items to be put in UTCB.
   *
   * @return Size of memory left which couldn't be put on the utcb
   * because no space is left. If this is not zero, the caller has to
   * handle this case! See sigma0.cc map_self for inspiration.
   */
  VMM_WARN_UNUSED
  unsigned long add_mappings(unsigned long addr, unsigned long size, unsigned long hotspot, unsigned rights,
                             bool frame = false, unsigned max_items = sizeof(msg) / sizeof(msg[0]) / 2)
  {
    while (size > 0) {
      unsigned minshift = Cpu::minshift(addr | (hotspot & ~0xffful) , size);
      assert(minshift >= Utcb::MINSHIFT);
      this->head.typed++;
      unsigned *item = this->item_start();
      if (item < &msg[head.untyped] ||
          head.typed > max_items ||
          (frame && !validate_send_bounds()))
        { this->head.typed --; return size; }
      item[1] = hotspot;
      item[0] = addr | ((minshift-Utcb::MINSHIFT) << 7) | rights;
      unsigned long mapsize = 1 << minshift;
      size    -= mapsize;
      addr    += mapsize;
      hotspot += mapsize;
    }
    return size;
  }

  void reset() {
    head.mtr = 0;
    this->msg[STACK_START] = 0;
  }
};
