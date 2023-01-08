/** @file
 * Next TLB implementation.
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
#pragma once

#include "model/config.h"
#include "memcache.h"

/**
 * A TLB implementation relying on the cache.
 */
class MemTlb : public MemCache
{
protected:
  CpuState *_cpu { };

private:

  typedef unsigned long long uint64_t;
  typedef unsigned           uint32_t;

  // pdpt cache for 32-bit PAE
  uint64_t  _pdpt[4]     { };
  uintptr_t _paging_mode { };

  enum {
    EFER_LME = 1u <<  8, EFER_LMA = 1u << 10, EFER_NXE = 1u << 11,
    CR0_WB   = 1u << 16, CR0_PG   = 1u << 31,
    CR4_PSE  = 1u <<  4, CR4_PAE  = 1u <<  5, CR4_LA57 = 1u << 12,
  };

  enum Features {
    FEATURE_NONE       =      0,
    FEATURE_PSE        = 1 << 0,
    FEATURE_PSE36      = 1 << 1,
    FEATURE_PAE        = 1 << 2,
    FEATURE_SMALL_PDPT = 1 << 3,
    FEATURE_LONG       = 1 << 4,
  };
  unsigned (*tlb_fill_func)(MemTlb *tlb, uintptr_t virt, unsigned type,
                            uintptr_t &phys) = nullptr;

#define AD_ASSIST(bits)							\
  if ((pte & (bits)) != (bits))						\
    {									\
    if (features & FEATURE_PAE) {					\
      if (Cpu::cmpxchg8b(entry->_ptr, pte, pte | bits) != pte) RETRY;	\
    }									\
    else								\
      if (Cpu::cmpxchg4b(entry->_ptr, pte, pte | bits) != pte) RETRY;	\
    }

  template <unsigned const features, typename PTE_TYPE>
  static unsigned tlb_fill(MemTlb *tlb, uintptr_t virt, unsigned type, uintptr_t &phys)
  {  return tlb->tlb_fill2<features, PTE_TYPE>(virt, type, phys); }


  template <unsigned const features, typename PTE_TYPE>
  unsigned tlb_fill2(uintptr_t virt, unsigned type, uintptr_t &phys)
  {
    PTE_TYPE rights = TYPE_R | TYPE_W | TYPE_U | TYPE_X;
    PTE_TYPE pte;

    if ( features & FEATURE_SMALL_PDPT)
      pte = PTE_TYPE(_pdpt[(virt >> 30) & 3]);
    else
      pte = PTE_TYPE(READ(cr3));

    if ( features & FEATURE_SMALL_PDPT && ~pte & 1ul)
      PF(virt, type & ~1u);

    if (~features & FEATURE_PAE || ~_paging_mode & EFER_NXE)
      type &= ~TYPE_X;

    unsigned len = (features & FEATURE_LONG) ? 4 : 2;
    bool is_sp;
    CacheEntry *entry = nullptr;
    do
      {
	if (entry) AD_ASSIST(0x20);
	if (features & FEATURE_PAE)  entry = get((pte & ~0xffful) | ((virt >> (len *  9)) & 0xff8ul), ~0xffful, 8, TYPE_R);
	else                         entry = get((pte & ~0xffful) | ((virt >> (len * 10)) & 0xffcul), ~0xffful, 4, TYPE_R);
	pte = *reinterpret_cast<PTE_TYPE *>(entry->_ptr);
	if (~pte & 1)  PF(virt, type & ~1u);
	rights &= pte | TYPE_X;
	len--;
	is_sp = len && len != 3 && pte & 0x80 && features & FEATURE_PSE;
	if (features & FEATURE_PAE && pte & (1ULL << 63)) rights &= ~TYPE_X;

	// reserved bit checking
	bool reserved_bit = false;
	if (features & FEATURE_PSE36)  reserved_bit = is_sp && pte & (1 << 21);
	else if (features & FEATURE_PAE)
	  {
	    reserved_bit = ((~_paging_mode & EFER_NXE) && (pte & (1ULL << 63)))
	      || ((~features & FEATURE_LONG) && features & FEATURE_PAE && (static_cast<uint64_t>(pte) >> 52) & 0x7ff)
	      || (((static_cast<uint64_t>(pte) & ~(1ULL << 63)) >> PHYS_ADDR_SIZE) || (is_sp && (pte >> 12) & ((1<<(len*9))-1)));
	  }
	if (reserved_bit)  PF(virt, type | 9);
      } while (len && !is_sp);

    // !wp: kernel write to read-only userpage? -> put the page as kernel read-write in the TLB
    if ((~_paging_mode & CR0_WB) && (type & TYPE_W) && (~type & TYPE_U) &&
        (~rights & TYPE_W) && (rights & TYPE_U))
      rights = (rights | TYPE_W) & ~TYPE_U;

    // enough rights?
    if ((rights & type) != type)  PF(virt, type | 1);

    // delete write flag, if we do not write and the dirty flag is not set
    if (~type & TYPE_W && ~pte & 1 << 6)  rights &= ~TYPE_W;

    // update A+D bits
    AD_ASSIST((rights & 3) << 5);

    unsigned size = ((features & FEATURE_PAE) ? 9 : 10) * len + 12;
    if (features & FEATURE_PSE36 && is_sp)
      phys = ((pte >> 22) | ((pte & 0x1fe000) >> 2));
    else {
      if (sizeof(uintptr_t) == 4 && ((pte >> size) >= ((1ull << sizeof(uintptr_t)) * 8)))
        Logging::panic("memtlb - unsupported size");

      phys = uintptr_t(pte >> size);
    }
    phys = (phys << size) | (virt & ((1ul << size) - 1));

    // The upper bits might contain OS specific data which should not
    // propagate into the TLB. (vmmon)
    // This includes the XD/NX bit when using PAE on 64 bit host (ssumpf).
    phys &= (1ULL << PHYS_ADDR_SIZE) - 1;

    return _fault;
  }

  int virt_to_phys(uintptr_t const virt, Type const type, uintptr_t &phys)
  {
    if (tlb_fill_func)
      return tlb_fill_func(this, virt, type, phys);

    phys = virt;
    return _fault;
  }

  /**
   * Find a CacheEntry to a virtual memory access.
   */
  CacheEntry *find_virtual(uintptr_t virt, size_t len, Type type)
  {
    uintptr_t phys1, phys2;

    if (!virt_to_phys(virt, type, phys1)) {
      if (!((virt ^ (virt + len - 1)) & ~0xffful))
        phys2 = ~0ul;
      else
        if (virt_to_phys(virt + len - 1, type, phys2))
          return 0;

      return get(phys1, phys2 & ~0xffful, len, type);
    }
    return 0;
  }

protected:

  Type user_access(Type type) {
    if (_cpu->cpl() == 3) return Type(TYPE_U | type);
    return type;
  }


  int init()
  {
    #ifdef __x86_64__
    auto const msr_efer = READ(efer);
    #else
    auto const msr_efer = 0;
    #endif

    /*
     * EFER_LMA is a status bit set by hardware (see Intel spec),
     * EFER_LME is changeable by guest.
     */
    _paging_mode = (READ(cr0) & ( CR0_PG  | CR0_WB))
                 | (READ(cr4) & ( CR4_PAE | CR4_PSE))
                 | (msr_efer  & (EFER_LME | EFER_LMA | EFER_NXE));

    /* no support for 57bit virtual addresses, requires Intel 5-level-paging */
    assert(!(READ(cr4) & CR4_LA57));

    /* fetch PDPTs in legacy 32bit PAE mode (when both LME AND LMA are off) */
    if ((_paging_mode & (CR0_PG | EFER_LMA | EFER_LME | CR4_PAE)) == (CR0_PG | CR4_PAE)) {
      for (unsigned i = 0; i < 4; i++) {

        auto const entry = get((READ(cr3) & ~0x1ful) + i * 8, ~0xffful, 8, TYPE_R);
        _pdpt[i] = *reinterpret_cast<uint64_t *>(entry->_ptr);

        if ((_pdpt[i] & 0x1e6) || (_pdpt[i] >> PHYS_ADDR_SIZE))
          GP0;
      }
    }

    // set paging mode
    tlb_fill_func = nullptr;
    if (_paging_mode & CR0_PG) {
      tlb_fill_func = &tlb_fill<FEATURE_NONE, uint32_t>;

      if (_paging_mode & CR4_PSE)
        tlb_fill_func = &tlb_fill<FEATURE_PSE | FEATURE_PSE36, uint32_t>;

      if (_paging_mode & CR4_PAE) {
        tlb_fill_func = &tlb_fill<FEATURE_PSE | FEATURE_PAE | FEATURE_SMALL_PDPT, uint64_t>;

        if (_paging_mode & (EFER_LME | EFER_LMA))
          tlb_fill_func = &tlb_fill<FEATURE_PSE | FEATURE_PAE | FEATURE_LONG, uint64_t>;
      }
    }

    return _fault;
  }


  /**
   * Read the len instruction-bytes at the given address into a buffer.
   */
  int read_code(uintptr_t virt, size_t len, void *buffer)
  {
    assert(len < 16);

    auto * const entry = find_virtual(virt & ~3ul,
                                      (len + (virt & 3ul) + 3) & ~3ul,
                                      user_access(Type(TYPE_X | TYPE_R)));
    if (entry) {
      assert(len <= entry->_len);
      memcpy(buffer, entry->_ptr + (virt & 3ul), len);
    } else {
      // fix CR2 value as we rounded down
      if (_fault == 0x80000b0e && _cpu->cr2 < virt)
        _cpu->cr2 = virt;
    }

    return _fault;
  }


  int prepare_virtual(uintptr_t virt, size_t len, Type type, void *&ptr)
  {
    bool   const round = (virt | len) & 3ul;
    auto * const entry = find_virtual(virt & ~3ul,
                                      (len + (virt & 3ul) + 3) & ~3ul,
                                      round ? Type(type | TYPE_R) : type);
    if (entry) {
      assert(len <= entry->_len);
      ptr = entry->_ptr + (virt & 3ul);
    }
    return _fault;
  }


  MemTlb(DBus<MessageMem> &mem, DBus<MessageMemRegion> &memregion)
  : MemCache(mem, memregion) {}
};
