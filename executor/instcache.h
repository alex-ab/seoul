/** @file
 * InstructionCache for NovaHalifax.
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

#ifdef __i386__
#define VMM_REG(X)          e ## X
#define VMM_ASM_WORD_TYPE   ".long"
#else
#define VMM_REG(X)          r ## X
#define VMM_ASM_WORD_TYPE   ".quad"
#endif

#define VMM_STRING(x)       # x
#define VMM_EXPAND(x)       VMM_STRING(x)

/**
 * Reverse MTR mapping.
 */
enum
  {
    RMTR_eip = MTD_RIP_LEN,
    RMTR_ripx = MTD_RIP_LEN,
    RMTR_efl = MTD_RFLAGS,
    RMTR_cr0 = MTD_CR,
    RMTR_cr2 = MTD_CR,
    RMTR_cr3 = MTD_CR,
    RMTR_cr4 = MTD_CR,
    RMTR_cs  = MTD_CS_SS,
    RMTR_ss  = MTD_CS_SS,
    RMTR_efer = MTD_EFER,
  };

/**
 * Faults.
 */
enum {
  FAULT_NOERROR,
  FAULT_RETRY,
  FAULT_RECALL,
  FAULT_UNIMPLEMENTED,
};

#include "memtlb.h"


enum {
  MRM_EAX    = 1 << 8,
  MRM_REG    = 1 << 9,
  MRM_SIB    = 1 << 10,
  MRM_SS     = 1 << 11,
  MRM_DISSHIFT = 12,
  MRM_DIS08 = 1 << MRM_DISSHIFT,
  MRM_DIS16 = 2 << MRM_DISSHIFT,
  MRM_DIS32 = 3 << MRM_DISSHIFT,
  MRM_NOBASE  = 1 << 14,
  MRM_NOINDEX = 1 << 15,
};

/**
 * Lookup table for modrm decoding.
 */
static const unsigned short modrminfo[64] =
  {
    0x36            , 0x37            , 0x56 | MRM_SS            , 0x57 | MRM_SS            , 0x06            , 0x07            ,        MRM_DIS16         , 0x03            ,
    0x36 | MRM_DIS08, 0x37 | MRM_DIS08, 0x56 | MRM_SS | MRM_DIS08, 0x57 | MRM_SS | MRM_DIS08, 0x06 | MRM_DIS08, 0x07 | MRM_DIS08, 0x50 | MRM_DIS08 | MRM_SS, 0x03 | MRM_DIS08,
    0x36 | MRM_DIS16, 0x37 | MRM_DIS16, 0x56 | MRM_SS | MRM_DIS16, 0x57 | MRM_SS | MRM_DIS16, 0x06 | MRM_DIS16, 0x07 | MRM_DIS16, 0x50 | MRM_DIS16 | MRM_SS, 0x03 | MRM_DIS16,
    MRM_EAX| MRM_REG, 0x01 | MRM_REG  , 0x02 | MRM_REG           , 0x03 | MRM_REG           , 0x04 | MRM_REG  , 0x05 | MRM_REG  , 0x06 | MRM_REG           , 0x07 | MRM_REG  ,

    MRM_EAX           , 0x01            , 0x02            , 0x03            , MRM_SIB           ,        MRM_DIS32         , 0x06            , 0x07            ,
    MRM_EAX| MRM_DIS08, 0x01 | MRM_DIS08, 0x02 | MRM_DIS08, 0x03 | MRM_DIS08, MRM_SIB| MRM_DIS08, 0x05 | MRM_DIS08 | MRM_SS, 0x06 | MRM_DIS08, 0x07 | MRM_DIS08,
    MRM_EAX| MRM_DIS32, 0x01 | MRM_DIS32, 0x02 | MRM_DIS32, 0x03 | MRM_DIS32, MRM_SIB| MRM_DIS32, 0x05 | MRM_DIS32 | MRM_SS, 0x06 | MRM_DIS32, 0x07 | MRM_DIS32,
    MRM_EAX| MRM_REG  , 0x01 | MRM_REG  , 0x02 | MRM_REG  , 0x03 | MRM_REG  , 0x04 | MRM_REG    , 0x05 | MRM_REG           , 0x06 | MRM_REG  , 0x07 | MRM_REG  ,
  };


class InstructionCache;

/**
 * The data that is cached between different runs.
 */
struct InstructionCacheEntry
{
  enum {
    MAX_INSTLEN = 15,
  };
  // the index into data where the main operand byte lives also used to find the MODRM byte
  unsigned char offset_opcode;
  unsigned char data[MAX_INSTLEN];
  unsigned flags;
  unsigned inst_len;
  unsigned operand_size;
  unsigned address_size;
  unsigned modrminfo;
  struct {
    uint16 raw;

    /* 1 - 16bit, 2 - 32bit, 3 - 64bit */
    unsigned size_type() const
    {
      if (raw & (1u << 9)) return 3;
      return (raw & (1u << 10)) ? 2 : 1;
    }
  } cs_ar;

  unsigned prefixes;

  bool prefix_lock() { return (prefixes & 0xff) == 0xf0; }

  /* REX prefix in 64bit 0100wrxb */
  bool prefix_rex()  { return (cs_ar.size_type() == 3) && ((prefixes & 0xf0) == 0x40); }
  bool reg64()       { return prefix_rex() && (prefixes & 0x4); /* REX.R */ }
  bool rex_w()       { return prefix_rex() && (prefixes & 0x8); /* REX.W */ }
  bool rex_b()       { return prefix_rex() && (prefixes & 0x1); /* REX.B */ }

  void __attribute__((regparm(3))) (*execute)(InstructionCache *instr, void *tmp_src, void *tmp_dst);
  void     *src;
  void     *dst;
  mword     immediate;
};


/**
 * An instruction cache that keeps decoded instructions.
 */
class InstructionCache : public MemTlb
{
  enum EFLAGS {
    EFL_ZF  = 1 <<  6,
    EFL_TF  = 1 <<  8,
    EFL_IF  = 1 <<  9,
    EFL_OF  = 1 << 11,
    EFL_IOPL= 3 << 12,
    EFL_NT  = 1 << 14,
    EFL_RF  = 1 << 16,
    EFL_VM  = 1 << 17,
    EFL_AC  = 1 << 18,
    EFL_VIF = 1 << 19,
    EFL_VIP = 1 << 20
  };


  enum {
    IC_ASM       = 1 <<  0,
    IC_SAVEFLAGS = 1 <<  1,
    IC_LOADFLAGS = 1 <<  2,
    IC_MODRM     = 1 <<  3,
    IC_DIRECTION = 1 <<  4,
    IC_READONLY  = 1 <<  5,
    IC_BYTE      = 1 <<  6,
    IC_LOCK      = 1 <<  7,
    IC_BITS      = 1 <<  8,
    IC_RMW       = 1 <<  9,
    IC_MOFS      = 1 << 10,
    IC_QWORD     = 1 << 11,
  };


  enum {
    SIZE = 64,
    ASSOZ = 4
  };

  unsigned _pos;
  unsigned long _tags[SIZE*ASSOZ];
  InstructionCacheEntry _values[SIZE*ASSOZ];

  unsigned long slot(unsigned long const tag) const {
    return ((tag ^ (tag/SIZE)) % SIZE) * ASSOZ; }


  // cpu state
  VCpu   * _vcpu;
  InstructionCacheEntry *_entry;
  mword _oeip { };
  mword _oesp { };
  unsigned _ointr_state;
  mword _dr6;
  mword _dr[4];
  unsigned _fpustate [512/sizeof(unsigned)] __attribute__((aligned(16)));

  int send_message(CpuMessage::Type type)
  {
    CpuMessage msg(type, _cpu, _mtr_in);
    _vcpu->executor.send(msg, true);
    return _fault;
  }

  int event_injection()
  {
    if (_mtr_in & MTD_INJ && _cpu->inj_info & 0x80000000 && !idt_traversal(_cpu->inj_info, _cpu->inj_error)) {
      _cpu->inj_info &= ~0x80000000;
      RETRY;
    }
    return _fault;
  };


  /**
   * Fetch code.
   */
  int fetch_code(InstructionCacheEntry *entry, unsigned len)
  {
    uintptr_t       virt  = READ(ripx) + entry->inst_len;
    uintptr_t const limit = READ(cs).limit_type();
    uintptr_t const base  = READ(cs).base;

    if ((~limit && limit < (virt + len - 1)) || ((entry->inst_len + len) > InstructionCacheEntry::MAX_INSTLEN)) {
      #ifdef __x86_64__
        Logging::printf("GP0 in fetch code ... efer=%lx\n", READ(efer));
      #endif
      Logging::printf("GP0 in fetch_code virt %lx cs base/limit %lx/%lx\n", virt, base, limit);
      GP0;
    }

    virt += base;

    read_code(virt, len, entry->data + entry->inst_len);
    entry->inst_len += len;

    if (_fault)
      Logging::printf("fetch code ... _fault=%x virt=%lx len=%x entry->inst_len=%x\n", _fault, virt, len, entry->inst_len);
    return _fault;
  }


  /**
   * Find a cache entry for the given state and checks whether it is
   * still valid.
   */
  bool find_entry(unsigned long &index)
  {
    auto const cs_ar  = READ(cs).ar;
    auto const linear = _cpu->ripx + READ(cs).base;

    for (auto i = slot(linear); i < slot(linear) + ASSOZ; i++)
      if (linear == _tags[i] &&  _values[i].inst_len)
	{
	  InstructionCacheEntry tmp;
	  tmp.inst_len = 0;
	  // revalidate entries
	  if (fetch_code(&tmp, _values[i].inst_len)) return false;

	  // either code modified or two entries with different bases?
	  if (memcmp(tmp.data, _values[i].data, _values[i].inst_len) || cs_ar != _values[i].cs_ar.raw)  continue;
	  index = i;
	  //COUNTER_INC("I$ ok");
	  return true;
	}
    // allocate new invalid entry
    index = slot(linear) + (_pos++ % ASSOZ);
    memset(_values + index, 0, sizeof(*_values));
    _values[index].cs_ar.raw = cs_ar;
    _values[index].prefixes = 0x8300; // default is to use the DS segment
    _tags[index] = linear;
    return false;
  }


  /**
   * Fetch the modrm byte including sib byte and displacement.
   */
  int get_modrm()
  {
    fetch_code(_entry, 1);
    unsigned char  modrm = _entry->data[_entry->inst_len - 1];

    /*
     * https://wiki.osdev.org/X86-64_Instruction_Encoding#ModR.2FM_and_SIB_bytes
     *
     * The first 32 entries of the modrminfo array are for 16bit.
     * The next  32 entries of the modrminfo array are for 32bit and 64bit usable.
     * For 64bit the rex.b (rex_b()) bit selects register r8-r15 instead of r0-r8.
     */

    unsigned short info = modrminfo[((!!((_entry->address_size >= 2))) << 5) |
                                    ((modrm >> 3) & 0x18) | (modrm & 0x7)];

    // sib byte
    if (info & MRM_SIB) {
      fetch_code(_entry, 1);

      if ((modrm & 0xc7) == 0x4 && (_entry->data[_entry->inst_len - 1] & 0x7) == 5)
        info |= MRM_DIS32 | MRM_NOBASE;

      info = static_cast<unsigned short>((info & ~0xff) | _entry->data[_entry->inst_len - 1]);

      if (((info >> 3) & 0xf) == 4)
        info |= MRM_NOINDEX;

      if (~info & MRM_NOBASE && ((info & 0xf) == 4 || (info & 0xf) == 5))
        info |= MRM_SS;
    }

    unsigned disp = ((info >> MRM_DISSHIFT) & 0x3);
    if (disp)
      fetch_code(_entry, 1 << (disp-1));

    _entry->modrminfo = info;

    // SS segment is default for this modrm?
    if (((_entry->prefixes & 0xff00) == 0x8300) && info & MRM_SS)
      _entry->prefixes = (_entry->prefixes & ~0xff00) | 0x200;

    return _fault;
  }



#include "insthelper.h"
#include "instructions.h"
#include "instructions.inc"

public:
  /**
   * Decode the instruction.
   */
  int get_instruction()
  {
    //COUNTER_INC("INSTR");
    unsigned long index = 0;
    if (!find_entry(index) && !_fault) {

      _entry = _values + index;
      _entry->address_size = _entry->operand_size = _entry->cs_ar.size_type();

      /* in long mode the default operand size is 32bit */
      if (_entry->cs_ar.size_type() == 3)
        _entry->operand_size = 2;

      for (int op_mode = 0; !_entry->execute && !_fault; )
      {
        /**
         * Handle a new byte of the instruction.
         *
         * The op_mode, keeps track which parts of the opcode bytes have
         * already been seen.  Negative if the whole instruction is fetched.
         */

        //fetch_code(_entry, 1) || handle_code_byte(_entry, _entry->data[_entry->inst_len-1], op_mode);

        bool const error = fetch_code(_entry, 1);

        if (!error) {
          auto const instruction = _entry->data[_entry->inst_len - 1];

          /*
           * REX prefix in 64bit 0100wrxb
           * - 0x40-0x4f (inc/dec) not available as in 32bit
           */
          bool const rex = (instruction & 0xf0) == 0x40;

          if (mode_64() && (rex)) {

            _entry->prefixes = instruction;

            bool const prefix_66 = (instruction & 0xff) == 0x66;
            bool const prefix_67 = (instruction & 0xff) == 0x67;

			if (prefix_66 || prefix_67) {
				Logging::printf("----------- ... A 66=%d 67=%d rip=%lx rex=%d ar.size_type()=%x operand_size=%u address_size=%u _entry=%p prefixes=%x\n",
				                prefix_66, prefix_67, _cpu->ripx, rex, _entry->cs_ar.size_type(),
				                _entry->operand_size, _entry->address_size, _entry, _entry->prefixes);
			 }

            if (_entry->rex_w())
              _entry->operand_size = 3;

            if (prefix_66 || prefix_67) {

              assert(_entry->address_size == 3);
//              if (_entry->rex_w()) /* force address size to 64bit */
//                _entry->address_size = 3;

              _entry->address_size = 2;

              if (!_entry->rex_w())
                _entry->operand_size = 1;

              Logging::printf("... B 66=%d 67=%d rip=%lx rex=%d ar.size_type()=%x operand_size=%u address_size=%u _entry=%p prefixes=%x\n",
                              prefix_66, prefix_67, _cpu->ripx, rex, _entry->cs_ar.size_type(),
                              _entry->operand_size, _entry->address_size, _entry, _entry->prefixes);
            }

            continue;
          }

          handle_code_byte(_entry, instruction, op_mode);

        }
      }

      if (_fault) {
        _entry->inst_len = 0;
        Logging::printf("decode fault %x ip=%lx\n", _fault, _cpu->ripx);
        return _fault;
      }

      assert(_values[index].execute);
      //COUNTER_INC("decoded");
    }

    _entry = _values + index;
    _cpu->ripx += _entry->inst_len;

    bool show = false;
    if (mode_64() && _entry->inst_len != _cpu->inst_len)
      show = true;


    if (debug || show) {
	Logging::printf("rip %x:%lx rsp %lx eax %x ebp %x prefix %x _fault=%x execute_ptr=%p inst_len=%u vs cpu->inst_len=%lu modrminfo=%x\n",
	                _cpu->cs.sel, _oeip, _oesp, _cpu->eax, _cpu->ebp, _entry->prefixes, _fault,
	                _values[index].execute, _entry->inst_len, _cpu->inst_len, _entry->modrminfo);
	Logging::printf(".byte ");
	for (unsigned i = 0; i < _entry->inst_len; i++)
	    Logging::printf("0x%02x%c", _entry->data[i], (i == _entry->inst_len - 1) ? '\n' : ',');
      }


//    if (_entry->inst_len != _cpu->inst_len)
//      Logging::panic("inst_len does not match");

    if (_entry->prefix_rex() && _entry->prefixes & 0x2 /* REX.X */) {
      Logging::printf("REX prefix %x NOT IMPLEMENTED !!!!!!!!!!!!!!!!! ripx=%lx\n", _entry->prefixes, _cpu->ripx);
      Logging::printf(".byte ");
      for (unsigned i = 0; i < _entry->inst_len; i++)
        Logging::printf("0x%02x%c", _entry->data[i], (i == _entry->inst_len - 1) ? '\n' : ',');
      Logging::panic("implement me\n");
    }

    return _fault;
  }

  bool mode_64()
  {
#ifdef __x86_64__
    return _cpu->efer & (1u << 10);
#else
    return false;
#endif
  }

  mword *get_reg32(unsigned reg_raw)
  {
    auto const reg = (reg_raw & 0x7u) + (_entry->reg64() ? 8u : 0u);

    if (mode_64() && reg != reg_raw)
      Logging::printf("%s %u != %u (raw)\n", __func__, reg, reg_raw);
    return _cpu->gpr + reg;
  }

  /**
   * Get a GPR.
   */
  template<bool bytereg>
  void *get_reg(unsigned reg_raw)
  {
    auto const reg = (reg_raw & 0x7) + (_entry->reg64() ? 8 : 0);

    if (mode_64() && bytereg && reg >= 4 && reg < 8)
       Logging::printf("%s reg=%u + byte %u\n", __func__, (reg & 0x3), ((reg & 0x4) >> 2));

    void *res = _cpu->gpr + reg;
    if (bytereg && reg >= 4 && reg < 8 /* sp, bp, si, di */)
      res = reinterpret_cast<char *>(_cpu->gpr+(reg & 0x3)) + ((reg & 0x4) >> 2);
    return res;
  }



  unsigned long modrm2virt()
  {
    auto          const info        = static_cast<unsigned short>(_entry->modrminfo);
    auto                disp_offset = _entry->data + _entry->offset_opcode + 1;
    unsigned      const gpr_r8_r15  = _entry->rex_b() ? 0x8 : 0;
    unsigned long       virt        = 0;

    if (info & MRM_SIB) {
      // add base + scaled index
      if (~info & MRM_NOBASE)   { virt += _cpu->gpr[gpr_r8_r15 + (info & 0x7)]; }
      if (~info & MRM_NOINDEX)  { virt += _cpu->gpr[gpr_r8_r15 + ((info >> 3) & 0x7)] << ((info >> 6) & 0x3); }
      disp_offset++;
    } else {
      if (info & 0xf || info & MRM_EAX) { virt += _cpu->gpr[gpr_r8_r15 + (info & 0x7)]; }
      if (info & 0xf0) { virt += _cpu->gpr[gpr_r8_r15 + ((info >> 4) & 0x7)]; }
    }

    unsigned disp = ((info >> MRM_DISSHIFT) & 0x3);

    switch (disp) {
      case 0:  break;
      case 1:  virt += *reinterpret_cast<char  *>(disp_offset); break;
      case 2:  virt += *reinterpret_cast<short *>(disp_offset); break;
      case 3:  virt += *reinterpret_cast<int   *>(disp_offset); break;
      default:
        Logging::printf("unknown disp size %u - check me XXX\n", disp);
        break;
    }

    if (_entry->flags & IC_BITS) {
      auto const bitofs = *get_reg32(_entry->data[_entry->offset_opcode] >> 3);
      virt += (bitofs >> 3) & ~((1ul << _entry->operand_size) - 1);
    }

    return virt;
  }


  int virt_to_ptr(void *&res, unsigned length, Type type, unsigned long virt)
  {
    InstructionCache::handle_segment((&_cpu->es) + ((_entry->prefixes >> 8) & 0x0f), virt, length, type & TYPE_W, false)
      || prepare_virtual(virt, length, type, res);
    return _fault;
  }


  /**
   * Convert modrm to a pointer in cache or RAM.
   */
  int modrm2mem(void *&res, unsigned length, Type type)
  {
    auto const info = _entry->modrminfo;
    if (info & MRM_REG)
      res = (length == 1) ? get_reg<1>(info) : get_reg<0>(info);
    else
      virt_to_ptr(res, length, type, modrm2virt());
    return _fault;
  }

#ifdef __x86_64__
#    define PARAM1       "=D"
#    define PARAM2       "=S"
#    define PARAM3       "=d"
#    define CLOBBER      "memory", "rax", "rcx"
#else
#    define PARAM1       "=a"
#    define PARAM2       "=d"
#    define PARAM3       "=c"
#    define CLOBBER      "memory"
#endif

  void call_asm(void *tmp_src, void *tmp_dst)
  {
    mword tmp_flag;
    mword dummy1, dummy2, dummy3;
    switch (_entry->flags & (IC_LOADFLAGS | IC_SAVEFLAGS))
      {
      case IC_SAVEFLAGS:
	asm volatile ("call *%4; pushf; pop %3"
		      : PARAM1(dummy1), PARAM2(dummy2), PARAM3(dummy3), "=g"(tmp_flag)
		      : "m"(_entry->execute), "0"(this), "1"(tmp_src), "2"(tmp_dst) : CLOBBER);
	_cpu->rflx = (_cpu->rflx & ~0x8d5) | (tmp_flag  & 0x8d5);
	_mtr_out |= MTD_RFLAGS;
	break;
      case IC_LOADFLAGS:
	tmp_flag = _cpu->efl & 0x8d5;
	asm volatile ("push %3; popf; call *%4;"
		      : PARAM1(dummy1), PARAM2(dummy2), PARAM3(dummy3), "+g"(tmp_flag)
		      : "m"(_entry->execute), "0"(this), "1"(tmp_src), "2"(tmp_dst) : CLOBBER);
	break;
      case IC_LOADFLAGS | IC_SAVEFLAGS:
	tmp_flag = _cpu->efl & 0x8d5;
	asm volatile ("push %3; popf; call *%4; pushf; pop %3"
		      : PARAM1(dummy1), PARAM2(dummy2), PARAM3(dummy3), "+g"(tmp_flag)
		      : "m"(_entry->execute), "0"(this), "1"(tmp_src), "2"(tmp_dst) : CLOBBER);
	_cpu->rflx = (_cpu->rflx & ~0x8d5) | (tmp_flag  & 0x8d5);
	_mtr_out |= MTD_RFLAGS;
	break;
      default:
	asm volatile ("call *%3;"
		      : PARAM1(dummy1), PARAM2(dummy2), PARAM3(dummy3)
		      : "m"(_entry->execute), "0"(this), "1"(tmp_src), "2"(tmp_dst) : CLOBBER);
	break;
      }
  }


  /**
   * Execute the instruction.
   */
  int execute()
  {

    //COUNTER_INC("executed");
    assert(_entry->execute);
    unsigned length = (_entry->flags & IC_BYTE) ? 1 : (_entry->flags & IC_QWORD ? 8 : 1 << _entry->operand_size);
    void *tmp_src   = _entry->src;
    void *tmp_dst   = _entry->dst;

    if (_entry->prefix_lock() && ((~_entry->flags & IC_LOCK) || (_entry->modrminfo & MRM_REG))) {
      Logging::panic("LOCK prefix %02x%02x%02x%02x at rip %x:%lx\n", _entry->data[0], _entry->data[1], _entry->data[2], _entry->data[3], _cpu->cs.sel, _cpu->ripx);
      UD0;
    }

    Type type = TYPE_R;
    if (!(_entry->flags & (IC_DIRECTION | IC_READONLY))) type = TYPE_W;
    if (_entry->flags & IC_RMW) type = Type(type | TYPE_R);
    if (_entry->flags & IC_MODRM)
      {
	if (modrm2mem(tmp_dst, length, type)) return _fault;
      }
    if (_entry->flags & IC_MOFS)
      {
	uintptr_t virt = 0;
	move(&virt, _entry->data+_entry->offset_opcode, _entry->address_size);
	if (virt_to_ptr(tmp_dst, length, type, virt)) return _fault;
      }
    if (_entry->flags & IC_DIRECTION)
      {
	void *tmp = tmp_src;
	tmp_src = tmp_dst;
	tmp_dst = tmp;
      }
    if (_entry->flags & IC_ASM)
      call_asm(tmp_src, tmp_dst);
    else
      _entry->execute(this, tmp_src, tmp_dst);

    /**
     * Have we accessed more than we are allowed to?
     * Do a recall with more state.
     */
    if (_mtr_read & ~_mtr_in)
      {
	Logging::panic("recall %x out of %x\n", _mtr_read, _mtr_in);
	// signal a recall
	//COUNTER_INC("recall");
	FAULT(this, FAULT_RECALL);
      };
    return _fault;
  }


  /**
   * Commits the instruction by setting the appropriate UTCB fields.
   */
  bool commit()
  {
    // irq blocking propagation
    if (_fault)  _cpu->intr_state = _ointr_state;
    if (_cpu->intr_state != _ointr_state)
      _mtr_out |= MTD_STATE;

    if (!_fault || _fault == FAULT_RETRY) {
      /* success */
      _mtr_out |= MTD_RIP_LEN | MTD_GPR_ACDB | MTD_R8_R15 | MTD_GPR_BSD;
      if (_cpu->rspx != _oesp) _mtr_out |= MTD_RSP;

      // XXX bugs?
      _mtr_out |= _mtr_in & ~(MTD_CR | MTD_TSC);
    } else {
	_cpu->ripx = _oeip;
	if (~_fault & 0x80000000)
	  {
	    if (_entry)  _cpu->inst_len = _entry->inst_len; else _cpu->inst_len = 0;
	    switch (_fault)
	      {
	      case FAULT_UNIMPLEMENTED:
		Logging::panic("unimplemented at line %d rip %lx\n", _debug_fault_line, _cpu->ripx);
		// unimplemented
		return false;
	      default:
		Logging::panic("internal fault %x at rip %lx\n", _fault, _cpu->ripx);
	      }
	  }
	else
	  {
	    _mtr_out |= MTD_INJ;

//	    if (_fault != 0x80000b0eu) /* don't show message for page fault */
	      Logging::printf("fault: %x old %x error %x cr2 %lx at rip %lx line %d\n", _fault, _cpu->inj_info,
			      _error_code, _cpu->cr2, _cpu->ripx, _debug_fault_line);
	    // consolidate two exceptions

	    // triple fault ?
	    unsigned old_info = _cpu->inj_info & ~INJ_WIN;
	    if (old_info == 0x80000b08)
	      {
		_cpu->inj_info = 0;
		// triple fault
		CpuMessage msg(CpuMessage::TYPE_TRIPLE, _cpu, _mtr_in);
		_vcpu->executor.send(msg, true);
	      }
	    else
	      {
		if ((old_info & _fault & 0x80000700) == 0x80000300)
		  if (((0x3c01 & (1 << (old_info & 0xff))) && (0x3c01 & (1 << (_fault & 0xff))))
		      || (old_info == 0x80000b0e && (0x7c01 & (1 << (_fault & 0xff)))))
		    {
		      _fault = 0x80000b08;
		      _error_code = 0;
		    }
		_cpu->inj_info = _fault;
		_cpu->inj_error = _error_code;
	      }
	  }
      }
    return true;
  }

public:

  void step(CpuMessage &msg) {
    _cpu = msg.cpu;
    _mtr_in = msg.mtr_in;
    _mtr_out =  msg.mtr_out;
    _fault = 0;
    if (!init()) {
      _entry = 0;
      _oeip = _cpu->ripx;
      _oesp = _cpu->rspx;
      _ointr_state = _cpu->intr_state;
      // remove sti+movss blocking
      _cpu->intr_state &= ~3;

      event_injection() || get_instruction() || execute();

      if (commit()) invalidate(true);
    } else {
      /* commit init() _fault */
      commit();
    }
    msg.mtr_out = _mtr_out;
  }

 InstructionCache(VCpu *vcpu) : MemTlb(vcpu->mem, vcpu->memregion), _pos(), _tags(), _values(), _vcpu(vcpu), _entry(), _ointr_state(), _dr6(), _dr(), _fpustate() { }
};
