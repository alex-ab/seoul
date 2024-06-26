// -*- Mode: C++ -*-
/**
 * @file
 * Message Type definitions.
 *
 * Copyright (C) 2009, Bernhard Kauer <bk@vmmon.org>
 * Economic rights: Technische Universitaet Dresden (Germany)
 *
 * Copyright (C) 2013 Jacek Galowicz, Intel Corporation.
 * Copyright (C) 2013 Markus Partheymueller, Intel Corporation.
 *
 * Copyright (C) 2021-2024 Alexander Boettcher
 *
 * This file is part of Seoul/Vancouver.
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

#include <nul/types.h>
#include <nul/compiler.h>

class VCpu;

struct MessageIOThread
{
  VCpu *vcpu;
  enum Type {
    TYPE_IOIN,
    TYPE_IOOUT,
    TYPE_MEM,
    TYPE_INPUT,
    TYPE_IRQ,
    TYPE_IRQLINES,
    TYPE_IRQNOTIFY,
    TYPE_NETWORK,
    TYPE_DISK,
    TYPE_DISKCOMMIT,
    TYPE_LEGACY,
    TYPE_TIME,
    TYPE_TIMER,
    TYPE_TIMEOUT,
    TYPE_PCICFG,
    TYPE_HOSTOP,
    TYPE_CPU,
  } type;
  enum Mode {
    MODE_NORMAL,
    MODE_EARLYOUT,
    MODE_FIFO,
    MODE_RR
  } mode;
  enum Sync {
    SYNC_SYNC,
    SYNC_ASYNC
  } sync;
  unsigned *value;
  void *ptr;
  void *sem;

  MessageIOThread(Type _type, Mode _mode, Sync _sync, void *_ptr) : vcpu(nullptr), type(_type), mode(_mode), sync(_sync), value(nullptr), ptr(_ptr), sem(nullptr) {}
  MessageIOThread(Type _type, Mode _mode, Sync _sync, unsigned *_value, void *_ptr) : vcpu(nullptr), type(_type), mode(_mode), sync(_sync), value(_value), ptr(_ptr), sem(nullptr) {}
};

/****************************************************/
/* IOIO messages                                    */
/****************************************************/
/**
 * An in() from an ioport.
 */
struct MessageIOIn
{
  enum Type {
    TYPE_INB = 0,
    TYPE_INW = 1,
    TYPE_INL = 2
  } type;
  unsigned short port;
  unsigned count;
  union {
    unsigned value;
    void *ptr;
  };
  MessageIOIn(Type _type, unsigned short _port) : type(_type), port(_port), count(0), value(~0u) {}
  MessageIOIn(Type _type, unsigned short _port, unsigned _count, void *_ptr) : type(_type), port(_port), count(_count), ptr(_ptr) {}
};

struct MessageHwIOIn : public MessageIOIn {
  MessageHwIOIn(Type _type, unsigned short _port) : MessageIOIn(_type, _port) {}
  MessageHwIOIn(Type _type, unsigned short _port, unsigned _count, void *_ptr) : MessageIOIn(_type, _port, _count, _ptr) {}
};


/**
 * An out() to an ioport.
 */
struct MessageIOOut {
  enum Type {
    TYPE_OUTB = 0,
    TYPE_OUTW = 1,
    TYPE_OUTL = 2
  } type;
  unsigned short port;
  unsigned count;
  union {
    unsigned value;
    void *ptr;
  };
  MessageIOOut(Type _type, unsigned short _port, unsigned _value) : type(_type), port(_port), count(0), value(_value) {}
  MessageIOOut(Type _type, unsigned short _port, unsigned _count, void *_ptr) : type(_type), port(_port), count(_count), ptr(_ptr) {}
};

struct MessageHwIOOut : public MessageIOOut {
  MessageHwIOOut(Type _type, unsigned short _port, unsigned _value) : MessageIOOut(_type, _port, _value) {}
  MessageHwIOOut(Type _type, unsigned short _port, unsigned _count, void *_ptr) : MessageIOOut(_type, _port, _count, _ptr) {}
};


/****************************************************/
/* Memory messages                                  */
/****************************************************/

/**
 * A dword aligned memory access.
 */
struct MessageMem
{
  enum {
    MSI_ADDRESS = 0xfee00000,
    MSI_DM      = 1 << 2,
    MSI_RH      = 1 << 3
  };
  bool read;
  uint64    phys;
  unsigned *ptr;
  MessageMem(bool _read, uint64 _phys, unsigned *_ptr) : read(_read), phys(_phys), ptr(_ptr) {}
};

/**
 * Request a region that is directly mapped into our memory.  Used for
 * mapping it to the user and optimizing internal access.
 *
 * Note, that clients can also return an empty region by not setting
 * the ptr.
 */
struct MessageMemRegion
{
  uintptr_t page;
  uintptr_t start_page     { 0 };
  uintptr_t count          { 0 };
  char *    ptr            { nullptr };
  bool      actual_physmem { false};
  bool      read_only      { false};
  unsigned long const cr0  { 0 };
  MessageMemRegion(uintptr_t _page, unsigned long cr0 = 0) : page(_page), cr0(cr0) {}
};


/****************************************************/
/* PCI messages                                     */
/****************************************************/

/**
 * A PCI config space transaction.
 */
struct MessagePciConfig
{
  enum Type {
    TYPE_READ,
    TYPE_WRITE,
    TYPE_PTR, ///< Return pointer to memory mapped PCI configuration space register
  } type;
  unsigned  bdf;
  unsigned  dword;
  unsigned  value;
  unsigned *ptr { nullptr };

  MessagePciConfig(unsigned _bdf, unsigned _dword) : type(TYPE_READ), bdf(_bdf), dword(_dword), value(0xffffffff) {}
  MessagePciConfig(unsigned _bdf, unsigned _dword, unsigned _value) : type(TYPE_WRITE), bdf(_bdf), dword(_dword), value(_value) {}

};

struct MessageHwPciConfig : public MessagePciConfig {
  MessageHwPciConfig(unsigned _bdf, unsigned _dword) : MessagePciConfig(_bdf, _dword) {}
  MessageHwPciConfig(unsigned _bdf, unsigned _dword, unsigned _value) : MessagePciConfig(_bdf, _dword, _value) {}

};


/****************************************************/
/* SATA messages                                    */
/****************************************************/

class FisReceiver;

// XXX Use SATA bus to comunicated FISes
/**
 * Set a drive on a port of an AHCI controller.
 */
struct MessageAhciSetDrive
{
	FisReceiver *drive;
	unsigned const port;

	MessageAhciSetDrive(FisReceiver *_drive, unsigned _port)
	: drive(_drive), port(_port) {}
};


/****************************************************/
/* IRQ messages                                     */
/****************************************************/


/**
 * Raise an IRQ.
 */
struct MessageIrq
{
  enum Type
    {
      ASSERT_IRQ,
      ASSERT_NOTIFY,
      DEASSERT_IRQ
    } type;
  unsigned char line;

  MessageIrq(Type _type, unsigned char _line) :  type(_type), line(_line) {}
};

struct MessageIrqLines : public MessageIrq {
  MessageIrqLines(Type _type, unsigned char _line) :  MessageIrq(_type, _line) {}
};

/**
 * Notify that a level-triggered IRQ can be reraised.
 */
struct MessageIrqNotify
{
  unsigned char baseirq;
  unsigned char mask;
  MessageIrqNotify(unsigned char _baseirq, unsigned char _mask) : baseirq(_baseirq), mask(_mask)  {}
};


/**
 * Message on the PIC bus.
 */
struct MessagePic
{
  unsigned char slave;
  unsigned char vector { 0 };
  MessagePic(unsigned char _slave) :  slave(_slave) { }
};

/**
 * IPI-Message on the APIC bus.
 */
struct MessageApic
{
  enum {
    /**
     * The HW uses a special cycle for broadcast EOIs. We model that
     * by performing a write transaction to the first IOAPIC EOI
     * registers that is snooped by all IOApics.
     */
    IOAPIC_EOI = 0xfec00040,
    ICR_DM     = 1 << 11,
    ICR_ASSERT = 1 << 14,
    ICR_LEVEL  = 1 << 15
  };
  unsigned icr; // only bits 0xcfff are used
  unsigned dst; // 32bit APIC ID
  void    *ptr; // to distinguish loops
  MessageApic(unsigned _icr, unsigned _dst, void *_ptr) : icr(_icr), dst(_dst), ptr(_ptr) {};
};

/****************************************************/
/* Legacy messages                                  */
/****************************************************/

/**
 * Various messages of the legacy chips such as PIT, PPI...
 */
struct MessageLegacy
{
  enum Type
    {
      GATE_A20,
      FAST_A20,
      RESET,
      INIT,
      NMI,
      INTR,
      DEASS_INTR,
      INTA,
      UNLOCK,
      CHECK_INTR,
    } type;
  unsigned value;
  MessageLegacy(Type _type, unsigned _value=0) : type(_type), value(_value) {}
};

/**
 * Pit messages.
 */
struct MessagePit
{
  enum Type
    {
      GET_OUT,
      SET_GATE
    } type;
  unsigned pit;
  bool value;
  MessagePit(Type _type, unsigned _pit, bool _value=false) : type(_type), pit(_pit), value(_value) {}
};


/****************************************************/
/* Keyboard and Serial messages                     */
/****************************************************/
/**
 * Message on the PS2 bus between a KeyboardController and a connected Keyboard/Mouse
 */
struct MessagePS2
{
  unsigned port;
  enum Type
    {
      NOTIFY,
      READ_KEY,
      SEND_COMMAND,
      NOTIFY_ON_REPLY,
    }  type;
  unsigned char value;
  MessagePS2(unsigned _port, Type _type, unsigned char _value) : port(_port), type(_type), value(_value) {}
};


/**
 * A keycode or a mouse packet. See keyboard.h for the format.
 */
struct MessageInput
{
  unsigned device;
  unsigned data;
  unsigned data2;

  MessageInput(unsigned _device=0, unsigned _data=0, unsigned _data2=0)
  : device(_device), data(_data), data2(_data2) { }
};


/**
 * An ascii character from the serial port.
 */
struct MessageSerial
{
  unsigned serial;
  unsigned char ch;
  MessageSerial(unsigned _serial, unsigned char _ch) : serial(_serial), ch(_ch) {}
};


/****************************************************/
/* Console messages                                 */
/****************************************************/

#include "host/vesa.h"

struct VgaRegs
{
  unsigned short mode;
  unsigned short cursor_style;
  unsigned cursor_pos;
  size_t offset;
};


typedef Vbe::ModeInfoBlock ConsoleModeInfo;

/**
 * VGA Console.
 */
struct MessageConsole
{
  enum Type
    {
      // allocate a new client
      TYPE_ALLOC_CLIENT,
      // allocate a new view for a client
      TYPE_ALLOC_VIEW,
      // free a view
      TYPE_FREE_VIEW,
      // get information about a mode
      TYPE_GET_MODEINFO,
      // get a FONT
      TYPE_GET_FONT,
      // switch to another view
      TYPE_SWITCH_VIEW,
      // update of content may stop
      TYPE_PAUSE,
      // update of content may resume
      TYPE_RESUME,
      // the user requests a debug feature
      TYPE_DEBUG,
      // content of vga/framebuffer changed by model
      TYPE_CONTENT_UPDATE,
      // notification from VMM to model that modeinfo can be changed
      TYPE_MODEINFO_UPDATE,
      // notification from model to VMM that new resolution was chosen by VM
      TYPE_RESOLUTION_CHANGE,
    } type;
  unsigned short id;
  unsigned short view;
  union
  {
    struct {
      char    * ptr;
      size_t size;
      VgaRegs *regs;
    };
    struct {
      unsigned index;
      ConsoleModeInfo *info;
    };
    struct {
      unsigned input_device;
      unsigned input_data;
    };
    struct {
      unsigned x;
      unsigned y;
      unsigned width;
      unsigned height;
      unsigned hot_x;
      unsigned hot_y;
      bool     hide;
    };
  };

	struct Pair { unsigned a; unsigned b;};

	MessageConsole(unsigned _index, ConsoleModeInfo *_info, unsigned short _id)
	: type(TYPE_GET_MODEINFO), id(_id), view(0), index(_index), info(_info) {}

	MessageConsole(Type _type, unsigned short _id, size_t _size = 0, char * _ptr = nullptr, VgaRegs *_regs = nullptr)
	: type(_type), id(_id), view(0), ptr(_ptr), size(_size), regs(_regs) {}

	MessageConsole(uint16 _id, uint16 v, bool hide = false,
	               Pair cord = { }, Pair size = { }, Pair hot = { })
	: type(TYPE_CONTENT_UPDATE), id(_id), view(v),
	  x(cord.a), y(cord.b), width(size.a), height(size.b),
	  hot_x(hot.a), hot_y(hot.b), hide(hide) { }
};


/**
 * VESA support.
 */
struct MessageVesa
{
  enum Type
    {
      // return available modes
      TYPE_GET_MODEINFO,
      // switch mode
      TYPE_SWITCH_MODE
    } type;
  unsigned index;
  Vbe::ModeInfoBlock *info;
  MessageVesa(unsigned _index, Vbe::ModeInfoBlock *_info) : type(TYPE_GET_MODEINFO), index(_index), info(_info) {}
  MessageVesa(unsigned _index) : type(TYPE_SWITCH_MODE), index(_index), info(nullptr) {}
};

/****************************************************/
/* HOST messages                                    */
/****************************************************/

class VCpu;
typedef void (*ServiceThreadFn)(void *) VMM_REGPARM(0) VMM_NORETURN;

/**
 * Request to the host, such as notify irq or request IO region.
 */
struct MessageHostOp
{
  enum Type
    {
      OP_ATTACH_IRQ,
      OP_NOTIFY_IRQ,
      OP_ATTACH_MSI,
      OP_ALLOC_IOIO_REGION,
      OP_ALLOC_IOMEM,
      OP_ALLOC_IOMEM_SMALL,
      OP_ALLOC_SEMAPHORE,
      OP_ALLOC_SERVICE_THREAD,
      OP_ALLOC_SERVICE_PORTAL,
      OP_DETACH_MEM,
      OP_ASSIGN_PCI,
      OP_VIRT_TO_PHYS,
      OP_GET_MODULE,
      OP_GET_MAC,
      OP_GUEST_MEM,
      OP_RESERVE_IO_RANGE,
      OP_VCPU_CREATE_BACKEND,
      OP_VCPU_BLOCK,
      OP_VCPU_RELEASE,
      OP_WAIT_CHILD,
      OP_NEXT_DIRTY_PAGE,
      OP_GET_CONFIG_STRING,
      OP_MIGRATION_RETRIEVE_INIT,
      OP_MIGRATION_START,
      OP_VM_OFF,
    } type;
  union {
    unsigned long value;
    void * obj;
  };
  union {
    struct {
      uintptr_t phys;
      size_t phys_len;
    };
    struct {
      char *ptr;
      size_t len;
      union {
        struct { phy_cpu_no cpu; };
        struct { phy_cpu_no len_short; };
       };
      char const * desc;
    };
    struct {
      unsigned module;
      char * start;
      size_t size;
      char * cmdline;
      size_t cmdlen;
    };
    struct {
      unsigned msi_gsi;
      unsigned msi_value;
      unsigned long long msi_address;
      bool     is_hpet;
    };
    struct {
      unsigned long long mac;
    };
    struct {
      VCpu *vcpu;
    };
    struct {
      ServiceThreadFn work;
      void *work_arg;
      unsigned prio;
      phy_cpu_no cpu;
      char const * name;
    } _alloc_service_thread;
  };

  static MessageHostOp alloc_service_thread(ServiceThreadFn work, void *arg,
                                            char const * name = 0,
                                            unsigned prio = ~0U, phy_cpu_no cpu = ~0U)
  {
    MessageHostOp n(OP_ALLOC_SERVICE_THREAD, 0UL);
    n._alloc_service_thread.work = work; n._alloc_service_thread.work_arg = arg;
    n._alloc_service_thread.prio = prio; n._alloc_service_thread.cpu      = cpu;
    n._alloc_service_thread.name = name;
    return n;
  }

  static MessageHostOp attach_msi(phy_cpu_no cpu, bool locked, uint16 rid, char const * name)
  {
    MessageHostOp n(OP_ATTACH_MSI, rid, !locked, cpu);
    n.desc = name;
    n.is_hpet = false;
    return n;
  }

  static MessageHostOp attach_hpet_msi(phy_cpu_no cpu, bool locked, void *mmio, char const * name)
  {
    MessageHostOp n(OP_ATTACH_MSI, reinterpret_cast<unsigned long>(mmio), !locked, cpu);
    n.desc = name;
    n.is_hpet = true;
    return n;
  }


  static MessageHostOp attach_irq(unsigned irq, phy_cpu_no cpu, bool locked, char const * name)
  {
    MessageHostOp n(OP_ATTACH_IRQ, irq, !locked, cpu);
    n.desc = name;
    return n;
  }

  explicit MessageHostOp(VCpu *_vcpu) : type(OP_VCPU_CREATE_BACKEND), value(0), vcpu(_vcpu) {}
  explicit MessageHostOp(unsigned _module, char * _start, size_t _size=0) : type(OP_GET_MODULE), module(_module), start(_start), size(_size), cmdlen(0)  {}
  explicit MessageHostOp(Type _type, unsigned long _value, size_t _len=0, unsigned _cpu=~0U) : type(_type), value(_value), ptr(0), len(_len), cpu(_cpu) {}
};


struct MessageAcpi
{
  enum  Type {
    ACPI_GET_TABLE,
    ACPI_GET_IRQ
  } type;
  union {
    struct {
      const char *name;
      unsigned instance;
      char *table;
      size_t len;
    };
    struct {
      unsigned parent_bdf;
      unsigned bdf;
      unsigned char pin;
      unsigned gsi;
    };
  };
  MessageAcpi(const char *_name): type(ACPI_GET_TABLE), name(_name), instance(0), table(0), len(0) {}
  MessageAcpi(unsigned _parent_bdf, unsigned _bdf, unsigned char _pin): type(ACPI_GET_IRQ), parent_bdf(_parent_bdf), bdf(_bdf), pin(_pin), gsi(~0u) {}
};

/**
 * Virtual ACPI: Fixed and General Purpose Events
 * can be triggered with these messages
 */
struct MessageAcpiEvent
{
    enum EventType {
        ACPI_EVENT_FIXED,
        ACPI_EVENT_GP,
        ACPI_EVENT_HOT_UNPLUG,
        ACPI_EVENT_HOT_REPLUG,
    } type;
    unsigned num;

    MessageAcpiEvent(EventType _type, unsigned _num)
        : type(_type), num(_num) {};
};

/**
 * Resource discovery between device models is done by the virtual
 * BIOS.
 *
 * Resources can be ACPI tables, BIOS BDA, EBDA,...
 */
struct MessageDiscovery
{
  enum Type {
    DISCOVERY,
    WRITE,
    READ
  } type;
  struct {
    char const * const resource;
    size_t             offset;
    union {
      void const * const data;
      unsigned   *       dw;
    };
    size_t count;
  };
  MessageDiscovery()
    : type(DISCOVERY), resource(nullptr), offset(0), data(nullptr), count(0) {}
  MessageDiscovery(const char * _resource, size_t _offset, const void * _data, size_t _count)
    : type(WRITE), resource(_resource), offset(_offset), data(_data), count(_count) {}
  MessageDiscovery(const char * _resource, size_t _offset, unsigned * _dw)
    : type(READ), resource(_resource), offset(_offset), dw(_dw), count(0) {}
};

/****************************************************/
/* DISK messages                                    */
/****************************************************/

struct DmaDescriptor;
struct DiskParameter;

/**
 * Request/read from the disk.
 */
struct MessageDisk
{
  enum Type
    {
      DISK_GET_PARAMS,
      DISK_READ,
      DISK_WRITE,
      DISK_FLUSH_CACHE
    } type;
  unsigned disknr;
  union
  {
    DiskParameter *params;
    struct {
      unsigned long long sector;
      unsigned long usertag;
      unsigned dmacount;
      DmaDescriptor *dma;
      unsigned long physoffset;	// TODO: Is this needed now?
      unsigned long physsize;
      unsigned long more;
    };
  };
  enum Status {
    DISK_OK = 0,
    DISK_STATUS_BUSY,
    DISK_STATUS_DEVICE,
    DISK_STATUS_DMA,
    DISK_STATUS_DMA_TOO_LARGE,
    DISK_STATUS_USERTAG,
    DISK_STATUS_RESUME,
    DISK_STATUS_SHIFT = 4,
    DISK_STATUS_MASK = (1 << DISK_STATUS_SHIFT) -1
  } error;
  MessageDisk(unsigned _disknr, DiskParameter *_params) : type(DISK_GET_PARAMS), disknr(_disknr), params(_params), error(DISK_OK) {}
  MessageDisk(Type _type, unsigned _disknr, unsigned long _usertag, unsigned long long _sector,
              unsigned _dmacount, DmaDescriptor *_dma, unsigned long _physoffset, unsigned long _physsize, unsigned long _left)
    : type(_type), disknr(_disknr), sector(_sector), usertag(_usertag), dmacount(_dmacount), dma(_dma), physoffset(_physoffset), physsize(_physsize), more(_left), error(DISK_OK) {}
};


/**
 * A disk.request is completed.
 */
struct MessageDiskCommit
{
  unsigned disknr;
  unsigned long usertag;
  MessageDisk::Status status;
  MessageDiskCommit(unsigned _disknr=0, unsigned long _usertag=0, MessageDisk::Status _status=MessageDisk::DISK_OK) : disknr(_disknr), usertag(_usertag), status(_status) {}
};


/****************************************************/
/* Executor messages                                */
/****************************************************/

class CpuState;

struct MessageBios
{
  VCpu *vcpu;
  CpuState *cpu;
  unsigned irq;
  unsigned mtr_out;
  MessageBios(VCpu *_vcpu, CpuState *_cpu, unsigned _irq) : vcpu(_vcpu), cpu(_cpu), irq(_irq), mtr_out() {}
};



/****************************************************/
/* Timer messages                                   */
/****************************************************/

typedef unsigned long long timevalue;


/**
 * Timer infrastructure.
 *
 * There is no frequency and clock here, as all is based on the same
 * clocksource.
 */
struct MessageTimer
{
  enum Type
    {
      TIMER_NEW,
      TIMER_REQUEST_TIMEOUT
    } type;
  unsigned  nr;
  timevalue abstime;
  MessageTimer() : type(TIMER_NEW), nr(0), abstime(0) {}
  MessageTimer(unsigned  _nr, timevalue _abstime) : type(TIMER_REQUEST_TIMEOUT), nr(_nr), abstime(_abstime) {}
};


/**
 * A timeout triggered.
 */
struct MessageTimeout
{
  unsigned  nr;
  timevalue time;
  MessageTimeout(unsigned  _nr, timevalue _time) : nr(_nr), time(_time) {}
};


/**
 * Returns the wall clock time in microseconds.
 *
 * It also contains a timestamp of the Motherboard clock in
 * microseconds, to be able to adjust to the time already passed and
 * to detect out-of-date values.
 */
struct MessageTime
{
  enum { FREQUENCY = 1000000U };
  timevalue wallclocktime;
  timevalue timestamp;
  MessageTime() :  wallclocktime(0), timestamp(0) {}
};


/****************************************************/
/* Network messages                                 */
/****************************************************/

struct MessageNetwork
{
	enum ops {
		PACKET_TO_MODEL,
		PACKET_TO_HOST,
		QUERY_MAC,
		LINK
	} const type;

	struct data {
		union {
			struct {
				void * buffer;
				size_t len;
			};
			unsigned long long mac;
			struct { bool link_up; };
		};
	} const data;

	unsigned client;
	bool     more;

	MessageNetwork(enum ops type, struct data const data, unsigned client, bool more)
	: type(type), data(data), client(client), more(more) { }
};

struct MessageRestore
{
    enum networkStrings {
        MAGIC_STRING_DEVICE_DESC = 0x8D06F00D
    };

    enum restoreTypes {
        RESTORE_RESTART = 0, // RESTART is sent over the restore bus for initialization
        RESTORE_TIMEOUTLIST,
        RESTORE_PIC,
        RESTORE_LAPIC,
        RESTORE_PIT,
        RESTORE_VGA,
        RESTORE_NIC,
        RESTORE_ACPI,
        RESTORE_VCPU,
        RESTORE_LAST,
        // This one is acutally a restore device type:
        // vga.cc will react on this, printing messages on the guest screen.
        VGA_DISPLAY_GUEST,
        VGA_VIDEOMODE,
        // This is for pass-through devices. They will un-/replug themselves
        // out of/into the guest before/after live migration
        PCI_PLUG,
    };
    unsigned long magic_string;
    // Use these enums on devtype
    unsigned devtype;
    // The device will note down how many bytes of this structure it actually uses.
    mword bytes;
    // Two variables which every device type can use for identification
    unsigned id1;
    unsigned id2;
    // write=true: Writing a device state onto disk. false: Reading back from disk
    bool write;

    // Space for saving the device state
    char *space;

    MessageRestore(unsigned _devtype, char *_space, bool _write) :
        magic_string(MAGIC_STRING_DEVICE_DESC), devtype(_devtype),
        bytes(0), id1(0), id2(0), write(_write), space(_space)
    {}
    bool magic_string_check() { return magic_string == MAGIC_STRING_DEVICE_DESC; }
};


struct MessageAudio
{
	enum Type { AUDIO_START, AUDIO_STOP, AUDIO_OUT, AUDIO_IN,
	            AUDIO_CONTINUE_TX, AUDIO_DRAIN_TX };

	enum Type type;
	uintptr_t data     { };
	unsigned  size     { };
	unsigned  consumed { };
	unsigned  id       { };

	unsigned period_bytes() const {
		if (type == AUDIO_START) return unsigned(data); else return 0; }

	unsigned buffer_bytes() const {
		if (type == AUDIO_START) return size; else return 0; }

	MessageAudio(Type const t) : type(t) { }
	MessageAudio(Type const t, unsigned i) : type(t), id(i) { }
	MessageAudio(Type const t, uintptr_t d, unsigned s, unsigned c)
	: type(t), data(d), size(s), consumed(c) { }
};
