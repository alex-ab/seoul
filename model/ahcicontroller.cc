/** @file
 * AHCI emulation.
 *
 * Copyright (C) 2008-2009, Bernhard Kauer <bk@vmmon.org>
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

#ifndef VMM_REGBASE
#include "nul/motherboard.h"
#include "model/sata.h"
#include "model/pci.h"
#include "service/lock.h"


class ParentIrqProvider
{
  public:
    virtual void trigger_irq (void * child) = 0;
    virtual ~ParentIrqProvider() { }
};

/**
 * A port of an AhciController.
 *
 * State: unstable
 * Features: register set, FIS
 * Missing: plenty
 */
class AhciPort : public FisReceiver
{
#include "model/simplemem.h"
  FisReceiver       * _drive  { };
  ParentIrqProvider * _parent { };
  unsigned _ccs;
  unsigned _inprogress;
  bool _need_initial_fis;

	DBus<MessageMemRegion>  *_bus_memregion { };
	DBus<MessageMem>        *_bus_mem       { };

#define  VMM_REGBASE "../model/ahcicontroller.cc"
#include "model/reg.h"

  /*
   * Noncopyable
   */
  AhciPort(AhciPort const &);
  AhciPort &operator = (AhciPort const &);

public:

  void set_parent(ParentIrqProvider *parent, DBus<MessageMemRegion> *bus_memregion, DBus<MessageMem> *bus_mem)
  {
    _parent = parent;
    _bus_memregion = bus_memregion;
    _bus_mem = bus_mem;
  }


  /**
   * Receive a FIS from the Device.
   */
  void receive_fis(size_t fislen, unsigned *fis) override
  {
    size_t copy_offset;

    // fis receiving enabled?
    // XXX bug in 2.6.27?
    //if (!_need_initial_fis && ~PxCMD & 0x10) { Logging::printf("skip FIS %x\n", fis[0]); return; }

    // update status and error fields
    PxTFD = (PxTFD & 0xffff0000) | fis[0] >> 16;

    switch (fis[0] & 0xff)
      {
      case 0x34: // d2h register fis
	assert(fislen == 5);
	copy_offset = 0x40;

	if (_need_initial_fis)
	  {
	    // update signature
	    PxSIG = fis[1] << 8 | (fis[3] & 0xff);
	    // set PxSTSS since a device is available
	    PxSSTS = (PxSSTS & ~0xfff) | 0x113;
	    _need_initial_fis = false;
	  }

	if (PxIE & 0x1)
		PxIS |= 1 << 1U;

	// we finished the current command
	if (~fis[0] & 0x80000 && fis[4])  { // !DRQ && dsf[6]
	  unsigned mask = 1 << (fis[4] - 1);
	  if (mask & ~_inprogress)
	    Logging::panic("XXX broken %x,%x inprogress %x\n", fis[0], fis[4], _inprogress);
	  _inprogress &= ~mask;
	  PxCI &= ~mask;
	  PxSACT &= ~mask;
	}
#if 0
	else
	  Logging::printf("not finished %x,%x inprogress %x\n", fis[0], fis[4], _inprogress);
#endif
	break;
      case 0x41: // dma setup fis
	assert(fislen == 7);
	copy_offset = 0;
	if (PxIE & 0x4)
		PxIS |= 2 << 1U;
	break;
      case 0x5f: // pio setup fis
	assert(fislen == 5);
	copy_offset = 0x20;

//	Logging::printf("PIO setup fis\n");
	if (PxIE & 0x2)
		PxIS |= 1 << 1U;
	break;
      default:
	Logging::panic("Invalid D2H FIS!");
      }

		// copy to user
		if (PxCMD & 0x10)
			copy_out(*_bus_memregion, *_bus_mem, PxFB + copy_offset, fis, fislen * 4);
		if (fis[0] & 0x4000) _parent->trigger_irq(this);
	};


	bool set_drive(FisReceiver *drive)
	{
		if (_drive) {
			/* trigger comreset on re-claim attempt */
			comreset();
			return true;
		}

		_drive = drive;

		return false;
	}


  void comreset()
  {
    // reset all device registers except CL+FB to default values
    PxIS   = PxIS_reset;
    PxIE   = PxIE_reset;
    PxCMD  = PxCMD_reset;
    PxTFD  = PxTFD_reset;
    PxSIG  = PxSIG_reset;
    PxSSTS = PxSSTS_reset;
    PxSCTL = PxSCTL_reset;
    PxSERR = PxSERR_reset;
    PxCI   = PxCI_reset;
    _need_initial_fis = true;
    _inprogress = 0;

    if (_drive) {

      // we use the legacy reset mechanism to transmit a COMRESET to the SATA disk
      unsigned fis[5] = { 0x27, 0, 0, 0x04000000, 0};
      _drive->receive_fis(5, fis);
      // toggle SRST in the control register
      fis[3] = 0;
      _drive->receive_fis(5, fis);
    }
  }


  unsigned execute_command(unsigned value)
  {
    COUNTER_INC("ahci cmd");

      // try to execute all active commands
      for (unsigned i = 0; i < 32; i++)
	{
	  unsigned slot = (_ccs >= 31) ? 0 : _ccs + 1;
	  _ccs = slot;

	  // XXX check for busy bit
	  if (value & ~_inprogress & (1 << slot))
	    {
	      enum { TFD_BUSY = 0x80 };
	      if (PxTFD & TFD_BUSY) break;

	      _inprogress |= 1 << slot;

	      unsigned  cl[4];
	      copy_in(*_bus_memregion, *_bus_mem, PxCLB + slot * 0x20, cl, sizeof(cl));
	      unsigned clflags  = cl[0];

	      unsigned ct[32];
	      copy_in(*_bus_memregion, *_bus_mem, cl[2], ct, (clflags & 0x1f) * sizeof(unsigned));
	      assert(~clflags & 0x20 && "ATAPI unimplemented");

	      // send a dma_setup_fis
	      // we reuse the reserved fields to send the PRD count and the slot
	      unsigned dsf[7] = { FIS_TYPE_DMA_SETUP, cl[2] + 0x80, 0,
	                          clflags >> 16, 0, cl[1], slot+1};
	      _drive->receive_fis(7, dsf);

	      // set BSY
	      PxTFD |= TFD_BUSY;
	      _drive->receive_fis(clflags & 0x1f, ct);
	    }
	}
      // make _css known
      PxCMD = (PxCMD & ~0x1f00) | ((_ccs & 0x1f) << 8);
      return 0;
  }


  AhciPort() : _ccs(), _inprogress(), _need_initial_fis() { AhciPort_reset(); };

  virtual ~AhciPort() { }
};

#else
#ifndef AHCI_CONTROLLER
VMM_REGSET(AhciPort,
       VMM_REG_RW(PxCLB,    0x0, 0, 0xfffffc00,)
       VMM_REG_RO(PxCLBU,   0x4, 0)
       VMM_REG_RW(PxFB,     0x8, 0, 0xffffff00,)
       VMM_REG_RO(PxFBU,    0xc, 0)
       VMM_REG_WR(PxIS,    0x10, 0, 0xdfc000af, 0, 0xdfc000af, COUNTER_INC("IS");)

       VMM_REG_RW(PxIE,    0x14, 0, 0x7dc0007f,)
       VMM_REG_WR(PxCMD,   0x18, 0, 0xf3000011, 0, 0,
	      // enable FRE
	      if ( PxCMD & 0x10 && ~oldvalue & 0x10) PxCMD |= 1 << 14;
	      // disable FRE
	      if (~PxCMD & 0x10 &&  oldvalue & 0x10) PxCMD &= ~(1 << 14);
	      // enable CR
	      if (PxCMD & 1 && ~oldvalue & 1) { PxCMD |= 1 << 15;  _ccs = 32; }
	      // disable CR
	      if (~PxCMD & 1 &&  oldvalue & 1)
		{
		  PxCMD &= ~(1 << 15);
		  // reset PxSACT
		  PxSACT = PxSACT_reset;
		  PxCI   = PxCI_reset;
		}
	      )
       VMM_REG_RW(PxTFD,   0x20, 0x7f, 0,)
       VMM_REG_RW(PxSIG,   0x24, 0xffffffff, 0,)
       VMM_REG_RW(PxSSTS,  0x28, 0, 0,)
       VMM_REG_RW(PxSCTL,  0x2c, 0, 0x00000fff,
	      switch (PxSCTL & 0xf) {
	      case 1: comreset(); break;
	      case 2:
		// put device in offline mode
		PxSSTS = PxSSTS_reset;
	      default:
		break;
	      })
       VMM_REG_WR(PxSERR,  0x30, 0, 0xffffffff, 0, 0xffffffff, )
       VMM_REG_WR(PxSACT,  0x34, 0, 0xffffffff, 0xffffffff, 0, )
       VMM_REG_WR(PxCI,    0x38, 0, 0xffffffff, 0xffffffff, 0, execute_command(PxCI); )
       VMM_REG_RO(PxSNTF,  0x3c, 0)
       VMM_REG_RO(PxFBS,   0x40, 0));


#else

VMM_REGSET(PCI,
       VMM_REG_RO(PCI_ID,        0x0, 0x275c8086)
       VMM_REG_RW(PCI_CMD_STS,   0x1, 0x100000, 0x0406,)
       VMM_REG_RO(PCI_RID_CC,    0x2, 0x01060102)
       VMM_REG_RW(PCI_ABAR,      0x9, 0, 0xffffe000,)
       VMM_REG_RO(PCI_SS,        0xb, 0x275c8086)
       VMM_REG_RO(PCI_CAP,       0xd, 0x80)
       VMM_REG_RW(PCI_INTR,      0xf, 0x0100, 0xff,)
       VMM_REG_RO(PCI_PID_PC,   0x20, 0x00008801)
       VMM_REG_RO(PCI_PMCS,     0x21, 0x0000)
       VMM_REG_RW(PCI_MSI_CTRL, 0x22, 0x00000005, 0x10000,)
       VMM_REG_RW(PCI_MSI_ADDR, 0x23, 0, 0xffffffff,)
       VMM_REG_RW(PCI_MSI_DATA, 0x24, 0, 0xffffffff,));



VMM_REGSET(AhciController,
       VMM_REG_RW(REG_CAP,   0x0, 0x40149f00 | (AhciController::MAX_PORTS - 1), 0,)
       VMM_REG_WR(REG_GHC,   0x4, 0x80000000, 0x3, 0x1, 0,
	      // reset HBA?
	      if (REG_GHC & 1) {
		for (unsigned i=0; i < MAX_PORTS; i++)  _ports[i].comreset();
		// set all registers to default values
		REG_IS  = REG_IS_reset;
		REG_GHC = REG_GHC_reset;
	      })
       VMM_REG_WR(REG_IS,    0x8, 0, 0xffffffff, 0x00000000, 0xffffffff, )
       VMM_REG_RW(REG_PI,    0xc, 1, 0,)
       VMM_REG_RO(REG_VS,   0x10, 0x00010200)
       VMM_REG_RO(REG_EML,  0x1c, 0x0)
       VMM_REG_RO(REG_EMC,  0x20, 0x0)
       VMM_REG_RO(REG_CAP2, 0x24, 0x0));

#endif
#endif

#ifndef VMM_REGBASE

/**
 * An AhciController on a PCI card.
 *
 * State: unstable
 * Features: PCI cfg space, AHCI register set, MSI delivery
 */
class AhciController : public ParentIrqProvider,
                       public StaticReceiver<AhciController>
{
	enum { MAX_PORTS = 32 };

	DBus<MessageIrqLines> &_bus_irqlines;
	DBus<MessageMem>      &_bus_mem;
	Seoul::Lock            _lock { };

	uint8     const _irq;
	AhciPort        _ports[MAX_PORTS];
	unsigned  const _bdf;

#define AHCI_CONTROLLER
#define  VMM_REGBASE "../model/ahcicontroller.cc"
#include "model/reg.h"

	bool match_bar(uint64 &address)
	{
		bool res = !((address ^ PCI_ABAR) & PCI_ABAR_mask);
		address &= ~PCI_ABAR_mask;
		return res;
	}

 public:

	void trigger_irq(void * child) override
	{
		auto index = reinterpret_cast<AhciPort *>(child) - _ports;
		if (index < 0 || index >= MAX_PORTS)
			Logging::panic("unknown ahci port");

		if (!(~REG_IS & (1 << index)))
			return;

		REG_IS |= 1 << index;

		if (!(REG_GHC & 0x2))
			return;

		// MSI?
		if (PCI_MSI_CTRL & 0x10000) {
			MessageMem msg(false, PCI_MSI_ADDR, &PCI_MSI_DATA);
			_bus_mem.send(msg);
		} else {
			MessageIrqLines msg(MessageIrq::ASSERT_IRQ, _irq);
			_bus_irqlines.send(msg);
		}
	}

	bool receive(MessageMem &msg)
	{
		auto addr = msg.phys;
		if (!match_bar(addr))
			return false;

		bool const access_ctrl  = addr < 0x100;
		bool const access_ports = !(access_ctrl) && (addr < 0x100 + MAX_PORTS * 0x80);

		if (!access_ctrl && !access_ports)
			return false;

		Seoul::Lock::Guard guard(_lock);

		if (!(PCI_CMD_STS & 0x2))
			return false;

		bool     ok     = false;
		unsigned uvalue = 0;

		if (access_ctrl) {
			assert(!(addr & 0x3));

			ok = msg.read ? AhciController_read (unsigned(addr), uvalue)
			              : AhciController_write(unsigned(addr), *msg.ptr);

			/* suppress write errors, if written value is same as in register */
			if (!ok && !msg.read) {
				ok = AhciController_read (unsigned(addr), uvalue);
				if (ok && uvalue != *msg.ptr)
					ok = false;
			}
		} else
		if (access_ports) {
			auto & port = _ports[(addr - 0x100) / 0x80];

			Seoul::Lock::Guard guard(port._lock);

			ok = msg.read ? port.AhciPort_read (addr & 0x7f, uvalue)
			              : port.AhciPort_write(addr & 0x7f, *msg.ptr);
		}

		if (ok && msg.read)
			*msg.ptr = uvalue;
		else if (!ok)
			Logging::printf("%s(%zx) %s failed\n", __PRETTY_FUNCTION__,
			                size_t(addr), msg.read ? "read" : "write");

		return true;
	}

	bool receive(MessageAhciSetDrive &msg)
	{
		if (msg.port > MAX_PORTS)
			return false;

		Seoul::Lock::Guard guard(_lock);

		if (_ports[msg.port].set_drive(msg.drive))
			return false;

		msg.drive = &_ports[msg.port];

		// enable it in the PI register
		REG_PI |= 1 << msg.port;

		/**
		 * fix CAP, according to the spec this is unnneeded, but Linux
		 * 2.6.24 checks and sometimes crash without it!
		 */
		unsigned count = 0;
		unsigned value = REG_PI;

		for (; value; value >>= 1) { count += value & 1; }

		REG_CAP = (REG_CAP & ~0x1f) | (count - 1);
		return true;
	}

	bool receive(MessagePciConfig &msg)
	{
		if (msg.bdf != _bdf)
			return false;

		Seoul::Lock::Guard guard(_lock);

		return PciHelper::receive(msg, this, _bdf);
	}

	AhciController(Motherboard &mb, unsigned char irq, unsigned bdf)
	: _bus_irqlines(mb.bus_irqlines), _bus_mem(mb.bus_mem), _irq(irq), _bdf(bdf)
	{
		for (auto & port : _ports)
			port.set_parent(this, &mb.bus_memregion, &mb.bus_mem);

		PCI_reset();
		AhciController_reset();
	};

	virtual ~AhciController() { }
};

PARAM_HANDLER(ahci,
	      "ahci:mem,bdf - attach an AHCI controller to a PCI bus.",
	      "Example: Use 'ahci:0xe0800000,0x30' to attach an AHCI controller to 00:06.0 on address.",
	      "If no bdf is given, the first free one is searched.",
	      "The AHCI controllers are automatically numbered, starting with 0."
	      )
{
	if (argv[0] == ~0UL)
		Logging::panic("virtio_gpu: missing bar address");

	auto  const irq_pin  = 1; /* PCI INTA# - hard coded in PCI_INTR definition */
	uint8 const irq_line = 5; /* defined by acpicontroller dsdt for INTA# */

	auto * dev = new AhciController(mb, irq_line,
	                                PciHelper::find_free_bdf(mb.bus_pcicfg,
	                                unsigned(argv[1])));
	mb.bus_mem.add(dev, AhciController::receive_static<MessageMem>);

	/* register PCI device */
	mb.bus_pcicfg.add(dev, AhciController::receive_static<MessagePciConfig>);

	/* register for AhciSetDrive messages */
	mb.bus_ahcicontroller.add(dev, AhciController::receive_static<MessageAhciSetDrive>);

	/* set MMIO region and IRQ line */
	dev->PCI_write(AhciController::PCI_ABAR_offset, unsigned(argv[0]));
	dev->PCI_write(AhciController::PCI_INTR_offset, irq_line);

	// enable IRQ, busmaster DMA and memory accesses
	dev->PCI_write(AhciController::PCI_CMD_STS_offset, 0x406);
}

#endif
