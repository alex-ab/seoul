/**
 * Host Intel 82576 driver.
 *
 * Copyright (C) 2010, Julian Stecklina <jsteckli@os.inf.tu-dresden.de>
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

/**
 * This device usually comes as a PCIe card with two functions, each
 * acting as a separate NIC with a few exceptions. There are several
 * shared resources: EEPROM, PHY(s), Flash. If access to those
 * resources is desired, the hardware-semaphore of the 82576 has to be
 * used. We can blissfully ignore it, because we do not need access to
 * any shared resource.
 */

#include <nul/types.h>
#include <nul/motherboard.h>
#include <service/hexdump.h>
#include <host/hostpci.h>
#include <host/host82576.h>
#include <host/jsdriver.h>
#include <service/net.h>


class Host82576 : public PciDriver,
                  public Base82576,
                  public StaticReceiver<Host82576>
{
private:
  DBus<MessageHostOp> &_bus_hostop;
  volatile uint32 *_hwreg;     // Device MMIO registers (128K)
  unsigned _hostirq;

  EthernetAddr _mac;

  // MSI-X
  unsigned _msix_table_size;

  struct msix_table {
    volatile uint64 msg_addr;
    volatile uint32 msg_data;
    volatile uint32 vector_control;
  } *_msix_table;

  const char *debug_getname() { return "Host82576"; }

  void log_device_status()
  {
    uint32 status = _hwreg[STATUS];
    const char *up = (status & STATUS_LU ? "UP" : "DOWN");
    const char *speed[] = { "10", "100", "1000", "1000" };
    msg(INFO, "%4s %sBASE-T %cD | %u VFs | %4d RX | %4d TX\n", up,
	speed[(status & STATUS_SPEED) >> STATUS_SPEED_SHIFT],
	status & STATUS_FD ? 'F' : 'H',
	(status & STATUS_NUMVF) >> STATUS_NUMVF_SHIFT,
	_hwreg[GPRC], _hwreg[GPTC]);
    if ((status & STATUS_IOV) == 0)
      msg(INFO, "SR-IOV is OFF. VFs will not work.\n");
  }

  void handle_vf_reset(unsigned vf_no)
  {
    msg(VF, "FLR on VF%d.\n", vf_no);
  }

  void vf_set_mac(unsigned vf_no, EthernetAddr mac)
  {
    msg(VF, "VF%u is now " MAC_FMT "\n", vf_no, MAC_SPLIT(&mac));
    _hwreg[RAL0 + vf_no*2] = mac.raw & 0xFFFFFFFFU;
    _hwreg[RAH0 + vf_no*2] = (mac.raw>>32) | RAH_AV | (1 << (RAH_POOLSEL_SHIFT + vf_no));
  }

  void handle_vf_message(unsigned vf_no)
  {
    assert(vf_no < 8);
    unsigned pfmbx    = PFMB0 + vf_no;
    unsigned pfmbxmem = PFMBMEM + vf_no*0x10;
    uint32 mbxmsg[0x10];

    // Message copy-in
    for (unsigned i = 0; i < 0x10; i++)
      mbxmsg[i] = _hwreg[pfmbxmem + i];

    // ACK Message and claim buffer
    _hwreg[pfmbx] = Ack;
    _hwreg[pfmbx] = PFU;

    //_hwreg[pfmbx] = PFU;
    if ((_hwreg[pfmbx] & PFU) == 0) {
      msg(VF, "Message from VF%u, but could not claim mailbox for reply.\n", vf_no);
      return;
    }

    switch (mbxmsg[0] & 0xFFFF) {
    case VF_RESET: {
      // XXX Clear relevant registers and stuff (i.e. do a real reset for the VF)
      _hwreg[VFRE] |= 1<<vf_no;
      _hwreg[VFTE] |= 1<<vf_no;
      
      // See 7.10.3.4: Rx Packets Switching
      _hwreg[VMOLR0 + vf_no] = VMOLR_DEFAULT
	| VMOLR_AUPE 		// Accept packets without VLAN header
	// | VMOLR_ROPE		// Accept packets that match the UTA.
	// | VMOLR_ROMPE	// Accept packets that match the MTA.
	| VMOLR_BAM		// Accept broadcast packets.
	| VMOLR_MPE		// Only works when RCTL.MPE is set, too.
	| VMOLR_STRVLAN 	// Strip VLAN header
	;

      EthernetAddr vf_mac = _mac;

      // XXX Make this configurable!
      vf_mac.byte[4] ^= vf_no;
      msg(VF, "VF%u sent RESET\n", vf_no);

      vf_set_mac(vf_no, vf_mac);

      _hwreg[pfmbxmem] = mbxmsg[0] | CMD_ACK;
      _hwreg[pfmbxmem + 1] = vf_mac.raw;
      _hwreg[pfmbxmem + 2] = (vf_mac.raw >> 32) & 0xffff;
      break;
    }
    case VF_SET_MULTICAST:
      msg(VF, "VF%u SET_MULTICAST(%u) -> ignore.\n", vf_no, (mbxmsg[0] >> 16) & 0xFF);
      // XXX Just ignore multicast for now.
      _hwreg[pfmbxmem] = VF_SET_MULTICAST | CMD_ACK | CTS;
      break;
    case VF_SET_LPE:
      msg(VF, "VF%u SET_LPE(%u).\n", vf_no, mbxmsg[1]);
      _hwreg[VMOLR0 + vf_no] = (_hwreg[VMOLR0 + vf_no] & ~VMOLR_RPML_MASK) 
	| (mbxmsg[1] & VMOLR_RPML_MASK) | VMOLR_LPE;
      // XXX Adjust value for VLAN tag size?
      _hwreg[pfmbxmem] = VF_SET_LPE | CMD_ACK | CTS;
      break;
    case VF_SET_MAC_ADDR:
      {
	EthernetAddr vf_mac;
	vf_mac.raw = static_cast<uint64>(_hwreg[pfmbxmem + 1]) |
	  (static_cast<uint64>(_hwreg[pfmbxmem + 2]) & 0xFFFF) << 32;
	vf_set_mac(vf_no, vf_mac);
      }
      _hwreg[pfmbxmem] = VF_SET_MAC_ADDR | CMD_ACK | CTS;
      break;
    case VF_SET_PROMISC: {
      msg(VF, "VF_SET_PROMISC %x%s\n", mbxmsg[0], (mbxmsg[0] & VF_SET_PROMISC_UNICAST) ? "PROMISC" : "");

      if (mbxmsg[0] & VF_SET_PROMISC_UNICAST) {
	// This message should only be generated by a switch or bridging VM.
	// Enable reception of "overflow" packets, i.e. those matched be the UTA.
	_hwreg[VMOLR0 + vf_no] |= VMOLR_ROPE;
	// Disable anti-spoof.
	_hwreg[DTXSWC] &= ~(1<<vf_no);
      } else {
	_hwreg[VMOLR0 + vf_no] &= ~VMOLR_ROPE;
	_hwreg[DTXSWC]         |= (1<<vf_no);
      }

      _hwreg[pfmbxmem] = VF_SET_PROMISC | CMD_ACK | CTS;
    }
      break;
    default:
      msg(VF, "Unrecognized message %04x.\n", mbxmsg[0] & 0xFFFF);
      hexdump(mbxmsg, 16); // Dump control world plus some data (if any)
      // Send NACK for unrecognized messages.
      _hwreg[pfmbxmem] = mbxmsg[0] | CMD_NACK;
    };

    _hwreg[pfmbx] = Sts;	// Send response
  }

  void handle_vf_ack(unsigned vf_no)
  {
    // msg(VF, "Message ACK from VF%d.\n", vf_no);
    unsigned pfmbx = PFMB0 + vf_no;
    _hwreg[pfmbx] = 0;		// Clear Ack
  }

public:

  EthernetAddr ethernet_address()
  {
    // Hardware initializes the Receive Address registers of queue 0
    // with the MAC address stored in the EEPROM.
    return EthernetAddr(_hwreg[RAL0] | ((static_cast<uint64>(_hwreg[RAH0]) & 0xFFFF) << 32));
  }

  bool receive(MessageIrq &irq_msg)
  {
    if (irq_msg.line != _hostirq || irq_msg.type != MessageIrq::ASSERT_IRQ)  return false;

    uint32 icr = _hwreg[ICR];

    if ((icr & _hwreg[IMS]) == 0) {
      // Spurious IRQ.
      msg(INFO, "Spurious IRQ! ICR %08x%s\n", icr, (icr & IRQ_TIMER) ? " Ping!" : "");
      log_device_status();
      return false;
    }

    if (icr & IRQ_LSC) {
      bool gone_up = (_hwreg[STATUS] & STATUS_LU) != 0;
      msg(INFO, "Link status changed to %s.\n", gone_up ? "UP" : "DOWN");
      log_device_status();
    }

    if (icr & IRQ_VMMB) {
      uint32 vflre  = _hwreg[VFLRE];
      uint32 mbvficr = _hwreg[MBVFICR];
      //uint32 mbvfimr = _hwreg[MBVFIMR];
      //msg(VF, "VMMB: VFLRE %08x MBVFICR %08x MBVFIMR %08x\n", vflre, mbvficr, mbvfimr);

      // Check FLRs
      for (uint32 mask = 1, cur = 0; cur < 8; cur++, mask<<=1)
	if (vflre & mask)
	  handle_vf_reset(cur);

      // Check incoming
      for (uint32 mask = 1, cur = 0; cur < 8; cur++, mask<<=1) {
	if (mbvficr & mask)
	  handle_vf_message(cur);
	if (mbvficr & (mask << 16)) // Check ACKs
	  handle_vf_ack(cur);
      }

      // Clear bits
      _hwreg[VFLRE]  = vflre;
      _hwreg[MBVFICR] = mbvficr;
    }

    _hwreg[EIMS] = 1;
    return true;
  };

  Host82576(HostPci pci, DBus<MessageHostOp> &bus_hostop, Clock *clock, unsigned bdf)
    : PciDriver(clock, ALL & ~IRQ, bdf), _bus_hostop(bus_hostop)
  {
    msg(INFO, "Found Intel 82576-style controller at %x. Attaching IRQ %u.\n", bdf, _hostirq);

    // We never use DMA, so we don't need this.
    //assign_pci();

    // Scan BARs and discover our register windows.
    _hwreg   = 0;

    for (unsigned bar_i = 0; bar_i < pci.MAX_BAR; bar_i++) {
      uint32 bar_addr = pci.BAR0 + bar_i;
      uint32 bar = pci.conf_read(_bdf, bar_addr);
      uint64 size  = pci.bar_size(_bdf, bar_addr);

      if (bar == 0) continue;
      msg(PCI, "BAR %u: %08x (size %08llx)\n", bar_i, bar, size);

      if ((bar & pci.BAR_IO) != 1) {
	// Memory BAR
	// XXX 64-bit bars!
	uint32 phys_addr = bar & pci.BAR_MEM_MASK;
	
	MessageHostOp iomsg(MessageHostOp::OP_ALLOC_IOMEM, phys_addr & ~0xFFF, size);
	if (bus_hostop.send(iomsg) && iomsg.ptr) {

	  if (size == (1<<17 /* 128K */)) {
	    _hwreg = reinterpret_cast<volatile uint32 *>(iomsg.ptr);
	    msg(INFO, "Found MMIO window at %p (phys %x).\n", _hwreg, phys_addr);
	  }

	  } else {
	    Logging::panic("%s could not map BAR %u.\n", __PRETTY_FUNCTION__, bar);
	  }
      }
    }

    if ((_hwreg == 0)) Logging::panic("Could not find 82576 register windows.\n");

    /// Initialization (4.5)
    msg(INFO, "Perform Global Reset.\n");
    // Disable interrupts
    _hwreg[IMC] = ~0U;
    _hwreg[EIMC] = ~0U;

    // Global Reset (5.2.3.3)
    _hwreg[CTRL] |= CTRL_GIO_MASTER_DISABLE;
    if (!wait(_hwreg[STATUS], STATUS_GIO_MASTER_ENABLE, 0))
      Logging::panic("%s: Device hang?", __PRETTY_FUNCTION__);

    _hwreg[CTRL] = _hwreg[CTRL] | CTRL_RST;
    spin(1000 /* 1ms */);
    if (!wait(_hwreg[CTRL], CTRL_RST, 0))
      Logging::panic("%s: Reset failed.", __PRETTY_FUNCTION__);

    // Disable Interrupts (again)
    _hwreg[IMC] = ~0U;
    _hwreg[EIMC] = ~0U;
    msg(INFO, "Global Reset successful.\n");

    // Configuring one MSI-X vector: We have to use Multiple-MSI-X
    // mode, because SR-IOV is on. Beware, that we can only use one
    // internal interrupt vector (vector 0). The 82576 has 25, but
    // each of the 8 VMs needs 3.
    _hwreg[GPIE] = GPIE_EIAME | GPIE_MULTIPLE_MSIX | GPIE_PBA | GPIE_NSICR; // 7.3.2.11

    // Configure MSI-X
    _hostirq = pci.get_gsi_msi(bus_hostop, bdf, 0);

    // Disable all RX/TX interrupts.
    for (unsigned i = 0; i < 8; i++)
      _hwreg[IVAR0 + i] = 0;

    // Map the TCP timer and other interrupt cause to internal vector 0.
    _hwreg[IVAR_MISC] = 0x8080;

    msg(INFO, "Attached to IRQ %u (MSI-X).\n", _hostirq);
    
    // VT Setup
    msg(INFO, "Configuring VFs...\n");

    // Enable packet dropping for all 16 queues.
    _hwreg[QDE] |= 0xFFFF;

    // Enable replication to allow VF-to-VF communication.
    // Disable default pool. It seems to have no good use in
    // replication mode. (See 7.10.3.4: Rx Packets Switching)
    _hwreg[VT_CTL] = VT_CTL_REP_ENABLE | VT_CTL_DIS_DEF_POOL;

    // Filter via MAC, always use default queue (0) of pool. In this
    // mode each VF has effectively only one RX queue.
    _hwreg[MRQC] = MRQC_MRQE_011;

    // Disable RX and TX for all VFs.
    _hwreg[VFRE] = 0;
    _hwreg[VFTE] = 0;

    // Allow all VFs to send IRQs to us.
    _hwreg[MBVFIMR] = 0xFF;

    // Make NIC promiscuous by enabling all bits in the UTA.
    for (unsigned i = 0; i < 128; i++)
      _hwreg[UTA0 + i] = ~0U;

    // Make NIC multicast promiscuous by enabling all bits in the MTA.
    // for (unsigned i = 0; i < 128; i++)
    //   _hwreg[MTA0 + i] = ~0U;

    // TX
    // Enable anti-spoof for all.
    _hwreg[DTXCTL] = 0;
    _hwreg[DTXSWC] = DTXSWC_LOOP_EN | 0xFF;

    // Wait for Link Up and VM mailbox events.
    msg(INFO, "Enabling interrupts...\n");
    _hwreg[EIAC] = 1;		// Autoclear EICR on IRQ.
    _hwreg[EIMS] = 1;
    _hwreg[EIAM] = 1;
    _hwreg[IMS] = IRQ_VMMB | IRQ_LSC | IRQ_TIMER;

    msg(INFO, "Configuring link parameters...\n");
    // Direct Copper link mode (set link mode to 0)
    _hwreg[CTRL_EXT] &= ~CTRL_EXT_LINK_MODE;
    // Enable PHY autonegotiation (4.5.7.2.1)
    _hwreg[CTRL] = (_hwreg[CTRL] & ~(CTRL_FRCSPD | CTRL_FRCDPLX)) | CTRL_SLU;

    _mac = ethernet_address();
    msg(INFO, "We are " MAC_FMT "\n", MAC_SPLIT(&_mac));

    // Don't wait for link up to enable RX and TX. We don't have to
    // synchronize with VFs this way.
    _hwreg[RCTL] |= RCTL_RXEN | RCTL_BAM | RCTL_MPE;
    _hwreg[TCTL] |= TCTL_TXEN;

    // PF Setup complete
    msg(INFO, "Notifying VFs that PF is done...\n");
    _hwreg[CTRL_EXT] |= CTRL_EXT_PFRSTD;

    msg(INFO, "Initialization complete.\n");

    // Starting timer (every 0.256s)
    //_hwreg[TCPTIMER] = 0xFF | TCPTIMER_ENABLE | TCPTIMER_LOOP | TCPTIMER_KICKSTART | TCPTIMER_FINISH;
  }
};

PARAM(host82576, {
    HostPci pci(mb.bus_hwpcicfg, mb.bus_hostop);
    unsigned found = 0;

    for (unsigned bdf, num = 0; (bdf = pci.search_device(0x2, 0x0, num++));) {
      unsigned cfg0 = pci.conf_read(bdf, 0x0);
      if (cfg0 == 0x10c98086) {
	if (found++ == argv[0]) {
	  Host82576 *dev = new Host82576(pci, mb.bus_hostop, mb.clock(), bdf);
	  mb.bus_hostirq.add(dev, &Host82576::receive_static<MessageIrq>);
	}
      }
    }
  },
  "host82576:instance - provide driver for Intel 82576 Ethernet controller.",
  "Example: host82576:0");

// EOF
