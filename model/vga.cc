/** @file
 * Basic VGA emulation.
 *
 * Copyright (C) 2007-2010, Bernhard Kauer <bk@vmmon.org>
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
#include "executor/bios.h"
#include "host/screen.h"
#include "service/lock.h"

/**
 * A VGA compatible device.
 *
 * State: unstable
 * Features: textmode 80x25 supported, cursor, VESA framebuffer
 * Missing: plenty, e.g: PCI, IRQ, many BIOS functions, 8-bit graphic modes
 * Documentation: FreeVGA chipset reference - vga.htm, Browns IRQ List
 */
class Vga : public StaticReceiver<Vga>, public BiosCommon
{
public:
  enum {
    LOW_BASE  = 0xa0000,
    LOW_SIZE  = 1<<17,
    TEXT_OFFSET = 0x18000 >> 1,
    EBDA_FONT_OFFSET = 0x1000,
    CONSOLE_ID = 0,
  };
private:
  Seoul::Lock    _lock { };
  uintptr_t      _framebuffer_ptr;
  uint32         _framebuffer_phys; /* in VBE 2 only 32 bit field */
  size_t         _framebuffer_size;
  unsigned       _ebda_segment { };
  unsigned       _vbe_mode { };
  mword          _last_videomode_request { };
  VgaRegs        _regs { };
  unsigned short _view { 0 };
  unsigned short _iobase;
  unsigned char  _crt_index { };
  unsigned char  _max_index { };
  bool           _restore_processed { };


  char * framebuffer_ptr() const { return reinterpret_cast<char *>(_framebuffer_ptr); }

  void puts_guest(const char *msg) {
    unsigned pos = _regs.cursor_pos - TEXT_OFFSET;

    bool update = false;
    for (size_t i=0; msg[i]; i++) {
      if (Screen::vga_putc(0x0f00 | uint16(msg[i]), reinterpret_cast<unsigned short *>(_framebuffer_ptr) + TEXT_OFFSET, pos))
        update = true;
    }
    update_cursor(0, ((pos / 80) << 8) | (pos % 80));

    if (update) {
        MessageConsole msg2(MessageConsole::TYPE_CONTENT_UPDATE, CONSOLE_ID);
        _mb.bus_console.send(msg2);
    }
  }


  /**
   * Update the cursor of a page and sync the hardware cursor with the
   * one of the active page.
   */
  void update_cursor(unsigned page, unsigned pos) {
    write_bda(0x50 + uint16((page & 0x7) * 2), pos, 2);
    pos = read_bda<unsigned>(0x50 + 2 * (read_bda<unsigned>(0x62) & 0x7));
    _regs.cursor_pos = TEXT_OFFSET + ((pos >> 8)*80 + (pos & 0xff));
  }


  /**
   * Get the page offset.
   */
  unsigned get_page(unsigned page) { return 0x800 * (page & 0x7); }

  /**
   * Return the text mode cursor position in characters for a given page.
   */
  unsigned get_pos(unsigned page) {
    unsigned res = read_bda<unsigned>(0x50 + (page & 0x7) * 2);
    return (res >> 8)*80 + (res & 0xff);
  }


  bool handle_reset(bool show)
  {
    _regs.offset       = TEXT_OFFSET;
    _regs.mode         = 0;
    _regs.cursor_pos   = 24*80 + TEXT_OFFSET;
    _regs.cursor_style = 0x0d0e;
    // and clear the screen
    memset(framebuffer_ptr(), 0, _framebuffer_size);
    if (show) puts_guest("    VgaBios booting...\n\n\n");
    return true;
  }

  static unsigned vesa_farptr(CpuState *cpu, void *p, void *base)  {
    return (unsigned(cpu->es.sel) << 16) | unsigned(cpu->di + reinterpret_cast<char *>(p) - reinterpret_cast<char *>(base));
  }

  unsigned get_vesa_mode(unsigned vesa_mode, ConsoleModeInfo *info)
  {
    for (MessageConsole msg2(0, info, CONSOLE_ID); (msg2.index < (Vbe::MAX_VESA_MODES - 1)) && _mb.bus_console.send(msg2); msg2.index++)
      {
	if (vesa_mode == info->_vesa_mode)
	  {

	    // fix memory info
	    size_t image_size = info->bytes_per_scanline * info->resolution[1];
	    if (!image_size || image_size > _framebuffer_size)
	      info->attr &= 0xfffeu;
	    else
	      {
		unsigned image_pages = unsigned((_framebuffer_size / image_size) - 1);
		if (image_pages > 0xff) image_pages = 0xff;
		info->number_images     = uint8(image_pages);
		info->number_images_bnk = uint8(image_pages);
		info->number_images_lin = uint8(image_pages);
	      }
	    return msg2.index;
	  }
      }
    return ~0u;
  }

  bool  handle_vesa(MessageBios &msg, CpuState *cpu)
  {
    static const char *oemstring = "Vancouver VESA BIOS";
    switch (cpu->ax)
      {
      case 0x4f00: // vesa information
	{
	  Vbe::InfoBlock v;
	  // clear the block
	  memset(&v, 0, sizeof(v));

	  // copy in the tag
	  if (!msg.vcpu->copy_in(cpu->es.base + cpu->di, &v, 4)) return false;

	  if (false)
		Logging::printf("VESA %x tag %x base %zx+%x esi %x\n", cpu->eax, v.tag, size_t(cpu->es.base), cpu->di, cpu->esi);

	  // we support VBE 2.0
	  v.version = 0x0200;

	  unsigned short *modes = reinterpret_cast<unsigned short *>(v.scratch);
	  v.video_mode_ptr = vesa_farptr(cpu, modes, &v);

	  // get all modes
	  ConsoleModeInfo info;
	  for (MessageConsole msg2(0, &info, CONSOLE_ID); msg2.index < (Vbe::MAX_VESA_MODES - 1) && _mb.bus_console.send(msg2); msg2.index++) {
	    *modes++ = info._vesa_mode;
	    /* remember highest mode index for flat panel request as preferred mode */
	    _max_index = uint8(msg2.index);
	  }
	  *modes++ = 0xffff;


	  // set the oemstring
	  char *p = reinterpret_cast<char *>(modes);
	  strcpy(p, oemstring);
	  v.oem_string = vesa_farptr(cpu, p, &v);
	  p += strlen(p) + 1;
	  assert (p < reinterpret_cast<char *>((&v)+1));

	  v.memory = uint16(_framebuffer_size >> 16);
	  uint16 copy_size = (v.tag == Vbe::TAG_VBE2) ? sizeof(v) : 256;
	  v.tag = Vbe::TAG_VESA;
	  msg.vcpu->copy_out(cpu->es.base + cpu->di, &v, copy_size);
          cpu->ax = 0x004f;
	}
	break;
      case 0x4f01: // get modeinfo
	{
	  ConsoleModeInfo info;
	  if (get_vesa_mode(cpu->ecx & 0x0fff, &info) != ~0u)
	    {
	      info.phys_base = _framebuffer_phys;
	      msg.vcpu->copy_out(cpu->es.base + cpu->di, &info, sizeof(info));
	      break;
	    }
	}
	cpu->ax = 0x024f;
	return true;
      case 0x4f02: // set vbemode
	{
	  ConsoleModeInfo info { };
	  _last_videomode_request = cpu->ebx;
	  unsigned index = get_vesa_mode(cpu->ebx & 0x0fff, &info);
	  if (index != ~0u && info.attr & 1)
	    {
	      // ok, we have the mode -> set it
	      if (false)
		      Logging::printf("VESA %x base %zx+%x esi %x mode %x\n", cpu->eax, size_t(cpu->es.base), cpu->di, cpu->esi, index);

	      // clear buffer
	      if (~cpu->ebx & 0x8000)  memset(framebuffer_ptr(), 0, _framebuffer_size);

	      // switch mode
	      _regs.mode = uint16(index);
	      _vbe_mode = cpu->ebx;

	      /* notify about vga/vesa switch */
	      MessageConsole msg(MessageConsole::TYPE_CONTENT_UPDATE, CONSOLE_ID);
	      _mb.bus_console.send(msg);
	      break;
	    }
	  cpu->ax = 0x024f;
	  return true;
	}
      case 0x4f03: // get vbemode
	cpu->bx = uint16(_vbe_mode);
	break;
      case 0x4f11: // flat panel info
      {
        unsigned long x = cpu->es.base & ~(0xfffful);
        unsigned long y = x | (cpu->di & (0xfffful));

        ConsoleModeInfo info {};
        MessageConsole pref(_max_index, &info, CONSOLE_ID);
        if (!_mb.bus_console.send(pref)) {
          Logging::printf("requesting resolution of mode index %u failed\n",
                          _max_index);
          return false;
        }

        Logging::printf("VESA: flat panel info %ux%u\n",
                        pref.info->resolution[0],
                        pref.info->resolution[1]);

        unsigned v = (unsigned(pref.info->resolution[1]) << 16) |
                      unsigned(pref.info->resolution[0]);
        msg.vcpu->copy_out(cpu->es.base + cpu->di, &v, sizeof(v));

        break;
      }
      case 0x4f15: // DCC
      default:
	return false;
      }
    cpu->ax = 0x004f;
    return true;
  }

  /**
   * Graphic INT.
   */
  bool handle_int10(MessageBios &msg)
  {
    CpuState *cpu = msg.cpu;
    //DEBUG(cpu);
    COUNTER_INC("int10");
    switch (cpu->ah)
      {
      case 0x00: // set mode
	// unsupported
	//DEBUG;
	break;
      case 0x01: // set cursor shape
	_regs.cursor_style = cpu->cx;
	break;
      case 0x02: // set cursor
	update_cursor(cpu->bh, cpu->dx);
	break;
      case 0x03: // get cursor
	  cpu->ax = 0;
	  cpu->cx = _regs.cursor_style;
	  cpu->dx = read_bda<unsigned short>(0x50 + (cpu->bh & 0x7) * 2);
	break;
      case 0x05: // set current page
	write_bda(0x62, cpu->al & 7, 1);
	_regs.offset = TEXT_OFFSET + get_page(cpu->al);
	break;
      case 0x06: // scroll up window
	{
	  auto const current_page = read_bda<unsigned char>(0x62);
	  unsigned short *base = reinterpret_cast<unsigned short *>(_framebuffer_ptr) + TEXT_OFFSET + get_page(current_page);
	  unsigned rows = (cpu->al == 0) ? 25 : cpu->al;
	  unsigned maxrow = cpu->dh < 25 ? cpu->dh : 24;
	  for (unsigned row = cpu->ch; row <= maxrow; row++)
	    for (unsigned col = cpu->cl; col < 80 && col <= cpu->dl; col++)
	      if ((row + rows) > maxrow)
		base[row*80 + col] = cpu->bh << 8;
	      else
		base[row*80 + col] = base[(row + rows)*80 + col];

	  MessageConsole msg(MessageConsole::TYPE_CONTENT_UPDATE, CONSOLE_ID);
	  _mb.bus_console.send(msg);
	}
	break;
      case 0x08: // read character attributes
	{
	  unsigned page = get_page(cpu->bh);
	  unsigned pos  = get_pos(cpu->bh);
	  cpu->ax = *(reinterpret_cast<unsigned short *>(_framebuffer_ptr) + TEXT_OFFSET + page + pos);
	}
	break;
      case 0x09: // write char+attr
      case 0x0a: // write char only
	{
	  unsigned page = get_page(cpu->bh);
	  unsigned pos = get_pos(cpu->bh);
	  for (unsigned i=0; i < cpu->cx; i++) {
	    unsigned offset = page + pos + i;

	    // check for overflow
	    if (offset < 0x800*8) {
		if (cpu->ah & 1) framebuffer_ptr()[2*(TEXT_OFFSET + offset) + 1] = cpu->bl;
		framebuffer_ptr()[2*(TEXT_OFFSET + offset) + 0] = cpu->al;

		MessageConsole msg(MessageConsole::TYPE_CONTENT_UPDATE, CONSOLE_ID);
		_mb.bus_console.send(msg);
	    }
	  }
	}
	break;
      case 0x0e: // write char - teletype output
	{
	  unsigned page  = get_page(cpu->bh);
	  unsigned pos   = get_pos(cpu->bh);
	  uint16   value = uint16(((framebuffer_ptr()[2*(TEXT_OFFSET + page + pos) + 1] & 0xff) << 8));

	  value = uint16(value | cpu->al);
	  bool const update = Screen::vga_putc(value, reinterpret_cast<unsigned short *>(_framebuffer_ptr) + TEXT_OFFSET + page, pos);
	  update_cursor(cpu->bh, ((pos / 80) << 8) | (pos % 80));

	  if (update) {
		MessageConsole msg(MessageConsole::TYPE_CONTENT_UPDATE, CONSOLE_ID);
		_mb.bus_console.send(msg);
	  }
	}
	break;
      case 0x0f: // get video mode
	cpu->ax = read_bda<unsigned short>(0x49);
	cpu->bh = read_bda<unsigned char>(0x62);
	break;
      case 0x12:
	switch (cpu->bl)
	  {
	  case 0x10:  // get ega info
	    cpu->bx = 0x0000;  //  color mode, 64 kb
	    cpu->cx = 0x0007;  //  0-features, mode: 7 (80x25)
	    break;
	  case 0x01: // unknown windows boot
	    DEBUG(cpu);
	    break;
	  default:
	    DEBUG(cpu);
	  }
	break;
      default:
	switch (cpu->ax)
	  {
	  case 0x1130:        // get font information
	    switch (cpu->bh) {
	    case 0:
	      cpu->es.sel   = read_bda<unsigned short>(4*0x1f);
	      cpu->bp       = cpu->es.sel >> 16;
	      cpu->es.sel  &= 0xffff;
	      cpu->es.base  = cpu->es.sel << 4;
	      break;
	    case 1:
	      cpu->es.sel   = read_bda<unsigned short>(4*0x43);
	      cpu->bp       = cpu->es.sel >> 16;
	      cpu->es.sel  &= 0xffff;
	      cpu->es.base  = cpu->es.sel << 4;
	      break;
	    case 5 ... 7:
	      cpu->es.sel      = uint16(_ebda_segment);
	      cpu->es.base     = cpu->es.sel << 4;
	      cpu->bp          = EBDA_FONT_OFFSET;
	      // we let the alternate tables start just before the
	      // font, as this byte would be zero, we are fine
	      if (cpu->bh == 7 || cpu->bh == 5) cpu->bp--;
	      break;
	    default:
	      DEBUG(cpu);
	    }
	    DEBUG(cpu);
	    cpu->cx = read_bda<unsigned short>(0x85) & 0xff;
	    cpu->dl = uint8(read_bda<unsigned short>(0x84));
	    msg.mtr_out |= MTD_DS_ES | MTD_GPR_BSD;
	    break;
	  case 0x1a00:        // display combination code
	    cpu->al = 0x1a;   // function supported
	    cpu->bx = 0x0008; // vga color active
	    break;
	  case 0x2000: // unknown during windows boot
	    cpu->efl |= 1;
	    msg.mtr_out |= MTD_RFLAGS;
	    // unsupported
	    break;
	  default:
	    if (!handle_vesa(msg, cpu))
	      return false;
	  }
      }
    //DEBUG(cpu);
    msg.mtr_out |= MTD_GPR_ACDB;
    return true;
  }

  void set_videomode(mword videomode)
  {
      ConsoleModeInfo info { };
      _regs.mode = uint16(get_vesa_mode(videomode & 0x0fff, &info));
  }

public:

	bool receive(MessageBios &msg)
	{
		switch(msg.irq) {
		case 0x10: {
			Seoul::Lock::Guard guard(_lock);
			return handle_int10(msg);
		}
		case BIOS_RESET_VECTOR: {
			Seoul::Lock::Guard guard(_lock);
			return handle_reset(true);
		}
		default:
			return false;
		}
	}


	bool receive(MessageIOOut &msg)
	{
		bool res = false;

		for (unsigned i = 0; i < (1u << msg.type); i++) {
			unsigned char value = uint8(msg.value >> i*8);

			if (!in_range(msg.port + i, _iobase, 32))
				continue;

			Seoul::Lock::Guard guard(_lock);

			switch (msg.port + i - _iobase) {
			case 0x0: // attribute address and write
			case 0x1: // attribute read
			case 0x8: // dac address write mode
			case 0x9: // dac data
			case 0xe: // graphics controller address
			case 0xf: // graphics controller data
				break;
			case 0x14: // crt address
				_crt_index = value;
				break;
			case 0x15: // crt data
				switch (_crt_index) {
				case 0x0a: // cursor scanline start
					_regs.cursor_style = uint16((value << 8) | (_regs.cursor_style & 0xff));
					break;
				case 0x0b: // cursor scanline end
					_regs.cursor_style = uint16((_regs.cursor_style & ~0xff) | value);
					break;
				case 0x0e: // cursor location high
					_regs.cursor_pos = TEXT_OFFSET + ((value << 8) | (_regs.cursor_pos & 0xff));
					break;
				case 0x0f: // cursor location low
					_regs.cursor_pos = (_regs.cursor_pos & ~0xff) | value;
					break;
				case 0x0c: // start address high
					_regs.offset = TEXT_OFFSET + ((value << 8) | (_regs.offset & 0xff));
					break;
				case 0x0d: // start address low
					_regs.offset = (_regs.offset & ~0xff) | value;
					break;
				default:
					break;
				}
				break;
			default:
				break;
			}

			res = true;
		}

		return res;
	}


	bool receive(MessageIOIn &msg)
	{
		bool res = false;

		for (unsigned i = 0; i < (1u << msg.type); i++) {
			if (!in_range(msg.port + i, _iobase, 32))
				continue;

			Seoul::Lock::Guard guard(_lock);

			unsigned char value = ~0;
			switch (msg.port + i - _iobase) {
			case 0x14: // crt address
				value = _crt_index;
				break;
			case 0x13: // alias of crt data
			case 0x15: // crt data
				switch (_crt_index) {
				case 0x0a: // cursor scanline start
					value = uint8(_regs.cursor_style >> 8);
					break;
				case 0x0b: // cursor scanline end
					value = uint8(_regs.cursor_style);
					break;
				case 0x0e: // cursor location high
					value = uint8((_regs.cursor_pos - TEXT_OFFSET) >> 8);
					break;
				case 0x0f: // cursor location low
					value = uint8((_regs.cursor_pos - TEXT_OFFSET));
					break;
				case 0x0c: // start addres high
					value = uint8((_regs.offset - TEXT_OFFSET) >> 8);
					break;
				case 0x0d: // start addres low
					value = uint8(_regs.offset);
					break;
				default:
					break;
				}

				break;
			default:
				break;
			}

			msg.value = (msg.value & ~(0xff << i*8)) | (value << i*8);
			res = true;
		}

		return res;
	}

  bool  claim(MessageMem &msg)
  {
    return ((in_range(msg.phys, _framebuffer_phys, _framebuffer_size)) || (in_range(msg.phys, LOW_BASE, LOW_SIZE)));
  }

  bool  receive(MessageMem &msg)
  {
    unsigned *ptr;
    if (in_range(msg.phys, _framebuffer_phys, _framebuffer_size))
      ptr = reinterpret_cast<unsigned *>(framebuffer_ptr() + msg.phys - _framebuffer_phys);
    else if (in_range(msg.phys, LOW_BASE, LOW_SIZE))
      ptr = reinterpret_cast<unsigned *>(framebuffer_ptr() + msg.phys - LOW_BASE);
    else return false;

    if (msg.read) *msg.ptr = *ptr; else *ptr = *msg.ptr;
    return true;
  }


  bool  receive(MessageMemRegion &msg)
  {
    if (in_range(msg.page, _framebuffer_phys >> 12, _framebuffer_size >> 12)) {
      msg.start_page = _framebuffer_phys >> 12;
      msg.count = _framebuffer_size >> 12;
    }
    else if (in_range(msg.page, LOW_BASE >> 12, LOW_SIZE >> 12)) {
	msg.start_page = LOW_BASE >> 12;
	msg.count = LOW_SIZE >> 12;
    }
    else return false;
    msg.ptr = framebuffer_ptr();
    return true;
  }

  bool  receive(MessageDiscovery &msg) {
    if (msg.type != MessageDiscovery::DISCOVERY) return false;
    discovery_write_dw("bda",  0x49,    3, 1); // current videomode
    discovery_write_dw("bda",  0x4a,   80, 1); // columns on screen
    discovery_write_dw("bda",  0x4c, 4000, 2); // screenbytes: 80*25*2
    for (unsigned i=0; i < 8; i++)             // cursor positions
      discovery_write_dw("bda",  0x50 + 2*i,    0, 2);
    discovery_write_dw("bda",  0x84,   24, 1); // rows - 1
    discovery_write_dw("bda",  0x85,   16, 1); // character height in scan-lines
    discovery_write_dw("bda",  0x62,    0, 1); // current page address
    discovery_write_dw("bda",  0x63, _iobase + 0x14, 2); // crt address


    MessageConsole msg2(MessageConsole::TYPE_GET_FONT, CONSOLE_ID);
    msg2.ptr = framebuffer_ptr();
    if (_mb.bus_console.send(msg2)) {
      // write it to the EBDA
      discovery_write_st("ebda", EBDA_FONT_OFFSET, framebuffer_ptr(), 0x1000);
      discovery_read_dw("bda", 0xe, _ebda_segment);
      // set font vector
      discovery_write_dw("realmode idt", 0x43 * 4, (_ebda_segment << 16) | EBDA_FONT_OFFSET);
    }
    return true;
  }

  bool receive(MessageRestore &msg)
  {
      const mword bytes = reinterpret_cast<mword>(&_restore_processed)
          -reinterpret_cast<mword>(&_view);

      if (msg.devtype == MessageRestore::RESTORE_RESTART) {
          _restore_processed = false;
          msg.bytes += bytes + sizeof(msg);
          return false;
      }

      if (msg.devtype == MessageRestore::VGA_DISPLAY_GUEST) {
          if (msg.write) memset(framebuffer_ptr(), 0, _framebuffer_size);
          puts_guest(msg.space);
          return true;
      }

      if (msg.devtype == MessageRestore::VGA_VIDEOMODE) {
          if (msg.write) {
              set_videomode(msg.bytes);
              MessageConsole cmsg(MessageConsole::TYPE_SWITCH_VIEW, CONSOLE_ID);
              cmsg.view = _view;
              _mb.bus_console.send(cmsg);
          }
          else
              msg.bytes = _last_videomode_request;
          return true;
      }

      if (msg.devtype != MessageRestore::RESTORE_VGA || _restore_processed) return false;

      if (msg.write) {
          msg.bytes = bytes;
          memcpy(msg.space, reinterpret_cast<void*>(&_view), bytes);

      }
      else {
          memcpy(reinterpret_cast<void*>(&_view), msg.space, bytes);
          set_videomode(_last_videomode_request);
      }

      //Logging::printf("%s VGA\n", msg.write?"Saved":"Restored");
      _restore_processed = true;
      return true;
  }


  Vga(Motherboard &mb, unsigned short iobase, char *fb_ptr, uint32 fb_phys, size_t fb_size)
    : BiosCommon(mb), _framebuffer_ptr((uintptr_t)fb_ptr), _framebuffer_phys(fb_phys), _framebuffer_size(fb_size), _iobase(iobase)
  {
    assert(!(fb_phys & 0xfff));
    assert(!(fb_size & 0xfff));

    handle_reset(false);


    // alloc console
    MessageConsole msg(MessageConsole::TYPE_ALLOC_VIEW, CONSOLE_ID, _framebuffer_phys, framebuffer_ptr(), &_regs);
    if (!mb.bus_console.send(msg))
      Logging::panic("could not alloc a VGA backend");
    _view = msg.view;

    if (false)
      Logging::printf("VGA console %x+%zx %zx\n", _framebuffer_phys, _framebuffer_size, _framebuffer_ptr);

    // switch to our console
    msg.type = MessageConsole::TYPE_SWITCH_VIEW;
    mb.bus_console.send(msg);
  }
};

PARAM_HANDLER(vga,
	      "vga:iobase - attach a virtual VGA controller.",
	      "Example: 'vga:0x3c0'",
	      "The framebuffersize is detected based on the host.",
	      "This also adds support for VGA and VESA graphics BIOS.")
{
  MessageConsole  msg_gui(MessageConsole::TYPE_ALLOC_CLIENT, Vga::CONSOLE_ID);
  if (!mb.bus_console.send(msg_gui))
    Logging::panic("vga: failed to alloc client gui\n");

  MessageHostOp msg_range(MessageHostOp::OP_RESERVE_IO_RANGE, msg_gui.size);
  if (!mb.bus_hostop.send(msg_range))
    Logging::panic("vga: failed to reserve io range\n");

  MessageHostOp msg_iomem(MessageHostOp::OP_ALLOC_IOMEM_SMALL, Vga::LOW_BASE, msg_gui.size, Vga::LOW_SIZE);
  if (!mb.bus_hostop.send(msg_iomem))
    Logging::panic("vga: failed to alloc io mem\n");

  if (msg_range.phys >= (1ull << 32))
    Logging::panic("framebuffer address to high\n");

  Vga *dev = new Vga(mb, uint16(argv[0]), msg_iomem.ptr, unsigned(msg_range.phys), msg_gui.size);
  mb.bus_ioin     .add(dev, Vga::receive_static<MessageIOIn>);
  mb.bus_ioout    .add(dev, Vga::receive_static<MessageIOOut>);
  mb.bus_bios     .add(dev, Vga::receive_static<MessageBios>);
  mb.bus_mem      .add(dev, Vga::receive_static<MessageMem>);
  mb.bus_mem.add_iothread_callback(dev, Vga::claim_static<MessageMem>);
  mb.bus_memregion.add(dev, Vga::receive_static<MessageMemRegion>);
  mb.bus_discovery.add(dev, Vga::receive_static<MessageDiscovery>);
  mb.bus_restore.add(dev, Vga::receive_static<MessageRestore>);
}
