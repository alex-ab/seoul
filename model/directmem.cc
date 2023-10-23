/** @file
 * Direct IOIO access.
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

#include "nul/motherboard.h"


/**
 * Bridge between guest and host memory.
 *
 * State: testing
 */
class DirectMemDevice : public StaticReceiver<DirectMemDevice>
{
  char      * const _ptr;
  uintptr_t   const _phys;
  size_t      const _size;
  bool        const _readonly;

  /*
   * Noncopyable
   */
  DirectMemDevice(DirectMemDevice const &);
  DirectMemDevice &operator = (DirectMemDevice const &);

 public:

  bool  receive(MessageMemRegion &msg)
  {
    if (!in_range(msg.page, _phys >> 12, _size >> 12))
      return false;

    Logging::printf("%s: %p base %lx+%zx\n", __PRETTY_FUNCTION__, _ptr, _phys, _size);

    msg.start_page = _phys >> 12;
    msg.count      = unsigned(_size >> 12);
    msg.ptr        = _ptr;
    msg.read_only  = _readonly;
    return true;
  }


  bool receive(MessageMem &msg) const
  {
    if (!in_range(msg.phys, _phys, _size))
      return false;

    if (_readonly && !msg.read)
      return false;

    unsigned * ptr = reinterpret_cast<unsigned *>(_ptr + msg.phys - _phys);

    if (msg.read) *msg.ptr = *ptr; else *ptr = *msg.ptr;

    return true;
  }


  DirectMemDevice(char *ptr, uintptr_t phys, size_t size, bool readonly)
  : _ptr(ptr), _phys(phys), _size(size), _readonly(readonly)
  {
    Logging::printf("directmem: %p base %lx+%zx %s\n",
                    ptr, phys, size, _readonly ? "readonly" : "");
  }
};


PARAM_HANDLER(mio,
	      "mio:base,size,readonly,dest=base - map hostmemory directly into the VM.",
	      "Example: 'mio:0xa0000,0x10000'.")
{
  auto const invalid_arg = ~0UL;
  auto const guest_base  = argv[0];
  bool const readonly    = !!argv[2];
  auto const dest        = (argv[3] == invalid_arg) ? guest_base : argv[3];

  if (argv[0] == invalid_arg || argv[1] == invalid_arg || argv[2] == invalid_arg)
    Logging::panic("directmem - invalid parameters\n");

  MessageHostOp msg(MessageHostOp::OP_ALLOC_IOMEM, guest_base, argv[1]);
  if (!mb.bus_hostop.send(msg) || !msg.ptr)
    Logging::panic("can not map IOMEM region %lx+%zx", msg.value, msg.len);

  DirectMemDevice *dev = new DirectMemDevice(msg.ptr, dest, argv[1], readonly);

  mb.bus_memregion.add(dev, DirectMemDevice::receive_static<MessageMemRegion>);
  mb.bus_mem      .add(dev, DirectMemDevice::receive_static<MessageMem>);
}
