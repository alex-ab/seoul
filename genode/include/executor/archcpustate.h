/*
 * User Thread Control Block (UTCB)
 *
 * Copyright (C) 2008, Udo Steinberg <udo@hypervisor.org>
 * Copyright (C) 2008-2010, Bernhard Kauer <bk@vmmon.org>
 * Copyright (C) 2011-2023, Alexander Boettcher
 * Copyright (C) 2012, Julian Stecklina <jsteckli@os.inf.tu-dresden.de>
 * Economic rights: Technische Universitaet Dresden (Germany)
 *
 * This file is part of Seoul.
 *
 * Soul is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 as
 * published by the Free Software Foundation.
 *
 * Seoul is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License version 2 for more details.
 */

#pragma once

#include <service/string.h>

enum {
  MTD_GPR_ACDB        = 1ul << 0,
  MTD_GPR_BSD         = 1ul << 1,
  MTD_RSP             = 1ul << 2,
  MTD_RIP_LEN         = 1ul << 3,
  MTD_RFLAGS          = 1ul << 4,
  MTD_DS_ES           = 1ul << 5,
  MTD_FS_GS           = 1ul << 6,
  MTD_CS_SS           = 1ul << 7,
  MTD_TR              = 1ul << 8,
  MTD_LDTR            = 1ul << 9,
  MTD_GDTR            = 1ul << 10,
  MTD_IDTR            = 1ul << 11,
  MTD_CR              = 1ul << 12,
  MTD_DR              = 1ul << 13,
  MTD_SYSENTER        = 1ul << 14,
  MTD_QUAL            = 1ul << 15,
  MTD_CTRL            = 1ul << 16,
  MTD_INJ             = 1ul << 17,
  MTD_STATE           = 1ul << 18,
  MTD_TSC             = 1ul << 19,
  MTD_EFER            = 1ul << 20,
  MTD_PDPTE           = 1ul << 21,
  MTD_R8_R15          = 1ul << 22,
  MTD_SYSCALL_SWAPGS  = 1ul << 23,
  MTD_IRQ             = MTD_RFLAGS | MTD_STATE | MTD_INJ | MTD_TSC,
  MTD_ALL             = (0x000fffffu & ~MTD_CTRL) | MTD_EFER | MTD_R8_R15 | MTD_SYSCALL_SWAPGS
};

enum {
  INJ_IRQWIN = 0x1000,
  INJ_NMIWIN = 0x0000, // XXX missing
  INJ_WIN    = INJ_IRQWIN | INJ_NMIWIN
};

class ArchCpuState : public Utcb
{
	public:

		/* set all values beside cpuid to zero. */
		void clear()
		{
			auto const cpuid = head.cpuid;
			memset(this, 0, sizeof(*this));
			head.cpuid = cpuid;
		}
};
