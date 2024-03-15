/** @file
 * Shared SATA definitions.
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

#include "service/lock.h"

class FisReceiver
{
	private:

		/*
		 * Noncopyable
		 */
		FisReceiver(FisReceiver const &);
		FisReceiver &operator = (FisReceiver const &);

	protected:

		enum { FIS_TYPE_DMA_SETUP = 0x41 };

		~FisReceiver() {}

	public:

		Seoul::Lock _lock { };

		FisReceiver() {}

		virtual void receive_fis(size_t fislen, unsigned *fis) = 0;
};
