/** @file
 * Fixed-width integer types.
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

#pragma once

/* include stddef for NULL */
#include <stddef.h>

/* include genode types */
#include <base/stdint.h>

#include <nul/compiler.h>

VMM_BEGIN_EXTERN_C

/* turn warning off for external headers */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"

#ifdef __MMX__
#include <mmintrin.h>
#endif

#ifdef __SSE2__
#include <emmintrin.h>
#endif

#ifdef __SSSE3__
#include <tmmintrin.h>
#endif

/* turn warnings on */
#pragma GCC diagnostic pop

VMM_END_EXTERN_C

/* Constant-width integer types. */
typedef genode_uint64_t  uint64;
typedef genode_uint32_t  uint32;
typedef genode_uint16_t  uint16;
typedef genode_uint8_t   uint8;
typedef Genode::addr_t   mword;
typedef mword            uintptr_t;

typedef genode_int64_t int64;
typedef genode_int32_t int32;
typedef genode_int16_t int16;
typedef genode_int8_t  int8;

/* NUL specific types */

typedef unsigned log_cpu_no;
typedef unsigned phy_cpu_no;
typedef unsigned cap_sel;       /* capability selector */

void * operator new[](size_t size, unsigned alignment);
void * operator new(size_t size, unsigned alignment);

/* EOF */
