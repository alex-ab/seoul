/** @file
 * Generic math helper functions.
 *
 * Copyright (C) 2007-2009, Bernhard Kauer <bk@vmmon.org>
 * Economic rights: Technische Universitaet Dresden (Germany)
 *
 * This file is part of Seoul/Vancouver.
 *
 * Seoul/Vancouver is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2 as
 * published by the Free Software Foundation.
 *
 * Seoul/Vancouver is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License version 2 for more details.
 */
#pragma once

#include <nul/types.h>
#include <service/logging.h>

#define union64(HIGH, LOW)          (uint64(HIGH) << 32 | (LOW))

struct Math
{
  /**
   * Divides <value> by <divisor> and returns the remainder
   */
  template<typename T>
  static T moddiv(T &value, T divisor)
  {
    T res = value % divisor;
    value /= divisor;
    return res;
  }

  /**
   * We are limited here by the ability to divide through a unsigned
   * long value, thus factor and divisor needs to be less than 1<<32.
   */
  static uint64 _muldiv128(uint64 const value, uint64 const factor, uint64 const divisor)
  {
    if (factor >= 1ull << 32 || divisor >= 1ull << 32)
      Logging::panic("%s: too large %llx %llx", __func__, factor, divisor);

    uint64 const low   = value & 0xFFFFFFFF;
    uint64 const high  = value >> 32;
    uint64       lower = low  * factor;
    uint64       upper = high * factor;
    uint64 const rem   = moddiv<uint64>(upper, divisor);

    lower += rem << 32;
    lower /= divisor;
    return (upper << 32) + lower;
  }

  static uint64 muldiv128(uint64 const value, uint64 const factor, uint64 const divisor)
  {
#ifdef __x86_64__
    return uint64(__int128_t(value) * __int128_t(factor) / __int128_t(divisor));
#else
    return _muldiv128(value, factor, divisor);
#endif
  }
};
