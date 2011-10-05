/** @file
 * Standard include file and asm implementation.
 *
 * Copyright (C) 2007-2008, Bernhard Kauer <kauer@tudos.org>
 * Economic rights: Technische Universitaet Dresden (Germany)
 *
 * This file is part of NUL (NOVA user land).
 *
 * NUL is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2 as
 * published by the Free Software Foundation.
 *
 * NUL is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License version 2 for more details.
 */

#include <service/string.h>

/************************************************************************
 * Memory functions.
 ************************************************************************/


int memcmp(const void *dst, const void *src, size_t count)
{
  const char *d = reinterpret_cast<const char *>(dst);
  const char *s = reinterpret_cast<const char *>(src);
  unsigned diff = 0;
  while (!diff && count--) diff = *d++ - *s++;
  return diff;
}


/************************************************************************
 * String functions.
 ************************************************************************/


size_t strnlen(const char *src, size_t maxlen)
{
  size_t i = 0;
  while (src[i] && maxlen--) i++;
  return i;
}


size_t strlen(const char *src) { return strnlen(src, ~0ul); }


char * strcpy(char *dst, const char *src)
{
  char *res = dst;
  unsigned char ch;
  do {
    ch = *src++;
    *dst++ = ch;
  } while (ch);
  return res;
}
