/** @file
 * Timer infrastucture.
 *
 * Copyright (C) 2007-2008, Bernhard Kauer <bk@vmmon.org>
 * Economic rights: Technische Universitaet Dresden (Germany)
 *
 * Copyright (C) 2013 Jacek Galowicz, Intel Corporation.
 * Copyright (C) 2013 Markus Partheymueller, Intel Corporation.
 * Copyright (C) 2024 Alexander Boettcher
 *
 * This file is part of Seoul.
 *
 * Seoul is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2 as
 * published by the Free Software Foundation.
 *
 * Seoul is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License version 2 for more details.
 */

#pragma once

#include "service/cpu.h"
#include "service/math.h"

#include <nul/message.h>
#include <nul/bus.h>
#include <nul/templates.h>


typedef unsigned long long timevalue;

/**
 * A clock returns the time in different time domains.
 *
 * The reference clock is the CPUs TSC.
 */
class Clock
{
	protected:

		timevalue const _source_freq;

	public:

		static timevalue time() { return Cpu::rdtsc(); }

		timevalue freq() const { return _source_freq; }

		/**
		 * Returns the clock in freq-time. If tsc is not specified, the
		 * current tsc value of time() is used.
		 */
		timevalue clock(timevalue const freq, timevalue const tsc = time()) const
		{
			return Math::muldiv128(tsc, freq, _source_freq);
		}

		timevalue convert_tsc_to(timevalue const non_tsc_freq, timevalue const tsc) const
		{
			return clock(non_tsc_freq, tsc);
		}

		timevalue convert_to_tsc(timevalue const non_tsc_freq, timevalue const non_tsc) const
		{
			return Math::muldiv128(non_tsc, _source_freq, non_tsc_freq);
		}

		/**
		 * Returns a timeout in absolute TSC time.
		 *
		 * @param thedelta The timeout in freq domain
		 * @param freq     Scale of @a thedelta
		 * @param base     TSC base (if not specified current tsc)
		 *
		 * The timeout equals to thedelta/freq seconds.
		 *
		 * Example: abstime(5, 1000) returns the time of base plus 5 ms.
		 */
		timevalue abstime(timevalue const thedelta, timevalue const freq,
		                  timevalue const base = time()) const
		{
			return base + convert_to_tsc(freq, thedelta);
		}

		/**
		 * Returns a delta in another frequency domain from an absolute TSC value.
		 */
		timevalue delta(timevalue const tsc_absolute,
		                timevalue const freq,
		                timevalue const tsc_now = time()) const
		{
			if (tsc_now >= tsc_absolute)
				return 0ull;

			return clock(freq, tsc_absolute - tsc_now);
		}

		Clock(timevalue source_freq) : _source_freq(source_freq) {}
};


/**
 * Keeping track of the timeouts.
 */
template <unsigned ENTRIES, typename DATA>
class TimeoutList : public StaticReceiver<TimeoutList<ENTRIES, DATA>>
{
  class TimeoutEntry
  {
    friend class TimeoutList<ENTRIES, DATA>;
    TimeoutEntry *_next;
    TimeoutEntry *_prev;
    timevalue _timeout;
    DATA * data;
    bool      _free;
  };

  TimeoutEntry  _entries[ENTRIES];

  bool _restore_processed;
public:
  /**
   * Alloc a new timeout object.
   */
  unsigned alloc(DATA * _data = 0)
  {
    unsigned i;
    for (i=1; i < ENTRIES; i++) {
      if (not _entries[i]._free) continue;
      _entries[i].data  = _data;
      _entries[i]._free = false;
      return i;
    }
    Logging::panic("Can't alloc a timer!\n");
    return 0;
  }

  /**
   * Dealloc a timeout object.
   */
  unsigned dealloc(unsigned nr, bool withcancel = false) {
    if (!nr || nr > ENTRIES - 1) return 0;
    if (_entries[nr]._free) return 0;

    // should only be done when no no concurrent access happens ...
    if (withcancel) cancel(nr);
    _entries[nr]._free = true;
    _entries[nr].data = 0;
    return 1;
  }

  /**
   * Cancel a programmed timeout.
   */
  int cancel(unsigned nr)
  {
    if (!nr || nr >= ENTRIES)  return -1;
    TimeoutEntry *current = _entries+nr;
    if (current->_next == current) return -2;
    int res = _entries[0]._next != current;

    current->_next->_prev =  current->_prev;
    current->_prev->_next =  current->_next;
    current->_next = current->_prev = current;
    return res;
  }


  /**
   * Request a new timeout.
   */
  int request(unsigned nr, timevalue to)
  {
    if (!nr || nr > ENTRIES)  return -1;
    timevalue old = timeout();
    TimeoutEntry *current = _entries + nr;
    cancel(nr);

    // keep a sorted list here
    TimeoutEntry *t = _entries;
    do { t = t->_next; }  while (t->_timeout < to);

    current->_timeout = to;
    current->_next = t;
    current->_prev = t->_prev;
    t->_prev->_next = current;
    t->_prev = current;
    return timeout() == old;
  }

  /**
   * Get the head of the queue.
   */
  unsigned trigger(timevalue now, DATA ** data = 0)
  {
    if (now < timeout())
      return 0u;

    auto i = _entries[0]._next - _entries;
    if (i < 0 || i >= long(ENTRIES)) {
      Logging::printf("invalid timer %ld\n", i);
      return 0u;
    }
    if (data)
      *data = _entries[i].data;
    return unsigned (i);
  }

  timevalue timeout() { assert(_entries[0]._next); return _entries[0]._next->_timeout; }
  void init()
  {
    for (unsigned i = 0; i < ENTRIES; i++)
      {
        _entries[i]._prev = _entries + i;
        _entries[i]._next = _entries + i;
        _entries[i].data  = 0;
        _entries[i]._free = true;
      }
    _entries[0]._timeout = ~0ULL;
  }

  TimeoutList() : _restore_processed(false) { init(); }

#define REL_PTR(ptr, offset) ( \
    reinterpret_cast<__typeof__(ptr)>( \
        reinterpret_cast<mword>(ptr) - reinterpret_cast<mword>(offset)) \
)
#define ABS_PTR(ptr, offset) ( \
    reinterpret_cast<__typeof__(ptr)>( \
        reinterpret_cast<mword>(ptr) + reinterpret_cast<mword>(offset)) \
)

  bool receive(MessageRestore &msg)
  {
      const mword bytes = reinterpret_cast<mword>(&_restore_processed)
          - reinterpret_cast<mword>(_entries);

      if (msg.devtype == MessageRestore::RESTORE_RESTART) {
          _restore_processed = false;
          msg.bytes += bytes + sizeof(msg);
          return false;
      }

      if (msg.devtype != MessageRestore::RESTORE_TIMEOUTLIST || _restore_processed) return false;

      unsigned long long rdtsc = Cpu::rdtsc();

      if (msg.write) {
          msg.bytes = bytes;
          memcpy(msg.space, reinterpret_cast<void*>(_entries), bytes);

          // Do not mess around with timeout entries of the running guest,
          // since we may want to let it continue after saving
          TimeoutEntry *entries = reinterpret_cast<TimeoutEntry*>(msg.space);
          for (unsigned i=0; i < ENTRIES; i++) {
              entries[i]._prev = REL_PTR(entries[i]._prev, _entries);
              entries[i]._next = REL_PTR(entries[i]._next, _entries);

              if (i == 0) continue;

              if (entries[i]._timeout <= rdtsc)
                  entries[i]._timeout = 0;
              else
                  entries[i]._timeout -= rdtsc;
          }
      }
      else {
          memcpy(reinterpret_cast<void*>(_entries), msg.space, bytes);
          for (unsigned i=0; i < ENTRIES; i++) {
              _entries[i]._prev = ABS_PTR(_entries[i]._prev, _entries);
              _entries[i]._next = ABS_PTR(_entries[i]._next, _entries);

              if (i == 0) continue;
              _entries[i]._timeout += rdtsc;
          }
      }

      //Logging::printf("%s Timeoutlist\n", msg.write ? "Saved" : "Restored");
      _restore_processed = true;
      return true;
  }
};
