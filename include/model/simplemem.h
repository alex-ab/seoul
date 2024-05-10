/**
 * SimpleMemoryAccess
 *
 * Copyright (C) 2010, Bernhard Kauer <bk@vmmon.org>
 * Economic rights: Technische Universitaet Dresden (Germany)
 *
 * Copyright (C) 2024, Alexander Boettcher
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


/**
 * Fast copy/inout version that only copies to mem-regions but not registers.
 */
bool _copy_check(DBus<MessageMemRegion> &memregion, uintptr_t address,
                 auto ptr, size_t count, auto const &fn)
{
	MessageMemRegion msg(address >> 12);

	if (!memregion.send(msg) || !msg.ptr || ((address + count) > ((msg.start_page + msg.count) << 12)))
		return false;

	return fn(msg);
}


/**
 * Copy from the guest to a buffer.
 */
bool copy_in(DBus<MessageMemRegion> &memregion, DBus<MessageMem> &mem,
             uintptr_t address, void *ptr, size_t count)
{
	if (_copy_check(memregion, address, ptr, count, [&](auto const & msg) {
		memcpy(ptr, msg.ptr + (address - (msg.start_page << 12)), count);
		return true;
	}))
		return true;

	auto p = reinterpret_cast<char *>(ptr);

	if (address & 3ul) {
		unsigned value { };

		MessageMem msg(true, address & ~3ul, &value);
		if (!mem.send(msg, true))
			return false;

		size_t l = 4 - (address & 3ul);
		if (l > count)
			l = count;

		value >>= 8 * (address & 3ul);
		memcpy(p, &value, l);

		p       += l;
		address += l;
		count   -= l;
	}

	assert(!(address & 3));

	while (count >= 4) {
		MessageMem msg(true, address, reinterpret_cast<unsigned *>(p));
		if (!mem.send(msg, true))
			return false;

		address += 4;
		p       += 4;
		count   -= 4;
	}

	if (count) {
		unsigned value { };

		MessageMem msg(true, address, &value);
		if (!mem.send(msg, true))
			return false;

		memcpy(p, &value, count);
	}

	return true;
}


/**
 * Copy from buffer to the guest.
 */
bool copy_out(DBus<MessageMemRegion> &memregion, DBus<MessageMem> &mem,
              uintptr_t address, void const *ptr, size_t count)
{
	if (_copy_check(memregion, address, ptr, count, [&](auto const & msg) {
		memcpy(msg.ptr + (address - (msg.start_page << 12)), ptr, count);
		return true;
	}))
		return true;

	auto p = reinterpret_cast<char const *>(ptr);

	if (address & 3ul) {
		unsigned value { };

		MessageMem msg(true, address & ~3ul, &value);
		if (!mem.send(msg, true))
			return false;

		size_t l = 4 - (address & 3ul);
		if (l > count)
			l = count;

		memcpy(reinterpret_cast<char *>(&value) + (address & 3ul), p, l);

		msg.read = false;
		if (!mem.send(msg, true))
			return false;

		p       += l;
		address += l;
		count   -= l;
	}

	assert(!(address & 3));

	while (count >= 4) {
		auto const tmp = reinterpret_cast<uintptr_t>(p);
		MessageMem msg(false, address, reinterpret_cast<unsigned *>(tmp));
		if (!mem.send(msg, true))
			return false;

		address += 4;
		p       += 4;
		count   -= 4;
	}

	if (count) {
		unsigned value { };

		MessageMem msg(true, address, &value);
		if (!mem.send(msg, true))
			return false;

		memcpy(&value, p, count);

		msg.read = false;
		if (!mem.send(msg, true))
			return false;
	}

	return true;
}
