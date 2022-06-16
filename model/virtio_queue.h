/**
 * Virtio split queue
 *
 * Copyright (C) 2021-2022, Alexander Boettcher
 *
 * This file is part of Seoul.
 *
 * Seoul is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 as
 * published by the Free Software Foundation.
 *
 * Seoul is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License version 2 for more details.
 */

#pragma once

namespace Virtio { class Queue; }

class Virtio::Queue
{
	public:

		struct index_ring {
			uint16 idx;

			index_ring operator += (unsigned i) {
				idx = (idx + i) % 65536; return *this; }

			index_ring operator + (uint16 const i) const {
				return index_ring { uint16((idx + i) % 65536) }; }

			bool operator != (index_ring const &o) const {
				return (idx != o.idx); }
		};

		struct desc
		{
			uint64 addr;
			uint32 len;
			uint16 flags;
			uint16 next;
		};

	private:

		struct index_desc { uint16 idx; };

		unsigned   max_index  { 0 };
		index_ring ring_index { 0 };

		struct
		{
			uintptr_t base;

			uint16 read(unsigned const off) const {
				return *reinterpret_cast<uint16 *>(base + off); }

			bool inject()
			{
				auto flags = read(0);
				return (flags & 1) == 0;
			}

			index_desc get(index_ring id, unsigned const max) const
			{
				auto i = read(4u + 2u * (id.idx % max));

				if (i >= max)
					Logging::panic("insane index %u >= %u\n", i, max);

				return index_desc { .idx = i };
			}

			index_ring idx() { return index_ring { .idx = read(2) }; }
		} queue_avail { };

		struct
		{
			uintptr_t base;

			template <typename T>
			void write(T const value, unsigned const off) const {
				*reinterpret_cast<T *>(base + off) = value; }

			void add(index_ring ri, index_desc di, size_t size, unsigned const max)
			{
				if (di.idx >= max)
					Logging::panic("index out of bound %u >= %u\n", di.idx, max);

				uint16 const flags = 0;
				write(flags, 0);

				uint64 const e = uint64(di.idx) | (uint64(size) << 32);
				write(e, 4u + 8 * (ri.idx % max));
			}

			void update(index_ring const &index) const
			{
				write(index.idx, 2);
			}
		} queue_used { };

		struct
		{
			uintptr_t base;

			struct desc get(index_desc idx, unsigned const max) const
			{
				if (idx.idx >= max)
					Logging::panic("index of descriptor out of bounds\n");
				return *reinterpret_cast<desc *>(base + (sizeof(desc) * idx.idx));
			}
		} queue_desc { };

	public:

		bool enabled() { return !!max_index; };

		void init(uintptr_t des, uintptr_t dev, uintptr_t drv, unsigned count)
		{
			max_index        = count;
			queue_avail.base = drv;
			queue_used .base = dev;
			queue_desc .base = des;
		}

		desc next_desc(desc const &de) const
		{
			enum {
				VIRTQ_DESC_F_NEXT     = 1, /* one further descriptor */
				VIRTQ_DESC_F_WRITE    = 2, /* device write only */
				VIRTQ_DESC_F_INDIRECT = 4  /* list of descriptors */
			};

			if (de.flags & VIRTQ_DESC_F_INDIRECT)
				Logging::printf("unsupported desc_f_indirect detected\n");

			if (!(de.flags & VIRTQ_DESC_F_NEXT))
				return { };

			return queue_desc.get(index_desc { .idx = de.next }, max_index);
		}

		template <typename FUNC>
		bool consume(index_ring &ri, FUNC func)
		{
			bool update = false;

			VMM_MEMORY_BARRIER;

			for (auto avail_idx = queue_avail.idx(); ri.idx != avail_idx.idx; ri += 1) {

				index_desc id = queue_avail.get(ri, max_index);
				desc       de = queue_desc.get(id, max_index);

				if (!de.addr || !de.len) { break; }

				try {
					size_t consumed = func(de, ri);
					if (!consumed) { break; }

					queue_used.add(ri, id, consumed, max_index);

					update = true;
				} catch (...) {
					Logging::panic("unhandled exception in virtio device\n");
					throw;
				}
			}

			if (update) {
				queue_used.update(ring_index);

				VMM_MEMORY_BARRIER;
			}

			return update && queue_avail.inject();
		}

		template <typename FUNC>
		bool consume(FUNC func)
		{
			return consume(ring_index, func);
		}
};
