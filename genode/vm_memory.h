/*
 * \brief  VM memory management
 */

/*
 * This file is distributed under the terms of the GNU General Public License
 * version 2.
 *
 * The code is partially based on the Seoul VMM, which is distributed
 * under the terms of the GNU General Public License version 2.
 */

#pragma once

#include <base/allocator.h>
#include <rm_session/connection.h>
#include <vm_session/connection.h>
#include <region_map/client.h>
#include <platform_session/connection.h>

namespace Seoul {

	using namespace Genode;

	class Vm_memory;

	typedef Genode::Constructible<Platform::Connection> Pci_platform;
}

class Seoul::Vm_memory
{
	private:

		Env            &_env;
		Vm_connection  &_vm_con;
		Pci_platform   &_platform;
		Rm_connection   _rm_reserve   { _env };

		static constexpr auto permit_overlap = 0x1000ul;

		uint64_t const  _vm_size;
		size_t   const  _io_mem_size  { 1ul << 30 };
		addr_t          _io_mem_alloc { 3 * (1ul << 30) }; /* configurable ? */
		addr_t          _local_addr   { };
		bool            _io_mem_gap   { false };
		bool     const  _verbose;

		addr_t _attach_at(Dataspace_capability const &ds, addr_t const at)
		{
			return _env.rm().attach(ds, {
				.size       = { },   .offset    = { },
				.use_at     = true,  .at        = at,
				.executable = { },   .writeable = { },
			}).convert<addr_t>(
				[&] (Env::Local_rm::Attachment &a) {
					a.deallocate = false; return addr_t(a.ptr); },
				[&] (Region_map::Attach_error e) -> addr_t { return 0ul; });
		}

		addr_t _reserve_local_range()
		{
			addr_t backing_store { };

			{
				auto const ds = _rm_reserve.create(size_t(_vm_size + (_io_mem_gap ? _io_mem_size : 0)));
				Region_map_client rm(ds);
				Attached_dataspace tmp(_env.rm(), rm.dataspace());
				backing_store = (addr_t)tmp.local_addr<void>();
				_rm_reserve.destroy(ds);
			}

			/* reserve gap not to be used by dynamic allocations */
			if (_io_mem_gap) {
				auto const ds = _rm_reserve.create(_io_mem_size);

				Region_map_client rm(ds);
				auto const check = _attach_at(rm.dataspace(),
				                              backing_store + _io_mem_alloc);

				if (check != backing_store + _io_mem_alloc)
					Logging::panic("reserved range attachment failed");
			}

			return backing_store;
		}

		/*
		 * Noncopyable
		 */
		Vm_memory(Vm_memory const &);
		Vm_memory &operator = (Vm_memory const &);

		struct Region : Genode::List<Region>::Element {
			Genode::addr_t                _guest_addr;
			Genode::addr_t                _local_addr;
			Genode::Dataspace_capability  _ds;
			Genode::addr_t                _ds_size;

			Region (Genode::addr_t const guest_addr,
			        Genode::addr_t const local_addr,
			        Genode::Dataspace_capability ds,
			        Genode::addr_t const ds_size)
			: _guest_addr(guest_addr), _local_addr(local_addr),
			  _ds(ds), _ds_size(ds_size)
			{ }

			bool overlap(addr_t const addr, addr_t const size) const
			{
				if (!size)
					return true;

				if (addr < _guest_addr && addr + size - 1 < _guest_addr)
					return false;
				if (addr > _guest_addr + _ds_size - 1)
					return false;

				return true;
			}
		};

		Genode::List<Region> _regions { };

		template <typename F>
		void for_each_region(F const &fn)
		{
			for (Region *r = _regions.first(); r; r = r->next())
				fn(*r);
		}

	public:

		Vm_memory(Env           &env,
		          Allocator     &alloc,
		          Vm_connection &vm_con,
		          Pci_platform  &platform,
		          addr_t const vm_size,
		          bool const verbose)
		:
			_env(env), _vm_con(vm_con), _platform(platform),
			_vm_size(vm_size), _verbose(verbose)
		{
			bool const passthrough = _platform.constructed();

			auto const pg_1g  = 1ul << 30ull;
			auto const pg_64m = 1ul << 26ull;
			auto const pg_4m  = 1ul << 22ull;

			auto max_offset = _vm_size;
			auto offset     = 0ull;

			auto step_size = [&](auto const v_size) {
#if 0
				if (passthrough)
					return (v_size > pg_64m) ? pg_64m : v_size;
				else
#endif
				auto size = (v_size > pg_1g) ? pg_1g : v_size;

				{
					if (size < pg_4m) return size;

					auto unaligned_4m = offset & (pg_4m - 1);

					if (unaligned_4m)
						return (size - unaligned_4m) & (pg_4m - 1);
				}

				{
					if (size < pg_1g) return size;

					auto unaligned_1g = offset & (pg_1g - 1);
					if (unaligned_1g)
						return size - unaligned_1g;
				}

				return size;
			};

			/* DMA address 0 is denied by Genode platform driver XXX */
			if (passthrough)
				offset = permit_overlap;

			for (auto ds_size = step_size(_vm_size); offset < max_offset;)
			{
				/* cut out io_mem region from normal memory */
				if (offset < _io_mem_alloc + _io_mem_size) {
					if (offset >= _io_mem_alloc) {
						offset = _io_mem_alloc + _io_mem_size;

						_io_mem_gap = true;
						max_offset += _io_mem_size;
						continue;
					}

					if (offset + ds_size > _io_mem_alloc) {
						ds_size = _io_mem_alloc - offset;
					}
				}

				try {
					error("--- ", Hex(offset), "+", Hex(ds_size), " -> ", Hex(offset + ds_size));
					auto const ds = passthrough
					              ? _platform->alloc_dma_buffer_at(ds_size, Cache::CACHED, offset)
					              : _env.ram().alloc(ds_size);

					if (passthrough) {
						auto dma_addr = _platform->dma_addr(ds);
						if (dma_addr != offset) {
							error("memory not usable for DMA ", Hex(dma_addr),
							      " vs ", Hex(offset));
							Logging::panic("passthrough failed");
						}
					}

					/* register ds for VM region */
					bool ok = add_region(alloc, offset, 0,
					                     ds, ds_size);
					if (!ok)
						Logging::panic("guest memory allocation failed");

					offset += ds_size;

					ds_size = max_offset - offset;
					ds_size = step_size(ds_size);

				} catch (Genode::Ram_allocator::Denied) {

					if (_verbose)
						log("reduce ds_size ", Hex(ds_size), "->",
						    Hex(ds_size >> 1));

					ds_size = ds_size >> 1;

					if      (ds_size > pg_1g) ds_size &= ~(pg_1g - 1);
					else if (ds_size > pg_4m) ds_size &= ~(pg_4m - 1);

					if (ds_size < 4096)
						throw;

					continue;
				}
			}

			/*
			 * DMA address 0 is denied by Genode platform driver XXX
			 *  attach nevertheless a range, but it won't be usable for DMA
			 */
			if (passthrough) {
				offset = 0;
				auto const ds_size = 4096;
				/* alloc_dma_buffer_at would be required */
				//auto const ds = _platform->alloc_dma_buffer_at(ds_size, Cache::CACHED, offset);
				auto const ds = _env.ram().alloc(ds_size);
				bool const ok = add_region(alloc, offset, 0, ds, ds_size);
				if (!ok)
					Logging::panic("guest memory allocation failed at 0");
			}

			/* reserve late, due to add_region using 'new (alloc)' above */
			_local_addr = _reserve_local_range();

			for_each_region([&](auto &region) {

				region._local_addr = _local_addr + region._guest_addr;

				_env.rm().attach(region._ds, {
					.size       =     0, .offset    = 0,
					.use_at     =  true, .at        = region._local_addr,
					.executable = false, .writeable = true,
				}).template convert<addr_t>(
					[&] (Env::Local_rm::Attachment &a) {
						a.deallocate = false; return addr_t(a.ptr); },
					[&] (Region_map::Attach_error e) -> addr_t { return 0ul; });
			});
		}

		/**
		 * Return pointer to locally mapped backing store
		 */
		char *backing_store_local_base()
		{
			return reinterpret_cast<char *>(_local_addr);
		}

		size_t backing_store_size() const
		{
			return size_t(_vm_size + (_io_mem_gap ? _io_mem_size : 0));
		}

		bool remove_region(addr_t const guest_addr, auto const &fn)
		{
			for (auto r = _regions.first(); r; r = r->next()) {
				if (r->_guest_addr != guest_addr)
					continue;

				_regions.remove(r);

				fn(r);

				return true;
			}

			return false;
		}

		bool add_region(Allocator    &alloc,
		                addr_t const  guest_addr,
		                addr_t const  local_addr,
		                Dataspace_capability ds,
		                Genode::addr_t const ds_size)
		{
			if (!ds_size)
				return false;

			for_each_region([&](auto &region) {
				if (!region.overlap(guest_addr, ds_size))
					return;

				if (_verbose || region._guest_addr > permit_overlap)
					warning("overlapping region added: ",
					        Hex(guest_addr), "+", Hex(ds_size),
					        " conflicts with ",
					        Hex(region._guest_addr), "+", Hex(region._ds_size));

				if (region._guest_addr > permit_overlap)
					Genode::sleep_forever();
			});

			if (_verbose)
				log("vm_memory: add_region ", Hex(guest_addr), "+", Hex(ds_size));

			_regions.insert(new (alloc) Region(guest_addr, local_addr, ds, ds_size));

			return true;
		}

		void dump_regions()
		{
			for_each_region([&](auto &region) {

				log("- vmm: ", Hex_range(region._local_addr, region._ds_size),
				    " - vm: ", Hex_range(region._guest_addr, region._ds_size),
				    " - ", Number_of_bytes(region._guest_addr), "+",
				           Number_of_bytes(region._ds_size));
			});
		}

		void attach_to_vm(Vm_connection &vm_con, addr_t g_phys, addr_t size,
		                  bool const writeable)
		{
			bool partial_match = false;

			do {
				partial_match = false;

				for_each_region([&](auto &region) {
					if (!region._ds_size || !size || size & 0xfffu) return;
					if (g_phys < region._guest_addr) return;
					if (g_phys > region._guest_addr + region._ds_size - 1) return;

					auto const ds_offset   = g_phys - region._guest_addr;
					auto const attach_size = min(size, region._ds_size - ds_offset);

					if (_verbose)
						log(__func__, " try attach ", Hex_range(g_phys, size),
						    " -> ", Hex_range(region._guest_addr + ds_offset, attach_size), " ",
						    " of region=", Hex_range(region._guest_addr, region._ds_size));

					vm_con.attach(region._ds, g_phys,
					              { .offset     = ds_offset,
					                .size       = attach_size,
					                .executable = true,
					                .writeable  = writeable }).with_result(
						[&] (auto) {

							if (_verbose)
								log(__func__, "   attached ", Hex_range(g_phys, attach_size),
								    " of region=", Hex_range(region._guest_addr, region._ds_size));
						},
						[&] (auto) { warning(__func__, " attach of ",
						                     Hex_range(g_phys, attach_size),
						                     " to ", Hex_range(region._guest_addr, region._ds_size),
						                     "failed!");
						});

					size   -= attach_size;
					g_phys += attach_size;

					partial_match = true;
				});
			} while(partial_match);

			if (size)
				warning(__func__, " region not found ", Hex(g_phys), "+", Hex(size));
		}

		void detach(Genode::addr_t const guest_addr, Genode::addr_t const size)
		{
			_vm_con.detach(guest_addr, size);
		}

		Genode::addr_t alloc_io_memory(Genode::addr_t const size)
		{
			addr_t const io_mem = _io_mem_alloc;

			_io_mem_alloc += size;

			return io_mem;
		}
};
