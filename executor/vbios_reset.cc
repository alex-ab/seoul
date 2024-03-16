/** @file
 * Virtual Bios reset routines.
 *
 * Copyright (C) 2009-2010, Bernhard Kauer <bk@vmmon.org>
 * Economic rights: Technische Universitaet Dresden (Germany)
 *
 * Copyright (C) 2013 Jacek Galowicz, Intel Corporation.
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
#include "executor/bios.h"
#include "service/lock.h"

/* This file contains the AML code of the DSDT in form
 * of a string, which is available under the symbol name
 * "AmlCode" */
#include "dsdt.h"

bool use_x2apic_mode;
PARAM_HANDLER(x2apic_mode,
	      "x2apic_mode - enable x2apic mode in the LAPICs")
{use_x2apic_mode = true;}

/**
 * Virtual Bios reset routines.
 * Features: init of PIC, PIT, bda+ebda, ACPI tables
 * Missing: flexible ACPI table size
 */
class VirtualBiosReset : public StaticReceiver<VirtualBiosReset>, public BiosCommon
{
	enum {
		SIZE_EBDA_KB  = 8,
		/**
		 * EBDA Layout
		 * 0x0000 - 0x0200 compatible EBDA
		 * 0x0200 - 0x0220 rsdp
		 * 0x1000 - 0x2000 8x16 font
		 */
	};

	uintptr_t const _mem_ptr;
	size_t          _mem_size { };
	Seoul::Lock     _lock     { };

	struct Resource {
		char const * name;
		size_t offset;
		size_t length;
		bool   acpi_table;
		bool   ready { };

		Resource() : name(nullptr), offset(0), length(0), acpi_table(false) {}

		Resource(const char *n, size_t o, size_t l, bool table)
		: name(n), offset(o), length(l), acpi_table(table) { }
	} _resources[32] = { };

	/**
	 * called on reset.
	 */
	bool reset_helper(MessageBios &msg)
	{
		if (!msg.cpu || !msg.vcpu)
			Logging::panic("insane MessageBios");

		VCpu     &vcpu  = *msg.vcpu;
		CpuState &state = *msg.cpu;

		bool const bsp = !vcpu.get_last();

		// the APIC
		state.eax = 0xfee00800 | (bsp ? 0x100U : 0U);
		state.edx = 0;
		state.ecx = 0x1b;

		CpuMessage msg1(CpuMessage::TYPE_WRMSR, &state, MTD_GPR_ACDB);
		vcpu.executor.send(msg1, true);

		// enable SVR, LINT0, LINT1
		{
			unsigned m [] = { 0x1ff, bsp ? 0x700U : 0x10700U, 0x400};

			MessageMem msg2[] = {
				MessageMem(false, 0xfee000f0, m + 0),
				MessageMem(false, 0xfee00350, m + 1),
				MessageMem(false, 0xfee00360, m + 2),
			};

			for (auto &msg3 : msg2)
				vcpu.mem.send(msg3, true);
		}

		// switch to x2apic mode?
		if (use_x2apic_mode) {
			state.eax = 0xfee00c00 | (bsp ? 0x100 : 0);
			vcpu.executor.send(msg1, true);
		}


		if (!bsp) {
			CpuEvent msg3(VCpu::EVENT_INIT);
			vcpu.bus_event.send(msg3, true);
			return true;
		}

		for (auto &resource : _resources) {
			resource = Resource();
			resource.ready = false;
		}

		// we are a BSP and init the platform

		// initialize PIT0
		// let counter0 count with minimal freq of 18.2hz
		outb(0x40+3, 0x24);
		outb(0x40+0, 0);

		// let counter1 generate 15usec refresh cycles
		outb(0x40+3, 0x56);
		outb(0x40+1, 0x12);

		// the master PIC
		// ICW1-4+IMR
		outb(0x20+0, 0x11);
		outb(0x20+1, 0x08); // offset 0x08
		outb(0x20+1, 0x04); // has slave on 2
		outb(0x20+1, 0x0f); // is buffer, master, AEOI and x86
		outb(0x20+1, 0xfc); // TIMER+keyboard IRQ needed

		// the slave PIC + IMR
		outb(0xa0+0, 0x11);
		outb(0xa0+1, 0x70); // offset 0x70
		outb(0xa0+1, 0x02); // is slave on 2
		outb(0xa0+1, 0x0b); // is buffer, slave, AEOI and x86
		outb(0xa0+1, 0xff);

		// INIT resources
		MessageMemRegion msg3(0);
		check1(false, !_mb.bus_memregion.send(msg3) || !msg3.ptr || !msg3.count, "no low memory available");

		// were we start to allocate stuff
		_mem_size = msg3.count << 12;

		// we use the lower 640k of memory
		if (_mem_size > 0xa0000) _mem_size = 0xa0000;

		// trigger discovery
		MessageDiscovery msg4;
		_mb.bus_discovery.send_fifo(msg4);

		// the ACPI IRQ is 9
		discovery_write_dw("FACP",  46, 9, 2);

		/* Initialize DSDT table.
		 * Its content is defined as AML bytecode in dsdt.h */
		discovery_write_st("DSDT", 0, "DSDT", 4);

		/* Initialize FACS table.
		 * The table is left empty. Linux demands its existence
		 * before switching to ACPI mode. */
		discovery_write_st("FACS", 0, "FACS", 4);

		// store what remains on memory in KB
		discovery_write_dw("bda", 0x13, _mem_size >> 10, 2);
		return jmp_int(msg, 0x19);
	}


	size_t alloc(size_t size, size_t alignment)
	{
		if ((size + alignment + 0x1000) > _mem_size)
			return 0;

		_mem_size -= size;
		_mem_size &= ~alignment;

		// clear region
		memset(mem_ptr() + _mem_size, 0, size);
		return _mem_size;
	}

private:

	bool with_resource(char const * const name, auto const &fn)
	{
		Resource *unlocked = nullptr;
		bool      wait     = false;
		bool      new_res  = false;

		{
			Seoul::Lock::Guard guard(_lock);

			for (auto & resource : _resources) {

				if (!resource.name) {
					resource.name =  name;
					unlocked      = &resource;
					new_res       = true;

					VMM_MEMORY_BARRIER;

					break;
				}

				if (!strcmp(resource.name, name)) {
					wait      = !resource.ready;
					unlocked  = &resource;
					break;
				}
			}
		}

		if (new_res) {
			if (_create_resource(*unlocked, name)) {
				unlocked->ready = true;
			} else {
				unlocked->name = nullptr;
				unlocked       = nullptr;
			}

			VMM_MEMORY_BARRIER;
		}

		if (wait) {
			/* should actual never happen */
			Logging::printf("-- wait %s\n", name);
			while (!unlocked->ready) {
				VMM_MEMORY_BARRIER;
			}
			Logging::printf("-- wait %s done\n", name);
		}

		if (unlocked)
			return fn(*unlocked);
		else
			Logging::panic("resource not available %s", name);

		return false;
	}

	unsigned _acpi_tablesize(Resource const &r) {
		return *reinterpret_cast<unsigned *>(mem_ptr() + r.offset + 4); }

	void _fix_acpi_checksum(Resource &r, size_t length, size_t chksum_offset)
	{
		char value = 0;

		for (size_t i = 0; i < length && i < r.length; i++) {
			value += mem_ptr()[r.offset + i];
		}

		mem_ptr()[r.offset + chksum_offset] -= value;
	}

	void _discovery_write_dw(auto &resource, size_t offset, size_t value, unsigned count)
	{
		MessageDiscovery msg(resource.name, offset, &value, count);
		_write_resource(resource, msg);
	}

	void _discovery_write_st(auto &resource, size_t offset, const void *value, unsigned count)
	{
		MessageDiscovery msg(resource.name, offset, value, count);
		_write_resource(resource, msg);
	}

	#define ACPI_OEM_ID "seoul "

	void _init_acpi_table(auto &resource)
	{
		#define ACPI_MANUFACTURER " seoul  "

		_discovery_write_st(resource,  0, resource.name, 4);
		_discovery_write_dw(resource,  8, 1, 1);
		_discovery_write_st(resource, 10, ACPI_OEM_ID, 6);
		_discovery_write_st(resource, 16, ACPI_MANUFACTURER, 8);
		_discovery_write_dw(resource, 24, 1, 4);
		_discovery_write_dw(resource, 28, 0, 4);
		_discovery_write_dw(resource, 32, 0, 4);
	}

	bool _create_resource(Resource &resource, const char *name)
	{
		if (!strcmp("realmode idt", name)) {
			resource = Resource(name, 0, 0x400, false);
			memset(mem_ptr() + resource.offset, 0, resource.length);
		} else if (!strcmp("bda", name)) {
			resource = Resource(name, 0x400, 0x200, false);
			memset(mem_ptr() + resource.offset, 0, resource.length);
		} else if (!strcmp("ebda", name)) {
			size_t ebda;
			check1(false, !(ebda = alloc(SIZE_EBDA_KB << 10, 0x10)));
			resource = Resource(name, ebda, SIZE_EBDA_KB << 10, false);
			discovery_write_dw("bda", 0xe, ebda >> 4, 2);
			_discovery_write_dw(resource, 0, SIZE_EBDA_KB, 1);
		} else if (!strcmp("reset", name))
			resource = Resource(name, BIOS_BASE + 0xfff0, 0x10, false);
		else if (!strcmp("bios", name))
			resource = Resource(name, BIOS_BASE, 0x1000, false);
		else if (!strcmp("RSDP", name)) {
			with_resource("ebda", [&](auto &r_ebda) {
				resource = Resource(name, r_ebda.offset + 0x200, 36, false);
				_discovery_write_st(resource, 0,  "RSD PTR ", 8);
				_discovery_write_st(resource, 9,  ACPI_OEM_ID, 6);
				// revision = 0 => ACPI version 1.0
				_discovery_write_dw(resource, 15, 0, 1);
				_fix_acpi_checksum(resource, 20, 8);

				return true;
			});
		} else if (!strcmp("DSDT", name)) {
			size_t table;
			check1(false, !(table = alloc(sizeof(AmlCode), 0x10)),
			       "allocate ACPI table failed");
			resource = Resource(name, table, sizeof(AmlCode), true);

			// FADT contains a pointer to the DSDT
			discovery_write_dw("FACP", 40, table, 4);

			/* The DSDT is completely defined as AML bytecode in dsdt.h
			 * which was compiled from ASL by the Intel ASL compiler */
			memcpy(mem_ptr() + table, AmlCode, sizeof(AmlCode));
		} else if (!strcmp("FACS", name)) {
			size_t table;
			check1(false, !(table = alloc(36, 64)), "allocate ACPI table failed");
			resource = Resource(name, table, 36, true);
			_init_acpi_table(resource);

			// FADT contains a pointer to the FACS
			discovery_write_dw("FACP", 36, table, 4);
		} else {
			// we create an ACPI table
			size_t table;
			check1(false, !(table = alloc(0x1000, 0x10)), "allocate ACPI table failed");
			resource = Resource(name, table, 0x1000, true);

			_init_acpi_table(resource);

			if (!strcmp(name, "RSDT")) {
				discovery_write_dw("RSDP", 16, table, 4);

				with_resource("RSDP", [&](auto &r_rsdp) {
					_fix_acpi_checksum(r_rsdp, 20, 8);
					return true;
				});
			} else {
				// add them to the RSDT
				with_resource("RSDT", [&](auto &r_rsdt) {
					unsigned rsdt_length = _acpi_tablesize(r_rsdt);

					// and write the pointer to the RSDT
					_discovery_write_dw(r_rsdt, rsdt_length, table, 4);

					return true;
				});
			}
		}

		return true;
	}

	char * mem_ptr() const { return reinterpret_cast<char *>(_mem_ptr); }

	uintptr_t _init_mem_ptr()
	{
		MessageMemRegion msg3(0);
		if (!_mb.bus_memregion.send(msg3) || !msg3.ptr)
			Logging::panic("no low memory available");

		// were we start to allocate stuff
		return reinterpret_cast<uintptr_t>(msg3.ptr);
	}

public:

	bool receive(MessageLegacy &msg)
	{
		if (msg.type != MessageLegacy::RESET)
			return false;

		/* detach read only mapping */
		MessageHostOp op(MessageHostOp::OP_DETACH_MEM, BIOS_BASE, BIOS_SIZE);
		_mb.bus_hostop.send(op);

		// jump to the reset helper
		discovery_write_dw("reset", 0, 0xea, 1);
		discovery_write_dw("reset", 1, BIOS_RESET_VECTOR, 2);
		discovery_write_dw("reset", 3, BIOS_BASE >> 4, 2);

		// the BIOS release date
		discovery_write_st("reset", 5, "01/01/08", 8);

		return false;
	}

	bool receive(MessageBios &msg)
	{
		switch(msg.irq) {
		case BIOS_RESET_VECTOR:
			return reset_helper(msg);
		case 0x18:
			Logging::printf("INT18 - new try\n");
			return jmp_int(msg, 0x19);
		default:
			return false;
		}
	}


	bool _write_resource(Resource &r, MessageDiscovery &msg)
	{
		size_t const needed_len = msg.offset + msg.count;
		check1(false, needed_len > r.length,
		       "WRITE no idea how to increase the table %s size from %zu to %zu",
		       msg.resource, r.length, needed_len);

		size_t table_len = _acpi_tablesize(r);
		// increase the length of an ACPI table.

		if (r.acpi_table && msg.offset >= 8 && needed_len > table_len) {
			_discovery_write_dw(r, 4, needed_len, 4);
			table_len = needed_len;
		}

		memcpy(mem_ptr() + r.offset + msg.offset, msg.data, msg.count);

		// and fix the checksum
		if (r.acpi_table)
			_fix_acpi_checksum(r, table_len, 9);

		return true;
	}

	/**
	 * React on device discovery.
	 */
	bool receive(MessageDiscovery &msg)
	{
		if (msg.type != MessageDiscovery::WRITE &&
		    msg.type != MessageDiscovery::READ)
			return false;

		if (msg.type == MessageDiscovery::WRITE) {

			size_t const needed_len = msg.offset + msg.count;

			with_resource(msg.resource, [&](auto &r) {
				return _write_resource(r, msg);
			});
		}

		if (msg.type == MessageDiscovery::READ) {

			Resource *r = nullptr;

			size_t const needed_len = msg.offset + 4;

			return with_resource(msg.resource, [&](auto &r) {
				check1(false, needed_len > r.length,
				       "READ no idea how to increase the table %s size from %zu to %zu",
				       msg.resource, r.length, needed_len);
				memcpy(msg.dw, mem_ptr() + r.offset + msg.offset, 4);

				return true;
			});
		}

		return false;
	}

	bool receive(MessageMemRegion &msg)
	{
		if ((msg.page < (BIOS_BASE >> 12)) ||
		    (msg.page >= ((BIOS_BASE + BIOS_SIZE) >> 12)))
			return false;

		if (!(msg.cr0 & 1u << 31))
			return false;

		/* map BIOS memory read only */
		msg.start_page = BIOS_BASE >> 12;
		msg.count      = BIOS_SIZE >> 12;
		msg.ptr        = mem_ptr() + BIOS_BASE;
		msg.read_only  = true;

		return true;
	}

	bool receive(MessageMem &msg)
	{
		if (!msg.read || !in_range(msg.phys, BIOS_BASE, BIOS_SIZE))
			return false;

		/* give read only access to bios area */
		Cpu::move<2>(msg.ptr, mem_ptr() + msg.phys);
		return true;
	}

	VirtualBiosReset(Motherboard &mb)
	: BiosCommon(mb), _mem_ptr(_init_mem_ptr()) {}
};

PARAM_HANDLER(vbios_reset,
	      "vbios_reset - provide reset handling for virtual BIOS functions.")
{
	VirtualBiosReset * dev = new VirtualBiosReset(mb);

	mb.bus_bios.add(dev,      VirtualBiosReset::receive_static<MessageBios>);
	mb.bus_legacy.add(dev,    VirtualBiosReset::receive_static<MessageLegacy>);
	mb.bus_discovery.add(dev, VirtualBiosReset::receive_static<MessageDiscovery>);
	mb.bus_mem.add(dev,       VirtualBiosReset::receive_static<MessageMem>);
	mb.bus_memregion.add(dev, VirtualBiosReset::receive_static<MessageMemRegion>);
}
