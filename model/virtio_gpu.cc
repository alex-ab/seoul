/**
 * Virtio GPU device, without 3D or VGA support
 *
 * Copyright (C) 2021-2024, Alexander Boettcher
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

#include "nul/motherboard.h"
#include "executor/bios.h"
#include "model/pci.h"
#include "service/lock.h"

#include "virtio_pci.h"

enum {
	GPU_MAX_SCANOUTS = 1,
	VIRTIO_GPU_EVENT = 1,
};

struct Virtio_gpu_config
{
	unsigned event { };

	unsigned read(unsigned const off) const
	{
		switch (off) {
		case 0x0:
			return event;
		case 0x8: /* scan out count, 1 - 16 */
			return 1;
		case 0xc: /* reserved according to 1.1 specification */
			return 0;
		default:
			Logging::printf("unknown gpu config read %x\n",
			                off);
			return 0;
		}
	}

	void write(unsigned const off, unsigned const value)
	{
		switch (off) {
		case 0x4:
			event &= ~value;
			break;
		default:
			Logging::printf("unknown gpu config write %x value=%x\n",
			                off, value);
			break;
		}
	}
};


/*********************************
 * structures of virtio gpu spec *
 *********************************/

enum { VIRTIO_GPU_FLAG_FENCE = 1 << 0 };

struct gpu_ctrl_header {
	uint32 type;
	uint32 flags;
	uint64 fence_id;
	uint32 ctx_id;
	uint32 padding;
} __attribute__((packed));

struct gpu_rect {
	uint32 x;
	uint32 y;
	uint32 width;
	uint32 height;
} __attribute__((packed));

struct gpu_resp_display_info {
	gpu_ctrl_header header;
	struct gpu_display {
		gpu_rect r;
		uint32 enabled;
		uint32 flags;
	} pmodes[GPU_MAX_SCANOUTS];
} __attribute__((packed));

enum virtio_gpu_formats {
	VIRTIO_GPU_FORMAT_B8G8R8X8_UNORM = 2
};

struct gpu_resource_create_2d {
	gpu_ctrl_header header;
	uint32          resource_id;
	uint32          format;
	uint32          width;
	uint32          height;
} __attribute__((packed));

struct gpu_resource_unref {
	gpu_ctrl_header header;
	uint32          resource_id;
	uint32          padding;
} __attribute__((packed));

struct gpu_mem_entry {
	uint64 addr;
	uint32 length;
	uint32 padding;
} __attribute__((packed));

struct gpu_resource_attach_backing {
	gpu_ctrl_header header;
	uint32          resource_id;
	uint32          nr_entries;
} __attribute__((packed));

struct gpu_resource_detach_backing {
	gpu_ctrl_header header;
	uint32          resource_id;
	uint32          padding;
} __attribute__((packed));

struct gpu_set_scanout {
	gpu_ctrl_header header;
	gpu_rect        r;
	uint32          scanout_id;
	uint32          resource_id;
} __attribute__((packed));

struct gpu_transfer_to_host_2d {
	gpu_ctrl_header header;
	gpu_rect        r;
	uint64          offset;
	uint32          resource_id;
	uint32          padding;
} __attribute__((packed));

struct gpu_resource_flush {
	gpu_ctrl_header header;
	gpu_rect        r;
	uint32          resource_id;
	uint32          padding;
} __attribute__((packed));

struct gpu_cursor_pos {
	uint32 scanout_id;
	uint32 x;
	uint32 y;
	uint32 padding;
} __attribute__((packed));

struct gpu_update_cursor {
	gpu_ctrl_header header;
	gpu_cursor_pos  pos;
	uint32          resource_id;
	uint32          hot_x;
	uint32          hot_y;
	uint32          padding;
} __attribute__((packed));


class Virtio_gpu: public StaticReceiver<Virtio_gpu>, Virtio::Device
{
	private:

		DBus<MessageConsole> &_bus_console;
		Seoul::Lock           _lock { };
		Virtio_gpu_config     _config          { };
		bool                  _memory_pressure { };
		bool const            _verbose;

		/*
		 * heuristic to hide window when exiting to vga/vesa console
		 * when used with linux cmdline: vga=0x318
		 */
		enum {
			HEURISTIC_UNDEFINED,
			HEURISTIC_DISABLE_SCANOUT,
			HEURISTIC_ENABLE_SCANOUT,
		} _heuristic_hide { HEURISTIC_UNDEFINED };

		~Virtio_gpu();

		struct resource_2d {
			void *        memory_vmm;
			gpu_mem_entry guest_fb_memory[1280];
			unsigned      valid_memory_entries;
			uint32        format;
			uint16        valid;
			uint32        resource_id;
			uint32        width;
			uint32        height;
			gpu_rect      scanout;
			uint32        scanout_id;
			bool          scanout_valid;
		} resources_2d [64] { };

		template <typename FUNC>
		bool create_resource_2d(FUNC const &fn)
		{
			for (auto &resource : resources_2d) {
				if (resource.valid)
					continue;
				resource.valid = true;
				fn(resource);
				return true;
			}
			return false;
		}

		void free_resource(struct resource_2d &res)
		{
			auto const size = res.width * res.height * 4; /* XXX */

			if (res.memory_vmm) {
				MessageConsole msg(MessageConsole::TYPE_FREE_VIEW, console_id);
				msg.view = uint16(2 + res.resource_id); /* XXX */
				_bus_console.send(msg);

				res.memory_vmm = nullptr;
			}

			res.valid = false;
			res.valid_memory_entries = 0;

			if (!_memory_pressure)
				return;

			bool all_succeeded = true;

			/* retry allocation since guest freed up some memory */
			for (auto &resource : resources_2d) {
				if (!resource.valid)
					continue;

				if (resource.memory_vmm)
					continue;

				auto const min_size = resource.width * resource.height * 4; /* XXX */

				MessageConsole msg(MessageConsole::TYPE_ALLOC_VIEW, console_id,
				                   min_size);
				msg.view = uint16(2 + resource.resource_id); /* XXX */
				if (!_bus_console.send(msg)) {
					all_succeeded = false;
				} else {
					Logging::printf("%s: allocation retry succeeded %ux%u %u\n", __func__,
					                resource.width, resource.height, resource.resource_id);

					resource.memory_vmm = msg.ptr;
				}
			}

			if (all_succeeded) {
				_memory_pressure = false;
			}
		}

		void reset_resources()
		{
			for (auto &resource : resources_2d) {
				if (resource.valid)
					free_resource(resource);
			}
		}

		template <typename FUNC>
		bool apply_to_resource_2d(uint32 const resource_id, FUNC const &fn)
		{
			for (auto &resource : resources_2d) {
				if (!resource.valid || resource.resource_id != resource_id)
					continue;

				fn(resource);
				return true;
			}
			return false;
		}

		template <typename FUNC>
		void copy_pixels(auto const &resource, unsigned const queue, FUNC const &fn)
		{
			unsigned const guest_stride = resource.width * 4;
			unsigned const vmm_stride   = guest_stride;

			if (!resource.memory_vmm)
				return;

			uint64 resource_offset = 0;
			uint64 x, y = 0;
			for (unsigned i = 0; i < resource.valid_memory_entries; i++) {
				auto vmm_ptr    = uintptr_t(resource.memory_vmm) + (resource_offset / guest_stride) * vmm_stride;
				auto from_guest = vmm_address(resource.guest_fb_memory[i].addr,
				                              resource.guest_fb_memory[i].length);

				if (!from_guest)
					return;

				unsigned       length    = resource.guest_fb_memory[i].length;
				unsigned const unaligned = unsigned(resource_offset % guest_stride);
				unsigned const row_rest  = VMM_MIN(length, guest_stride - unaligned);

				/* since whole row not available to fn, copy w/o fn invocation */
				if (unaligned) {
					memcpy(reinterpret_cast<void *>(vmm_ptr + unaligned),
					       reinterpret_cast<void *>(from_guest),
					       row_rest);
					vmm_ptr    += vmm_stride;
					from_guest += row_rest;
					length     -= row_rest;
				}

				unsigned const max_row = length / guest_stride;
				unsigned const guest_y = unsigned((resource_offset + row_rest) / guest_stride);
				for (unsigned row = 0; row < max_row; row ++) {
#if 0
					memcpy(reinterpret_cast<void *>(vmm_ptr   + row * vmm_stride),
					       reinterpret_cast<void *>(from_guest + row * guest_stride),
					       guest_stride);
#else
					bool loop_continue = fn(uintptr_t(vmm_ptr + row * vmm_stride),
					                        from_guest + row * guest_stride,
					                        guest_y    + row);
					if (!loop_continue)
						return;
#endif
				}

				unsigned const rest = length % guest_stride;
				/* since whole row not available to fn, copy w/o fn invocation */
				if (rest) {
					memcpy(reinterpret_cast<void *>(vmm_ptr + max_row * vmm_stride),
					       reinterpret_cast<void *>(from_guest + max_row * guest_stride),
					       rest);
				}

				/* not use length variable */
				resource_offset += resource.guest_fb_memory[i].length;
			}
		}

		size_t _gpu_display_info(uintptr_t, size_t, uintptr_t, size_t);
		size_t _gpu_create_2d(uintptr_t, size_t, uintptr_t, size_t);
		size_t _gpu_res_unref(uintptr_t, size_t, uintptr_t, size_t);
		size_t _gpu_res_flush(uintptr_t, size_t, uintptr_t, size_t);
		size_t _gpu_set_scanout(uintptr_t, size_t, uintptr_t, size_t);
		size_t _gpu_move_to_host(uintptr_t, size_t, uintptr_t, size_t, unsigned);
		size_t _gpu_attach_backing(uintptr_t, size_t, uintptr_t, size_t, uintptr_t,
		                           size_t, unsigned);
		size_t _gpu_detach_backing(uintptr_t, size_t, uintptr_t, size_t);
		size_t _gpu_cursor(uintptr_t, size_t, uintptr_t, size_t, bool, unsigned);

	public:

		uint16 const console_id { 1 };
		struct gpu_rect mode_host { };
		struct gpu_rect mode_next { };
		unsigned shape_size { };
		uint64   host_fb    { };
		uint64   shape_ptr  { };

		Virtio_gpu(DBus<MessageIrqLines>  &bus_irqlines,
		           DBus<MessageMemRegion> &bus_memregion,
		           DBus<MessageConsole>   &bus_console,
		           uint64 const bar_addr,
		           uint8  const irq_pin,
		           uint8  const irq_line,
		           uint16 const bdf,
		           bool   const verbose)
		:
			Virtio::Device(bus_irqlines, bus_memregion, irq_pin,
			               irq_line, bdf,
			               16            /* virtio type */,
			               0x03800001,   /* display + GPU + rev 1 */
			               bar_addr,
			               2             /* queues */),
			_bus_console(bus_console),
			_verbose(verbose)
		{ }

		bool receive(MessagePciConfig &msg)
		{
			if (msg.bdf != _bdf)
				return false;

			return sync_and_irq(_lock, [&]() {
				return Virtio::Device::receive(msg); });
		}

		bool receive(MessageBios &msg)
		{
			switch(msg.irq) {
			case BiosCommon::BIOS_RESET_VECTOR:

				Seoul::Lock::Guard guard(_lock);

				_config = { };
				reset_resources();
				reset();
			};

			return false;
		}

		bool receive(MessageMem &msg)
		{
			if (msg.phys < _phys_bar_base || _phys_bar_base + PHYS_BAR_SIZE <= msg.phys)
				return false;

			return sync_and_irq(_lock, [&]() {

				auto const offset = unsigned(msg.phys - _phys_bar_base);

				switch (offset) {
				case BAR_OFFSET_CONFIG ... BAR_OFFSET_CONFIG + RANGE_SIZE - 1:
					if (msg.read)
						*msg.ptr = _config.read(offset - BAR_OFFSET_CONFIG);
					else
						_config.write(offset - BAR_OFFSET_CONFIG, *msg.ptr);

					return true;
				default:
					return Virtio::Device::receive(msg);
				}
			});
		}

		void update_mode_host(unsigned const width, unsigned const height)
		{
			MessageConsole msg2(MessageConsole::TYPE_ALLOC_CLIENT, console_id);
			_bus_console.send(msg2);

			host_fb = uintptr_t(msg2.ptr);

			mode_host.width  = width;
			mode_host.height = height;
		}

		bool receive(MessageConsole const &msg)
		{
			if (msg.type != MessageConsole::TYPE_MODEINFO_UPDATE ||
			    msg.id != console_id)
				return false;

			/* too small resolutions cause interesting effects - debug hell */
			if (msg.width < 320 || msg.height < 200)
				return true;

			return sync_and_irq(_lock, [&]() {

				mode_next.width  = msg.width;
				mode_next.height = msg.height;

				/* avoid causing high allocation frequency for small pixel change */
				bool handle = (int(mode_host.width)  - int(mode_next.width)  >=  16) ||
				              (int(mode_host.width)  - int(mode_next.width)  <= -16) ||
				              (int(mode_host.height) - int(mode_next.height) >=  16) ||
				              (int(mode_host.height) - int(mode_next.height) <= -16);

				if (!handle)
					return true;

				update_mode_host(mode_next.width, mode_next.height);

				if (_verbose)
					Logging::printf("virtio gpu %ux%u triggered by host\n",
					                mode_host.width, mode_host.height);

				/* notify guest */
				_config.event = VIRTIO_GPU_EVENT;

				config_changed();
				inject_irq();

				return true;
			});
		}

		enum {
			VIRTIO_GPU_CMD_GET_DISPLAY_INFO        = 0x100,
			VIRTIO_GPU_CMD_RESOURCE_CREATE_2D      = 0x101,
			VIRTIO_GPU_CMD_RESOURCE_UNREF          = 0x102,
			VIRTIO_GPU_CMD_SET_SCANOUT             = 0x103,
			VIRTIO_GPU_CMD_RESOURCE_FLUSH          = 0x104,
			VIRTIO_GPU_CMD_TRANSFER_TO_HOST_2D     = 0x105,
			VIRTIO_GPU_CMD_RESOURCE_ATTACH_BACKING = 0x106,
			VIRTIO_GPU_CMD_RESOURCE_DETACH_BACKING = 0x107,

			VIRTIO_GPU_CMD_UPDATE_CURSOR    = 0x0300,
			VIRTIO_GPU_CMD_MOVE_CURSOR      = 0x0301,

			VIRTIO_GPU_RESP_OK_NODATA       = 0x1100,
			VIRTIO_GPU_RESP_OK_DISPLAY_INFO = 0x1101,
		};

		template <typename TYPE, typename FUNC>
		size_t check(uintptr_t const request,  size_t const request_size,
		             uintptr_t const response, size_t const response_size,
		             char const * const identifier, FUNC const &fn)
		{
			TYPE volatile * const guest_data = reinterpret_cast<TYPE *>(request);
			if (request_size < sizeof(*guest_data)) {
				Logging::printf("%s: invalid request\n", identifier);
				return 0UL;
			}
			if (!response || response_size < sizeof(gpu_ctrl_header)) {
				Logging::printf("%s: response invalid\n", identifier);
				return 0UL;
			}

			TYPE const host_data = *const_cast<TYPE *>(guest_data);

			/*
			 * Make sure compiler wrote all data to local variable,
			 * before doing sanity & security checks.
			 */
			VMM_MEMORY_BARRIER;

			if (!fn(host_data)) {
				Logging::printf("failed check\n");
				return 0UL;
			}

			auto &answer = *reinterpret_cast<gpu_ctrl_header *>(response);
			memset(&answer, 0, sizeof(answer));
			if (host_data.header.flags & VIRTIO_GPU_FLAG_FENCE) {
				answer.fence_id = host_data.header.fence_id;
				answer.flags    = VIRTIO_GPU_FLAG_FENCE;
			}
			answer.type = VIRTIO_GPU_RESP_OK_NODATA;

//			Logging::printf("ack request_size=%lu\n", request_size);
			return request_size;
		}

		void notify (unsigned queue) override
		{
			if (queue >= QUEUES_MAX) {
				Logging::printf("unknown queue number %u\n", queue);
				return;
			}

			/* queue == 0, controlq --- queue == 1, cursorq */

			auto &used_queue = _queues[queue].queue;

			bool inject = used_queue.consume([&] (auto const descriptor, auto) {

				auto const request = vmm_address(descriptor.addr, descriptor.len);
				auto const request_size = descriptor.len;

#if 0
				Logging::printf(".... %lx+%x\n",
				                request, request_size);
#endif

				if (!request || request_size < 24) {
					Logging::printf("gpu, invalid request\n");
					return 0UL;
				}

				gpu_ctrl_header * const header = reinterpret_cast<gpu_ctrl_header *>(request);

				/* whitelist the cases where we are sure that the adhere to the required semantic */
				if ((header->flags & VIRTIO_GPU_FLAG_FENCE) &&
				    (header->type != VIRTIO_GPU_CMD_TRANSFER_TO_HOST_2D)) {
					Logging::printf("fence id set - unsupported - type=%x fence_id=%llx\n", header->type, header->fence_id);
					return 0UL;
				}

				if (_verbose)
					Logging::printf("gpu, type %x flags=%x fence=%llx \n",
					                header->type, header->flags, header->fence_id);

				uintptr_t response = 0; size_t response_size = 0;
				uintptr_t desc2 = 0; size_t desc2_size = 0;

				auto desc1 = used_queue.next_desc(descriptor);
				if (desc1.len) {
					response      = vmm_address(desc1.addr, desc1.len);
					response_size = desc1.len;
				}

				auto descx  = used_queue.next_desc(desc1);
				if (descx.len) {
					desc2      = vmm_address(descx.addr, descx.len);
					desc2_size = descx.len;
				}

				if ((desc2 || desc2_size) && header->type != VIRTIO_GPU_CMD_RESOURCE_ATTACH_BACKING)
					Logging::printf("unexpected type using more descriptor %x\n",
					                header->type);

				switch (header->type) {
				case VIRTIO_GPU_CMD_GET_DISPLAY_INFO:
					return _gpu_display_info(request, request_size,
					                         response, response_size);
				case VIRTIO_GPU_CMD_RESOURCE_CREATE_2D:
					return _gpu_create_2d(request, request_size,
					                      response, response_size);

				case VIRTIO_GPU_CMD_RESOURCE_UNREF:
					return _gpu_res_unref(request, request_size,
					                      response, response_size);
				case VIRTIO_GPU_CMD_UPDATE_CURSOR:
				case VIRTIO_GPU_CMD_MOVE_CURSOR:
					return _gpu_cursor(request, request_size,
					                   response, response_size,
					                   header->type == VIRTIO_GPU_CMD_UPDATE_CURSOR,
					                   queue);
				case VIRTIO_GPU_CMD_RESOURCE_ATTACH_BACKING:
					return _gpu_attach_backing(request, request_size, response,
					                           response_size, desc2,
					                           desc2_size, queue);
				case VIRTIO_GPU_CMD_RESOURCE_DETACH_BACKING:
					return _gpu_detach_backing(request, request_size,
					                           response, response_size);
				case VIRTIO_GPU_CMD_SET_SCANOUT:
					return _gpu_set_scanout(request, request_size,
					                        response, response_size);
				case VIRTIO_GPU_CMD_TRANSFER_TO_HOST_2D:
					return _gpu_move_to_host(request, request_size, response,
					                         response_size, queue);
				case VIRTIO_GPU_CMD_RESOURCE_FLUSH:
					return _gpu_res_flush(request, request_size,
					                      response, response_size);
				default:
					Logging::printf("gpu, unsupported type %x fence=%llx\n",
					                header->type, header->fence_id);
				}

				return 0UL;
			});

			if (inject)
				inject_irq();
		}

		uint32 dev_feature     (unsigned)         override { return 0u; }
		void   drv_feature_ack (unsigned, uint32) override { }
		uint32 drv_feature     (unsigned)         override { return 0u; }

		void notify_power(unsigned value) override
		{
			Logging::printf("virtio_gpu: power change %x\n", value);

			if ((value & 3) == 0) {
				/* reset */
/*
				_config = { };
				reset_resources();
				reset();
//				reset_queues();
*/
			}

			if (!host_fb || (value & 3) != 3)
				return;

			memset((void *)host_fb, 0, mode_host.width * mode_host.height * 4);

			MessageConsole msg(console_id, 0 /* view */, false /* hide */,
			                   { 0, 0 },
			                   { mode_host.width, mode_host.height });

			_bus_console.send(msg);
		}
};

size_t Virtio_gpu::_gpu_display_info(uintptr_t const in,  size_t const in_size,
                                     uintptr_t const out, size_t const out_size)
{
	auto const name = "gpu, display info";

	if (!out || out_size < sizeof(gpu_resp_display_info)) {
		Logging::printf("%s: response invalid\n", name);
		return 0UL;
	}

	auto &info = *reinterpret_cast<gpu_resp_display_info *>(out);
	info = { };

	info.header.type = VIRTIO_GPU_RESP_OK_DISPLAY_INFO;
	info.pmodes[0].r = { .x = 0, .y = 0,
	                     .width = mode_host.width, .height = mode_host.height };
	info.pmodes[0].enabled = true;

	return in_size;
}

size_t Virtio_gpu::_gpu_create_2d(uintptr_t request,  size_t request_size,
                                  uintptr_t response, size_t response_size)
{
	auto const name = "gpu, create 2d";

	return check<gpu_resource_create_2d>(request, request_size,
	                                     response, response_size,
	                                     name, [&](auto const &res) {

		if (res.format != VIRTIO_GPU_FORMAT_B8G8R8X8_UNORM) {
			Logging::printf("%s: unsupported format %u\n",
			                name, res.format);
			return false;
		}

		MessageConsole msg(MessageConsole::TYPE_ALLOC_VIEW, console_id,
		                   res.width * res.height * 4); /* XXX */
		msg.view = uint16(2 + res.resource_id); /* XXX */

		if (!_bus_console.send(msg)) {
			Logging::printf("%s: allocation failed %ux%u id=%u\n", name,
			                res.width, res.height, res.resource_id);
			msg.ptr = nullptr;
			_memory_pressure = true;
		}

		if (!create_resource_2d([&](auto &resource){
			resource.resource_id = res.resource_id;
			resource.format      = res.format;
			resource.width       = res.width;
			resource.height      = res.height;
			resource.memory_vmm  = msg.ptr;
		})) {
			Logging::printf("%s: too many 2d resources\n", name);
			return false;
		}

		if (_verbose)
			Logging::printf("%s: id=%u %ux%u format=%u (%ux%u %ux%u)\n",
			                name, res.resource_id, res.width,
			                res.height, res.format,
			                mode_host.width, mode_host.height,
			                mode_next.width, mode_next.height);

		return true;
	});
}

size_t Virtio_gpu::_gpu_res_unref(uintptr_t request,  size_t request_size,
                                  uintptr_t response, size_t response_size)
{
	auto const name = "gpu, unref";

	return check<gpu_resource_unref>(request, request_size,
	                                 response, response_size,
	                                 name,
	                                 [&](gpu_resource_unref const &unref) {

		bool found = apply_to_resource_2d(unref.resource_id, [&](auto &resource) {
			free_resource(resource);
		});

		if (!found)
			Logging::printf("%s: id=%u not found\n", name, unref.resource_id);

		return true;
	});
}

size_t Virtio_gpu::_gpu_res_flush(uintptr_t const in,  size_t const in_size,
                                  uintptr_t const out, size_t const out_size)
{
	auto const name = "gpu, flush";

	return check<gpu_resource_flush>(in, in_size, out, out_size, name,
	                                 [&](gpu_resource_flush const &flush) {

		bool success = true;

		bool found = apply_to_resource_2d(flush.resource_id, [&](auto &resource) {
			success = resource.scanout_valid;
			if (!success || !resource.memory_vmm)
				return;

#if 0
			if (flush.r.x      != resource.scanout.x ||
			    flush.r.y      != resource.scanout.y ||
			    flush.r.width  != resource.scanout.width ||
			    flush.r.height != resource.scanout.height)
			{
				Logging::printf("%s: id=%u scan_id=%u "
				                "%ux%u+%ux%u -> %ux%u+%ux%u\n",
				                name, flush.resource_id,
				                resource.scanout_id,
				                flush.r.x, flush.r.y,
				                flush.r.width, flush.r.height,
				                resource.scanout.x, resource.scanout.y,
				                resource.scanout.width, resource.scanout.height);
			}
#endif

			unsigned const vmm_stride   = mode_host.width * 4;
			unsigned const guest_stride = resource.width * 4;

			if ((flush.r.width  > resource.width)  ||
			    (flush.r.height > resource.height) ||
			    (flush.r.x > resource.width  - flush.r.width) ||
			    (flush.r.y > resource.height - flush.r.height) ||
			    (flush.r.x + flush.r.width > resource.width)) {
				success = false;
				Logging::printf("%s: out of bound flush - skip\n", name);
				return;
			}

			if ((flush.r.width  > resource.scanout.width)  ||
			    (flush.r.height > resource.scanout.height) ||
			    (flush.r.x > resource.scanout.width  - flush.r.width) ||
			    (flush.r.y > resource.scanout.height - flush.r.height) ||
			    (flush.r.x + flush.r.width > resource.scanout.width)) {
				success = false;
				Logging::printf("%s: out of bound flush - skip\n", name);
				return;
			}

			bool calc_sum = _heuristic_hide == HEURISTIC_ENABLE_SCANOUT &&
			                !flush.r.x && flush.r.width  == mode_host.width &&
			                !flush.r.y && flush.r.height == mode_host.height;
			uint64 sum = 0;

			for (unsigned y = flush.r.y; y < flush.r.y + flush.r.height; y ++) {

				if (y >= mode_host.height)
					break;

				if (flush.r.x >= mode_host.width)
					continue;

				unsigned width_cpy = flush.r.width;
				if (flush.r.x + width_cpy >= mode_host.width)
					width_cpy = mode_host.width - flush.r.x;

				memcpy(reinterpret_cast<void *>(host_fb + y * vmm_stride + flush.r.x * 4),
				       reinterpret_cast<void *>(uintptr_t(resource.memory_vmm) + y * guest_stride + flush.r.x * 4),
				       width_cpy * 4);

				if (calc_sum) {
					for (unsigned i = 0; i < width_cpy * 4; i += 4)
						sum += *reinterpret_cast<uint32 *>(host_fb + y * vmm_stride + flush.r.x * 4 + i);
				}
			}

			if (calc_sum && !sum)
				Logging::printf("virtio_gpu: hide window");

			if ((!calc_sum && _heuristic_hide != HEURISTIC_UNDEFINED) || sum)
				_heuristic_hide = HEURISTIC_UNDEFINED;

			auto const hide = calc_sum && !sum;
			auto const view = 0;

			MessageConsole msg(console_id, view, hide,
			                   { flush.r.x, flush.r.y },
			                   { flush.r.width, flush.r.height });

			_bus_console.send(msg);
		});

		if (!found || !success)
			Logging::printf("%s - not found resource_id %u %s\n", name,
			                flush.resource_id, success ? "" : " unexpected error occurred");

		return true;
	});
}

size_t Virtio_gpu::_gpu_set_scanout(uintptr_t request,  size_t request_size,
                                    uintptr_t response, size_t response_size)
{
	auto const name = "gpu, set scanout";

	return check<gpu_set_scanout>(request, request_size, response,
	                              response_size, name,
	                              [&](gpu_set_scanout const &scanout) {

		if (_verbose)
			Logging::printf("%s: id=%u %ux%x+%ux%u scanout=%u (%s)", name,
			                scanout.resource_id,
			                scanout.r.x, scanout.r.y,
			                scanout.r.width, scanout.r.height,
			                scanout.scanout_id,
			                scanout.resource_id == 0 ? "disable" : "enable");

		if (scanout.resource_id == 0)
			_heuristic_hide = HEURISTIC_DISABLE_SCANOUT;
		else
		if (scanout.resource_id) {
			if (_heuristic_hide == HEURISTIC_DISABLE_SCANOUT)
				_heuristic_hide = HEURISTIC_ENABLE_SCANOUT;
		}

		/* XXX resource_id == 0 -> disable - how to handle XXX */
		apply_to_resource_2d(scanout.resource_id, [&](auto &resource) {
			resource.scanout       = scanout.r;
			resource.scanout_id    = scanout.scanout_id;
			resource.scanout_valid = scanout.resource_id != 0;
		});

		if (scanout.resource_id && scanout.r.width && scanout.r.height)
		{
			MessageConsole msg(MessageConsole::TYPE_RESOLUTION_CHANGE, console_id);
			msg.view   = 0;
			msg.x      = msg.y = 0;
			msg.width  = scanout.r.width;
			msg.height = scanout.r.height;

			_bus_console.send(msg);
		}

		return true;
	});
}

size_t Virtio_gpu::_gpu_cursor(uintptr_t const in,  size_t const in_size,
                               uintptr_t const out, size_t const out_size,
                               bool update, unsigned const queue)
{
	auto const name = "gpu, cursor";;

	auto const &guest_ptr = *reinterpret_cast<gpu_update_cursor *>(in);
	if (in_size < sizeof(guest_ptr)) {
		Logging::printf("%s: invalid request\n", name);
		return 0UL;
	}
	/* spec says to return VIRTIO_GPU_RESP_OK_NODATA ... but have no out ... */
	if (out) {
		Logging::printf("%s: unexpected cursor out\n", name);
		return 0;
	}

	auto const cursor = guest_ptr;

	/*
	 * Make sure compiler wrote all data to local variable,
	 * before doing sanity & security checks.
	 */
	VMM_MEMORY_BARRIER;

	if (!shape_ptr || !shape_size) {
		Logging::printf("%s: no shape support\n", name);
		return in_size;
	}

	/*
	 * In case of just move, e.g. !update, the resource id is not valid
	 * according to spec. We don't support that guest may move the mouse,
	 * so just ack it.
	 */
	if (!update)
		return in_size;

	bool found = apply_to_resource_2d(cursor.resource_id, [&](auto const &resource) {

		if (!resource.valid_memory_entries) {
			Logging::printf("%s: no valid memory entries\n", name);
			return;
		}

		if (update) {
			uint64 resource_offset = 0;
			for (unsigned i = 0; i < resource.valid_memory_entries; i++) {
				auto const shape_guest = vmm_address(resource.guest_fb_memory[i].addr,
				                                     resource.guest_fb_memory[i].length);

				if (!shape_guest)
					return;

				if (resource.guest_fb_memory[i].length > shape_size ||
				    resource_offset > shape_size - resource.guest_fb_memory[i].length) {
					Logging::printf("%s: cursor too large\n", name);
					return;
				}

				memcpy(reinterpret_cast<void *>(shape_ptr + resource_offset),
				       reinterpret_cast<void *>(shape_guest),
				       resource.guest_fb_memory[i].length);

				resource_offset += resource.guest_fb_memory[i].length;
			}
		}


		MessageConsole msg(console_id, 1 /* view */, false /* hide */,
		                   { VMM_MIN(cursor.pos.x, mode_host.width ),
		                     VMM_MIN(cursor.pos.y, mode_host.height) },
		                   { resource.width, resource.height },
		                   { cursor.hot_x, cursor.hot_y });

		_bus_console.send(msg);
	});

	if (update && !found && cursor.resource_id)
		Logging::printf("%s: resource id not found resource_id=%u\n", name, cursor.resource_id);

	return in_size;
}

size_t Virtio_gpu::_gpu_attach_backing(uintptr_t const in,   size_t const in_size,
                                       uintptr_t const data, size_t const data_size,
                                       uintptr_t const out,  size_t out_size,
                                       unsigned const queue)
{
	auto const name = "gpu, attach";

	return check<gpu_resource_attach_backing>(in, in_size, out, out_size, name,
	                                          [&](gpu_resource_attach_backing const &attach) {

		unsigned const nr_entries = attach.nr_entries;

		if (_verbose)
			Logging::printf("%s: id=%u memory entries=%u\n", name,
			                attach.resource_id, nr_entries);

		/* spec is ambiguous a bit - gpu_mem_entry are in data */
		auto * mem_desc = reinterpret_cast<gpu_mem_entry *>(data);
		if (!data || data_size < sizeof(*mem_desc) * nr_entries) {
			Logging::printf("%s: invalid memory entries\n", name);
			return false;
		}

		bool fail  = true;
		bool found = apply_to_resource_2d(attach.resource_id, [&](auto &resource) {

			unsigned const max_entries = sizeof(resource.guest_fb_memory) /
			                             sizeof(resource.guest_fb_memory[0]);
			if (nr_entries > max_entries) {
				Logging::printf("%s: too many entries %u\n", name, nr_entries);
				return;
			}
			memcpy(resource.guest_fb_memory, mem_desc,
			       sizeof(resource.guest_fb_memory[0]) * nr_entries);
			memset(resource.guest_fb_memory + nr_entries, 0,
			       sizeof(resource.guest_fb_memory[0]) * (max_entries - nr_entries));

			VMM_MEMORY_BARRIER;

			{
				uint64 resource_offset = 0;
				for (unsigned i = 0; i < nr_entries; i++) {
#if 0
					Logging::printf("gpu, attach: phys=%llx+%x offset_range?=[%llx-%llx)",
					                resource.guest_fb_memory[i].addr, resource.guest_fb_memory[i].length,
					                resource_offset,
					                resource_offset + resource.guest_fb_memory[i].length);

					Logging::printf("gpu, attach: vmm=%lx+%x offset_range?=[%llx-%llx)",
					                vmm_address(resource.guest_fb_memory[i].addr, resource.guest_fb_memory[i].length),
					                resource.guest_fb_memory[i].length,
					                resource_offset,
					                resource_offset + resource.guest_fb_memory[i].length);
#endif
					/* check for valid guest memory */
					if (!vmm_address(resource.guest_fb_memory[i].addr,
					                 resource.guest_fb_memory[i].length)) {
						fail = true;
						resource.valid_memory_entries = 0;
						Logging::printf("%s: invalid guest memory descriptors - ignore\n", name);
						return;
					}
					resource_offset += resource.guest_fb_memory[i].length;
				}

				if (_verbose)
					Logging::printf("%s: %u memory descriptors, memory size %llx\n",
					                name, nr_entries, resource_offset);
				resource.valid_memory_entries = nr_entries;
				fail = false;
			}
			return;
		});

		if (!found || fail)
			Logging::printf("%s: failure during guest descriptor parsing\n", name);

		return true;
	});
}

size_t Virtio_gpu::_gpu_detach_backing(uintptr_t const request,
                                       size_t    const request_size,
                                       uintptr_t const response,
                                       size_t    const response_size)
{
	auto const name = "gpu, detach";

	return check<gpu_resource_detach_backing>(request, request_size,
	                                          response, response_size,
	                                          name,
	                                          [&](gpu_resource_detach_backing const &detach)
	{
		Logging::printf("%s: id=%u\n", name, detach.resource_id);

		bool found = apply_to_resource_2d(detach.resource_id, [&](auto &resource) {
			if (!resource.valid_memory_entries)
				return;

			unsigned const max_entries = sizeof(resource.guest_fb_memory) /
			                             sizeof(resource.guest_fb_memory[0]);

			memset(resource.guest_fb_memory, 0,
			       sizeof(resource.guest_fb_memory[0]) *
			       (VMM_MIN(resource.valid_memory_entries, max_entries)));
			resource.valid_memory_entries = 0;
		});

		if (!found)
			Logging::printf("%s: resource id not found\n", name);

		return true;
	});
}

size_t Virtio_gpu::_gpu_move_to_host(uintptr_t const in,  size_t const in_size,
                                     uintptr_t const out, size_t const out_size,
                                     unsigned const queue)
{
	auto const name = "gpu, transfer to host";

	return check<gpu_transfer_to_host_2d>(in, in_size, out, out_size, name,
	                                      [&](gpu_transfer_to_host_2d const &transfer) {

		if (!apply_to_resource_2d(transfer.resource_id, [&](auto const &resource) {

			if (false)
				Logging::printf("%s: id=%u %ux%u+%ux%u offset=%llx "
				                "-> %ux%u memory_vmm=%p",
				                name, transfer.resource_id,
				                transfer.r.x, transfer.r.y,
				                transfer.r.width, transfer.r.height,
				                transfer.offset,
				                resource.width, resource.height,
				                resource.memory_vmm);

			/*
			 * offset is transfer.r.y * resource.width + transfer.r.x
			 * since we have to translate the dma mappings anyway, it is of
			 * no use. (if we cache the translation then it may help)
			 */
			if ((transfer.r.width  >  resource.width)  ||
			    (transfer.r.height >  resource.height) ||
			    (transfer.r.x >  resource.width  - transfer.r.width) ||
			    (transfer.r.y >  resource.height - transfer.r.height) ||
			    (transfer.r.x + transfer.r.width > resource.width)) {
					Logging::printf("%s: out of bound transfer - skip\n", name);
					return;
			}

			copy_pixels(resource, queue, [&](uintptr_t host, uintptr_t guest, unsigned const y) {
				if (y < transfer.r.y)
					return true;  /* continue loop */
				if (transfer.r.y + transfer.r.height <= y)
					return false; /* stop loop */

				memcpy(reinterpret_cast<void *>(host  + transfer.r.x * 4),
				       reinterpret_cast<void *>(guest + transfer.r.x * 4),
				       transfer.r.width * 4);

				return true;  /* continue loop */
			});
		})) {
			if (_verbose)
				Logging::printf("%s: unknown 2d resource\n", name);
			return true;
		}
		return true;
	});
}

PARAM_HANDLER(virtio_gpu,
	      "virtio_gpu:mem,bdf - attach an virtio gpu device to the PCI bus",
	      "Example: 'virtio_gpu:0xe0400000,0x30' to attach on 00:06.0 on address",
	      "If no bdf is given a free one is used.")
{
	bool const verbose = false;

	unsigned const bdf = PciHelper::find_free_bdf(mb.bus_pcicfg, unsigned(argv[1]));
	if (bdf >= 1u << 16)
		Logging::panic("virtio_gpu: invalid bdf\n");

	auto const irq_pin  =  2; /* PCI INTB# */
	auto const irq_line = 10; /* defined by acpicontroller dsdt INTB# */
	auto const bar_base = argv[0];

	if (argv[0] == ~0UL)
		Logging::panic("virtio_gpu: missing bar address");

	Virtio_gpu *dev = new Virtio_gpu(mb.bus_irqlines, mb.bus_memregion,
	                                 mb.bus_console, bar_base, irq_pin,
	                                 irq_line, uint16(bdf), verbose);

	mb.bus_pcicfg .add(dev, Virtio_gpu::receive_static<MessagePciConfig>);
	mb.bus_mem    .add(dev, Virtio_gpu::receive_static<MessageMem>);
	mb.bus_bios   .add(dev, Virtio_gpu::receive_static<MessageBios>);
	mb.bus_console.add(dev, Virtio_gpu::receive_static<MessageConsole>);

	MessageConsole  msg(MessageConsole::TYPE_ALLOC_CLIENT, dev->console_id);
	mb.bus_console.send(msg);

	dev->host_fb = uintptr_t(msg.ptr);

	ConsoleModeInfo info { };
	msg.type  = MessageConsole::TYPE_GET_MODEINFO;
	msg.id    = dev->console_id;
	msg.index = 1; /* XXX */
	msg.info  = &info;
	mb.bus_console.send(msg);

	dev->mode_host.width  = info.resolution[0];
	dev->mode_host.height = info.resolution[1];

	dev->mode_next.width  = info.resolution[0];
	dev->mode_next.height = info.resolution[1];

	Logging::printf("virtio gpu %ux%u\n",
	                dev->mode_host.width, dev->mode_host.height);

	MessageConsole msg2(MessageConsole::TYPE_ALLOC_VIEW, dev->console_id);
	msg2.view = 1;
	if (mb.bus_console.send(msg2)) {
		dev->shape_ptr  = uintptr_t(msg2.ptr);
		dev->shape_size = unsigned(msg2.size);
	}
}
