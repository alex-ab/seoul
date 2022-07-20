/**
 * Virtio audio device
 *
 * Copyright (C) 2022, Alexander Boettcher
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

#include "virtio_pci.h"


struct Virtio_sound_state
{
	uint32 period_bytes { };
	uint32 buffer_bytes { };
	uint32 consumed_bytes { };

	Virtio::Queue::index_ring idx_start { };
	Virtio::Queue::index_ring idx_end   { };
	bool                      idx_valid { };
	bool                      idx_done  { };

	struct {
		unsigned idx_head { };
		unsigned idx_tail { };
		unsigned ids[16]  { };

		void set(unsigned const msg_id) { ids[idx_head] = msg_id; }

		void set_tail(unsigned const msg_id)
		{
			ids[idx_tail] = msg_id;
			idx_tail = (idx_tail + 1) % (sizeof(ids) / sizeof(ids[0]));
		}

		bool no_idx_left() const
		{
			auto next = (idx_tail + 1) % (sizeof(ids) / sizeof(ids[0]));
			return next == idx_head;
		}

		bool match(unsigned msg_id) const { return ids[idx_head] == msg_id; }

		void advance() {
			idx_head = (idx_head + 1) % (sizeof(ids) / sizeof(ids[0])); }
	} host_msg;

	unsigned read(unsigned const off)
	{
		switch (off) {
		case 0:
			return 1u; /* jacks */
		case 4:
			return 1u; /* streams */
		case 8:
			return 1u; /* channel maps */
		default:
			Logging::printf("virtio_sound: unknown config read=%u\n", off);
			return 0;
		}
	}

	void write(unsigned const off, unsigned const value)
	{
		switch (off) {
		default:
			Logging::printf("virtio_sound: unknown config write %x value=%x\n",
			                off, value);
			break;
		}
	}
};

class Virtio_sound: public StaticReceiver<Virtio_sound>, Virtio::Device
{
	private:

		DBus<MessageAudio> &_bus_audio;
		Clock              &_clock;

		Virtio_sound_state _state  { };

		bool _verbose { false };


		~Virtio_sound();

		struct snd_hdr {
			uint32 code;
		};

		enum { CHANNELS = 2 };

		enum {
			/* jack control request types */
			VIRTIO_SND_R_JACK_INFO = 1,
			VIRTIO_SND_R_JACK_REMAP,
			/* PCM control request types */
			VIRTIO_SND_R_PCM_INFO = 0x100,
			VIRTIO_SND_R_PCM_SET_PARAMS,
			VIRTIO_SND_R_PCM_PREPARE,
			VIRTIO_SND_R_PCM_RELEASE,
			VIRTIO_SND_R_PCM_START,
			VIRTIO_SND_R_PCM_STOP,
			/* channel map control request types */
			VIRTIO_SND_R_CHMAP_INFO = 0x200,
			/* jack event types */
			VIRTIO_SND_EVT_JACK_CONNECTED = 0x1000,
			VIRTIO_SND_EVT_JACK_DISCONNECTED,
			/* PCM event types */
			VIRTIO_SND_EVT_PCM_PERIOD_ELAPSED = 0x1100,
			VIRTIO_SND_EVT_PCM_XRUN,
			/* common status codes */
			VIRTIO_SND_S_OK = 0x8000,
			VIRTIO_SND_S_BAD_MSG,
			VIRTIO_SND_S_NOT_SUPP,
			VIRTIO_SND_S_IO_ERR
		};

		struct snd_query_info {
			struct snd_hdr hdr;
			uint32 start_id;
			uint32 count;
			uint32 size;
		} __attribute__((packed));

		struct snd_info {
			uint32 hda_fn_nid;
		};

		/* supported jack features */
		enum {
			VIRTIO_SND_JACK_F_REMAP = 0
		};

		struct snd_jack_info {
			struct snd_info hdr;
			uint32 features; /* 1 << VIRTIO_SND_JACK_F_XXX */
			uint32 hda_reg_defconf;
			uint32 hda_reg_caps;
			uint8  connected;
			uint8  padding[7];
		} __attribute__((packed));

		/* supported PCM stream features */
		enum {
			VIRTIO_SND_PCM_F_SHMEM_HOST = 0,
			VIRTIO_SND_PCM_F_SHMEM_GUEST,
			VIRTIO_SND_PCM_F_MSG_POLLING,
			VIRTIO_SND_PCM_F_EVT_SHMEM_PERIODS,
			VIRTIO_SND_PCM_F_EVT_XRUNS
		};

		/* supported PCM sample formats */
		enum {
			/* analog formats (width / physical width) */
			VIRTIO_SND_PCM_FMT_IMA_ADPCM = 0, /* 4 / 4 bits */
			VIRTIO_SND_PCM_FMT_MU_LAW, /* 8 / 8 bits */
			VIRTIO_SND_PCM_FMT_A_LAW, /* 8 / 8 bits */
			VIRTIO_SND_PCM_FMT_S8, /* 8 / 8 bits */
			VIRTIO_SND_PCM_FMT_U8, /* 8 / 8 bits */
			VIRTIO_SND_PCM_FMT_S16, /* 16 / 16 bits */
			VIRTIO_SND_PCM_FMT_U16, /* 16 / 16 bits */
			VIRTIO_SND_PCM_FMT_S18_3, /* 18 / 24 bits */
			VIRTIO_SND_PCM_FMT_U18_3, /* 18 / 24 bits */
			VIRTIO_SND_PCM_FMT_S20_3, /* 20 / 24 bits */
			VIRTIO_SND_PCM_FMT_U20_3, /* 20 / 24 bits */
			VIRTIO_SND_PCM_FMT_S24_3, /* 24 / 24 bits */
			VIRTIO_SND_PCM_FMT_U24_3, /* 24 / 24 bits */
			VIRTIO_SND_PCM_FMT_S20,   /* 20 / 32 bits */
			VIRTIO_SND_PCM_FMT_U20, /* 20 / 32 bits */
			VIRTIO_SND_PCM_FMT_S24, /* 24 / 32 bits */
			VIRTIO_SND_PCM_FMT_U24, /* 24 / 32 bits */
			VIRTIO_SND_PCM_FMT_S32, /* 32 / 32 bits */
			VIRTIO_SND_PCM_FMT_U32, /* 32 / 32 bits */
			VIRTIO_SND_PCM_FMT_FLOAT, /* 32 / 32 bits */
			VIRTIO_SND_PCM_FMT_FLOAT64, /* 64 / 64 bits */
			/* digital formats (width / physical width) */
			VIRTIO_SND_PCM_FMT_DSD_U8, /* 8 / 8 bits */
			VIRTIO_SND_PCM_FMT_DSD_U16, /* 16 / 16 bits */
			VIRTIO_SND_PCM_FMT_DSD_U32, /* 32 / 32 bits */
			VIRTIO_SND_PCM_FMT_IEC958_SUBFRAME /* 32 / 32 bits */
		};

		/* supported PCM frame rates */
		enum {
			VIRTIO_SND_PCM_RATE_5512 = 0,
			VIRTIO_SND_PCM_RATE_8000,
			VIRTIO_SND_PCM_RATE_11025,
			VIRTIO_SND_PCM_RATE_16000,
			VIRTIO_SND_PCM_RATE_22050,
			VIRTIO_SND_PCM_RATE_32000,
			VIRTIO_SND_PCM_RATE_44100,
			VIRTIO_SND_PCM_RATE_48000,
			VIRTIO_SND_PCM_RATE_64000,
			VIRTIO_SND_PCM_RATE_88200,
			VIRTIO_SND_PCM_RATE_96000,
			VIRTIO_SND_PCM_RATE_176400,
			VIRTIO_SND_PCM_RATE_192000,
			VIRTIO_SND_PCM_RATE_384000
		};

		enum {
			SND_D_OUTPUT = 0,
			SND_D_INPUT
		};

		struct snd_pcm_info {
			struct snd_info hdr;
			uint32 features; /* 1 << VIRTIO_SND_PCM_F_XXX */
			uint64 formats; /* 1 << VIRTIO_SND_PCM_FMT_XXX */
			uint64 rates; /* 1 << VIRTIO_SND_PCM_RATE_XXX */
			uint8  direction;
			uint8  channels_min;
			uint8  channels_max;
			uint8  padding[5];
		} __attribute__((packed));

		struct snd_pcm_hdr {
			struct snd_hdr hdr;
			uint32 stream_id;
		} __attribute__((packed));

		struct snd_pcm_set_params {
			struct snd_pcm_hdr hdr;
			uint32 buffer_bytes;
			uint32 period_bytes;
			uint32 features; /* 1 << VIRTIO_SND_PCM_F_XXX */
			uint8  channels;
			uint8  format;
			uint8  rate;
			uint8  padding;
		} __attribute__((packed));

		/* standard channel position definition */
		enum {
			VIRTIO_SND_CHMAP_NONE = 0, /* undefined */
			VIRTIO_SND_CHMAP_NA, /* silent */
			VIRTIO_SND_CHMAP_MONO, /* mono stream */
			VIRTIO_SND_CHMAP_FL, /* front left */
			VIRTIO_SND_CHMAP_FR, /* front right */
			VIRTIO_SND_CHMAP_RL, /* rear left */
			VIRTIO_SND_CHMAP_RR, /* rear right */
			VIRTIO_SND_CHMAP_FC, /* front center */
			VIRTIO_SND_CHMAP_LFE, /* low frequency (LFE) */
			VIRTIO_SND_CHMAP_SL, /* side left */
			VIRTIO_SND_CHMAP_SR, /* side right */
			VIRTIO_SND_CHMAP_RC, /* rear center */
			VIRTIO_SND_CHMAP_FLC, /* front left center */
			VIRTIO_SND_CHMAP_FRC, /* front right center */
			VIRTIO_SND_CHMAP_RLC, /* rear left center */
			VIRTIO_SND_CHMAP_RRC, /* rear right center */
			VIRTIO_SND_CHMAP_FLW, /* front left wide */
			VIRTIO_SND_CHMAP_FRW, /* front right wide */
			VIRTIO_SND_CHMAP_FLH, /* front left high */
			VIRTIO_SND_CHMAP_FCH, /* front center high */
			VIRTIO_SND_CHMAP_FRH, /* front right high */
			VIRTIO_SND_CHMAP_TC, /* top center */
			VIRTIO_SND_CHMAP_TFL, /* top front left */
			VIRTIO_SND_CHMAP_TFR, /* top front right */
			VIRTIO_SND_CHMAP_TFC, /* top front center */
			VIRTIO_SND_CHMAP_TRL, /* top rear left */
			VIRTIO_SND_CHMAP_TRR, /* top rear right */
			VIRTIO_SND_CHMAP_TRC, /* top rear center */
			VIRTIO_SND_CHMAP_TFLC, /* top front left center */
			VIRTIO_SND_CHMAP_TFRC, /* top front right center */
			VIRTIO_SND_CHMAP_TSL, /* top side left */
			VIRTIO_SND_CHMAP_TSR, /* top side right */
			VIRTIO_SND_CHMAP_LLFE, /* left LFE */
			VIRTIO_SND_CHMAP_RLFE, /* right LFE */
			VIRTIO_SND_CHMAP_BC, /*bottom center */
			VIRTIO_SND_CHMAP_BLC, /* bottom left center */
			VIRTIO_SND_CHMAP_BRC /* bottom right center */
		};

		/* maximum possible number of channels */
		#define VIRTIO_SND_CHMAP_MAX_SIZE 18
		struct snd_chmap_info {
			struct snd_hdr hdr;
			uint8 direction;
			uint8 channels;
			uint8 positions[VIRTIO_SND_CHMAP_MAX_SIZE];
		} __attribute__((packed));

		struct snd_pcm_xfer {
			uint32 stream_id;
		};

		struct snd_pcm_status {
			uint32 status;
			uint32 latency_bytes;
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
			if (!response || response_size < sizeof(snd_hdr)) {
				Logging::printf("%s: response invalid\n", identifier);
				return 0UL;
			}

			TYPE const host_data = *const_cast<TYPE *>(guest_data);

			/*
			 * Make sure compiler wrote all data to local variable,
			 * before doing sanity & security checks.
			 */
			VMM_MEMORY_BARRIER;

			auto &answer = *reinterpret_cast<snd_hdr *>(response);
			memset(&answer, 0, sizeof(answer));

			answer.code = fn(host_data);

			return request_size;
		}

		size_t _jack_info(uintptr_t, size_t, uintptr_t, size_t, uintptr_t, size_t);
		size_t _pcm_info(uintptr_t, size_t, uintptr_t, size_t, uintptr_t, size_t);
		size_t _pcm_set_param(uintptr_t, size_t, uintptr_t, size_t);
		size_t _pcm_op(uintptr_t, size_t, uintptr_t, size_t, unsigned, Virtio::Queue &);
		size_t _chmap_info(uintptr_t, size_t, uintptr_t, size_t, uintptr_t, size_t);

	public:

		Virtio_sound(DBus<MessageIrqLines>  &bus_irqlines,
		             DBus<MessageMemRegion> &bus_memregion,
		             DBus<MessageAudio>     &bus_audio,
		             Clock                  &clock,
		             unsigned char irq, unsigned short bdf)
		:
			Virtio::Device(bus_irqlines, bus_memregion, irq, bdf,
			               25 /* type */,
			               0xf7a90000ull /* phys bar address XXX */,
			               4 /* queues */),
			_bus_audio(bus_audio),
			_clock(clock)
		{ }

		bool receive(MessageBios &msg) {
			switch(msg.irq) {
			case BiosCommon::BIOS_RESET_VECTOR:
				_state = { };
				reset();
			};

			return false;
		}

		bool receive(MessagePciConfig &msg) {
			return Virtio::Device::receive(msg); }

		bool receive(MessageMem &msg)
		{
			if (msg.phys < _phys_bar_base || _phys_bar_base + PHYS_BAR_SIZE <= msg.phys)
				return false;

			unsigned const offset = msg.phys - _phys_bar_base;

			switch (offset) {
			case BAR_OFFSET_CONFIG ... BAR_OFFSET_CONFIG + RANGE_SIZE - 1:
				if (msg.read)
					*msg.ptr = _state.read(offset - BAR_OFFSET_CONFIG);
				else
					_state.write(offset - BAR_OFFSET_CONFIG, *msg.ptr);

				return true;
			default:
				return Virtio::Device::receive(msg);
			}
		}

		bool receive(MessageAudio &msg)
		{
			auto &queue_tx = _queues[2].queue;

			if (msg.type != MessageAudio::Type::AUDIO_CONTINUE_TX)
				return false;

			if (_state.idx_valid) {
				if (!_state.host_msg.match(msg.id))
					return true;

				_state.idx_done = true;
			}

			bool restart = _state.idx_done;

			/* continue if we got blocked by VMM host side */
			if (!tx(queue_tx)) {
				tx_drain(queue_tx);
				msg.type = MessageAudio::Type::AUDIO_DRAIN_TX;
				return true;
			}

			if (restart) {
				if (!tx(queue_tx)) {
					tx_drain(queue_tx);
					msg.type = MessageAudio::Type::AUDIO_DRAIN_TX;
				}
			}

			return true;
		}

		void tx_drain(Virtio::Queue &tx_queue)
		{
			bool const inject = tx_queue.consume([&] (auto const descriptor, auto ring) {
				auto const request = vmm_address(descriptor.addr, descriptor.len);
				size_t const request_size = descriptor.len;

				auto const header = reinterpret_cast<snd_pcm_xfer *>(request);

				if (!request || request_size < sizeof(*header)) {
					Logging::printf("virtio_sound, invalid tx request\n");
					return 0UL;
				}

				return request_size;
			});

			_state.idx_done = false;

			_state.idx_end   = { };
			_state.idx_start = { };
			_state.idx_valid = false;

			_state.host_msg  = { };

			_state.consumed_bytes = 0;

			if (inject)
				inject_irq();
		}

		enum Tx_return { DONE, DELAY, DRAIN };

		Tx_return tx_handle(Virtio::Queue             &tx_queue,
		                    Virtio::Queue::desc const &descriptor,
		                    unsigned                  &host_msg_id)
		{
			unsigned i = 0;

			unsigned descriptor_read_bytes = 0;

			auto data_desc = descriptor;
			while (true) {
				i++;
				data_desc = tx_queue.next_desc(data_desc);
				if (!data_desc.len || _state.consumed_bytes >= _state.period_bytes)
					break;

				auto const data      = vmm_address(data_desc.addr, data_desc.len);
				auto const data_size = data_desc.len;

				if (!data) {
					Logging::printf("virtio_sound, invalid tx data\n");
					return Tx_return::DELAY;
				}

				/* skip already processed data */
				if (descriptor_read_bytes + data_size <= _state.consumed_bytes) {
					descriptor_read_bytes += data_size;
					continue;
				}

				auto const offset = _state.consumed_bytes - descriptor_read_bytes;

				MessageAudio msg(MessageAudio::Type::AUDIO_OUT, data,
				                 data_size, offset);

				if (!_bus_audio.send(msg)) {
					Logging::printf("virtio_sound: processing error\n");
					return Tx_return::DELAY;
				}

				if (msg.type == MessageAudio::Type::AUDIO_DRAIN_TX)
					return Tx_return::DRAIN;

				host_msg_id = msg.id;

				_state.consumed_bytes += msg.consumed - offset;

				if (msg.consumed < data_size) {
					/* wait to be resumed by AUDIO_CONTINUE_TX */
					return Tx_return::DELAY;
				}

				descriptor_read_bytes += data_size;
			}

			if (data_desc.len) {
#if 0
				auto const request = vmm_address(descriptor.addr, descriptor.len);
				auto const request_size = descriptor.len;

				if (request) {
					auto const status = reinterpret_cast<snd_pcm_status *>(request);
					Logging::printf("%u: status=%u latency_bytes=%u\n", i, status->status, status->latency_bytes);
				}
#endif
			}
			return Tx_return::DONE;
		}

		bool tx_loop(Virtio::Queue &tx_queue, Virtio::Queue::index_ring ring,
		             unsigned loop_cnt)
		{
			Virtio::Queue::index_ring xring1 = ring;
			Virtio::Queue::index_ring xring0 = _state.idx_end;
			Virtio::Queue::index_ring xring  = _state.idx_end + 1;
			Virtio::Queue::index_ring xring2 = xring;

			bool drain = false;

			tx_queue.consume(xring, [&] (auto const descriptor, auto ring) {
				auto const request = vmm_address(descriptor.addr, descriptor.len);
				size_t const request_size = descriptor.len;

				auto const header = reinterpret_cast<snd_pcm_xfer *>(request);

				if (!request || request_size < sizeof(*header)) {
					Logging::printf("virtio_sound, invalid tx 2 request\n");
					return 0UL;
				}

				unsigned host_msg_id = 0;
				auto const tx_result = tx_handle(tx_queue, descriptor, host_msg_id);
				if (tx_result == Tx_return::DRAIN) {
					drain = true;
					return 0UL;
				}
				if (tx_result != Tx_return::DONE)
					return 0UL;

				_state.idx_end = ring;

				_state.host_msg.set_tail(host_msg_id);

				_state.consumed_bytes = 0;

				if (!_state.host_msg.no_idx_left()) {
					if (!tx_loop(tx_queue, ring, loop_cnt + 1)) {
						drain = true;
						return 0UL;
					}
				} else
					Logging::printf("virtio_sound, no idx left ?\n");

				return 0UL;
			});

			return drain ? false : true;
		}

		bool tx(Virtio::Queue &tx_queue)
		{
			bool drain = false;

			bool const inject = tx_queue.consume([&] (auto const descriptor, auto ring) {
				auto const request = vmm_address(descriptor.addr, descriptor.len);
				size_t const request_size = descriptor.len;

				auto const header = reinterpret_cast<snd_pcm_xfer *>(request);

				if (!request || request_size < sizeof(*header)) {
					Logging::printf("virtio_sound, invalid tx request\n");
					return 0UL;
				}

				if (_state.idx_done) {
					_state.idx_done = false;

					_state.host_msg.advance();

					if (_state.idx_start != _state.idx_end)
						_state.idx_start += 1;
					else {
						_state.idx_valid = false;
					}

					return request_size;
				}

				if (!_state.idx_valid) {
					unsigned host_msg_id = 0;
					auto tx_result = tx_handle(tx_queue, descriptor, host_msg_id);
					if (tx_result == Tx_return::DRAIN) {
						drain = true;
						return 0UL;
					}
					if (tx_result != Tx_return::DONE)
						return 0UL;

					_state.idx_start  = ring;
					_state.idx_end    = ring;

					_state.host_msg.set(host_msg_id);
					_state.host_msg.set_tail(host_msg_id);

					_state.idx_valid  = true;

					_state.consumed_bytes = 0;
				}

				if (!tx_loop(tx_queue, ring, 0))
					drain = true;

				return 0UL;
			});

			if (drain)
				return false;

			if (inject)
				inject_irq();

			return true;
		}

		void notify (unsigned queue) override
		{
			/* queue: 0 - controlq, 1 - eventq, 2 - tx, 3 - rx */

			auto &queue_use = _queues[queue].queue;
			auto &queue_tx  = _queues[2].queue;

			if (&queue_use == &queue_tx) {
				if (!tx(queue_tx))
					tx_drain(queue_tx);
				return;
			}

			if (queue != 0)
				Logging::printf("unknown queue %u\n", queue);

			bool inject = queue_use.consume([&] (auto const descriptor, auto) {
				auto const request = vmm_address(descriptor.addr, descriptor.len);
				auto const request_size = descriptor.len;

				auto const header = reinterpret_cast<snd_hdr *>(request);

				if (!request || request_size < sizeof(*header)) {
					Logging::printf("virtio_sound, invalid request\n");
					return 0UL;
				}

				uintptr_t response = 0; size_t response_size = 0;
				uintptr_t desc2 = 0; size_t desc2_size = 0;

				auto desc1 = queue_use.next_desc(descriptor);
				if (desc1.len) {
					response      = vmm_address(desc1.addr, desc1.len);
					response_size = desc1.len;
				}

				auto descx = queue_use.next_desc(desc1);
				if (descx.len) {
					desc2      = vmm_address(descx.addr, descx.len);
					desc2_size = descx.len;
				}

				if (_verbose)
					Logging::printf("virtio_sound %u"
					                ".... %lx+%u %lx+%lu %lx+%lu\n",
					                header->code, request, request_size,
					                response, response_size, desc2, desc2_size);

				switch (header->code) {
				case VIRTIO_SND_R_JACK_INFO:
					return _jack_info(request, request_size, response,
					                  response_size, desc2, desc2_size);
				case VIRTIO_SND_R_PCM_INFO:
					return _pcm_info(request, request_size, response,
					                 response_size, desc2, desc2_size);
				case VIRTIO_SND_R_PCM_SET_PARAMS:
					return _pcm_set_param(request, request_size, response,
					                      response_size);
				case VIRTIO_SND_R_PCM_PREPARE:
				case VIRTIO_SND_R_PCM_RELEASE:
				case VIRTIO_SND_R_PCM_START:
				case VIRTIO_SND_R_PCM_STOP:
					return _pcm_op(request, request_size, response,
					               response_size, header->code, queue_tx);
				case VIRTIO_SND_R_CHMAP_INFO: 
					return _chmap_info(request, request_size, response,
					                   response_size, desc2, desc2_size);
				default:
					Logging::printf("virtio_sound, unsupported %u"
					                ".... %lx+%u %lx+%lu %lx+%lu\n",
					                header->code, request, request_size,
					                response, response_size, desc2, desc2_size);
				}
				return 0UL;
			});

			if (inject)
				inject_irq();
		}

		uint32 dev_feature     (unsigned)         override { return 0u; }
		void   drv_feature_ack (unsigned, uint32) override { }
		uint32 drv_feature     (unsigned)         override { return 0u; }
};

size_t Virtio_sound::_jack_info(uintptr_t const in,  size_t const in_size,
                                uintptr_t const out, size_t const out_size,
                                uintptr_t const data, size_t const data_size)
{
	auto const name = "virtio_sound, jack info";

	return check<snd_query_info>(in, in_size, out, out_size, name,
	                             [&](snd_query_info const &info) {
		auto * info_out = reinterpret_cast<snd_jack_info *>(data);

		if (!data || data_size < sizeof(*info_out)) {
			Logging::printf("%s: invalid info out structure\n", name);
			return VIRTIO_SND_S_BAD_MSG;
		}

		memset(info_out, 0, sizeof(*info_out));
		/* XXX more ? */
//		info_out->hda_reg_defconf /* indicates a pin default configuration value (see HDA, section 7.3.3.31) */
//		info_out->hda_reg_caps /* indicates a pin capabilities value (see HDA, section 7.3.4.9) */
		info_out->hdr.hda_fn_nid = 1; /* HDA spec 7.1.2 */
		info_out->connected = 1;

		return VIRTIO_SND_S_OK;
	});

}

size_t Virtio_sound::_pcm_info(uintptr_t const in,  size_t const in_size,
                               uintptr_t const out, size_t const out_size,
                               uintptr_t const data, size_t const data_size)
{
	auto const name = "virtio_sound, pcm info";

	return check<snd_query_info>(in, in_size, out, out_size, name,
	                             [&](snd_query_info const &info) {
		auto * info_out = reinterpret_cast<snd_pcm_info *>(data);

		if (!data || data_size < sizeof(*info_out)) {
			Logging::printf("%s: invalid info out structure\n", name);
			return VIRTIO_SND_S_BAD_MSG;
		}

		memset(info_out, 0, sizeof(*info_out));

		info_out->features     = 1ull << VIRTIO_SND_PCM_F_SHMEM_GUEST;
		if (sizeof(float) == 4)
			info_out->formats  = 1ull << VIRTIO_SND_PCM_FMT_FLOAT;
		else
			info_out->formats  = 1ull << VIRTIO_SND_PCM_FMT_FLOAT64;
		info_out->rates        = 1ull << VIRTIO_SND_PCM_RATE_44100;
		info_out->direction    = SND_D_OUTPUT;
		info_out->channels_min = CHANNELS;
		info_out->channels_max = CHANNELS;

		return VIRTIO_SND_S_OK;
	});
}

size_t Virtio_sound::_pcm_set_param(uintptr_t const in,  size_t const in_size,
                                    uintptr_t const out, size_t const out_size)
{
	auto const name = "virtio_sound, pcm set param";

	return check<snd_pcm_set_params>(in, in_size, out, out_size, name,
	                                 [&](snd_pcm_set_params const &params) {

		if (_verbose)
			Logging::printf("params set - stream_id=%u "
			                "buffer_bytes %u period_bytes=%u "
			                "features=%x channels=%u format=%u rate=%u\n",
			                params.hdr.stream_id,
			                params.buffer_bytes,
			                params.period_bytes,
			                params.features, /* 1 << VIRTIO_SND_PCM_F_XXX */
			                params.channels,
			                params.format,
			                params.rate);

		if (sizeof(float) == 4 && params.format != VIRTIO_SND_PCM_FMT_FLOAT)
			return VIRTIO_SND_S_NOT_SUPP;
		if (sizeof(float) == 8 && params.format != VIRTIO_SND_PCM_FMT_FLOAT64)
			return VIRTIO_SND_S_NOT_SUPP;
		if (params.channels != CHANNELS || params.rate != VIRTIO_SND_PCM_RATE_44100)
			return VIRTIO_SND_S_NOT_SUPP;

		_state.period_bytes = params.period_bytes;
		_state.buffer_bytes = params.buffer_bytes;

		return VIRTIO_SND_S_OK;
	});
}

size_t Virtio_sound::_pcm_op(uintptr_t const in,  size_t const in_size,
                             uintptr_t const out, size_t const out_size,
                             unsigned const type, Virtio::Queue &queue_tx)
{
	auto const name = "virtio_sound, pcm op";

	return check<snd_pcm_hdr>(in, in_size, out, out_size, name,
	                          [&](snd_pcm_hdr const &stream) {

		if (_verbose)
			Logging::printf("%s %s %s\n", name, __func__,
			               (type == VIRTIO_SND_R_PCM_START)   ? "start" :
			               (type == VIRTIO_SND_R_PCM_STOP)    ? "stop"  :
			               (type == VIRTIO_SND_R_PCM_PREPARE) ? "prepare"  :
			               (type == VIRTIO_SND_R_PCM_RELEASE) ? "release" : "unknown");

		if (type == VIRTIO_SND_R_PCM_START) {
			MessageAudio msg(MessageAudio::Type::AUDIO_START);
			_bus_audio.send(msg);
		} else
		if (type == VIRTIO_SND_R_PCM_STOP) {
			/* ack all tx in flight */
			tx_drain(queue_tx);

			MessageAudio msg(MessageAudio::Type::AUDIO_STOP);
			_bus_audio.send(msg);
		}

		return VIRTIO_SND_S_OK;
	});
}

size_t Virtio_sound::_chmap_info(uintptr_t const in,  size_t const in_size,
                                 uintptr_t const out, size_t const out_size,
                                 uintptr_t const data, size_t const data_size)
{
	auto const name = "virtio_sound, channel map info";

	return check<snd_query_info>(in, in_size, out, out_size, name,
	                             [&](snd_query_info const &info) {
		auto * info_out = reinterpret_cast<snd_chmap_info *>(data);

		if (!data || data_size < sizeof(*info_out)) {
			Logging::printf("%s: invalid info out structure\n", name);
			return VIRTIO_SND_S_BAD_MSG;
		}

		memset(info_out, 0, sizeof(*info_out));
		info_out->direction    = SND_D_OUTPUT;
		info_out->channels     = CHANNELS;
		info_out->positions[0] = VIRTIO_SND_CHMAP_FL;
		info_out->positions[1] = VIRTIO_SND_CHMAP_FR;

		return VIRTIO_SND_S_OK;
	});
}

PARAM_HANDLER(virtio_sound,
	      "virtio_sound:bdf,irq - attach an virtio sound to the PCI bus",
	      "Example: 'virtio_sound:,15'.",
	      "If no bdf is given a free one is used.")
{
	Virtio_sound *dev = new Virtio_sound(mb.bus_irqlines, mb.bus_memregion,
	                                     mb.bus_audio,
	                                     *mb.clock(), argv[1],
	                                     PciHelper::find_free_bdf(mb.bus_pcicfg, argv[0]));

	mb.bus_pcicfg.add(dev, Virtio_sound::receive_static<MessagePciConfig>);
	mb.bus_mem   .add(dev, Virtio_sound::receive_static<MessageMem>);
	mb.bus_bios  .add(dev, Virtio_sound::receive_static<MessageBios>);
	mb.bus_audio .add(dev, Virtio_sound::receive_static<MessageAudio>);

	Logging::printf("virtio sound\n");
}
