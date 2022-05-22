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
	uint32 consumed_byte_descriptor { };

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
		DBus<MessageTimer> &_bus_timer;
		Clock              &_clock;

		Virtio_sound_state _state    { };
		unsigned           _timer_nr { };

		bool               _timeout_active { false };
		bool               _tx_processed   { false };

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
		size_t _pcm_op(uintptr_t, size_t, uintptr_t, size_t, unsigned);
		size_t _chmap_info(uintptr_t, size_t, uintptr_t, size_t, uintptr_t, size_t);

	public:

		Virtio_sound(DBus<MessageIrqLines>  &bus_irqlines,
		             DBus<MessageMemRegion> &bus_memregion,
		             DBus<MessageAudio>     &bus_audio,
		             DBus<MessageTimer>     &bus_timer,
		             Clock                  &clock,
		             unsigned char irq, unsigned short bdf)
		:
			Virtio::Device(bus_irqlines, bus_memregion, irq, bdf,
			               25 /* type */,
			               0xf7a90000ull /* phys bar address XXX */,
			               4 /* queues */),
			_bus_audio(bus_audio),
			_bus_timer(bus_timer),
			_clock(clock)
		{
			MessageTimer msg_timer;
			if (!_bus_timer.send(msg_timer))
				Logging::panic("%s can't get a timer", __PRETTY_FUNCTION__);

			_timer_nr = msg_timer.nr;
		}

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
			if (msg.phys < _phys_bar_base || _phys_bar_base + _phys_bar_size <= msg.phys)
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

		bool receive(MessageTimeout &msg)
		{
			if (msg.nr != _timer_nr) return false;

//			Logging::printf("timeout virtio\n");
			_timeout_active = false;

			/* ack processed data */
			tx(_queues[2].queue);

			/* start reading next descriptors if available */
			tx(_queues[2].queue);

			return true;
		}

		bool receive(MessageAudio &msg)
		{
			if (msg.type != MessageAudio::Type::AUDIO_CONTINUE_TX)
				return false;

			/* continue if we got blocked to VMM host side */
			tx(_queues[2].queue);

			return true;
		}

		void tx(Virtio::Queue &tx_queue)
		{
			bool const inject = tx_queue.consume([&] (auto const descriptor) {
				auto const request = vmm_address(descriptor.addr, descriptor.len);
				size_t const request_size = descriptor.len;

				auto const header = reinterpret_cast<snd_pcm_xfer *>(request);

				if (!request || request_size < sizeof(*header)) {
					Logging::printf("virtio_sound, invalid tx request\n");
					return 0UL;
				}

				if (_tx_processed) {
					if (_timeout_active)
						return 0UL;
					_tx_processed = false;
					return request_size;
				}

				if (!_tx_processed) {
					if (!_timeout_active) {
						_timeout_active = true;

//						unsigned ms = _state.buffer_bytes / CHANNELS / sizeof(float) / 44;
						unsigned ms = _state.period_bytes / CHANNELS / sizeof(float) / 44;

						MessageTimer timeout(_timer_nr, _clock.abstime(ms, 1000));
						if (!_bus_timer.send(timeout))
							Logging::panic("%s timeout", __PRETTY_FUNCTION__);
					}
				}

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
						return 0UL;
					}

					/* skip already processed data */
					if (descriptor_read_bytes + data_size <= _state.consumed_bytes) {
//						Logging::printf("%u: %lx+%u skip %llx\n", i, data, data_size, data_desc.addr);
						descriptor_read_bytes += data_size;
						continue;
					}

					auto const offset = _state.consumed_bytes - descriptor_read_bytes;

/*
					Logging::printf("%u: %lx+%u offset=%u (descriptor_read_bytes=%u) %llx\n",
					                i, data, data_size, offset, descriptor_read_bytes, data_desc.addr);
*/
					MessageAudio msg(MessageAudio::Type::AUDIO_OUT, data,
					                 data_size, offset);
					if (!_bus_audio.send(msg)) {
						Logging::printf("virtio_sound: processing error\n");
						return 0UL;
					}

					_state.consumed_bytes += msg.consumed - offset;

					if (msg.consumed < data_size) {
						/* wait to be resumed by AUDIO_CONTINUE_TX */
						return 0UL;
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

				_tx_processed = true;
				_state.consumed_bytes = 0;

				return 0UL;
			});

			if (inject) {
//				Logging::printf("tx queue irq ?\n");
				inject_irq();
			}
		}

		void notify (unsigned queue) override
		{
			/* queue: 0 - controlq, 1 - eventq, 2 - tx, 3 - rx */

			auto &used_queue = _queues[queue].queue;

			if (queue == 2) {
				tx(used_queue);
				return;
			}

			Logging::printf("virtio_sound: notify %u", queue);
			bool inject = used_queue.consume([&] (auto const descriptor) {
				auto const request = vmm_address(descriptor.addr, descriptor.len);
				auto const request_size = descriptor.len;

				auto const header = reinterpret_cast<snd_hdr *>(request);

				if (!request || request_size < sizeof(*header)) {
					Logging::printf("virtio_sound, invalid request\n");
					return 0UL;
				}

				uintptr_t response = 0; size_t response_size = 0;
				uintptr_t desc2 = 0; size_t desc2_size = 0;

				auto desc1 = used_queue.next_desc(descriptor);
				if (desc1.len) {
					response      = vmm_address(desc1.addr, desc1.len);
					response_size = desc1.len;
				}

				auto descx = used_queue.next_desc(desc1);
				if (descx.len) {
					desc2      = vmm_address(descx.addr, descx.len);
					desc2_size = descx.len;
				}

				Logging::printf("%u.... %lx+%u %lx+%lu %lx+%lu\n",
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
					               response_size, header->code);
				case VIRTIO_SND_R_CHMAP_INFO: 
					return _chmap_info(request, request_size, response,
					                   response_size, desc2, desc2_size);
				default:
					Logging::printf("sound, unsupported code %x\n", header->code);
				}
				return 0UL;
			});

			if (inject)
				inject_irq();
		}
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

		Logging::printf("todo - user buffer bytes - period \n");

		_state.period_bytes = params.period_bytes;
		_state.buffer_bytes = params.buffer_bytes;

		return VIRTIO_SND_S_OK;
	});
}

size_t Virtio_sound::_pcm_op(uintptr_t const in,  size_t const in_size,
                             uintptr_t const out, size_t const out_size,
                             unsigned const type)
{
	auto const name = "virtio_sound, pcm op";

	return check<snd_pcm_hdr>(in, in_size, out, out_size, name,
	                          [&](snd_pcm_hdr const &stream) {

		if (type == VIRTIO_SND_R_PCM_PREPARE) {
			MessageAudio msg(MessageAudio::Type::AUDIO_START);
			_bus_audio.send(msg);
		} else
		if (type == VIRTIO_SND_R_PCM_RELEASE) {
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
	                                     mb.bus_audio, mb.bus_timer,
	                                     *mb.clock(), argv[1],
	                                     PciHelper::find_free_bdf(mb.bus_pcicfg, argv[0]));

	mb.bus_pcicfg .add(dev, Virtio_sound::receive_static<MessagePciConfig>);
	mb.bus_mem    .add(dev, Virtio_sound::receive_static<MessageMem>);
	mb.bus_bios   .add(dev, Virtio_sound::receive_static<MessageBios>);
	mb.bus_timeout.add(dev, Virtio_sound::receive_static<MessageTimeout>);
	/* XXX different way to restart submitting ... */
	mb.bus_audio .add(dev, Virtio_sound::receive_static<MessageAudio>);

	Logging::printf("virtio sound\n");
}
