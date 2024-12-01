/**
 * Virtio network device
 *
 * Copyright (C) 2024-2026, Josef Soentgen
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
#include <service/net.h>
#include <nul/net.h>

#include "virtio_pci.h"

struct Virtio_network_config
{
	struct reg_0
	{
		uint8  mac[6];
		uint16 status;
		uint16 max_virtqueue_pairs;
	};

	reg_0 reg { };

	void reset()
	{
		reg.status              = 0;
		reg.max_virtqueue_pairs = 0;
	}

	unsigned read(unsigned const offset) const
	{
		switch (offset) {
		case 0:
			return reg.mac[0] | (reg.mac[1] << 8) | (reg.mac[2] << 16) | (reg.mac[3] << 24);
		case 4:
			return reg.mac[4] | (reg.mac[5] << 8) | (reg.status << 16);
		default:
			return 0;
		}
	}

	void write(unsigned const off, unsigned const value) { }
};


struct virtio_net_hdr
{
	uint8  flags;
	uint8  gso_type; /* 0 => NONE */
	uint16 hdr_len;
	uint16 gso_size;
	uint16 csum_start;
	uint16 csum_offset;
	uint16 num_buffers; /* only valid when MRG_RXBUF */
} __attribute__((packed));


static void print(virtio_net_hdr const &hdr)
{
	Logging::printf("flags: 0x%x gso_type: %u hdr_len: %u "
	                "gso_size: %u csum_start: %u csum_offset: %u num_buffers: %u\n",
	                 hdr.flags, hdr.gso_type, hdr.hdr_len, hdr.gso_size, hdr.csum_start,
	                 hdr.csum_offset, hdr.num_buffers);
}


class Virtio_network: public StaticReceiver<Virtio_network>, Virtio::Device
{
	private:

		Seoul::Lock           _lock { };
		Virtio_network_config _network_config { };

		unsigned const _net_id;

		~Virtio_network();

		DBus<MessageNetwork> &_bus_network;

	public:

		Virtio_network(DBus<MessageNetwork>   &bus_network,
		               DBus<MessageIrqLines>  &bus_irqlines,
		               DBus<MessageMem>       &bus_mem,
		               DBus<MessageMemRegion> &bus_memregion,
		               uint64 const mac_addr,
		               uint64 const bar_addr,
		               uint8  const irq_pin,
		               uint8  const irq_line,
		               uint16 const bdf,
		               uint32 const net_id,
		               bool   const msix,
		               bool   const verbose)
		:
			Virtio::Device(bus_irqlines, bus_mem, bus_memregion,
			               irq_pin, irq_line, bdf,
			               1 /* virtio type */,
			               0x20000000, /* pci class code (ethernet) */
			               bar_addr,
			               2 /* queues */, msix),
			_net_id       { net_id },
			_bus_network  { bus_network }
		{
			_verbose = verbose;

			EthernetAddr const mac { mac_addr };
			for (unsigned i = 0; i < 6; i++)
				_network_config.reg.mac[i] = mac.byte[i];
		}

		bool receive(MessageBios &msg)
		{
			switch (msg.irq) {
			case BiosCommon::BIOS_RESET_VECTOR:

				Seoul::Lock::Guard guard(_lock);

				_network_config.reset();
				reset();
			};

			return false;
		}

		bool receive(MessagePciConfig &msg)
		{
			if (msg.bdf != _bdf)
				return false;

			return sync_and_irq(_lock, [&]() {
				return Virtio::Device::receive(msg); });
		}

		bool receive(MessageMem &msg)
		{
			if (msg.phys < _phys_bar_base || _phys_bar_base + PHYS_BAR_SIZE <= msg.phys)
				return false;

			return sync_and_irq(_lock, [&]() {

				unsigned const offset = unsigned(msg.phys - _phys_bar_base);

				switch (offset) {
				case BAR_OFFSET_CONFIG ... BAR_OFFSET_CONFIG + RANGE_SIZE - 1:
					if (msg.read)
						*msg.ptr = _network_config.read(offset - BAR_OFFSET_CONFIG);
					else
						_network_config.write(offset - BAR_OFFSET_CONFIG, *msg.ptr);

					return true;
				default:
					return Virtio::Device::receive(msg);
				}
			});
		}

		bool receive(MessageNetwork &msg)
		{
			if (msg.client != _net_id)
				return false;

			if (msg.type != MessageNetwork::LINK &&
			    msg.type != MessageNetwork::PACKET_TO_MODEL)
				return false;

			Seoul::Lock::Guard guard(_lock);

			if (msg.type == MessageNetwork::LINK) {
				if (_network_config.reg.status != msg.data.link_up) {
					_network_config.reg.status = msg.data.link_up;

					Logging::printf("virtio_network %u: link state %s\n",
					                _net_id, _network_config.reg.status ? "UP"  : "DOWN");
				}

				config_changed();

				inject_irq();
				return true;
			}

			const EthernetAddr &dst = *reinterpret_cast<const EthernetAddr *>(msg.data.buffer);
			Logging::printf("Packet for " MAC_FMT "\n", MAC_SPLIT(&dst));

			auto &rx_queue = _queues[0].queue;

			bool inject = rx_queue.consume([&] (auto const descriptor, auto) {

				auto const request = vmm_address(descriptor.addr, descriptor.len);
				auto const request_size = descriptor.len;

				if (!request) {
					Logging::printf("virtio-net: invalid rx descriptor");
					return 0UL;
				}

				auto & header = *reinterpret_cast<virtio_net_hdr *>(request);

				memset(&header, 0, sizeof(virtio_net_hdr));
				header.num_buffers = 1;

				if (header.flags)
					Logging::printf("virtio-net: header-flags 0x%x set\n",
					                header.flags);

				uint8       *dst     = (uint8*)request + sizeof(virtio_net_hdr);
				size_t const dst_len = request_size - sizeof(virtio_net_hdr);
				void const  *src     = msg.data.buffer;
				size_t const src_len = msg.data.len;
				if (dst_len < src_len) {
					Logging::printf("virtio-net: ignore overly large RX packet\n");
					return 0UL;
				}

				memcpy(dst, src, src_len);

				size_t const consumed = src_len + sizeof(virtio_net_hdr);

				return consumed;
			});

			if (inject)
				inject_irq();

			// Logging::printf("Packet for " MAC_FMT " received\n", MAC_SPLIT(&dst));
			return true;
		}

		void notify(unsigned queue) override
		{
			bool const tx = queue & 1;

			Logging::printf("notify queue %u\n", queue);

			if (!tx)
				return;

			auto &tx_queue = _queues[queue].queue;

			bool inject = tx_queue.consume([&] (auto const descriptor, auto) {
				auto const request = vmm_address(descriptor.addr, descriptor.len);
				auto const request_size = descriptor.len;

				Logging::printf("packet notify\n");

				auto const & header = *reinterpret_cast<virtio_net_hdr *>(request);

				if (!request || request_size < sizeof(header)) {
					Logging::printf("virtio-net: invalid tx request");
					return 0UL;
				}

				if (header.flags)
					Logging::printf("virtio-net: header-flags 0x%x set\n",
					                header.flags);

				auto ptr    = (void * const)(request + sizeof(header));
				auto length = request_size - sizeof(header);

				if (!length) {
					Logging::printf("virtio-net: invalid tx request with 0 length");
					return (unsigned long)request_size;
				}

				MessageNetwork msg(MessageNetwork::PACKET_TO_HOST,
				                   { .buffer = ptr, .len = length },
				                   _net_id, false);

				auto &dst = *reinterpret_cast<const EthernetAddr *>(msg.data.buffer);
				Logging::printf("Packet to " MAC_FMT "\n", MAC_SPLIT(&dst));

				_bus_network.send(msg);

				return (unsigned long)request_size;
			});

			if (inject)
				inject_irq();
		}

		void notify_power(unsigned value) override
		{
			if ((value & 3) != 0)
				return;

			_network_config.reset();
			reset();
		}

		uint32 dev_feature(unsigned word) override
		{
			if (word == 0)
				return 0u
				     | 1u <<  5 /* VIRTIO_NET_F_MAC */
				     | 1u << 16 /* VIRTIO_NET_F_STATUS */
				     ;

			return 0u;
		}

		void drv_feature_ack(unsigned value, uint32 ack_value) override { }

		uint32 drv_feature(unsigned value) override { return 0u; }
};

PARAM_HANDLER(virtio_net,
	      "virtio_net:mem,irq,msix,verbose- attach an virtio network device to the PCI bus",
	      "Example: 'virtio_net:0xe0200000,0x30,1,0'",
	      "If no bdf is given a free one is used.")
{
	unsigned const bdf = PciHelper::find_free_bdf(mb.bus_pcicfg, unsigned(argv[1]));
	if (bdf >= 1u << 16)
		Logging::panic("virtio_net: invalid bdf\n");

	auto const net_id = NicID::generate_new_id();

	MessageHostOp msg(MessageHostOp::OP_GET_MAC, net_id);
	if (!mb.bus_hostop.send(msg))
		Logging::panic("Could not get a MAC address");

	auto const irq_pin  =  3; /* PCI INTC# */
	auto const irq_line = 11; /* defined by acpicontroller dsdt for INTC# */
	auto const bar_base = argv[0];

	bool const msix    = (argv[2] == ~0UL) ? true  : !!argv[2];
	bool const verbose = (argv[3] == ~0UL) ? false : !!argv[3];

	if (argv[0] == ~0UL)
		Logging::panic("virtio_net: missing bar address");

	auto dev = new Virtio_network(mb.bus_network,
	                              mb.bus_irqlines,
	                              mb.bus_mem,
	                              mb.bus_memregion,
	                              Endian::hton64(msg.mac) >> 16,
	                              bar_base, irq_pin, irq_line,
	                              uint16(bdf), net_id,
	                              msix, verbose);

	mb.bus_pcicfg .add(dev, Virtio_network::receive_static<MessagePciConfig>);
	mb.bus_mem    .add(dev, Virtio_network::receive_static<MessageMem>);
	mb.bus_network.add(dev, Virtio_network::receive_static<MessageNetwork>);
	mb.bus_bios   .add(dev, Virtio_network::receive_static<MessageBios>);

	Logging::printf("%x:%x.%u virtio network\n",
	                (bdf >> 8) & 0xff, (bdf >> 3) & 0x1f, bdf & 0x7);
}
