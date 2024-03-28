/** @file
 * SataDrive virtualisation.
 *
 * Copyright (C) 2008-2009, Bernhard Kauer <bk@vmmon.org>
 * Economic rights: Technische Universitaet Dresden (Germany)
 *
 * Copyright (C) 2014-2024, Alexander Boettcher
 *
 * This file is part of Seoul/Vancouver.
 *
 * Seoul/Vancouver is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2 as
 * published by the Free Software Foundation.
 *
 * Seoul/Vancouver is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License version 2 for more details.
 */

#include "nul/motherboard.h"
#include "host/dma.h"
#include "model/sata.h"


/**
 * A SATA drive. It contains the register set of a SATA drive and
 * speaks the SATA transport layer protocol with its
 * FISes (Frame Information Structure).
 *
 * State: unstable
 * Features: read,write,identify
 * Missing: better error handling, many commands
 */
class SataDrive : public FisReceiver, public StaticReceiver<SataDrive>
{
	private:

		#include "model/simplemem.h"

		DBus<MessageDisk>   &_bus_disk;
		FisReceiver         &_peer;

		unsigned       const _hostdisk;
		DiskParameter  const _params;

		bool           const _verbose;
		unsigned char        _multiple   { };
		unsigned char        _ctrl       { };
		unsigned char        _status     { };
		unsigned char        _error      { };

		unsigned             _regs  [ 4] { };
		unsigned             _splits[33] { };

		struct {
			uint64 prdbase;
			uint32 prd_count;
			uint32 slot_id;
		}  _dsf { };

		static unsigned constexpr DMA_DESCRIPTORS = 64;

		DmaDescriptor _dma[DMA_DESCRIPTORS] { };

		struct Operation {
			uint64 sector;
			size_t len;
			size_t prd; /* physical region descriptor */
			size_t lastoffset;
			bool   read;

			bool valid() const { return len; }
		} _resume { };

		enum Job_result { JOB_OK, JOB_RETRY, JOB_ERROR };

		/**
		 * A command is completed.
		 * We send a register d2h FIS to the host.
		 */
		void _complete_command()
		{
			// remove DRQ
			_status = uint8(_status & ~0x8);

			unsigned d2h[5];
			d2h[0] = _error << 24 | _status << 16 | 0x4000 | (_regs[0] & 0x0f00) | 0x34;
			d2h[1] = _regs[1];
			d2h[2] = _regs[2];
			d2h[3] = _regs[3] & 0xffff;
			// we overload the reserved value with the last transmitted command
			d2h[4] = _dsf.slot_id;
			// make sure we never reuse this!
			_dsf.slot_id = 0;
			_peer.receive_fis(5, d2h);
		}


		void _send_pio_setup_fis(unsigned short length, bool irq = false)
		{
			unsigned psf[5];

			// move from BSY to DRQ
			_status = uint8((_status & ~0x80) | 0x8);

			psf[0] = _error << 24 | _status << 16 | (irq ? 0x4000 : 0) | 0x2000 | (_regs[0] & 0x0f00) | 0x5f;
			psf[1] = _regs[1];
			psf[2] = _regs[2];
			psf[3] = _status << 24 | (_regs[3] & 0xffff);
			psf[4] = length;
			_peer.receive_fis(5, psf);
		}


		void _send_dma_setup_fis(bool const direction) const
		{
			unsigned dsf[7];
			memset(dsf, 0, sizeof(dsf));
			dsf[0] = 0x800 /* interrupt */ | (direction ? 0x200 : 0)| 0x41;
			_peer.receive_fis(7, dsf);
		}

		void _comreset()
		{
			// initialize state
			_regs[3] = 1;
			_regs[2] = 0;
			_regs[1] = _params.flags & DiskParameter::FLAG_ATAPI ? 0xeb1401 : 1;
			_status  = 0x40; // DRDY
			_error   = 1;
			_ctrl    = uint8(_regs[3] >> 24);

			memset(_splits, 0, sizeof(_splits));

			_complete_command();
		}

		void       _readwrite_sectors(bool, bool);
		void       _execute_sata_command();
		size_t     _push_data(size_t length, void *data, bool &irq);
		void       _build_identify_buffer(uint16 * const identify) const;
		Job_result _do_operation(struct Operation &);

		void receive_fis(size_t fislen, unsigned *fis) override;

		/*
		 * Noncopyable
		 */
		SataDrive             (SataDrive const &);
		SataDrive &operator = (SataDrive const &);

		~SataDrive();

		FisReceiver &_request_peer(DBus<MessageAhciSetDrive> &bus_ahci,
		                           unsigned port_id, unsigned ctrl_id)
		{
			// XXX put on SATA bus
			MessageAhciSetDrive msg(this, port_id);
			bool result = bus_ahci.send(msg, ctrl_id);

			if (!result || !msg.drive)
				Logging::panic("AHCI controller not found for drive\n");

			return *msg.drive;
		}

	public:

		bool receive(MessageDiskCommit &msg);

		SataDrive(DBus<MessageDisk>         &bus_disk,
		          DBus<MessageMemRegion>    *bus_memregion,
		          DBus<MessageMem>          *bus_mem,
		          DBus<MessageAhciSetDrive> &bus_ahci,
		          unsigned          const    hostdisk,
		          DiskParameter     const   &params,
		          unsigned          const    port_id,
		          unsigned          const    ctrl_id,
		          bool              const    verbose)
		: _bus_memregion(bus_memregion), _bus_mem(bus_mem), _bus_disk(bus_disk),
		  _peer(_request_peer(bus_ahci, port_id, ctrl_id)),
		  _hostdisk(hostdisk), _params(params), _verbose(verbose)
		{
			if (_verbose)
				Logging::printf("SATA disk %x flags %x sectors %zx\n",
				                hostdisk, _params.flags, size_t(_params.sectors));

			/* trigger explicitly comreset in set_drive due to this retry */
			MessageAhciSetDrive msg(this, port_id);
			bus_ahci.send(msg, ctrl_id);
		}
};


/**
 * We build the identify response in a buffer to allow to use _push_data.
 */
void SataDrive::_build_identify_buffer(uint16 * const identify) const
{
	for (unsigned i = 0; i < 20; i++)
		identify[27+i] = uint16(_params.name[2*i] << 8 | _params.name[2*i+1]);

	identify[47] = 0x80ff;
	identify[49] = 0x0300;
	identify[50] = 0x4001; // capabilities
	identify[53] = 0x0006; // bytes 64-70, 88 are valid
	identify[59] = uint16(0x100 | _multiple); // the multiple count

	unsigned maxlba28 = (_params.sectors >> 32) ? ~0u : unsigned(_params.sectors);
	identify[60] = uint16(maxlba28 & 0xffff);
	identify[61] = uint16(maxlba28 >> 16);

	identify[64] = 3;      // pio 3+4
	identify[75] = 0x1f;   // NCQ depth 32
	identify[76] = 0x002;   // disabled NCQ + 1.5gbit
	identify[80] = 1 << 6; // major version number: ata-6
	identify[83] = 0x4000 | 1 << 10; // lba48
	identify[86] = 1 << 10; // lba48 enabled
	identify[88] = 0x203f;  // ultra DMA5 enabled

	memcpy(identify+100, &_params.sectors, 8);

	identify[0xff] = 0xa5;
	unsigned char checksum = 0;
	for (unsigned i = 0; i < 512; i++)
		checksum += reinterpret_cast<unsigned char *>(identify)[i];
	identify[0xff] -= uint16(uint16(checksum) << 8);
}


/**
 * Push data to the user by doing DMA via the PRDs.
 *
 * Return the number of byte written.
 */
size_t SataDrive::_push_data(size_t length, void *data, bool &irq)
{
	if (!_dsf.prd_count) return 0;

	unsigned       prd = 0;
	size_t      offset = 0;

	while (offset < length && prd < _dsf.prd_count)
	{
		unsigned prdvalue[4];
		copy_in(uintptr_t(_dsf.prdbase + prd*16), prdvalue, 16);

		irq = irq || prdvalue[3] & 0x80000000;
		size_t sublen = (prdvalue[3] & 0x3fffff) + 1;
		if (sublen > length - offset) sublen = length - offset;
		copy_out(uintptr_t(union64(prdvalue[1], prdvalue[0])), reinterpret_cast<char *>(data)+offset, sublen);
		offset += sublen;
		prd++;
	}

	// mark them as consumed!
	_dsf.prd_count -= prd;
	return offset;
}


void SataDrive::_readwrite_sectors(bool const read, bool const lba48_ext)
{
	unsigned long long sector;
	size_t len;

	if (lba48_ext) {
		len = (_regs[3] & 0xffff) << 9;
		if (!len) len = 0x10000 << 9;

		sector = (uint64(_regs[2] & 0xffffff) << 24)
		         | uint64(_regs[1] & 0xffffff);
	} else {
		len = (_regs[3] & 0xff) << 9;
		if (!len) len = 0x100 << 9;

		sector = _regs[1] & 0x0fffffff;
	}

	bool const valid_slot_id = _dsf.slot_id < (sizeof(_splits) / sizeof(_splits[0]));

	assert(valid_slot_id);
	assert(_splits[_dsf.slot_id] == 0);

	if (!_dsf.prd_count || !valid_slot_id || _splits[_dsf.slot_id]) {
		Logging::printf("error ? check\n");
		return;
	}

	if (_resume.len)
		Logging::printf("resume operation pending - bug ahead\n");

	Operation new_job = { .sector = sector, .len = len, .prd = 0,
	                .lastoffset = 0, .read = read };

	switch (_do_operation(new_job)) {
	case Job_result::JOB_OK:
		break;
	case Job_result::JOB_RETRY:
		_resume = new_job;
		break;
	case Job_result::JOB_ERROR:
		Logging::printf("sata disk %s failed\n", __func__);
		break;
	}
}


SataDrive::Job_result SataDrive::_do_operation(struct Operation &contract)
{
	unsigned constexpr block_size = 512;
	unsigned constexpr block_mask = block_size - 1;
	unsigned long      max_len    = contract.len;

	while (contract.len) {

		auto job = contract;

		size_t transfer = 0;
		if (job.lastoffset) job.prd--;

		unsigned dmacount = 0;
		for (; job.prd  < _dsf.prd_count  &&
		       dmacount < DMA_DESCRIPTORS &&
		       job.len  > transfer
		     ; job.prd++, dmacount++)
		{
			unsigned prdvalue[4] { };

			bool ok = copy_in(uintptr_t(_dsf.prdbase + job.prd*16), prdvalue, 16);
			assert(ok);
			if (!ok)
				return Job_result::JOB_ERROR; /* guest cheats with us */

			VMM_MEMORY_BARRIER;

			size_t sublen = ((prdvalue[3] & 0x3fffff) + 1) - job.lastoffset;

			if (transfer + sublen > max_len)
				break;

			if (sublen > job.len - transfer)
				sublen = job.len - transfer;

			_dma[dmacount].byteoffset = uintptr_t(union64(prdvalue[1], prdvalue[0]) + job.lastoffset);
			_dma[dmacount].bytecount = sublen;

			transfer += sublen;
			job.lastoffset = 0;
		}

		// remove all entries that completely contribute to the even entry and split larger ones
		while (transfer & block_mask) {
			assert(dmacount);

			if (_dma[dmacount-1].bytecount > (transfer & block_mask)) {
				job.lastoffset = _dma[dmacount-1].bytecount - (transfer & block_mask);
				transfer &= ~block_mask;
			} else {
				transfer -= _dma[dmacount-1].bytecount;
				dmacount--;
			}
		}

		// are there bytes left to transfer, but we do not have enough PRDs?
		assert(dmacount);
		if (!dmacount && (job.len - transfer < block_size))
			return Job_result::JOB_RETRY; // job.len - transfer;

		/**
		 * The new entries do not fit into DMA_DESCRIPTORS, do a single sector transfer
		 * This means we have to do a read in our own buffer and than copy them out
		 */
		if (!dmacount)
		  Logging::panic("single sector transfer unimplemented!");

		_splits[_dsf.slot_id]++;

		MessageDisk msg(job.read ? MessageDisk::DISK_READ
		                         : MessageDisk::DISK_WRITE,
		                _hostdisk, _dsf.slot_id, job.sector, dmacount, _dma,
		                0, ~0ul, job.len - transfer);

		bool const ok = _bus_disk.send(msg);
		assert(ok);

		switch (msg.error) {
		case MessageDisk::DISK_STATUS_DMA_TOO_LARGE:
			/* revert not handled request */
			_splits[_dsf.slot_id]--;

			/* try again with smaller host limit */
			max_len = msg.more;
			continue;
		case MessageDisk::DISK_STATUS_BUSY:
			/* revert not handled request */
			_splits[_dsf.slot_id]--;

			/* _resume tracks the state to re-try this operation */
			return Job_result::JOB_RETRY;
		case MessageDisk::DISK_OK:
			break;
		default:
			Logging::panic("unhandled MessageDisk error %u\n", msg.error);
			return Job_result::JOB_RETRY;
		};

		job.sector += transfer >> 9;

		assert(job.len >= transfer);
		job.len -= transfer;

		/* commit finished job */
		contract = job;
		max_len  = contract.len;
	}

	return Job_result::JOB_OK;
}


void SataDrive::_execute_sata_command()
{
	bool lba48_command = false;
	bool read = false;
	unsigned char atacmd = (_regs[0] >> 16) & 0xff;

	switch (atacmd) {
	case 0x24: // READ SECTOR EXT
	case 0x25: // READ DMA EXT
	case 0x29: // READ MULTIPLE EXT
		lba48_command = true;
		[[fallthrough]];
	case 0x20: // READ SECTOR
	case 0xc4: // READ MULTIPLE
	case 0xc8: // READ DMA
		if (atacmd == 0x25 || atacmd == 0xc8)
			_send_dma_setup_fis(true);
		else
			_send_pio_setup_fis(512);
		_readwrite_sectors(true, lba48_command);
		break;
	case 0x40: /* read verify without any transfer of data */
	case 0x41:
	case 0x42:
		_complete_command();
		break;
	case 0x34: // WRITE SECTOR EXT
	case 0x35: // WRITE DMA EXT
	case 0x39: // WRITE MULTIPLE EXT
		lba48_command = true;
		[[fallthrough]];
	case 0x30: // WRITE SECTOR
	case 0xc5: // WRITE MULITIPLE
	case 0xca: // WRITE DMA
		if (atacmd == 0x35 || atacmd == 0xca)
			_send_dma_setup_fis(false);
		else
			_send_pio_setup_fis(512);
		_readwrite_sectors(false, lba48_command);
		break;
	case 0x60: // READ  FPDMA QUEUED
		read = true;
		[[fallthrough]];
	case 0x61: // WRITE FPDMA QUEUED
	{
		// some idiot has switched feature and sector count regs in this case!
		unsigned feature = _regs[3] & 0xffff;
		unsigned count = (_regs[0] >> 24) | ((_regs[2] >> 16) & 0xff00);
		_regs[3] = (_regs[3] & 0xffff0000) | count;
		_regs[0] = (_regs[0] & 0x00ffffff) | (feature << 24);
		_regs[2] = (_regs[2] & 0x00ffffff) | ((feature << 16) & 0xff000000);

		_send_dma_setup_fis(read);
		_readwrite_sectors(read, true);
		break;
	}
	case 0xc6: // SET MULTIPLE
		_multiple = _regs[3] & 0xff;
		_complete_command();
		break;
	case 0xe0: // STANDBY IMMEDIATE
		Logging::printf("ahci: STANDBY IMMEDIATE\n");
		_error  |= 4;
		_status |= 1;
		_complete_command();
		break;
	case 0xec: // IDENTIFY
	{
		if (_verbose)
			Logging::printf("ahci: IDENTIFY\n");

		// start pio command
		_send_pio_setup_fis(512);

		unsigned short identify[256] { };
		_build_identify_buffer(identify);
		bool irq = false;
		_push_data(512, identify, irq);

		if (_verbose)
			Logging::printf("ahci: IDENTIFY transferred\n");

		_complete_command();
		break;
	}
	case 0xef: // SET FEATURES
		if (_verbose)
			Logging::printf("ahci: SET FEATURES %x sc %x\n", _regs[0] >> 24, _regs[3] & 0xff);
		_complete_command();
		break;
	case 0xf5: // Security Freeze Lock - no-op
		_complete_command();
		break;
	default:
		Logging::panic("should execute command %x\n", _regs[0] >> 16);
	}
}


/**
 * Receive a FIS from the controller.
 */
void SataDrive::receive_fis(size_t fislen, unsigned *fis)
{
	if (fislen <= 1)
		return;

	switch (fis[0] & 0xff) {
	case 0x27:  // register FIS
		assert(fislen ==5);

		// remain in reset asserted state when receiving a normal command
		if (_ctrl & 0x4 && _regs[0] & 0x8000)  { _complete_command(); break; }

		// copyin to our registers
		memcpy(_regs, fis, sizeof(_regs));

		if (_regs[0] & 0x8000)
			_execute_sata_command();
		else {

			// update ctrl register
			_ctrl = uint8(_regs[3] >> 24);

			// software reset?
			if (_ctrl & 0x4)
				_comreset();
			else
				_complete_command();
		}

		break;
	case FIS_TYPE_DMA_SETUP:
		assert(fislen == 7);

		/*
		 * Note: reserved FIS fields dword 3 & 6 are re-used by the
		 * AHCI controller and SATA drive model to transfer the PRD count and
		 * the split slot id.
		 */
		_dsf = { .prdbase   = union64(fis[2], fis[1]),
		         .prd_count = fis[3],
		         .slot_id   = fis[6] };
		break;
	default:
		assert(!"Invalid H2D FIS!");
		break;
	}
}


bool SataDrive::receive(MessageDiskCommit &msg)
{
	if (msg.disknr != _hostdisk)
		return false;

	Seoul::Lock::Guard guard(_peer._lock);

	if ((msg.status == MessageDisk::DISK_STATUS_RESUME)) {
		if (_resume.valid()) {
			switch (_do_operation(_resume)) {
			case Job_result::JOB_OK:
				msg.status = MessageDisk::DISK_OK;
				_resume    = { }; /* done */
				return true;
			default:
				msg.status = MessageDisk::DISK_STATUS_BUSY;
				return true;
			}
		} else
			Logging::printf("satadisk: insane resume state - trouble ahead\n");
	}

	if (msg.usertag >= sizeof(_splits) / sizeof(_splits[0]))
		return false;

	assert(_splits[msg.usertag]);
	assert(msg.status == MessageDisk::DISK_OK);

	if (!--_splits[msg.usertag] && !_resume.valid()) {
		_dsf.slot_id = unsigned(msg.usertag);
		_complete_command();
	}

	return true;
}


PARAM_HANDLER(drive,
	      "drive:host_drive_id,controller,port[,verbose] - put a drive to the given port of an ahci controller by using a drive from host as backend.",
	      "Example: 'drive:0,1,2' to put the first host device drive on the third port of the second controller.")
{
	unsigned      const hostdisk = unsigned(argv[0]);
	DiskParameter       params   = { };

	MessageDisk msg0(hostdisk, &params);
	check0(!mb.bus_disk.send(msg0) || msg0.error != MessageDisk::DISK_OK,
	       "sata drive could not get disk %x parameters error %x",
	       hostdisk, msg0.error);

	auto drive = new SataDrive(mb.bus_disk, &mb.bus_memregion, &mb.bus_mem,
	                           mb.bus_ahcicontroller, hostdisk, params,
	                           unsigned(argv[2]), unsigned(argv[1]),
	                           (argv[3] == ~0UL) ? false : (argv[3] != 0UL));
	mb.bus_diskcommit.add(drive, SataDrive::receive_static<MessageDiskCommit>);
}
