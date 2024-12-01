/**
 * Virtio filesystem device
 *
 * Copyright (C) 2024-2025, Alexander Boettcher
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


#define S_IFMT  00170000
#define S_IFSOCK 0140000
#define S_IFLNK	 0120000
#define S_IFREG  0100000
#define S_IFBLK  0060000
#define S_IFDIR  0040000
#define S_IFCHR  0020000
#define S_IFIFO  0010000
#define S_ISUID  0004000
#define S_ISGID  0002000
#define S_ISVTX  0001000

#define S_ISLNK(m)	(((m) & S_IFMT) == S_IFLNK)
#define S_ISREG(m)	(((m) & S_IFMT) == S_IFREG)
#define S_ISDIR(m)	(((m) & S_IFMT) == S_IFDIR)
#define S_ISCHR(m)	(((m) & S_IFMT) == S_IFCHR)
#define S_ISBLK(m)	(((m) & S_IFMT) == S_IFBLK)
#define S_ISFIFO(m)	(((m) & S_IFMT) == S_IFIFO)
#define S_ISSOCK(m)	(((m) & S_IFMT) == S_IFSOCK)

/**********************
 * Structures of FUSE *
 **********************/

enum fuse_opcode
{
	FUSE_LOOKUP  =  1,
	FUSE_FORGET  =  2,
	FUSE_GETATTR =  3,
	FUSE_INIT    = 26,
	FUSE_OPENDIR = 27,
	FUSE_READDIR = 28,
	FUSE_RELDIR  = 29,
};

struct fuse_attr {
	uint64 ino;
	uint64 size;
	uint64 blocks;
	uint64 atime;
	uint64 mtime;
	uint64 ctime;
	uint32 atimensec;
	uint32 mtimensec;
	uint32 ctimensec;
	uint32 mode;
	uint32 nlink;
	uint32 uid;
	uint32 gid;
	uint32 rdev;
	uint32 blksize;
	uint32 flags;
} __attribute__((packed));

struct fuse_open_in {
	uint32 flags;
	uint32 open_flags;
} __attribute__((packed));

struct fuse_open_out {
	uint64 fh;
	uint32 open_flags;
	uint32 padding;
} __attribute__((packed));

struct fuse_in_header {
	uint32 len;
	uint32 opcode;
	uint64 unique;
	uint64 nodeid;
	uint32 uid;
	uint32 gid;
	uint32 pid;
	uint16 total_extlen; /* length of extensions in 8byte units */
	uint16 padding;
} __attribute__((packed));

struct fuse_out_header {
	uint32 len;
	int32  error;
	uint64 unique;
} __attribute__((packed));

struct fuse_init_in {
	uint32 major;
	uint32 minor;
	uint32 max_readahead;
	uint32 flags;
	uint32 flags2;
	uint32 unused[11];
} __attribute__((packed));

struct fuse_init_out {
	uint32 major;
	uint32 minor;
	uint32 max_readahead;
	uint32 flags;
	uint32 unused[12];
} __attribute__((packed));

struct fuse_getattr_in {
	uint32 getattr_flags;
	uint32 dummy;
	uint64 fh;
} __attribute__((packed));

struct fuse_attr_out {
	uint64 attr_valid;
	uint32 attr_valid_nsec;
	uint32 dummy;
	struct fuse_attr attr;
} __attribute__((packed));

struct fuse_dirent {
	uint64 ino;
	uint64 off;
	uint32 namelen;
	uint32 type;
	char   name[];
} __attribute__((packed));

struct fuse_entry_out {
	uint64 nodeid;		/* Inode ID */
	uint64 generation;	/* Inode generation: nodeid:gen must be unique for the fs's lifetime */
	uint64 entry_valid;	/* Cache timeout for the name */
	uint64 attr_valid;	/* Cache timeout for the attributes */
	uint32 entry_valid_nsec;
	uint32 attr_valid_nsec;
	struct fuse_attr attr;
} __attribute__((packed));

struct fuse_read_in {
	uint64 fh;
	uint64 offset;
	uint32 size;
	uint32 read_flags;
	uint64 lock_owner;
	uint32 flags;
	uint32 padding;
} __attribute__((packed));

struct fuse_release_in {
	uint64 fh;
	uint32 flags;
	uint32 release_flags;
	uint64 lock_owner;
} __attribute__((packed));

/* Virtio spec 1.2, 5.11 File system, 5.11.4 Device configuration layout */
struct Virtio_fs_config
{
	bool use_notify_queue { };

	unsigned read(unsigned const off) const
	{
		char const tag[36] = "mytag";

		auto const word = off / 4;

		switch (word) {
		case 0 ... 36 / 4 - 1: /* tag name */ /* XXX garbage behind tag */
			return *(unsigned *)(tag + word * 4);
		case 36 / 4:
			return  1u; /* number of queues */
		case 40 / 4:
			return 64u; /* notify_buf_size */
		default:
			Logging::panic("unknown fs config read %u\n", off);
			return 0;
		}
	}

	void write(unsigned const, unsigned const) {
		/* not allowed according to spec - nop */ }
};


class Virtio_fs: public StaticReceiver<Virtio_fs>, Virtio::Device
{
	private:

		DBus<MessageFs> &_bus_fs;

		unsigned const _fs_id;
//		unsigned    const  _device { 0x10002 };
		Seoul::Lock      _lock      { };
		Virtio_fs_config _fs_config { };

		unsigned const _blk_size = 512; /* XXX */

		~Virtio_fs();

	public:

		Virtio_fs(DBus<MessageIrqLines>  &bus_irqlines,
		          DBus<MessageMemRegion> &bus_memregion,
		          DBus<MessageFs>        &bus_fs,
		          uint64 const bar_addr,
		          uint8  const irq_pin,
		          uint8  const irq_line,
		          uint16 const bdf,
		          unsigned const fs_id)
		:
			Virtio::Device(bus_irqlines, bus_memregion, irq_pin, irq_line, bdf,
			               26 /* virtio type */,
			               0x01080001, /* pci class code (mass storage), sub class (other), prog if, rev. id */
			               bar_addr,
			               3 /* queues */),
			_bus_fs(bus_fs), _fs_id(fs_id)
		{
			_verbose = false;
		}

		bool receive(MessageFsCommit const &);

		bool receive(MessageBios &msg)
		{
			switch (msg.irq) {
			case BiosCommon::BIOS_RESET_VECTOR:

				Seoul::Lock::Guard guard(_lock);

				_fs_config = { };
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
						*msg.ptr = _fs_config.read(offset - BAR_OFFSET_CONFIG);
					else
						_fs_config.write(offset - BAR_OFFSET_CONFIG, *msg.ptr);

					return true;
				default:
					return Virtio::Device::receive(msg);
				}
			});
		}

		void notify (unsigned queue, bool more);
		void notify (unsigned queue) override { notify(queue, false); }

		enum { VIRTIO_FS_F_NOTIFICATION = 1 };

		uint32 dev_feature (unsigned const sel) override
		{
			if (sel == 0) return VIRTIO_FS_F_NOTIFICATION;

			return 0u;
		}

		void   drv_feature_ack (unsigned sel, uint32 value) override {
			Logging::printf("%s sel=%u value=%x\n", __func__, sel, value);
			if (sel == 0)
				_fs_config.use_notify_queue = value & VIRTIO_FS_F_NOTIFICATION;
		}

		uint32 drv_feature     (unsigned)         override {
			Logging::panic("%s\n", __func__);
			return 0u;
		}

		void notify_power(unsigned value) override
		{
			Logging::panic("virtio_fs:%s implement me value=%x", __func__, value);
#if 0
			Logging::printf("virtio_fs: power change %x\n", value);

			if ((value & 3) != 0)
				return;

			_fs_config = { };
			reset();
#endif
		}

		unsigned fuse_op_init   (auto const &, auto &, uint64);
		unsigned fuse_op_lookup (auto const &, auto &, uint64);
		unsigned fuse_op_getattr(auto const &, auto &, uint64);
		unsigned fuse_op_opendir(auto const &, auto &, uint64);
		unsigned fuse_op_readdir(auto const &, auto &, uint64,
		                         unsigned, bool, bool &);
		unsigned fuse_op_reldir (auto const &, auto &, uint64);
};


void Virtio_fs::notify(unsigned queue, bool more)
{
	/*
	 * queue 0 (hipro)
	 * queue 1 (notification) - iif VIRTIO_FS_F_NOTIFICATION negotiated
	 * queue 2 (request)
	 */

	if (queue != 1 && !_fs_config.use_notify_queue)
		Logging::printf("virtio_fs: notify %u %s", queue,
		                queue == 0 ? "HIPRO" :
		                queue == 1 && _fs_config.use_notify_queue ? "NOTIFY" : "REQUEST");

	if (queue >= QUEUES_MAX) {
		Logging::printf("unknown queue\n");
		return;
	}

	auto &used_queue = _queues[queue].queue;

	bool inject = used_queue.consume([&] (auto const &desc0, auto) {
		auto const request      = vmm_address(desc0.addr, desc0.len);
		auto const request_size = desc0.len;

		if (!request || request_size < sizeof(fuse_in_header)) {
			Logging::printf("virtio_fs, invalid request\n");
			return 0U;
		}

		auto const &in     = *reinterpret_cast<fuse_in_header *>(request);
		auto const  unique = in.unique;

		if (in.opcode == FUSE_FORGET) {
			Logging::printf("FUSE_FORGET ? nodeid=%u\n", in.nodeid);
			return request_size;
		}

		auto desc1 = used_queue.next_desc(desc0);
		auto desc2 = used_queue.next_desc(desc1);
		auto desc3 = used_queue.next_desc(desc2);
		auto desc4 = used_queue.next_desc(desc3);

		auto const response      = vmm_address(desc2.addr, desc2.len);
		auto const response_size = desc2.len;

		if (desc4.len)
			Logging::printf("next response desc2=%u desc3 %u desc4=%u ??? \n",
			                desc2.len, desc3.len, desc4.len);

		if (!response || response_size < sizeof(fuse_out_header)) {
			Logging::printf("virtio_fs, invalid response\n");
			return 0U;
		}

		auto  res   = 0u;
		int32 err   = 0 /* error propagation ? */;
		bool  delay = false;

		switch (in.opcode) {
		case FUSE_INIT   : res = fuse_op_init   (desc1, desc3, in.nodeid); break;
		case FUSE_LOOKUP : res = fuse_op_lookup (desc1, desc3, in.nodeid); break;
		case FUSE_GETATTR: res = fuse_op_getattr(desc1, desc3, in.nodeid); break;
		case FUSE_OPENDIR: res = fuse_op_opendir(desc1, desc3, in.nodeid); break;
		case FUSE_READDIR: res = fuse_op_readdir(desc1, desc3, in.nodeid,
		                                         queue,  more,     delay); break;
		case FUSE_RELDIR : res = fuse_op_reldir (desc1, desc3, in.nodeid); break;
		default:
			Logging::printf("virtio_fs: unknown opcode %u len=%u unique=%llx\n",
			                in.opcode, in.len, unique);
			break;
		}

		if (delay)
			return 0u;

		auto &out  = *reinterpret_cast<fuse_out_header *>(response);
		out.len    = sizeof(out) + res;
		out.error  = err;
		out.unique = unique;

		return request_size;
	});

	if (inject)
		inject_irq();
}


unsigned Virtio_fs::fuse_op_init(auto const &in, auto &out, uint64 const nodeid)
{
	if (!in.len || !out.len)
		return 0u;

	auto const in_addr = vmm_address(in.addr, in.len);
	auto const in_size = in.len;

	auto const out_addr = vmm_address(out.addr, out.len);
	auto const out_size = out.len;

	if (!in_addr || !out_addr || in_size  < sizeof(fuse_init_in)
	                          || out_size < sizeof(fuse_init_out))
		return 0u;

	auto const &init_in  = *reinterpret_cast<fuse_init_in *>(in_addr);
	auto       &init_out = *reinterpret_cast<fuse_init_out *>(out_addr);

	Logging::printf("%s nodeid=%llx - %u.%u flags=%x readahead=%llu\n", __func__,
	                nodeid, init_in.major, init_in.minor, init_in.flags,
	                init_in.max_readahead);

	init_out = { };

	/* XXX which flags needs to be initialized how XXX */
	init_out.major         = init_in.major;
	init_out.minor         = init_in.minor;
	init_out.flags         = 0;
	init_out.max_readahead = init_in.max_readahead;

	return out_size;
}


unsigned Virtio_fs::fuse_op_opendir(auto const &in, auto &out,
                                    uint64 const nodeid)
{
	if (!in.len || !out.len)
		return 0u;

	auto const in_addr = vmm_address(in.addr, in.len);
	auto const in_size = in.len;

	auto const out_addr = vmm_address(out.addr, out.len);
	auto const out_size = out.len;

	if (!in_addr || !out_addr || in_size  < sizeof(fuse_open_in)
	                          || out_size < sizeof(fuse_open_out))
		return 0u;

	auto const &open_in  = *reinterpret_cast<fuse_open_in  *>(in_addr);
	auto       &open_out = *reinterpret_cast<fuse_open_out *>(out_addr);

	if (_verbose)
		Logging::printf("%s: nodeid=%llu flags=%x %x\n", __func__, nodeid,
		                open_in.flags, open_in.open_flags);

	MessageFs msg(MessageFs::OPEN_DIR, _fs_id, nodeid);
	if (!_bus_fs.send(msg))
		return 0u;

	if (!msg.ok())
		return 0u;

	open_out.fh         = msg.fh;
	open_out.open_flags = 0; /* XXX */

	return out_size;
}


unsigned Virtio_fs::fuse_op_readdir(auto const &in, auto &out,
                                    uint64 const nodeid, unsigned queue,
                                    bool more, bool &delay)
{
	unsigned const read_dir_end = ~0u;

	if (!in.len || !out.len)
		return 0u;

	auto const in_addr = vmm_address(in.addr, in.len);
	auto const in_size = in.len;

	auto const out_addr = vmm_address(out.addr, out.len);
	auto const out_size = out.len;

	if (!in_addr || !out_addr || in_size  < sizeof(fuse_read_in)
	                          || out_size < sizeof(fuse_dirent))
		return 0u;

	auto const &read_in = *reinterpret_cast<fuse_read_in *>(in_addr);

	if (_verbose)
		Logging::printf("%s: fh=%llx offset=%llx+%lx r_flags=%x flags=%x\n",
		                __func__, read_in.fh, read_in.offset, read_in.size,
		                read_in.read_flags, read_in.flags);

	if (read_in.offset >= read_dir_end)
		return 0u;

/*
	if (read_in.offset)
		Logging::printf("---------- partial read not required by now\n");
*/

	unsigned ret_size = 0;

	auto &entry = *reinterpret_cast<fuse_dirent *>(out_addr + ret_size);

	auto const ei = 0;
	auto const ed = ei + sizeof(entry.ino) + sizeof(entry.off);
	auto const et = ed + sizeof(entry.namelen);
	auto const en = ed + sizeof(entry.namelen) + sizeof(entry.type);

	/* FUSE_DIRENT_ALIGN in Linux is set to 8 */
	enum { FUSE_DIRENT_ALIGN = 8 };

	MessageFs msg(MessageFs::READ_DIR, _fs_id, nodeid,
	              S_IFDIR, S_IFLNK, S_IFREG, ed, ei, et, en,
	              queue, FUSE_DIRENT_ALIGN);
	msg.fh = read_in.fh;

	msg.buffer.start  = out_addr;
	msg.buffer.size   = (read_in.size < out_size) ? read_in.size : out_size;
	msg.buffer.offset = 0;

	if (!_bus_fs.send(msg))
		return 0u;

	if (!msg.ok())
		return 0u;

	if (msg.buffer.offset == 0) {
		delay = true;
		return 0u;
	}

	if (msg.read_dir_empty())
		return 0u;

	while (ret_size < msg.buffer.offset)
	{
		auto &entry = *reinterpret_cast<fuse_dirent *>(out_addr + ret_size);

		if (_verbose)
			Logging::printf("%s:%u - inode=%u type='%s' %u len=%u '%s'",
			                __func__, __LINE__, entry.ino,
			                entry.type == S_IFREG ? "file" :
			                entry.type == S_IFDIR ? "dir" :
			                entry.type == S_IFLNK ? "lnk" : "unknown",
			                entry.type,
			                entry.namelen, entry.name);

		ret_size += msg.align(int32(sizeof(entry) + entry.namelen),
		                            FUSE_DIRENT_ALIGN);

		entry.off = ret_size; /* next direntry offset, for us for tracking */

		if (!more && ret_size >= msg.buffer.offset)
			entry.off = read_dir_end; /* no next entry */
	}

	return ret_size;
}


unsigned Virtio_fs::fuse_op_reldir(auto const &in, auto &out,
                                   uint64 const nodeid)
{
	if (!in.len)
		return 0u;

	auto const in_addr = vmm_address(in.addr, in.len);
	auto const in_size = in.len;

	if (!in_addr || in_size < sizeof(fuse_release_in))
		return 0u;

	auto const &release_in = *reinterpret_cast<fuse_release_in *>(in_addr);

	if (_verbose)
		Logging::printf("%s: nodeid=%llx fh=%llx flags=%x release_flags=%x lock_owner=%llx\n",
		                __func__, nodeid, release_in.fh, release_in.flags,
		                release_in.release_flags, release_in.lock_owner);

	MessageFs msg(MessageFs::CLOSE_DIR, _fs_id, nodeid);
	msg.fh = release_in.fh;

	_bus_fs.send(msg);

	return 0u;
}


unsigned Virtio_fs::fuse_op_lookup(auto const &in, auto &out,
                                   uint64 const nodeid)
{
	if (!in.len || !out.len)
		return 0u;

	auto const in_addr = vmm_address(in.addr, in.len);
	auto const in_size = in.len;

	auto const out_addr = vmm_address(out.addr, out.len);
	auto const out_size = out.len;

	if (!in_addr || !out_addr)
		return 0u;

	auto const   open_in =  reinterpret_cast<char const * const>(in_addr);
	auto       &open_out = *reinterpret_cast<fuse_entry_out *>(out_addr);

	if (out_size < sizeof(open_out))
		return 0;

	if (_verbose)
		Logging::printf("%s: nodeid=%llu name='%s' %lu\n",
		                __func__, nodeid, open_in, in_size);

	MessageFs msg(MessageFs::LOOKUP, _fs_id, nodeid,
	              S_IFDIR, S_IFLNK, S_IFREG);

	msg.buffer.start  = uintptr_t(open_in);
	msg.buffer.size   = in_size;
	msg.buffer.offset = 0;

	if (!_bus_fs.send(msg))
		return 0u;

	if (!msg.ok())
		return 0u;

	open_out = { };

	open_out.nodeid      = msg.fh; /* or parent id ? */
	open_out.generation  = 0; /* XXX ? */
	open_out.entry_valid = 10; /* 10s */
	open_out.attr_valid  = 10; /* 10s */
	open_out.entry_valid_nsec = 0; /* + 0ns */
	open_out.attr_valid_nsec  = 0; /* + 0ns */

	open_out.attr.ino    = msg.fh;
	open_out.attr.size   = msg.status_file_size();
	open_out.attr.blocks = msg.align(msg.status_file_size() / _blk_size, _blk_size);

	open_out.attr.mtime     = msg.status_mod_time();
	open_out.attr.mtimensec = 0;
#if 0
	uint64 atime;
	uint64 ctime;
	uint32 atimensec;
	uint32 ctimensec;
#endif
	uint32 const rwx = (msg.readable   ? 1 : 0) |
	                   (msg.writeable  ? 2 : 0) |
	                   (msg.executable ? 4 : 0);

	open_out.attr.mode = msg.status_file_type() | rwx;
#if 0
	uint32 nlink;
	uint32 uid;
	uint32 gid;
	uint32 rdev;
#endif
	open_out.attr.blksize = _blk_size;
#if 0
	uint32 flags;
#endif


	return out_size;
}


unsigned Virtio_fs::fuse_op_getattr(auto const &in, auto &out,
                                    uint64 const nodeid)
{
	if (!in.len || !out.len)
		return 0u;

	auto const in_addr = vmm_address(in.addr, in.len);
	auto const in_size = in.len;

	auto const out_addr = vmm_address(out.addr, out.len);
	auto const out_size = out.len;

	if (!in_addr || !out_addr || in_size  < sizeof(fuse_getattr_in)
	                          || out_size < sizeof(fuse_attr_out))
		return 0u;

	auto const &attr_in  = *reinterpret_cast<fuse_getattr_in *>(in_addr);
	auto       &attr_out = *reinterpret_cast<fuse_attr_out   *>(out_addr);

	if (_verbose)
		Logging::printf("%s: nodeid=%llx fh=%llu flags=%lx\n", __func__,
		                nodeid, attr_in.fh, attr_in.getattr_flags);

	MessageFs msg(MessageFs::GET_ATTR, _fs_id, nodeid,
	              S_IFDIR, S_IFLNK, S_IFREG);

	if (!_bus_fs.send(msg))
		return 0u;

	if (!msg.ok())
		return 0u;

	/* request by fuse_do_getattr of linux/fs/fuse/dir.c */

	attr_out = { };

	attr_out.attr_valid      = 10; /*   10s  */
	attr_out.attr_valid_nsec =  0; /* +  0ns */

	attr_out.attr.ino    = nodeid;

	attr_out.attr.size   = msg.status_file_size();
	attr_out.attr.blocks = msg.align(msg.status_file_size() / _blk_size, _blk_size);

	attr_out.attr.mtime     = msg.status_mod_time();
	attr_out.attr.mtimensec = 0;
#if 0
	uint64 atime;
	uint64 ctime;
	uint32 atimensec;
	uint32 ctimensec;
#endif
	uint32 const rwx = (msg.readable   ? 1 : 0) |
	                   (msg.writeable  ? 2 : 0) |
	                   (msg.executable ? 4 : 0);

	attr_out.attr.mode = msg.status_file_type() | rwx;
#if 0
	uint32 nlink;
	uint32 uid;
	uint32 gid;
	uint32 rdev;
#endif
	attr_out.attr.blksize = _blk_size;
#if 0
	uint32 flags;
#endif

	return out_size;
}


bool Virtio_fs::receive(MessageFsCommit const &msg)
{
	if (msg.fs_id != _fs_id)
		return false;

	sync_and_irq(_lock, [&]() {
		notify(msg.fs_delayed.queue, msg.more_read_dirs);
		return true;
	});

	return true;
}


PARAM_HANDLER(virtio_fs,
	      "virtio_fs:mem,bdf - attach an virtio fs to the PCI bus",
	      "Example: 'virtio_fs:0xe0200000'",
	      "If no bdf is given a free one is used.")
{
	unsigned const bdf = PciHelper::find_free_bdf(mb.bus_pcicfg, unsigned(argv[1]));
	if (bdf >= 1u << 16)
		Logging::panic("virtio_fs: invalid bdf\n");

	auto const irq_pin  =  3; /* PCI INTC# */
	auto const irq_line = 11; /* defined by acpicontroller dsdt for INTC# */
	auto const bar_base = argv[0];

	if (argv[0] == ~0UL)
		Logging::panic("virtio_fs: missing bar address");

	unsigned const fs_id = 1; /* XXX - allocator */

	auto dev = new Virtio_fs(mb.bus_irqlines, mb.bus_memregion, mb.bus_fs,
	                         bar_base, irq_pin, irq_line, uint16(bdf), fs_id);

	mb.bus_pcicfg   .add(dev, Virtio_fs::receive_static<MessagePciConfig>);
	mb.bus_mem      .add(dev, Virtio_fs::receive_static<MessageMem>);
	mb.bus_bios     .add(dev, Virtio_fs::receive_static<MessageBios>);
	mb.bus_fs_commit.add(dev, Virtio_fs::receive_static<MessageFsCommit>);

	Logging::printf("Virtio Filesystem\n");
}
