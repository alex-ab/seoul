/**
 * Virtio filesystem device
 *
 * Copyright (C) 2024-2026, Alexander Boettcher
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

/* mode */
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

#define FUSE_LX_ENOENT     2
#define FUSE_LX_EIO        5
#define FUSE_LX_ENOSYS    38
#define FUSE_LX_ENOTEMPTY 39

/* flags */
#define O_ACCMODE 00000003
#define O_RDONLY  00000000
#define O_WRONLY  00000001
#define O_RDWR    00000002

#define FUSE_GETATTR_FH (1 << 0)

/**
 * Bitmasks for fuse_setattr_in.valid
 */
#define FUSE_LX_FATTR_MODE         (1 << 0)
#define FUSE_LX_FATTR_UID          (1 << 1)
#define FUSE_LX_FATTR_GID          (1 << 2)
#define FUSE_LX_FATTR_SIZE         (1 << 3)
#define FUSE_LX_FATTR_ATIME        (1 << 4)
#define FUSE_LX_FATTR_MTIME        (1 << 5)
#define FUSE_LX_FATTR_FH           (1 << 6)
#define FUSE_LX_FATTR_ATIME_NOW    (1 << 7)
#define FUSE_LX_FATTR_MTIME_NOW    (1 << 8)
#define FUSE_LX_FATTR_LOCKOWNER    (1 << 9)
#define FUSE_LX_FATTR_CTIME        (1 << 10)
#define FUSE_LX_FATTR_KILL_SUIDGID (1 << 11)

/**********************
 * Structures of FUSE *
 **********************/

enum fuse_opcode
{
	FUSE_LOOKUP          =  1,
	FUSE_FORGET          =  2,
	FUSE_GETATTR         =  3,
	FUSE_SETATTR         =  4,
//	FUSE_READLINK        =  5,
//	FUSE_SYMLINK         =  6,
//	FUSE_MKNOD           =  8,
	FUSE_MKDIR           =  9,
	FUSE_UNLINK          = 10,
	FUSE_RMDIR           = 11,
	FUSE_RENAME          = 12,
//	FUSE_LINK            = 13,
	FUSE_OPEN            = 14,
	FUSE_READ            = 15,
	FUSE_WRITE           = 16,
//	FUSE_STATFS          = 17,
	FUSE_RELEASE         = 18,
//	FUSE_FSYNC           = 20,
//	FUSE_SETXATTR        = 21,
	FUSE_GETXATTR        = 22,
//	FUSE_LISTXATTR       = 23,
//	FUSE_REMOVEXATTR     = 24,
	FUSE_FLUSH           = 25,
	FUSE_INIT            = 26,
	FUSE_OPENDIR         = 27,
	FUSE_READDIR         = 28,
	FUSE_RELDIR          = 29,
//	FUSE_FSYNCDIR        = 30,
//	FUSE_GETLK           = 31,
//	FUSE_SETLK           = 32,
//	FUSE_SETLKW          = 33,
//	FUSE_ACCESS          = 34,
	FUSE_CREATE          = 35,
//	FUSE_INTERRUPT       = 36,
//	FUSE_BMAP            = 37,
	FUSE_DESTROY         = 38,
//	FUSE_IOCTL           = 39,
//	FUSE_POLL            = 40,
//	FUSE_NOTIFY_REPLY    = 41,
//	FUSE_BATCH_FORGET    = 42,
//	FUSE_FALLOCATE       = 43,
//	FUSE_READDIRPLUS     = 44,
//	FUSE_RENAME2         = 45,
//	FUSE_LSEEK           = 46,
//	FUSE_COPY_FILE_RANGE = 47,
//	FUSE_SETUPMAPPING    = 48,
//	FUSE_REMOVEMAPPING   = 49,
	FUSE_SYNCFS          = 50,
//	FUSE_TMPFILE         = 51,
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

struct fuse_create_in {
	uint32 flags;
	uint32 mode;
	uint32 umask;
	uint32 open_flags;	/* FUSE_OPEN_... */
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
	uint16 max_background;
	uint16 congestion_threshold;
	uint32 max_write;
	uint32 time_gran;
	uint16 max_pages;
	uint16 map_alignment;
	uint32 flags2;
	uint32 unused[7];
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

struct fuse_setattr_in {
	uint32 valid;
	uint32 padding;
	uint64 fh;
	uint64 size;
	uint64 lock_owner;
	uint64 atime;
	uint64 mtime;
	uint64 ctime;
	uint32 atimensec;
	uint32 mtimensec;
	uint32 ctimensec;
	uint32 mode;
	uint32 unused4;
	uint32 uid;
	uint32 gid;
	uint32 unused5;
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

struct fuse_flush_in {
	uint64 fh;
	uint32 unused;
	uint32 padding;
	uint64 lock_owner;
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

struct fuse_write_in {
	uint64 fh;
	uint64 offset;
	uint32 size;
	uint32 write_flags;
	uint64 lock_owner;
	uint32 flags;
	uint32 padding;
} __attribute__((packed));

struct fuse_write_out {
	uint32 size;
	uint32 padding;
} __attribute__((packed));

struct fuse_release_in {
	uint64 fh;
	uint32 flags;
	uint32 release_flags;
	uint64 lock_owner;
} __attribute__((packed));

struct fuse_mkdir_in {
	uint32 mode;
	uint32 umask;
} __attribute__((packed));

struct fuse_rename_in {
	uint64 newdir;
} __attribute__((packed));

struct fuse_rename2_in {
	uint64 newdir;
	uint32 flags;
	uint32 padding;
} __attribute__((packed));

struct fuse_getxattr_in {
	uint32 size;
	uint32 padding;
} __attribute__((packed));

struct fuse_getxattr_out {
	uint32 size;
	uint32 padding;
} __attribute__((packed));

struct fuse_syncfs_in {
	uint64 padding;
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
			Logging::printf("virtiofs: unknown fs config read %u\n", off);
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
		          DBus<MessageMem>       &bus_mem,
		          DBus<MessageMemRegion> &bus_memregion,
		          DBus<MessageFs>        &bus_fs,
		          uint64 const bar_addr,
		          uint8  const irq_pin,
		          uint8  const irq_line,
		          uint16 const bdf,
		          uint32 const fs_id,
		          bool   const msix,
		          bool   const verbose)
		:
			Virtio::Device(bus_irqlines, bus_mem, bus_memregion,
			               irq_pin, irq_line, bdf,
			               26 /* virtio type */,
			               0x01080001, /* pci class code (mass storage), sub class (other), prog if, rev. id */
			               bar_addr,
			               3 /* queues */, msix),
			_bus_fs(bus_fs), _fs_id(fs_id)
		{
			_verbose = verbose;
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

		void notify(unsigned queue, bool more);
		void notify(unsigned queue) override { notify(queue, false); }

		enum { VIRTIO_FS_F_NOTIFICATION = 1 };

		uint32 dev_feature(unsigned const sel) override
		{
			if (sel == 0) return VIRTIO_FS_F_NOTIFICATION;

			return 0u;
		}

		void drv_feature_ack(unsigned sel, uint32 value) override
		{
			if (sel == 0)
				_fs_config.use_notify_queue = value & VIRTIO_FS_F_NOTIFICATION;
		}

		uint32 drv_feature(unsigned) override
		{
			Logging::printf("virtiofs: %s unsupported\n", __func__);
			return 0u;
		}

		void notify_power(unsigned value) override
		{
			Logging::printf("virtio_fs:%s implement me value=%x", __func__, value);
#if 0
			Logging::printf("virtio_fs: power change %x\n", value);

			if ((value & 3) != 0)
				return;

			_fs_config = { };
			reset();
#endif
		}

		typedef Virtio::Queue::desc Desc;

		unsigned fuse_op_destroy (uint64);
		unsigned fuse_op_init    (Desc const &, Desc &, uint64);
		unsigned fuse_op_unlink  (int32 &, Desc const &, Desc &, uint64);
		unsigned fuse_op_rmdir   (int32 &, Desc const &, Desc &, uint64);
		unsigned fuse_op_getxattr(Desc const &, Desc &, uint64, int32 &);
		unsigned fuse_op_create  (int32 &, Desc const &, Desc &, uint64, unsigned);
		unsigned fuse_op_mkdir   (int32 &, Desc const &, Desc &, uint64, unsigned);
		unsigned fuse_op_rename  (int32 &, Desc const &, uint64, unsigned);
		unsigned fuse_op_flush   (Desc const &, uint64, unsigned, bool &);
		unsigned fuse_op_syncfs  (Desc const &, uint64, unsigned, bool &);
		unsigned fuse_op_lookup  (int32 &, Desc const &, Desc &, uint64, unsigned);
		unsigned fuse_op_getattr (int32 &, Desc const &, Desc &, uint64, unsigned);
		unsigned fuse_op_setattr (int32 &, Desc const &, Desc &, uint64, unsigned);
		unsigned fuse_op_read    (int32 &, Desc const &, Desc &, uint64, unsigned, bool &);
		unsigned fuse_op_open    (int32 &, Desc const &, Desc &, uint64, bool);
		unsigned fuse_op_close   (Desc const &, Desc &, uint64, bool);
		unsigned fuse_op_readdir (int32 &, Desc const &, Desc &, uint64,
		                          unsigned, bool, bool &);
		unsigned fuse_op_write   (int32 &, Desc const &, Desc const &, uint64,
		                          unsigned, Desc &, bool &);

		void populate(MessageFs const &, uint64, struct fuse_attr &);
};


void Virtio_fs::populate(MessageFs const &msg, uint64 const ino,
                         struct fuse_attr &attr)
{
	attr.ino    = ino;
	attr.size   = msg.status_file_size();
	attr.blocks = msg.align(msg.status_file_size() / _blk_size, _blk_size);

	attr.mtime     = msg.status_mod_time();
	attr.mtimensec = 0;
#if 0
	uint64 atime;
	uint64 ctime;
	uint32 atimensec;
	uint32 ctimensec;
#endif
	uint32 const rwx = (msg.readable   ? 1 : 0) |
	                   (msg.writeable  ? 2 : 0) |
	                   (msg.executable ? 4 : 0);

	attr.mode = msg.status_file_type() | rwx;
#if 0
	uint32 nlink;
	uint32 uid;
	uint32 gid;
	uint32 rdev;
#endif
	attr.blksize = _blk_size;
#if 0
	uint32 flags;
#endif
}


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

	auto &used_queue = _queues[queue];

	bool inject = used_queue.queue.consume([&] (auto const &desc0, auto) {
		auto const request      = vmm_address(desc0.addr, desc0.len);
		auto const request_size = desc0.len;

		if (!request || request_size < sizeof(fuse_in_header)) {
			Logging::printf("virtio_fs, invalid request\n");
			return 0U;
		}

		auto const &in     = *reinterpret_cast<fuse_in_header *>(request);
		auto const  unique = in.unique;

		if (in.opcode == FUSE_FORGET) {
			Logging::printf("FUSE_FORGET ? nodeid=%llu\n", in.nodeid);
			return request_size;
		}

		auto desc1 = used_queue.queue.next_desc(desc0);
		auto desc2 = used_queue.queue.next_desc(desc1);
		auto desc3 = used_queue.queue.next_desc(desc2);
		auto desc4 = used_queue.queue.next_desc(desc3);

		auto const response = (in.opcode == FUSE_WRITE)   ? vmm_address(desc3.addr, desc3.len)
		                    : (in.opcode == FUSE_DESTROY) ? vmm_address(desc1.addr, desc1.len)
		                    : vmm_address(desc2.addr, desc2.len);
		auto const response_size = (in.opcode == FUSE_WRITE)   ? desc3.len
		                         : (in.opcode == FUSE_DESTROY) ? desc1.len
		                         : desc2.len;

		if (desc4.len && (in.opcode != FUSE_WRITE && in.opcode != FUSE_READ)) {
			auto desc5 = used_queue.queue.next_desc(desc4);
			Logging::printf("opcode=%d next response desc2=%u desc3 %u desc4=%u ??? desc5=%u \n",
			                in.opcode, desc2.len, desc3.len, desc4.len, desc5.len);
		}

		if (!response || response_size < sizeof(fuse_out_header)) {
			Logging::printf("virtio_fs, invalid response %lx %d\n",
			                response, response_size);
			Logging::printf("opcode=%d next response desc1=%u desc2=%u\n",
			                in.opcode, desc1.len, desc2.len);
			return 0U;
		}

		auto  res   = 0u;
		int32 err   = 0 /* error propagation */;
		bool  delay = false;

		switch (in.opcode) {
		case FUSE_DESTROY : res = fuse_op_destroy (in.nodeid); break;
		case FUSE_INIT    : res = fuse_op_init    (desc1, desc3, in.nodeid); break;
		case FUSE_UNLINK  : res = fuse_op_unlink  (err, desc1, desc3, in.nodeid); break;
		case FUSE_RENAME  : res = fuse_op_rename  (err, desc1, in.nodeid, queue); break;
		case FUSE_SYNCFS  : res = fuse_op_syncfs  (desc1, in.nodeid, queue, delay); break;
		case FUSE_FLUSH   : res = fuse_op_flush   (desc1, in.nodeid, queue, delay); break;
		case FUSE_RMDIR   : res = fuse_op_rmdir   (err, desc1, desc3, in.nodeid); break;
		case FUSE_GETXATTR: res = fuse_op_getxattr(desc1, desc3, in.nodeid, err); break;
		case FUSE_MKDIR   : res = fuse_op_mkdir   (err, desc1, desc3, in.nodeid, queue); break;
		case FUSE_CREATE  : res = fuse_op_create  (err, desc1, desc3, in.nodeid, queue); break;
		case FUSE_GETATTR : res = fuse_op_getattr (err, desc1, desc3, in.nodeid, queue); break;
		case FUSE_SETATTR : res = fuse_op_setattr (err, desc1, desc3, in.nodeid, queue); break;
		case FUSE_LOOKUP  : res = fuse_op_lookup  (err, desc1, desc3, in.nodeid, queue); break;
		case FUSE_READ    : res = fuse_op_read    (err, desc1, desc3, in.nodeid, queue, delay); break;
		case FUSE_WRITE   : res = fuse_op_write   (err, desc1, desc2, in.nodeid, queue, desc4, delay); break;
		case FUSE_OPEN    :
		case FUSE_OPENDIR : res = fuse_op_open    (err, desc1, desc3, in.nodeid, in.opcode == FUSE_OPENDIR); break;
		case FUSE_READDIR : res = fuse_op_readdir (err, desc1, desc3, in.nodeid, queue, more, delay); break;
		case FUSE_RELEASE :
		case FUSE_RELDIR  : res = fuse_op_close   (desc1, desc3, in.nodeid, in.opcode == FUSE_RELDIR); break;
		default:
			Logging::printf("virtio_fs: unsupported opcode %u len=%u unique=%llx\n",
			                in.opcode, in.len, unique);
			err = -FUSE_LX_ENOSYS;
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
		inject_irq_via_queue(used_queue);
}


unsigned Virtio_fs::fuse_op_init(Desc const &in, Desc &out, uint64 const nodeid)
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

	init_out = { };

	/* XXX which flags needs to be initialized how XXX */
	init_out.major         = init_in.major;
	init_out.minor         = init_in.minor;
	init_out.flags         = 0;
	init_out.max_readahead = init_in.max_readahead;
	init_out.max_write     = 8192; /* XXX choose */

	Logging::printf("%s nodeid=%llx - %u.%u flags=%x->%x "
	                "readahead=%u max_write=%u\n", __func__,
	                nodeid, init_out.major, init_out.minor, init_in.flags,
	                init_out.flags,
	                init_out.max_readahead, init_out.max_write);

	return out_size;
}


unsigned Virtio_fs::fuse_op_destroy(uint64 const nodeid)
{
	MessageFs msg(MessageFs::DESTROY, _fs_id, nodeid);

	_bus_fs.send(msg);

	return 0u;
}


unsigned Virtio_fs::fuse_op_unlink(int32 &err, Desc const &in, Desc &out, uint64 const nodeid)
{
	if (!in.len)
		return 0u;

	auto const in_addr = vmm_address(in.addr, in.len);
	auto const in_size = in.len;

	if (!in.addr)
		return 0u;

	auto name_size = in_size;
	auto name_ptr  = in_addr;

	MessageFs msg(MessageFs::UNLINK, _fs_id, nodeid);
	msg.buffer.start = name_ptr;
	msg.buffer.size  = name_size;

	if (!_bus_fs.send(msg) || !msg.ok())
		err = -FUSE_LX_ENOENT;

	return 0u;
}


unsigned Virtio_fs::fuse_op_mkdir(int32 &err, Desc const &in, Desc &out,
                                  uint64 const nodeid, unsigned const queue)
{
	if (!in.len || !out.len)
		return 0u;

	auto const in_addr = vmm_address(in.addr, in.len);
	auto const in_size = in.len;

	auto const out_addr = vmm_address(out.addr, out.len);
	auto const out_size = out.len;

	if (!in_addr || !out_addr || in_size  < sizeof(fuse_mkdir_in)
	                          || out_size < sizeof(fuse_entry_out))
		return 0u;

	auto const &mkdir_in  = *reinterpret_cast<fuse_mkdir_in  *>(in_addr);
	auto       &entry_out = *reinterpret_cast<fuse_entry_out *>(out_addr);

	auto name_size = in_size - sizeof(mkdir_in);
	auto name_ptr  = in_addr + sizeof(mkdir_in);

	MessageFs msg(MessageFs::MAKE_DIR, _fs_id, nodeid, queue,
	              S_IFDIR, S_IFLNK, S_IFREG);
	msg.buffer.start = name_ptr;
	msg.buffer.size  = name_size;

	if (!_bus_fs.send(msg) || !msg.ok()) {
		err = -FUSE_LX_ENOENT;
		return 0u;
	}

	entry_out = { };

	entry_out.nodeid      = msg.nodeid;
	entry_out.generation  = 0; /* XXX ? */
	entry_out.entry_valid = 10; /* 10s */
	entry_out.attr_valid  = 10; /* 10s */
	entry_out.entry_valid_nsec = 0; /* + 0ns */
	entry_out.attr_valid_nsec  = 0; /* + 0ns */

	populate(msg, msg.nodeid, entry_out.attr);

	return out_size;
}


unsigned Virtio_fs::fuse_op_rename(int32 &err, Desc const &in,
                                   uint64 const nodeid, unsigned const queue)
{
	if (!in.len)
		return 0u;

	auto const in_addr = vmm_address(in.addr, in.len);
	auto const in_size = in.len;

	if (!in_addr || in_size < sizeof(fuse_rename_in))
		return 0u;

	auto const &rename_in = *reinterpret_cast<fuse_rename_in *>(in_addr);
	auto const  str_size  = in_size - sizeof(fuse_rename_in);
	auto const  old_ptr   = in_addr + sizeof(fuse_rename_in);
	uintptr_t   old_size  = 0;

	if (_verbose) {
		{
			unsigned i = 0;
			for (; ((char *)old_ptr)[i] && i < str_size; i++) { }

			old_size = (i >= str_size) ? str_size : i + 1;
		}

		auto const  new_size = str_size - old_size;
		auto const  new_ptr  = in_addr + sizeof(fuse_rename_in) + old_size;

		Logging::printf("%s old %lu %s\n", __func__, old_size, (char *)old_ptr);
		Logging::printf("%s new %lu %s\n", __func__, new_size, (char *)new_ptr);
	}

	MessageFs msg(MessageFs::RENAME, _fs_id, nodeid);
	msg.buffer.start = old_ptr;
	msg.buffer.size  = str_size;
	msg.fh           = rename_in.newdir;

	if (!_bus_fs.send(msg) || !msg.ok()) {
		err = -FUSE_LX_ENOENT;
		return 0u;
	}

	return 0u;
}


unsigned Virtio_fs::fuse_op_create(int32 &err, Desc const &in, Desc &out,
                                   uint64 const nodeid, unsigned const queue)
{
	if (!in.len || !out.len)
		return 0u;

	auto const in_addr = vmm_address(in.addr, in.len);
	auto const in_size = in.len;

	auto const out_addr = vmm_address(out.addr, out.len);
	auto const out_size = out.len;

	if (!in_addr || !out_addr || in_size  < sizeof(fuse_create_in)
	                          || out_size < sizeof(fuse_entry_out) +
	                                        sizeof(fuse_open_out))
		return 0u;

	auto const &create_in = *reinterpret_cast<fuse_create_in *>(in_addr);
	auto       &entry_out = *reinterpret_cast<fuse_entry_out *>(out_addr);
	auto       &open_out  = *reinterpret_cast<fuse_open_out  *>(out_addr + sizeof(entry_out));

	auto name_size = in_size - sizeof(create_in);
	auto name_ptr  = in_addr + sizeof(create_in);

	if (_verbose)
		Logging::printf("%s flags=%x mode=%x umask=%x, open_flags=%x %s\n",
		                __func__,
		                create_in.flags, create_in.mode,
		                create_in.umask, create_in.open_flags,
		                (char *)name_ptr);

	MessageFs msg(MessageFs::CREATE, _fs_id, nodeid, queue,
	              S_IFDIR, S_IFLNK, S_IFREG);
	msg.buffer.start = name_ptr;
	msg.buffer.size  = name_size;

	auto const fl = (create_in.flags & O_ACCMODE);
	msg.readable   = fl == O_RDONLY || fl == O_RDWR;
	msg.writeable  = fl == O_WRONLY || fl == O_RDWR;
	msg.executable = false;

	if (!_bus_fs.send(msg) || !msg.ok()) {
		err = -FUSE_LX_ENOENT;
		return 0u;
	}

	open_out.fh         = msg.fh;
	open_out.open_flags = 0;

	entry_out = { };

	entry_out.nodeid      = msg.nodeid;
	entry_out.generation  = 0; /* XXX ? */
	entry_out.entry_valid = 10; /* 10s */
	entry_out.attr_valid  = 10; /* 10s */
	entry_out.entry_valid_nsec = 0; /* + 0ns */
	entry_out.attr_valid_nsec  = 0; /* + 0ns */

	populate(msg, msg.nodeid, entry_out.attr);

	return out_size;
}


unsigned Virtio_fs::fuse_op_flush(Desc     const &in   , uint64 const  nodeid,
                                  unsigned const  queue, bool         &delay)
{
	if (!in.len)
		return 0u;

	auto const in_addr = vmm_address(in.addr, in.len);
	auto const in_size = in.len;

	if (!in_addr || in_size  < sizeof(fuse_flush_in))
		return 0u;

	auto const &flush_in = *reinterpret_cast<fuse_flush_in *>(in_addr);

	MessageFs msg(MessageFs::SYNC, _fs_id, nodeid, queue);
	msg.fh = flush_in.fh;

	if (!_bus_fs.send(msg) || !msg.ok())
		return 0u;

	if (msg.buffer.offset == 0) {
		delay = true;
		return 0u;
	}

	return 0u;
}


unsigned Virtio_fs::fuse_op_syncfs(Desc     const &in   , uint64 const  nodeid,
                                   unsigned const  queue, bool         &delay)
{
	if (!in.len)
		return 0u;

	auto const in_addr = vmm_address(in.addr, in.len);
	auto const in_size = in.len;

	if (!in_addr || in_size < sizeof(fuse_syncfs_in))
		return 0u;

	MessageFs msg(MessageFs::SYNC, _fs_id, nodeid, queue);

	if (!_bus_fs.send(msg) || !msg.ok())
		return 0u;

	if (msg.buffer.offset == 0) {
		Logging::printf("%s delayed\n", __func__);
		delay = true;
		return 0u;
	}

	Logging::printf("%s done\n", __func__);

	return 0u;
}


unsigned Virtio_fs::fuse_op_open(int32 &err, Desc const &in, Desc &out,
                                 uint64 const nodeid, bool const isdir)
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

	/* flags are presumably FUSE_ASYNC_DIO == 0x8000 and friends */
	if (_verbose)
		Logging::printf("%s: nodeid=%llu flags=%x %x\n", __func__, nodeid,
		                open_in.flags, open_in.open_flags);

	MessageFs msg(isdir ? MessageFs::OPEN_DIR : MessageFs::OPEN_FILE,
	              _fs_id, nodeid);

	auto const fl  = open_in.flags & O_ACCMODE;
	msg.readable   = fl == O_RDONLY || fl == O_RDWR;
	msg.writeable  = fl == O_WRONLY || fl == O_RDWR;
	msg.executable = false;

	if (!_bus_fs.send(msg) || !msg.ok()) {
		err = -FUSE_LX_ENOENT;
		return 0u;
	}

	open_out.fh         = msg.fh;
	open_out.open_flags = 0;

	return out_size;
}


unsigned Virtio_fs::fuse_op_readdir(int32 &err, Desc const &in, Desc &out,
                                    uint64 const  nodeid, unsigned const  queue,
                                    bool   const  more  , bool           &delay)
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
		Logging::printf("%s: fh=%llx offset=%llx+%x r_flags=%x flags=%x\n",
		                __func__, read_in.fh, read_in.offset, read_in.size,
		                read_in.read_flags, read_in.flags);

	if (read_in.offset >= read_dir_end)
		return 0u;

	unsigned ret_size = 0;

	auto &entry = *reinterpret_cast<fuse_dirent *>(out_addr + ret_size);

	auto const ei = 0;
	auto const ed = ei + sizeof(entry.ino) + sizeof(entry.off);
	auto const et = ed + sizeof(entry.namelen);
	auto const en = ed + sizeof(entry.namelen) + sizeof(entry.type);

	/* FUSE_DIRENT_ALIGN in Linux is set to 8 */
	enum { FUSE_DIRENT_ALIGN = 8 };

	MessageFs msg(MessageFs::READ_DIR, _fs_id, nodeid, queue,
	              S_IFDIR, S_IFLNK, S_IFREG, ed, ei, et, en,
	              FUSE_DIRENT_ALIGN);
	msg.fh = read_in.fh;

	msg.buffer.start  = out_addr;
	msg.buffer.size   = (read_in.size < out_size) ? read_in.size : out_size;
	msg.buffer.offset = 0;

	if (!_bus_fs.send(msg) || !msg.ok()) {
		err = -FUSE_LX_ENOENT;
		return 0u;
	}

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
			Logging::printf("%s:%u - inode=%llu type='%s' %u len=%u '%s'",
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


unsigned Virtio_fs::fuse_op_rmdir(int32 &err, Desc const &in, Desc &out,
                                  uint64 const nodeid)
{
	if (!in.len)
		return 0u;

	auto const in_addr = vmm_address(in.addr, in.len);
	auto const in_size = in.len;

	MessageFs msg(MessageFs::REMOVE_DIR, _fs_id, nodeid);
	msg.buffer.start = in_addr;
	msg.buffer.size  = in_size;

	if (!_bus_fs.send(msg) || !msg.ok()) {
		err = -FUSE_LX_ENOTEMPTY;
		return 0u;
	}

	return 0u;
}


unsigned Virtio_fs::fuse_op_close(Desc   const &in    , Desc &out,
                                  uint64 const  nodeid, bool  isdir)
{
	if (!in.len)
		return 0u;

	auto const in_addr = vmm_address(in.addr, in.len);
	auto const in_size = in.len;

	if (!in_addr || in_size < sizeof(fuse_release_in))
		return 0u;

	auto const &release_in = *reinterpret_cast<fuse_release_in *>(in_addr);

	if (_verbose)
		Logging::printf("%s: nodeid=%llx fh=%llx flags=%x release_flags=%x "
		                "lock_owner=%llx %s\n",
		                __func__, nodeid, release_in.fh, release_in.flags,
		                release_in.release_flags, release_in.lock_owner,
		                isdir ? " dir" : " file");

	MessageFs msg(isdir ? MessageFs::CLOSE_DIR : MessageFs::CLOSE_FILE, _fs_id, nodeid);
	msg.fh = release_in.fh;

	_bus_fs.send(msg);

	return 0u;
}


unsigned Virtio_fs::fuse_op_lookup(int32 &err, Desc const &in, Desc &out,
                                   uint64 const nodeid, unsigned const queue)
{
	if (!in.len || !out.len)
		return 0u;

	auto const in_addr = vmm_address(in.addr, in.len);
	auto const in_size = in.len;

	auto const out_addr = vmm_address(out.addr, out.len);
	auto const out_size = out.len;

	if (!in_addr || !out_addr)
		return 0u;

	auto const  open_in   =  reinterpret_cast<char const *>(in_addr);
	auto       &entry_out = *reinterpret_cast<fuse_entry_out *>(out_addr);

	if (out_size < sizeof(entry_out))
		return 0;

	if (_verbose)
		Logging::printf("%s: nodeid=%llu name='%s' %u\n",
		                __func__, nodeid, open_in, in_size);

	MessageFs msg(MessageFs::LOOKUP, _fs_id, nodeid, queue,
	              S_IFDIR, S_IFLNK, S_IFREG);

	msg.buffer.start  = uintptr_t(open_in);
	msg.buffer.size   = in_size;
	msg.buffer.offset = 0;

	if (!_bus_fs.send(msg) || !msg.ok()) {
		err = -FUSE_LX_ENOENT;
		return out_size;
	}

	entry_out = { };

	entry_out.nodeid      = msg.fh; /* or parent id ? */
	entry_out.generation  = 0; /* XXX ? */
	entry_out.entry_valid = 10; /* 10s */
	entry_out.attr_valid  = 10; /* 10s */
	entry_out.entry_valid_nsec = 0; /* + 0ns */
	entry_out.attr_valid_nsec  = 0; /* + 0ns */

	populate(msg, msg.fh, entry_out.attr);

	return out_size;
}


unsigned Virtio_fs::fuse_op_getattr(int32 &err, Desc const &in, Desc &out,
                                    uint64 const nodeid, unsigned const queue)
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
		Logging::printf("%s: nodeid=%llx fh=%llu flags=%x\n", __func__,
		                nodeid, attr_in.fh, attr_in.getattr_flags);

	MessageFs msg(MessageFs::GET_ATTR, _fs_id, nodeid, queue,
	              S_IFDIR, S_IFLNK, S_IFREG);

	if (attr_in.getattr_flags & FUSE_GETATTR_FH)
		msg.fh = attr_in.fh;

	if (!_bus_fs.send(msg) || !msg.ok()) {
		err = -FUSE_LX_ENOENT;
		return 0u;
	}

	/* request by fuse_do_getattr of linux/fs/fuse/dir.c */

	attr_out = { };

	attr_out.attr_valid      = 10; /*   10s  */
	attr_out.attr_valid_nsec =  0; /* +  0ns */

	populate(msg, nodeid, attr_out.attr);

	return out_size;
}


unsigned Virtio_fs::fuse_op_setattr(int32 &err, Desc const &in, Desc &out,
                                    uint64 const nodeid, unsigned const queue)
{
	if (!in.len || !out.len)
		return 0u;

	auto const in_addr = vmm_address(in.addr, in.len);
	auto const in_size = in.len;

	auto const out_addr = vmm_address(out.addr, out.len);
	auto const out_size = out.len;

	if (!in_addr || !out_addr || in_size  < sizeof(fuse_setattr_in)
	                          || out_size < sizeof(fuse_attr_out))
		return 0u;

	auto const &attr_in  = *reinterpret_cast<fuse_setattr_in *>(in_addr);
	auto       &attr_out = *reinterpret_cast<fuse_attr_out   *>(out_addr);

//	if (_verbose)
		Logging::printf("%s: nodeid=%llx fh=%llu valid=%x size=%llx\n",
		                __func__,
		                nodeid, attr_in.fh, attr_in.valid, attr_in.size);

	MessageFs msg(MessageFs::SET_ATTR, _fs_id, nodeid, queue,
	              S_IFDIR, S_IFLNK, S_IFREG);

	/* XXX manage all FUSE_LX_* ? */
	auto const warn_valid = attr_in.valid & ~uint32(FUSE_LX_FATTR_FH |
	                                                FUSE_LX_FATTR_SIZE |
	                                                FUSE_LX_FATTR_LOCKOWNER);

	if (warn_valid)
		Logging::printf("%s: unsupported valid bit=%x\n",
		                __func__, attr_in.valid);

	if (attr_in.valid & FUSE_LX_FATTR_FH)
		msg.fh = attr_in.fh;

	if (attr_in.valid & FUSE_LX_FATTR_SIZE) {
		msg.buffer.start = 1; /* valid */
		msg.buffer.size  = attr_in.size;
	}

	if (!_bus_fs.send(msg) || !msg.ok()) {
		err = -FUSE_LX_ENOENT;
		return 0u;
	}

	/* request by fuse_do_getattr of linux/fs/fuse/dir.c */

	attr_out = { };

	attr_out.attr_valid      = 10; /*   10s  */
	attr_out.attr_valid_nsec =  0; /* +  0ns */

	populate(msg, nodeid, attr_out.attr);

	return out_size;
}


unsigned Virtio_fs::fuse_op_getxattr(Desc     const &in,
                                     Desc           &out,
                                     uint64   const  nodeid,
                                     int32          &err)
{
	if (!in.len || !out.len)
		return 0u;

	auto const in_addr = vmm_address(in.addr, in.len);
	auto const in_size = in.len;

	auto const out_addr = vmm_address(out.addr, out.len);
	auto const out_size = out.len;

	if (!in_addr || !out_addr || in_size  < sizeof(fuse_getxattr_in)
	                          || out_size < sizeof(fuse_getxattr_out))
		return 0u;

	auto const &attr_in  = *reinterpret_cast<fuse_getxattr_in  *>(in_addr);
	auto       &attr_out = *reinterpret_cast<fuse_getxattr_out *>(out_addr);

	auto name_size = in_size - sizeof(attr_in);
	auto name_ptr  = in_addr + sizeof(attr_in);

	Logging::printf("%s: nodeid=%llx size=%x name_size=%lu name='%s'\n", __func__,
	                nodeid, attr_in.size, name_size, (char *)name_ptr);

	err = -FUSE_LX_ENOSYS;

	attr_out = { };

	return out_size;
}


unsigned Virtio_fs::fuse_op_read(int32 &err, Desc const &in, Desc &first_desc,
                                 uint64 const nodeid, unsigned queue,
                                 bool &delay)
{
	if (!in.len)
		return 0u;

	auto const in_addr = vmm_address(in.addr, in.len);
	auto const in_size = in.len;

	if (!in_addr || in_size < sizeof(fuse_read_in))
		return 0u;

	auto const &read_in = *reinterpret_cast<fuse_read_in *>(in_addr);

	if (_verbose)
		Logging::printf("%s: fh=%llx offset=%llx+%x r_flags=%x flags=%x\n",
		                __func__, read_in.fh, read_in.offset, read_in.size,
		                read_in.read_flags, read_in.flags);

	uint64 out_addr_offset = 0;
	uint64 ret_read        = 0;

	auto out = first_desc;

	do {
		if (!out.len)
			return 0u;

		if (_verbose)
			Logging::printf("%s: fh=%llx offset=%llx+%x r_flags=%x flags=%x\n",
			                __func__, read_in.fh, read_in.offset + ret_read,
			               read_in.size, read_in.read_flags, read_in.flags);

		auto const out_addr = vmm_address(out.addr, out.len);
		auto const out_size = out.len;

		if (!out_addr || !out_size)
			return 0u;

		auto &used_queue = _queues[queue].queue;

		MessageFs msg(MessageFs::READ_FILE, _fs_id, nodeid, queue);

		msg.buffer.start  = out_addr + out_addr_offset;
		msg.buffer.size   = (read_in.size - ret_read < out_size - out_addr_offset)
		                  ?  read_in.size - ret_read : out_size - out_addr_offset;
		msg.buffer.offset = read_in.offset + ret_read;

		if (!_bus_fs.send(msg) || !msg.ok()) {
			err = -FUSE_LX_ENOENT;
			return 0u;
		}

		if (msg.buffer.offset == 0) {
			delay = true;
			return 0u;
		}

		if (_verbose)
			Logging::printf("msg offset %llu size=%llu -> ret_read=%llu -> %llu-%llu -> %llu\n",
			                msg.buffer.offset, msg.buffer.size,
			                ret_read, msg.buffer.offset, read_in.offset, msg.buffer.offset - read_in.offset);

		ret_read = msg.buffer.offset - read_in.offset;

		uint64 locate_pos = 0;
		out = first_desc;
		do {
			locate_pos += out.len;

			if (ret_read < locate_pos) {
				out_addr_offset = ret_read - (locate_pos - out.len);

				auto desc = used_queue.next_desc(out);
				if (!desc.len) {
					/* break loop for now -- actually size is required of file */
					out = desc;
				}

				break;
			}

			out = used_queue.next_desc(out);
			out_addr_offset = 0;
		} while (out.len && locate_pos < ret_read);

	} while (out.len && ret_read < read_in.size);

	if (_verbose)
		Logging::printf("read file ret_read=%llu<?%u delay=%d\n", ret_read, read_in.size, delay);

	return unsigned(ret_read);
}


unsigned Virtio_fs::fuse_op_write(int32 &err, Desc const &in, Desc const &data_in,
                                  uint64 const nodeid, unsigned const queue,
                                  Desc &out, bool &delay)
{
	if (!in.len || !out.len) {
		err = -FUSE_LX_EIO;
		return 0u;
	}

	auto const in_addr = vmm_address(in.addr, in.len);
	auto const in_size = in.len;

	auto const data_addr = vmm_address(data_in.addr, data_in.len);
	auto const data_size = data_in.len;

	auto const out_addr = vmm_address(out.addr, out.len);
	auto const out_size = out.len;

	if (!in_addr || !out_addr || in_size  < sizeof(fuse_write_in)
	                          || out_size < sizeof(fuse_write_out)) {
		err = -FUSE_LX_EIO;
		return 0u;
	}

	auto const &write_in  = *reinterpret_cast<fuse_write_in  *>(in_addr);
	auto       &write_out = *reinterpret_cast<fuse_write_out *>(out_addr);

	if (data_size != write_in.size) {
		if (_verbose)
			Logging::printf("%s: insane data size - skip write\n", __func__);
		err = -FUSE_LX_EIO;
		return 0u;
	}

	if (_verbose)
		Logging::printf("%s: write in fh=%llx off=%llx size=%x "
		                "write_flags=%x lock_owner=%llx flags=%x\n",
		                __func__,
		                write_in.fh,
		                write_in.offset,
		                write_in.size,
		                write_in.write_flags,
		                write_in.lock_owner,
		                write_in.flags);

	MessageFs msg(MessageFs::WRITE_FILE, _fs_id, nodeid, queue);
	msg.fh = write_in.fh;

	msg.buffer.start  = data_addr;
	msg.buffer.size   = data_size;
	msg.buffer.offset = write_in.offset;

	if (!_bus_fs.send(msg) || !msg.ok()) {
		write_out.size = 0u;
		err = -FUSE_LX_EIO;
		return 0u;
	}

	if (msg.buffer.offset == 0) {
		delay = true;
		return 0u;
	}

	write_out.size = write_in.size;

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
	      "virtio_fs:mem,bdf,msix,verbose - attach an virtio fs to the PCI bus",
	      "Example: 'virtio_fs:0xe0200000,1,0'",
	      "If no bdf is given a free one is used.")
{
	unsigned const bdf = PciHelper::find_free_bdf(mb.bus_pcicfg, unsigned(argv[1]));
	if (bdf >= 1u << 16)
		Logging::panic("virtio_fs: invalid bdf\n");

	auto const irq_pin  =  3; /* PCI INTC# */
	auto const irq_line = 11; /* defined by acpicontroller dsdt for INTC# */
	auto const bar_base = argv[0];

	bool const msix    = (argv[2] == ~0UL) ? true  : !!argv[2];
	bool const verbose = (argv[3] == ~0UL) ? false : !!argv[3];

	if (argv[0] == ~0UL)
		Logging::panic("virtio_fs: missing bar address");

	unsigned const fs_id = 1; /* XXX - allocator */

	auto dev = new Virtio_fs(mb.bus_irqlines, mb.bus_mem, mb.bus_memregion,
	                         mb.bus_fs, bar_base, irq_pin, irq_line,
	                         uint16(bdf), fs_id, msix, verbose);

	mb.bus_pcicfg   .add(dev, Virtio_fs::receive_static<MessagePciConfig>);
	mb.bus_mem      .add(dev, Virtio_fs::receive_static<MessageMem>);
	mb.bus_bios     .add(dev, Virtio_fs::receive_static<MessageBios>);
	mb.bus_fs_commit.add(dev, Virtio_fs::receive_static<MessageFsCommit>);

	Logging::printf("%x:%x.%u virtio filesystem - %s\n",
	                (bdf >> 8) & 0xff, (bdf >> 3) & 0x1f, bdf & 0x7,
	                msix ? "MSIX" : "GSI");
}
