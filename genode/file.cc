/*
 * \brief  VMM MessageFile to Genode file-system session adpater
 * \author Alexander Boettcher
 */

/*
 * Copyright (C) 2025-2026 Alexander Boettcher
 *
 * This file is distributed under the terms of the GNU General Public License
 * version 2.
 *
 * The code is partially based on the Seoul VMM, which is distributed
 * under the terms of the GNU General Public License version 2.
 *
 * Modifications by Intel Corporation are contributed under the terms and
 * conditions of the GNU General Public License version 2.
 */

#include "file.h"


void Seoul::Filesystem::_close_dir(MessageFs &msg)
{
	if (msg.fh == root.value) /* keep root dir open */
		return;

	Genode::Mutex::Guard guard(mutex);

	with_dir(msg.nodeid, [&](auto &dir) {
		if (dir.dir.value != msg.fh)
			warning("close_dir: fh does not match inode ", dir.dir.value, " ", msg.fh);

		log("CLOSE_DIR: nodeid=", msg.nodeid, " fh=", msg.fh);

		Dir_handle h { msg.fh };

		try {
			fs.close(h);
			dir.dir.value = 0;

		} catch (...) {
			error("closing dir failed");
		}
	}, [&]() {
		error("CLOSE_DIR: unknown nodeid ", msg.nodeid);
	});
}


void Seoul::Filesystem::_open_file(MessageFs &msg)
{
	Genode::Mutex::Guard guard(mutex);

	with_file(msg.nodeid, [&](auto &file) {
		try {
			/* XXX check whether open file.handle matches mode/create, may be open */
			if (!file.handle.value) {
				/* XXX attributes mode, create */
				file.handle = fs.file(file.dir, file.name.string(), READ_ONLY, false);
			}

			msg.fh = file.handle.value;

			if (!msg.fh) {
				error(" open file: failed ", file.name);
				msg.fail();
			}
		} catch (...) {
			error(" open file: failed due to exception ", file.name);
			msg.fail();
		}
	}, [&] {
		error(" open file: unknown nodeid");
		msg.fail();
	});
}


void Seoul::Filesystem::_close_file(MessageFs &msg)
{
	if (msg.fh == root.value) /* keep root dir open */
		return;

	Genode::Mutex::Guard guard(mutex);

	with_file(msg.nodeid, [&](auto &file) {
		if (file.handle.value != msg.fh) {
			warning(" close file: fh does not match ",
			        file.handle.value, " ", msg.fh);
			return;
		}

		try {
			fs.close(file.handle);
			file.handle.value = 0;

		} catch (...) {
			error(" close file: failed due to exception ", file.name);
		}
	}, [&]() {
		error(" close file: unknown nodeid ", msg.nodeid);
		msg.fail();
	});
}


void Seoul::Filesystem::_read_file(MessageFs &msg)
{
	Genode::Mutex::Guard guard(mutex);

	if (_queued_file_read && _read_file_pending.constructed()) {
		/*
		 * mb.bus_fs_commit.send(*_read_file_pending)
		 * passes here. Commit finish current read of file.
		 */
		msg.buffer = _read_file_pending->fs_delayed.buffer;

		_read_file_pending.destruct();
		_queued_file_read = false;
		return;
	}

	with_file(msg.nodeid, [&](auto &file) {
		_queued_file_read = _queue_read(file.handle, msg.buffer.size, msg.buffer.offset);

		_read_file_pending.construct(fs_id, msg.nodeid, msg);

		if (!_queued_file_read) {
			msg.buffer.offset = 0; /* read delayed - info for model */
			return;
		}

		tx.wakeup();

		bool early = _try_early_ack_and_release(Packet::READ, [&](auto &pkg) {

			_copy_data(msg, pkg);
			_read_file_pending.destruct();
			_queued_file_read = false;
			return true;
		});

		if (!early)
			msg.buffer.offset = 0; /* read delayed - info for model */
	}, [&]() {
		error(" read file: unknown nodeid ", msg.nodeid);
		msg.fail();
	});
}


void Seoul::Filesystem::_write_file(MessageFs &msg)
{
	Genode::Mutex::Guard guard(mutex);

	bool match = false;

	with_file(msg.nodeid, [&](auto &file) {
		error("got handle write nodeid=", msg.nodeid, " handle=", file.handle.value);

		/* multiple user/open handles per nodeid/file potentially */
		if (file.handle.value != msg.fh)
			return;

		match = true;

		bool ok = _queue_write(file.handle, msg.buffer.start,
		                       msg.buffer.size, msg.buffer.offset);
		if (!ok) {
			error("fail write");
			Logging::panic("add delay support and batching support");
		}

		/* ever wakeup, also in case of fail ? ... mmh */
		tx.wakeup();
	}, [&]() {
		error(" write file: unknown handle ", msg.nodeid);
		msg.fail();
	});

	if (!match)
		msg.fail();
}


void Seoul::Filesystem::_remove_dir(MessageFs &msg)
{
	Genode::Mutex::Guard guard(mutex);

	with_dir(msg.nodeid, [&](auto &parent_dir) {
		String<MAX_PATH_LEN> name(Cstring(reinterpret_cast<char *>(msg.buffer.start), msg.buffer.size));
		String<MAX_PATH_LEN> g_path { parent_dir.path, "/", name };

		uint64_t key = 0;

		try {
			with_each_dir([&](auto &d) {
				d.with_dir([&](auto &dir) {
					if (!msg.ok() || dir.path != g_path)
						return;

					key = d.key();

					with_open_dir_tmp(dir, [&](auto const dir_handle) {
						if (fs.num_entries(dir_handle))
							msg.fail();
					});
				});
			});

			if (!msg.ok())
				return;

			if (key) {
				with_dir_entry(key, [&](auto &entry) {
					entry.with_dir([&](auto &dir) {
						if (dir.dir.value)
							fs.close(dir.dir);
					});
					_dirs.remove(&entry);
					destroy(heap, &entry);
				}, [&]() { error(" - File::_remove_dir - unexpected close"); });
			}

			fs.unlink(parent_dir.dir, name.string());

		} catch (...) {
			msg.fail();
			error(" File::_remove_dir: failed");
		}
	}, [&] {
		error(" File::_remove_dir: unknown dir id ", msg.nodeid);
		msg.fail();
	});
}


void Seoul::Filesystem::_make_dir(MessageFs &msg)
{
	Genode::Mutex::Guard guard(mutex);

	with_dir(msg.nodeid, [&](auto &dir) {
		String<MAX_PATH_LEN> name(Cstring(reinterpret_cast<char *>(msg.buffer.start), msg.buffer.size));
		String<MAX_PATH_LEN> g_path { dir.path, "/", name };

		try {
			Dir_handle fh { fs.dir(g_path.string(), true) };

			Status status = fs.status(fh);

			auto entry = new (heap) Avl_dir(status.inode, g_path);
			_dirs.insert(entry);

			msg.nodeid = status.inode;
			msg.add_status(fh.value, status.size,
			               status.modification_time.ms_since_1970,
			               status.directory(), status.symlink());

			msg.writeable  = status.rwx.writeable;
			msg.readable   = status.rwx.readable;
			msg.executable = status.rwx.executable;

			fs.close(fh);
		} catch (...) {
			msg.fail();
			error(" File::_mkdir: failed");
		}
	}, [&] {
		error(" File::_mkdir: unknown dir id ", msg.nodeid);
		msg.fail();
	});
}


void Seoul::Filesystem::_open_dir(MessageFs &msg)
{
	Genode::Mutex::Guard guard(mutex);

	bool verbose = true;

	if (verbose)
		log(" File::OPEN_DIR nodeid=", msg.nodeid);

	with_dir(msg.nodeid, [&](auto &dir) {
		try {
			with_open_dir(msg, dir, [&]() {
				if (verbose)
					log (" File::OPEN_DIR nodeid=", msg.nodeid, " -> fh=", dir.dir.value, " '", dir.path, "'");

				msg.fh = dir.dir.value;
			});
		} catch (...) {
			error("open dir failed '", dir.path, "'");
			msg.fail();
		}
	}, [&] {
		error("OPEN_DIR: unknown nodeid");
		msg.fail();
	});
}


void Seoul::Filesystem::_read_dir(MessageFs &msg)
{
	{
		Genode::Mutex::Guard guard(mutex);

		with_dir(msg.nodeid, [&](auto &dir) {
			if (dir.dir.value != msg.fh)
				warning("read_dir: fh does not match inode");
		}, [&]() {
			error("READ_DIR: unknown nodeid ", msg.nodeid);
		});

		if (_read_dir_pending.constructed()) {
			/*
			 * mb.bus_fs_commit.send(*_read_dir_pending)
			 * passes here. Commit collected dirs and finish
			 * current read_dir
			 */
			msg.buffer = _read_dir_pending->fs_delayed.buffer;

			_read_dir_pending.destruct();

			return;
		}

		Dir_handle const dir { msg.fh };

		/* continuation of case A shortage */
		if (_pending.queued) {
			_read_dir_pending.construct(fs_id, msg.nodeid, msg);

			Signal_transmitter(_handler).submit();

			return;
		}

		_pending.expect  = fs.num_entries(dir);
		_pending.queued  = _queue_read_dir(dir, _pending.expect);
		_pending.missing = _pending.expect - _pending.queued;

		error("--- read dir expect=", _pending.expect);

		if (!_pending.queued) {
			msg.empty_read_dir();
			return;
		}

		_read_dir_pending.construct(fs_id, msg.nodeid, msg);
	}

	tx.wakeup();
}


void Seoul::Filesystem::_create(MessageFs &msg)
{
	Genode::Mutex::Guard guard(mutex);

	with_dir(msg.nodeid, [&](auto &dir) {
		String<MAX_PATH_LEN> name(Cstring(reinterpret_cast<char *>(msg.buffer.start), msg.buffer.size));

		Mode mode = ( msg.readable && msg.writeable) ? READ_WRITE
		          : (!msg.readable && msg.writeable) ? WRITE_ONLY
		          : READ_ONLY;

		try {
			auto fh = fs.file(dir.dir, name.string(), mode, true /* create */);
			error(" File::_create: file created ", name, " ", fh.value);

			Status status = fs.status(fh);

			auto entry = new (heap) Avl_file(status.inode);
			entry->with_file([&](auto &file) {
				file.handle    = fh;
				file.name      = name;
				file.dir_inode = msg.nodeid;
			});

			_files.insert(entry);

			error("node id ", status.inode, " ", status.directory(), " ", status.symlink(), " size=", status.size);

			msg.nodeid = status.inode;
			msg.add_status(fh.value, status.size,
			               status.modification_time.ms_since_1970,
			               status.directory(), status.symlink());

			msg.writeable  = status.rwx.writeable;
			msg.readable   = status.rwx.readable;
			msg.executable = status.rwx.executable;
		} catch (...) {
			msg.fail();
			error(" File::_create: failed");
		}
	}, [&] {
		error(" File::_create: unknown dir id ", msg.nodeid);
		msg.fail();
	});
}


void Seoul::Filesystem::_unlink(MessageFs &msg)
{
	Genode::Mutex::Guard guard(mutex);

	with_dir(msg.nodeid, [&](auto &parent_dir) {
		String<MAX_PATH_LEN> name(Cstring(reinterpret_cast<char *>(msg.buffer.start), msg.buffer.size));
		uint64_t key = 0;

		try {
			with_each_file([&](auto &f) {
				f.with_file([&](auto &file) {
					if (file.dir_inode == msg.nodeid && file.name == name)
						key = f.key();
				});
			});

			if (key) {
				with_file_entry(key, [&](auto &entry) {
					entry.with_file([&](auto &file) {
						if (file.handle.value)
							fs.close(file.handle);
					});
					_files.remove(&entry);
					destroy(heap, &entry);
				}, [&]() { error(" - unexpected unlink issue"); });
			}

			fs.unlink(parent_dir.dir, name.string());

		} catch (...) {
			msg.fail();
			error(" File::_unlink: failed\n");
		}
	}, [&] {
		error(" File::_unlink: unknown id ", msg.nodeid);
		msg.fail();
	});
}


void Seoul::Filesystem::_lookup(MessageFs &msg)
{
	Genode::Mutex::Guard guard(mutex);

	auto * path = reinterpret_cast<char const *>(msg.buffer.start);

	bool verbose = true;

	with_dir(msg.nodeid, [&](auto &parent_dir) {

		try {
			if (verbose)
				log(" File::_lookup A parent inode=", msg.nodeid,
				    " fh=", parent_dir.dir.value, " path='", path, "'");

			with_open_dir(msg, parent_dir, [&]() {

				Dir_handle parent = parent_dir.dir;

				if (verbose)
					log(" File::_lookup B parent inode=", msg.nodeid,
					    " fh=", parent.value, " path='", path, "'");

				/* STAT_ONLY leads to already_open exception for files XXX */
				File_handle h = fs.file(parent, path, READ_ONLY, false);

				Status status = fs.status(h);

				if (verbose)
					log(" File::LOOKUP C -> inode=", status.inode,
					    " size=", status.size,
					    " time=", status.modification_time.ms_since_1970);

				if (status.directory()) {

					String<MAX_PATH_LEN> g_path { parent_dir.path, "/", path };

					if (verbose)
						log(" File::LOOKUP - dir '", g_path,
						    "' -> nodeid=", status.inode);

					with_dir(status.inode, [&](auto &dir) {
						if (dir.dir.value && dir.path != g_path)
							warning("already open ? dir nodeid=", status.inode, " already fh=", dir.dir.value, " '", dir.path, "' vs '", g_path, "'");
						dir.path = g_path;
					}, [&]() {
						auto entry = new (heap) Avl_dir(status.inode, g_path);
						_dirs.insert(entry);
					});

					fs.close(h);
				} else {
					if (verbose)
						log(" File::LOOKUP - file in '", parent_dir.path, "' nodeid=", parent_dir.dir.value, " msg.nodeid=", msg.nodeid,
						    "' -> nodeid=", status.inode);

					with_file(status.inode, [&](auto &file) {
						error("file already open -- ", file.handle.value, " close_new=", h.value);
						fs.close(h);
					}, [&]() {
						auto entry = new (heap) Avl_file(status.inode);
						entry->with_file([&](auto &file) {
							file.handle    = h;
							file.name      = path;
							file.dir_inode = msg.nodeid;
						});

						_files.insert(entry);
					});
				}

				msg.nodeid = status.inode;
				msg.add_status(status.inode, status.size,
				               status.modification_time.ms_since_1970,
				               status.directory(), status.symlink());

				msg.writeable  = status.rwx.writeable;
				msg.readable   = status.rwx.readable;
				msg.executable = status.rwx.executable;
			});
		} catch (File_system::Lookup_failed) {
			msg.fail();
		} catch (...) {
			msg.fail();
			error(" File::_lookup: nodedid=", msg.nodeid, " - exception");
		}
	}, [&] {
		error(" File::_lookup: unknown id ", msg.nodeid);
		msg.fail();
	});
}


void Seoul::Filesystem::_sync(MessageFs &msg)
{
	Genode::Mutex::Guard guard(mutex);

	if (_queued_sync && _sync_pending.constructed()) {
		msg.buffer = _sync_pending->fs_delayed.buffer;
		_sync_pending.destruct();
		_queued_sync = false;
		return;
	}

	with_file(msg.nodeid, [&](auto &file) {
		_queued_sync = _queue_sync(file.handle);

		_sync_pending.construct(fs_id, msg.nodeid, msg);

		msg.buffer.offset = 0; /* delayed sync */

		if (!_queued_sync)
			return;

		tx.wakeup();

		_try_early_ack_and_release(Packet::SYNC, [&](auto &) {

			msg.buffer.offset = 1; /* sync done */

			_queued_sync = false;
			_sync_pending.destruct();

			return true;
		});
	}, [&]() {
		error(" sync: unknown nodeid ", msg.nodeid);
		msg.fail();
	});
}


void Seoul::Filesystem::_get_attr(MessageFs &msg)
{
	Genode::Mutex::Guard guard(mutex);

	bool try_file = false;

	with_dir(msg.nodeid, [&](auto &entry) {
		try {
			log("GET_ATTR nodeid=", msg.nodeid);

			with_open_dir(msg, entry, [&]() {
				Status status = fs.status(entry.dir);

				msg.add_status(status.inode, status.size,
				               status.modification_time.ms_since_1970,
				               status.directory(), status.symlink());

				msg.writeable  = status.rwx.writeable;
				msg.readable   = status.rwx.readable;
				msg.executable = status.rwx.executable;

			});
		} catch(...) {
			msg.fail();
			error(" File::GET_ATTR dir exception");
		}
	}, [&]() {
		error(" _get_attr: unknown nodeid ", msg.nodeid);
		try_file = true;
	});

	if (!try_file)
		return;

	with_file(msg.nodeid, [&](auto &file) {
		try {
			error(" File::GET_ATTR file fh=", msg.fh, " nodeid=", msg.nodeid, " file.handle=", file.handle.value);

/*
			File_handle h = file.handle;
			if (!file.handle.value) {
				File_handle h = fs.file(parent, path, READ_ONLY, false);
			}

			Status status = fs.status(h);

			msg.nodeid = status.inode;
			msg.add_status(fh.value, status.size,
			               status.modification_time.ms_since_1970,
			               status.directory(), status.symlink());

			msg.writeable  = status.rwx.writeable;
			msg.readable   = status.rwx.readable;
			msg.executable = status.rwx.executable;
*/

		} catch(...) {
			msg.fail();
			error(" File::GET_ATTR file exception");
		}
	}, [&]() {
		error(" _get_attr: unknown nodeid ", msg.nodeid);
		msg.fail();
	});
}


bool Seoul::Filesystem::_handle_read_dir(Packet &packet)
{
	bool verbose = true;
	bool wakeup  = false;

	bool const ok = with_packet(packet, [&](auto) {

		if (!packet.succeeded() || !_read_dir_pending.constructed()) {
			error("queue read dir failure");
			return false;
		}

		auto &msg = _read_dir_pending->fs_delayed;

		auto count   = packet.length() / sizeof(Directory_entry);
		auto entries = reinterpret_cast<Directory_entry *>(tx.packet_content(packet));

		if (!count) {
			if (_pending.queued) {
				_pending.queued --;
				error(" -- empty read - pending.queued=", _pending.queued);
				if (!_pending.queued)
					wakeup = true;
			}
		}

		for (unsigned i = 0; i < count; i++) {
			auto &entry    = entries[i];
			auto  name     = reinterpret_cast<char const *>(entry.name.buf);
			auto  name_len = strnlen(name, MAX_NAME_LEN);

			if (verbose)
				log("dir ", i, " : ", name, " ", name_len,
				    " inode=", entry.inode);

			bool ok = msg.add_read_dir(name, unsigned(name_len), entry.inode,
			                           entry.type == Node_type::DIRECTORY,
			                           entry.type == Node_type::SYMLINK);

			if (!ok) {
				/* no space in the buffer - case A shortage */
				_pending.pkg = packet;
				_read_dir_pending->more_read_dirs = !!_pending.queued;
				wakeup = true;
				return false;
			}

			if (_pending.queued) {
				_pending.queued --;
				if (!_pending.queued)
					wakeup = true;
			}
		}

		return true;
	});

	/* request more read-dirs if initially not complete */
	if (_pending.missing && _read_dir_pending.constructed()) {

		if (!_pending.queued)
			wakeup = false;

		auto &msg = _read_dir_pending->fs_delayed;
		Dir_handle const dir { msg.fh };

		auto more = _queue_read_dir(dir, _pending.expect,
		                            _pending.expect - _pending.missing);
		_pending.missing -= more;
		_pending.queued  += more;

		if (more)
			tx.wakeup();
	}

	if (wakeup) {

		mutex.release();

		mb.bus_fs_commit.send(*_read_dir_pending);

		mutex.acquire();
	}

	return ok;
}


void Seoul::Filesystem::_handle_read_file(Packet &packet)
{
	bool const ok = with_packet(packet, [&](auto) {

		if (!packet.succeeded() || !_read_file_pending.constructed()) {
			error("queue read file failure");
			return false;
		}

		auto &msg = _read_file_pending->fs_delayed;

		_copy_data(msg, packet);

		mutex.release();

		mb.bus_fs_commit.send(*_read_file_pending);

		mutex.acquire();

		return true;
	});

	if (!ok)
		Logging::printf("unhandled case XXX\n");
}


void Seoul::Filesystem::_handle_packet_stream()
{
	while (true) {
		auto packet = _pending.pkg;

		/* not safe check, SYNC e.g. have size 0 XXX */
		if (_pending.pkg.size()) {
			_pending.pkg = { };
		} else {
			if (tx.ack_avail())
				packet = tx.try_get_acked_packet();
			else
				return;
		}
#if 0
		Node_handle handle()    const { return _handle;   }
		Opcode      operation() const { return _op;       }
		seek_off_t  position()  const { return _op != Opcode::WRITE_TIMESTAMP ? _position : 0; }
		size_t      length()    const { return _op != Opcode::WRITE_TIMESTAMP ? _length : 0;   }
		bool        succeeded() const { return _success;  }
#endif

		switch (packet.operation()) {
		default:
			error(__func__, " unknown operation ",
			      unsigned(packet.operation()), " ",
			      packet.succeeded() ? " succeeded" : " failed",
			      " handle=", packet.handle().value);
			break;

		case Packet::READ:
		{
			bool verbose = false;

			if (verbose)
				log(__func__, " READ ",
				    packet.succeeded() ? "succeeded" : "failure",
				    " handle=", packet.handle(),
				    " len=", packet.length(), "/?", sizeof(Directory_entry),
				    " node handle ", packet.handle().value,
				    " offset=", packet.offset());

			if (_read_file_pending.constructed()) {
				_handle_read_file(packet);
				continue;
			}

			if (_handle_read_dir(packet))
				continue;
			else
				return;
		}
		case Packet::WRITE:
			error(__func__, " WRITE");
			break;

		case Packet::SYNC:
		{
			bool wakeup = false;

			bool const ok = with_packet(packet, [&](auto) {

				if (!packet.succeeded() || !_sync_pending.constructed()) {
					error("queue sync failure");
					return false;
				}

				_sync_pending->fs_delayed.buffer.offset = 1; /* done hint */

				wakeup = true;
				return true;
			});

			if (wakeup) {
				mutex.release();

				mb.bus_fs_commit.send(*_sync_pending);

				mutex.acquire();
			}

			/* error message already before, if appropriate */
			if (!ok)
				return;

			continue;
		}
		case Packet::CONTENT_CHANGED:
			error(__func__, " CONTENT CHANGED");
			break;

		case Packet::WRITE_TIMESTAMP:
			error(__func__, " WRITE TIMESTAMP");
			break;
		};

		tx.release_packet(packet);
	}
}


void Seoul::Filesystem::_handle_ack()
{
	Genode::Mutex::Guard guard(mutex);

	_handle_packet_stream();

	bool kick = (_read_file_pending.constructed() && !_queued_file_read) ||
	            (_sync_pending     .constructed() && !_queued_sync);

	if (kick)
		error("re-try on read file or sync required - implement me");
}


bool Seoul::Filesystem::receive(MessageFs &msg)
{
	if (fs_id != msg.fs_id)
		return false;

	switch (msg.type) {
	case MessageFs::OPEN_DIR   : _open_dir  (msg); break;
	case MessageFs::READ_DIR   : _read_dir  (msg); break;
	case MessageFs::CLOSE_DIR  : _close_dir (msg); break;
	case MessageFs::MAKE_DIR   : _make_dir  (msg); break;
	case MessageFs::REMOVE_DIR : _remove_dir(msg); break;
	case MessageFs::OPEN_FILE  : _open_file (msg); break;
	case MessageFs::READ_FILE  : _read_file (msg); break;
	case MessageFs::WRITE_FILE : _write_file(msg); break;
	case MessageFs::CLOSE_FILE : _close_file(msg); break;
	case MessageFs::CREATE     : _create    (msg); break;
	case MessageFs::GET_ATTR   : _get_attr  (msg); break;
	case MessageFs::LOOKUP     : _lookup    (msg); break;
	case MessageFs::UNLINK     : _unlink    (msg); break;
	case MessageFs::SYNC       : _sync      (msg); break;
	default:
		Logging::panic("fs: unsupported call");
		break;
	}

	return true;
}
