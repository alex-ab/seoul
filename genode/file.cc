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

	bool _verbose = false;

	Genode::Mutex::Guard guard(mutex);

	with_dir(msg.nodeid, [&](auto &dir) {
		if (dir.handle.value != msg.fh)
			warning(" File::_close_dir: fh does not match inode ",
			        dir.handle.value, " ", msg.fh);

		if (_verbose)
			log(" File::_close_dir: nodeid=", msg.nodeid, " fh=", msg.fh);

		Dir_handle h { msg.fh };

		try {
			fs.close(h);
			dir.handle.value = 0;

		} catch (...) {
			error(" File::_close_dir: failed due to exception");
			msg.fail();
		}
	}, [&]() {
		error(" File::_close_dir: unknown nodeid ", msg.nodeid);
		msg.fail();
	});
}


void Seoul::Filesystem::_open_file(MessageFs &msg)
{
	Genode::Mutex::Guard guard(mutex);

	with_file(msg.nodeid, [&](auto &file) {
		with_dir(file.dir_nodeid, [&](auto &dir) {
			Mode mode = ( msg.readable && msg.writeable) ? READ_WRITE
			          : (!msg.readable && msg.writeable) ? WRITE_ONLY
			          : READ_ONLY;
			try {
				if (file.handle.value) {
					warning(" file already open -- close and reopen");
					fs.close(file.handle);
				}

				file.handle = fs.file(dir.handle, file.name.string(), mode,
				                      false /* no create */);

				msg.fh = file.handle.value;

				if (!msg.fh) {
					error(" File::_open_file: failed ", file.name);
					msg.fail();
				}
			} catch (...) {
				error(" File::_open_file: failed due to exception ", file.name);
				msg.fail();
			}
		}, [&] {
			error(" File::_open_file: unknown dir nodeid");
			msg.fail();
		});
	}, [&] {
		error(" File::_open_file: unknown file nodeid");
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
			warning(" File::_close_file: fh does not match ",
			        file.handle.value, " ", msg.fh);
			return;
		}

		try {
			fs.close(file.handle);
			file.handle.value = 0;

		} catch (...) {
			error(" File::close_file: failed due to exception ", file.name);
		}
	}, [&]() {
		error(" File::close_file: unknown nodeid ", msg.nodeid);
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
		error(" File::_read_file: unknown nodeid ", msg.nodeid);
		msg.fail();
	});
}


void Seoul::Filesystem::_write_file(MessageFs &msg)
{
	Genode::Mutex::Guard guard(mutex);

	if (_queued_write && _write_pending.constructed()) {
		msg.buffer = _write_pending->fs_delayed.buffer;
		_write_pending.destruct();
		_queued_write = false;
	}

	bool match = false;

	with_file(msg.nodeid, [&](auto &file) {

		/* multiple user/open handles per nodeid/file potentially */
		if (file.handle.value != msg.fh)
			return;

		match = true;

		if (msg.buffer.size > 4096) {
			warning(" write file of size ", msg.buffer.size, " ", file.name);
			warning(" bulk size ", tx.bulk_buffer_size(),
			       "  tx queue size=", unsigned(Session::TX_QUEUE_SIZE));
		}

		bool ok = _queue_write(file.handle, msg.buffer.start,
		                       msg.buffer.size, msg.buffer.offset);
		if (!ok) {
			_queued_write = true;
			_write_pending.construct(fs_id, msg.nodeid, msg);

			msg.buffer.offset = 0; /* read delayed - info for model */

		} else
			msg.buffer.offset += msg.buffer.size;

		/* ever wakeup, also in case of fail ? ... mmh */
		tx.wakeup();
	}, [&]() {
		error(" File::_write_file: unknown handle ", msg.nodeid);
		msg.fail();
	});

	if (!match) {
		error(" File::_write_file: no match");
		with_file(msg.nodeid, [&](auto &file) {
			error(" - ", msg.nodeid, " ", file.handle.value, "!=", msg.fh, " ", file.name);
		}, [&]() {
			error(" FIle::_write_file: unknown handle ", msg.nodeid);
			msg.fail();
		});
		msg.fail();
	}
}


void Seoul::Filesystem::_remove_dir(MessageFs &msg)
{
	Genode::Mutex::Guard guard(mutex);

	with_dir(msg.nodeid, [&](auto &parent_dir) {
		String_dir name(Cstring(reinterpret_cast<char *>(msg.buffer.start), msg.buffer.size));
		String_dir g_path { parent_dir.path, "/", name };

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
						if (dir.handle.value)
							fs.close(dir.handle);
					});
					_dirs.remove(&entry);
					destroy(heap, &entry);
				}, [&]() { error(" - File::_remove_dir - unexpected close"); });
			}

			fs.unlink(parent_dir.handle, name.string());

		} catch (...) {
			msg.fail();
			error(" File::_remove_dir: failed");
		}
	}, [&] {
		error(" File::_remove_dir: unknown dir id ", msg.nodeid);
		msg.fail();
	});
}


void Seoul::Filesystem::_rename(MessageFs &msg)
{
	bool verbose = true;

	String_dir src(Cstring(reinterpret_cast<char *>(msg.buffer.start), msg.buffer.size));
	String_dir dst(Cstring(reinterpret_cast<char *>(msg.buffer.start + src.length()), msg.buffer.size - src.length()));

	auto const dir_nodeid_src = msg.nodeid;
	auto const dir_nodeid_dst = msg.fh;

	if (verbose)
		log(" _rename: ", dir_nodeid_src, "->", dir_nodeid_dst,
		    " '", src, "' -> '", dst, "'");

	Genode::Mutex::Guard guard(mutex);

	with_dir(dir_nodeid_src, [&](auto &dir_src) {
		with_dir(dir_nodeid_dst, [&](auto &dir_dst) {

			Dir_handle from = dir_src.handle;
			Dir_handle to   = dir_dst.handle;

			uint64_t src_key = 0;
			uint64_t dst_key = 0;

			with_each_file([&](auto &f) {
				f.with_file([&](auto &file) {
					if (!src_key && file.dir_nodeid == dir_nodeid_src && file.name == src)
						src_key = f.key();
					if (!dst_key && file.dir_nodeid == dir_nodeid_dst && file.name == dst)
						dst_key = f.key();
				});
			});

			try {
				fs.move(from, src.string(), to, dst.string());

				if (src_key) {
					with_file_entry(src_key, [&](auto &entry) {
						entry.with_file([&](auto &file) {
							file.dir_nodeid = dir_nodeid_dst;
							file.name       = dst;
						});
					}, [&]() { error(" File::_rename - unexpected src"); });
				}

				if (dst_key) {
					with_file_entry(dst_key, [&](auto &entry) {
						entry.with_file([&](auto &file) {
							if (file.handle.value) {
								warning(" File::_rename: close open dst file ?! ", dst);
								fs.close(file.handle);
							}
						});
						_files.remove(&entry);
						destroy(heap, &entry);
					}, [&]() { error(" File::_rename - unexpected rename dst issue"); });
				}
			} catch (...) {
				msg.fail();
				error(" File::_rename: failed - exception");
			}
		}, [&] { msg.fail(); });
	}, [&] { msg.fail(); });

	if (!msg.ok())
		error(" File::_rename failed: ", dir_nodeid_src, "->", dir_nodeid_dst,
		      "'", src, "' -> '", dst, "'");
}


void Seoul::Filesystem::_make_dir(MessageFs &msg)
{
	Genode::Mutex::Guard guard(mutex);

	with_dir(msg.nodeid, [&](auto &dir) {
		String_dir name(Cstring(reinterpret_cast<char *>(msg.buffer.start), msg.buffer.size));
		String_dir g_path { dir.path, "/", name };

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
			error(" File::_mkdir: failed due to exception");
		}
	}, [&] {
		error(" File::_mkdir: unknown dir id ", msg.nodeid);
		msg.fail();
	});
}


void Seoul::Filesystem::_open_dir(MessageFs &msg)
{
	Genode::Mutex::Guard guard(mutex);

	bool verbose = false;

	with_dir(msg.nodeid, [&](auto &dir) {
		try {
			with_open_dir(msg, dir, [&]() {
				if (verbose)
					log (" File::_opend_dir nodeid=", msg.nodeid, " -> fh=",
					     dir.handle.value, " '", dir.path, "'");

				msg.fh = dir.handle.value;
			});
		} catch (...) {
			error(" File::_open_dir failed due to exception");
			msg.fail();
		}
	}, [&] {
		error(" File::_open_dir: unknown nodeid");
		msg.fail();
	});
}


void Seoul::Filesystem::_read_dir(MessageFs &msg)
{
	{
		Genode::Mutex::Guard guard(mutex);

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

		if (!_pending.queued) {
			msg.empty_read_dir();
			return;
		}

		_read_dir_pending.construct(fs_id, msg.nodeid, msg);
	}

	tx.wakeup();
}


void Seoul::Filesystem::_destroy(MessageFs const &msg)
{
	Genode::Mutex::Guard guard(mutex);

	for (Avl_file * entry = nullptr; entry = _files.first();) {
		entry->with_file([&](auto &file) {
			/* keep root dir open */
			if (file.handle == root)
				return;

			if (file.handle.value) {
				try {
					fs.close(file.handle);
				} catch (...) {
					error(" File::_destroy: exception during closing file");
				}
			}
		});

		_files.remove(entry);
		destroy(heap, entry);
	}

	for (Avl_dir * entry = nullptr; entry = _dirs.first();) {
		entry->with_dir([&](auto &dir) {
			/* keep root dir open */
			if (dir.handle == root)
				return;

			if (dir.handle.value) {
				try {
					fs.close(dir.handle);
				} catch (...) {
					error(" File::_destroy: exception during closing dir");
				}
			}
		});
		_dirs.remove(entry);
		destroy(heap, entry);
	}

	/* re-add root */
	_add_root();
}


void Seoul::Filesystem::_create(MessageFs &msg)
{
	Genode::Mutex::Guard guard(mutex);

	with_dir(msg.nodeid, [&](auto &dir) {
		String_dir name(Cstring(reinterpret_cast<char *>(msg.buffer.start), msg.buffer.size));

		Mode mode = ( msg.readable && msg.writeable) ? READ_WRITE
		          : (!msg.readable && msg.writeable) ? WRITE_ONLY
		          : READ_ONLY;

		try {
			auto fh = fs.file(dir.handle, name.string(), mode, true /* create */);

			Status status = fs.status(fh);

			auto entry = new (heap) Avl_file(status.inode);
			entry->with_file([&](auto &file) {
				file.handle     = fh;
				file.name       = name;
				file.dir_nodeid = msg.nodeid;
			});

			_files.insert(entry);

			msg.nodeid = status.inode;
			msg.add_status(fh.value, status.size,
			               status.modification_time.ms_since_1970,
			               status.directory(), status.symlink());

			msg.writeable  = status.rwx.writeable;
			msg.readable   = status.rwx.readable;
			msg.executable = status.rwx.executable;
		} catch (...) {
			msg.fail();
			error(" File::_create: failed due to exception");
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
		String_dir name(Cstring(reinterpret_cast<char *>(msg.buffer.start), msg.buffer.size));
		uint64_t key = 0;

		try {
			with_each_file([&](auto &f) {
				f.with_file([&](auto &file) {
					if (!key && file.dir_nodeid == msg.nodeid && file.name == name)
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
				}, [&]() { error(" File::_unlink - unexpected unlink issue"); });
			}

			fs.unlink(parent_dir.handle, name.string());

		} catch (...) {
			msg.fail();
			error(" File::_unlink: failed due to exception");
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

	bool verbose = false;

	with_dir(msg.nodeid, [&](auto &parent_dir) {

		try {
			if (verbose)
				log(" File::_lookup A parent inode=", msg.nodeid,
				    " fh=", parent_dir.handle.value, " path='", path, "'");

			with_open_dir(msg, parent_dir, [&]() {

				Dir_handle parent = parent_dir.handle;

				if (verbose)
					log(" File::_lookup B parent inode=", msg.nodeid,
					    " fh=", parent.value, " path='", path, "'");

				/* STAT_ONLY leads to already_open exception for files XXX */
				File_handle h = fs.file(parent, path, READ_ONLY, false);

				Status status = fs.status(h);

				if (verbose)
					log(" File::_lookup C -> inode=", status.inode,
					    " size=", status.size,
					    " time=", status.modification_time.ms_since_1970);

				if (status.directory()) {

					String_dir g_path { parent_dir.path, "/", path };

					if (verbose)
						log(" File::_lookup - dir '", g_path,
						    "' -> nodeid=", status.inode);

					with_dir(status.inode, [&](auto &dir) {
						if (dir.handle.value && dir.path != g_path)
							warning(" File::_lookup already open ?");
						dir.path = g_path;
					}, [&]() {
						auto entry = new (heap) Avl_dir(status.inode, g_path);
						_dirs.insert(entry);
					});

					fs.close(h);
				} else {
					if (verbose)
						log(" File::_lookup - file in '", parent_dir.path,
						    "' nodeid=", parent_dir.handle.value,
						    " msg.nodeid=", msg.nodeid,
						    "' -> nodeid=", status.inode);

					with_file(status.inode, [&](auto &file) {
						fs.close(h);
					}, [&]() {
						auto entry = new (heap) Avl_file(status.inode);
						entry->with_file([&](auto &file) {
							file.name       = path;
							file.dir_nodeid = msg.nodeid;
						});

						_files.insert(entry);

						fs.close(h);
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
		warning(" File::_sync: unknown nodeid ", msg.nodeid);
		msg.fail();
	});
}


void Seoul::Filesystem::_get_attr(MessageFs &msg)
{
	Genode::Mutex::Guard guard(mutex);

	bool try_file = false;

	with_dir(msg.nodeid, [&](auto &dir) {
		try {
			with_open_dir(msg, dir, [&]() {
				Status status = fs.status(dir.handle);

				msg.add_status(status.inode, status.size,
				               status.modification_time.ms_since_1970,
				               status.directory(), status.symlink());

				msg.writeable  = status.rwx.writeable;
				msg.readable   = status.rwx.readable;
				msg.executable = status.rwx.executable;

			});
		} catch(...) {
			msg.fail();
			error(" File::_get_attr: failed due to exception");
		}
	}, [&]() {
		try_file = true;
	});

	if (!try_file)
		return;

	with_file(msg.nodeid, [&](auto &file) {
		with_dir(file.dir_nodeid, [&](auto &dir) {
			try {
				File_handle h = file.handle;
				if (!file.handle.value)
					h = fs.file(dir.handle, file.name.string(), READ_ONLY,
					            false /* no create */);

				Status status = fs.status(h);

				msg.add_status(status.inode, status.size,
				               status.modification_time.ms_since_1970,
				               status.directory(), status.symlink());

				msg.writeable  = status.rwx.writeable;
				msg.readable   = status.rwx.readable;
				msg.executable = status.rwx.executable;

				if (!file.handle.value)
					fs.close(h);
			} catch(...) {
				msg.fail();
				error(" File::_get_attr: file exception");
			}
		}, [&]() {
			error(" File::_get_attr: unknown dir nodeid ", msg.nodeid);
			msg.fail();
		});
	}, [&]() {
		error(" _get_attr: unknown file nodeid ", msg.nodeid);
		msg.fail();
	});
}


void Seoul::Filesystem::_set_attr(MessageFs &msg)
{
	Genode::Mutex::Guard guard(mutex);

	bool try_file = false;

	with_dir(msg.nodeid, [&](auto &entry) {
		try {
			error(" File::_set_attr nodeid=", msg.nodeid, " todo dir");
		} catch(...) {
			msg.fail();
			error(" File::_set_attr: due to exception - dir");
		}
	}, [&]() {
		try_file = true;
	});

	if (!try_file)
		return;

	with_file(msg.nodeid, [&](auto &file) {
		try {
			if (msg.buffer.start) /* valid check for msg.buffer.size */
				fs.truncate(file.handle, msg.buffer.size);

			Status status = fs.status(file.handle);

			msg.add_status(status.inode, status.size,
			               status.modification_time.ms_since_1970,
			               status.directory(), status.symlink());

			msg.writeable  = status.rwx.writeable;
			msg.readable   = status.rwx.readable;
			msg.executable = status.rwx.executable;

		} catch(...) {
			msg.fail();
			error(" File::_set_attr: due to exception - file");
		}
	}, [&]() {
		error(" File::_set_attr: unknown nodeid ", msg.nodeid);
		msg.fail();
	});
}


bool Seoul::Filesystem::_handle_read_dir(Packet &packet)
{
	bool verbose = false;
	bool wakeup  = false;

	bool const ok = with_packet(packet, [&](auto) {

		if (!packet.succeeded() || !_read_dir_pending.constructed()) {
			error(" File::_queue_read_dir failure");
			return false;
		}

		auto &msg = _read_dir_pending->fs_delayed;

		auto count   = packet.length() / sizeof(Directory_entry);
		auto entries = reinterpret_cast<Directory_entry *>(tx.packet_content(packet));

		if (!count) {
			if (_pending.queued) {
				_pending.queued --;
				if (!_pending.queued)
					wakeup = true;
			}
		}

		for (unsigned i = 0; i < count; i++) {
			auto &entry    = entries[i];
			auto  name     = reinterpret_cast<char const *>(entry.name.buf);
			auto  name_len = strnlen(name, MAX_NAME_LEN);

			if (verbose)
				log(" File::_handle_read_dir ", i, " : ", name, " ", name_len,
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
			error(" File::_handle_read_file failure");
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
		warning(" File::_handle_read_file - unexpected");
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

		switch (packet.operation()) {
		default:
			error(__func__, " unknown operation ",
			      unsigned(packet.operation()), " ",
			      packet.position(), " ", packet.length(), " ",
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
		{
			bool wakeup = false;

			if (!packet.succeeded())
				error(__func__, " WRITE ",
				      packet.succeeded() ? "succeeded" : "failed",
				      " handle=", packet.handle().value,
				      " pos=", packet.position());

			if (_queued_write)
				wakeup = true;

			if (wakeup) {
				mutex.release();

				mb.bus_fs_commit.send(*_write_pending);

				mutex.acquire();
			}

			break;
		}
		case Packet::SYNC:
		{
			bool wakeup = false;

			bool const ok = with_packet(packet, [&](auto) {

				if (!packet.succeeded() || !_sync_pending.constructed()) {
					error(__func__, " queue sync failure");
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
	case MessageFs::RENAME     : _rename    (msg); break;
	case MessageFs::CREATE     : _create    (msg); break;
	case MessageFs::DESTROY    : _destroy   (msg); break;
	case MessageFs::GET_ATTR   : _get_attr  (msg); break;
	case MessageFs::SET_ATTR   : _set_attr  (msg); break;
	case MessageFs::LOOKUP     : _lookup    (msg); break;
	case MessageFs::UNLINK     : _unlink    (msg); break;
	case MessageFs::SYNC       : _sync      (msg); break;
	default:
		Logging::panic("fs: unsupported call");
		break;
	}

	return true;
}
