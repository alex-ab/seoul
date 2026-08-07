/*
 * \brief  File handling
 * \author Alexander Boettcher
 * \date   2025-11-28
 */

/*
 * Copyright (C) 2025-2026 Alexander Boettcher
 *
 * This file is part of Seoul, which is distributed
 * under the terms of the GNU General Public License version 2.
 */

#pragma once

/* Genode includes */
#include <base/heap.h>
#include <util/bit_allocator.h>
#include <file_system_session/connection.h>

/* Seoul includes */
#include <nul/motherboard.h>

namespace Seoul {
	class Filesystem;
	class Avl_dir;
	class Avl_file;

	template <typename> class Avl;

	using namespace Genode;
	using namespace File_system;

	typedef File_system::Packet_descriptor Packet;
	typedef File_system::Session           Session;
	typedef String<File_system::MAX_PATH_LEN> String_dir;
}


template <typename T>
class Seoul::Avl : public Avl_tree<T>
{
	public:

		bool apply_until_true(auto const &fn) const
		{
			if (auto f = Avl_tree<T>::first())
				return f->apply_until_true(fn);

			return false;
		}
};


class Seoul::Avl_dir : public Genode::Avl_node<Seoul::Avl_dir>
{
	private:

		uint64_t const _key;

		struct {
			Dir_handle handle { 0 };
			String_dir path   { };
		} _dir;

		/*
		 * Noncopyable
		 */
		Avl_dir             (Avl_dir const &);
		Avl_dir &operator = (Avl_dir const &);

	public:

		Avl_dir(uint64_t key, String_dir &path)
		: _key(key), _dir { .path = path } { }

		auto key() const { return _key; }

		bool higher(Avl_dir *e) const { return e->_key > _key; }

		void with_entry(uint64_t key, auto const &fn, auto const &fn_error)
		{
			if (key == _key)
				return fn(*this);

			auto obj = this->child(key > _key);

			if (obj)
				obj->with_entry(key, fn, fn_error);
			else
				fn_error();
		}

		void with_dir(auto const &fn)       { fn(_dir); }
		void with_dir(auto const &fn) const { fn(_dir); }

		bool apply_until_true(auto const &fn) const
		{
			typedef Seoul::Avl_dir NT;

			if (NT * l = child(Avl_node<NT>::LEFT))
				if (l->apply_until_true(fn)) return true;

			if (fn(*static_cast<NT const *>(this)))
				return true;

			if (NT * r = child(Avl_node<NT>::RIGHT))
				if (r->apply_until_true(fn))
					return true;

			return false;
		}
};


class Seoul::Avl_file : public Genode::Avl_node<Seoul::Avl_file>
{
	private:

		uint64_t const _key;

		struct {
			File_handle          handle     { 0 };
			uint64_t             dir_nodeid { 0 };
			String<MAX_NAME_LEN> name;
		} _entry { };

		/*
		 * Noncopyable
		 */
		Avl_file             (Avl_file const &);
		Avl_file &operator = (Avl_file const &);

	public:

		Avl_file(uint64_t key) : _key(key) { }

		auto key() const { return _key; }

		bool higher(Avl_file *e) const { return e->_key > _key; }

		void with_entry(uint64_t key, auto const &fn, auto const &fn_error)
		{
			if (key == _key)
				return fn(*this);

			auto obj = this->child(key > _key);

			if (obj)
				obj->with_entry(key, fn, fn_error);
			else
				fn_error();
		}

		void with_file(auto const &fn)       { fn(_entry); }
		void with_file(auto const &fn) const { fn(_entry); }

		bool apply_until_true(auto const &fn) const
		{
			typedef Seoul::Avl_file NT;

			if (NT * l = child(Avl_node<NT>::LEFT))
				if (l->apply_until_true(fn)) return true;

			if (fn(*static_cast<NT const *>(this)))
				return true;

			if (NT * r = child(Avl_node<NT>::RIGHT))
				if (r->apply_until_true(fn))
					return true;

			return false;
		}
};


class Seoul::Filesystem : public StaticReceiver<Filesystem>
{
	private:

		Motherboard             &mb;
		Env                     &env;
		Heap                     heap  { env.ram(), env.rm() };
		Allocator_avl            alloc { &heap };
		File_system::Connection  fs    { env, alloc };
		Dir_handle               root  { fs.dir("/", false) };
		Session::Tx::Source     &tx    { *fs.tx() };

		Genode::Mutex            mutex { };

		unsigned const  _fs_id;
		bool            _wrap { };
		uint64_t        _nodeid_counter { };
		uint64_t const  _root_nodeid;

		Avl<Avl_file>  _files     { };
		Avl<Avl_dir>   _dirs      { };

		Signal_handler<Filesystem> _handler { env.ep(), *this, &Filesystem::_handle_submit };

		void _handle_submit() { _handle_ack(); }

		void _handle_ack();
		void _handle_packet_stream();

		void _forget    (MessageFs const &);
		void _destroy   (MessageFs const &);
		void _create    (MessageFs &);
		void _rename    (MessageFs &);
		void _sym_link  (MessageFs &);
		void _read_link (MessageFs &);
		void _lookup    (MessageFs &);
		void _sync      (MessageFs &);
		void _unlink    (MessageFs &);
		void _get_attr  (MessageFs &);
		void _set_attr  (MessageFs &);
		void _open_file (MessageFs &);
		void _read_file (MessageFs &);
		void _write_file(MessageFs &);
		void _close_file(MessageFs &);
		void _make_dir  (MessageFs &);
		void _open_dir  (MessageFs &);
		void _read_dir  (MessageFs &);
		void _remove_dir(MessageFs &);
		void _close_dir (MessageFs &);

		void _lookup_sym(MessageFs &, Dir_handle, String_dir const &,
		                 char const *, size_t, unsigned long sym_handle = 0ul);

		bool _handle_read_dir (Packet &);

		enum { MAX_PACKETS = 64 };

		Bit_allocator<MAX_PACKETS> _idx_alloc { };

		/* read_dir operation in flight */
		struct {
			Packet   pkg;
			unsigned expect;
			unsigned queued;
			unsigned missing;
		} _pending { };

		Constructible<MessageFsCommit> _read_link_pending { };
		Constructible<MessageFsCommit> _read_file_pending { };
		Constructible<MessageFsCommit> _read_dir_pending  { };
		Constructible<MessageFsCommit> _sync_pending      { };
		Constructible<MessageFsCommit> _write_pending     { };

		bool _queued_sync      { };
		bool _queued_file_read { };
		bool _queued_link_read { };
		bool _queued_write     { };

		bool with_new_packet(auto const &fn)
		{
			Packet packet { };

			bool ok = fn(packet);

			return ok;
		}

		bool with_packet(auto &packet, auto const &fn)
		{
			bool ok = fn(packet);

			if (!ok)
				return ok;

			tx.release_packet(packet);

			return ok;
		}

		uint64_t new_nodeid()
		{
			while (true) {
				uint64_t nodeid = _nodeid_counter++;

				if (nodeid == _root_nodeid) continue;
				if (!nodeid) { log("nodeid wrapped"); _wrap = true; continue; }

				if (!_wrap) return nodeid;

				uint64_t cnt { };

				if (_dirs.apply_until_true([&](auto const &d) {
					cnt ++;
					return !!(nodeid == d.key());
				}))
					continue;

				if (_files.apply_until_true([&](auto const &f) {
					cnt ++;
					return !!(nodeid == f.key());
				}))
					continue;

				if (cnt >= 4096)
					warning("file - potential performance issues");

				return nodeid;
			}
		}

		void with_open_dir(MessageFs const &msg, auto &entry, auto const &fn)
		{
			if (msg.nodeid > _root_nodeid && !entry.handle.value) {

				Dir_handle h { fs.dir(entry.path.string(), false) };

				entry.handle = h;

				fn();
			} else
				fn();
		}

		void with_open_dir_tmp(MessageFs const &msg, auto const &fn, auto const &fn_unknown)
		{
			with_dir(msg.nodeid, [&](auto &entry) {
				if (msg.nodeid > _root_nodeid && !entry.handle.value) {

					Dir_handle h { fs.dir(entry.path.string(), false) };

					fn(h, entry);

					fs.close(h);
				} else
					fn(entry.handle, entry);
			}, fn_unknown);
		}

		void with_open_dir_tmp(auto &entry, auto const &fn)
		{
			if (!entry.handle.value) {

				Dir_handle h { fs.dir(entry.path.string(), false) };

				fn(h);

				fs.close(h);
			} else
				fn(entry.handle);
		}

		void with_dir_entry(uint64_t key, auto const &fn_hit, auto const &fn_unknown)
		{
			if (_dirs.first())
				_dirs.first()->with_entry(key, fn_hit, fn_unknown);
			else
				fn_unknown();
		}

		void with_dir(uint64_t key, auto const &fn_hit, auto const &fn_unknown)
		{
			if (!_dirs.first()) {
				fn_unknown();
				return;
			}

			_dirs.first()->with_entry(key, [&](auto &entry) {
				entry.with_dir(fn_hit);
			}, fn_unknown);
		}

		void with_file_entry(uint64_t key, auto const &fn_hit, auto const &fn_unknown)
		{
			if (_files.first())
				_files.first()->with_entry(key, fn_hit, fn_unknown);
			else
				fn_unknown();
		}

		void with_file(uint64_t key, auto const &fn_hit, auto const &fn_unknown)
		{
			if (!_files.first()) {
				fn_unknown();
				return;
			}

			_files.first()->with_entry(key, [&](auto &entry) {
				entry.with_file(fn_hit);
			}, fn_unknown);
		}

		bool apply_dir_until_true(auto const &fn) const {
			return _dirs.apply_until_true(fn); }
		bool apply_file_until_true(auto const &fn) const {
			return _files.apply_until_true(fn); }

		unsigned _queue_read_dir(Dir_handle const &dir_handle,
		                         unsigned   const  num_entries,
		                         unsigned   const  start_i = 0)
		{
			auto packet_size = sizeof(Directory_entry);

			for (unsigned i = start_i; i < num_entries; i++) {

				bool ok = with_new_packet([&](auto &packet) {

					auto res = tx.alloc_packet_attempt(packet_size);

					bool ok  = res.convert<bool>([&](auto const p) {

						packet = Packet(p, dir_handle, Packet::READ,
						                packet_size, i * packet_size);

						if (tx.try_submit_packet(packet))
							return true;

						tx.release_packet(p);

						return false;
					}, [&](auto) { return false; });

					return ok;
				});

				if (!ok) {
					/* diagnostic - message can be removed */
					log("partial read dir ", i, "/", num_entries);
					return i - start_i;
				}
			}

			return num_entries - start_i;
		}

		bool _queue_read(File_handle const &file_handle,
		                 size_t      const  size,
		                 seek_off_t  const  pos)
		{
			bool ok = with_new_packet([&](auto &packet) {

				auto res = tx.alloc_packet_attempt(size);

				bool ok  = res.convert<bool>([&](auto const p) {

					packet = Packet(p, file_handle, Packet::READ, mword(size), pos);

					if (tx.try_submit_packet(packet))
						return true;

					tx.release_packet(p);

					return false;
				}, [&](auto) { return false; });

				return ok;
			});

			return ok;
		}

		bool _queue_write(File_handle const &fh,
		                  uintptr_t   const  start,
		                  size_t      const  size,
		                  seek_off_t  const  pos)
		{
			bool ok = with_new_packet([&](auto &packet) {

				auto res = tx.alloc_packet_attempt(size);

				bool ok  = res.convert<bool>([&](auto const p) {

					packet = Packet(p, fh, Packet::WRITE, size, pos);

					__builtin_memcpy(reinterpret_cast<void *>(tx.packet_content(packet)),
					                 reinterpret_cast<void *>(start), size);

					if (tx.try_submit_packet(packet))
						return true;

					tx.release_packet(p);

					return false;
				}, [&](auto) { return false; });

				return ok;
			});

			return ok;
		}

		bool _queue_sync(File_handle const &fh)
		{
			bool ok = with_new_packet([&](auto &packet) {

				auto res = tx.alloc_packet_attempt(0);

				bool ok  = res.convert<bool>([&](auto const p) {

					packet = Packet(p, fh, Packet::SYNC, 0, 0);

					if (tx.try_submit_packet(packet))
						return true;

					tx.release_packet(p);

					return false;
				}, [&](auto) { return false; });

				return ok;
			});

			return ok;
		}

		bool _try_early_ack_and_release(Packet::Opcode const  operation,
		                                auto           const &fn)
		{
			if (_pending.pkg.size())
				return false;

			if (!tx.ack_avail())
				return false;

			auto pkg = tx.try_get_acked_packet();

			if (pkg.operation() != operation) {
				_pending.pkg = pkg;
				return false;
			}

			return with_packet(pkg, [&](auto &p) { return fn(pkg); });
		}

		void _copy_data(MessageFs &msg, Packet &packet)
		{
			msg.buffer.size    = Genode::min(msg.buffer.size, packet.length());
			msg.buffer.offset += msg.buffer.size;

			::memcpy(reinterpret_cast<void *>(msg.buffer.start),
			         reinterpret_cast<void *>(tx.packet_content(packet)),
			         mword(msg.buffer.size));
		}

		bool _handle_async_read(Packet &packet, auto &check)
		{
			return with_packet(packet, [&](auto) {

				if (!packet.succeeded() || !check.constructed()) {
					error(" File::_handle_async_read failure");
					return false;
				}

				auto &msg = check->fs_delayed;

				_copy_data(msg, packet);

				mutex.release();

				mb.bus_fs_commit.send(*check);

				mutex.acquire();

				return true;
			});
		}

		void _read_async(MessageFs &msg, auto &active, auto &obj)
		{
			if (active && obj.constructed()) {
				/*
				 * mb.bus_fs_commit.send(*check)
				 * passes here. Commit finish current read of file.
				 */
				msg.buffer = obj->fs_delayed.buffer;

				obj.destruct();
				active = false;
				return;
			}

			with_file(msg.nodeid, [&](auto &file) {
				active = _queue_read(file.handle, mword(msg.buffer.size), msg.buffer.offset);

				obj.construct(_fs_id, msg.nodeid, msg);

				if (!active) {
					msg.buffer.offset = 0; /* read delayed - info for model */
					return;
				}

				tx.wakeup();

				bool early = _try_early_ack_and_release(Packet::READ, [&](auto &pkg) {

					_copy_data(msg, pkg);
					obj.destruct();
					active = false;
					return true;
				});

				if (!early)
					msg.buffer.offset = 0; /* read delayed - info for model */
			}, [&]() {
				error(" File::_read_async: unknown nodeid ", msg.nodeid);
				msg.fail();
			});
		}

		uint64_t lookup_dir(auto const &msg, String_dir const &name)
		{
			uint64_t nodeid { };

			with_dir(msg.nodeid, [&](auto &parent_dir) {
				String_dir g_path { parent_dir.path, "/", name };

				apply_dir_until_true([&](auto &d) {
					d.with_dir([&](auto &dir) {

						if (dir.path == g_path)
							nodeid = d.key();
					});

					return !!nodeid;
				});
			}, [&] { /* reflected as invalid node id */ });

			return nodeid;
		}

		uint64_t lookup_file(auto const &msg, String_dir const &name) const
		{
			uint64_t nodeid { };

			apply_file_until_true([&](auto &f) {
				f.with_file([&](auto &file) {
					if (file.dir_nodeid != msg.nodeid)
						return;

					if (file.name == name)
						nodeid = f.key();
				});

				return !!nodeid;
			});

			return nodeid;
		}

		void _add_root()
		{
			String_dir p { "" };
			auto e = new (heap) Avl_dir(_root_nodeid, p);
			e->with_dir([&](auto &dir) { dir.handle = root; });
			_dirs.insert(e);
		}

	public:

		Filesystem(Env &env, Motherboard &mb, unsigned fsid);

		bool receive(MessageFs &);
};
