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

	using namespace Genode;
	using namespace File_system;

	typedef File_system::Packet_descriptor Packet;
	typedef File_system::Session           Session;
	typedef String<File_system::MAX_PATH_LEN> String_dir;
}


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

		auto key() const { return _key; }
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

		unsigned const           fs_id;

		unsigned const           root_nodeid { 1 }; /* XXX ever the case ? */

		Avl_tree<Avl_file> _files     { };
		Avl_tree<Avl_dir>  _dirs      { };

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

		void _lookup_sym(MessageFs &, Dir_handle, char const *, size_t,
		                 unsigned long sym_handle = 0ul);

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

		void with_open_dir(MessageFs const &msg, auto &entry, auto const &fn)
		{
			if (msg.nodeid > root_nodeid && !entry.handle.value) {

				Dir_handle h { fs.dir(entry.path.string(), false) };

				entry.handle = h;

				fn();
			} else
				fn();
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

		void with_each_dir (auto const &fn) const { _dirs .for_each(fn); }
		void with_each_file(auto const &fn) const { _files.for_each(fn); }

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

				obj.construct(fs_id, msg.nodeid, msg);

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

		void _add_root()
		{
			String_dir p { "" };
			auto e = new (heap) Avl_dir(root_nodeid, p);
			e->with_dir([&](auto &dir) { dir.handle = root; });
			_dirs.insert(e);
		}

	public:

		Filesystem(Env &env, Motherboard &mb, unsigned fsid)
		: mb(mb), env(env), fs_id(fsid)
		{
			if (root.value != 0) {
				error("Filesystem offline - unexpected state");
				return;
			}

			fs.sigh(_handler);

			mb.bus_fs.add(this, receive_static<MessageFs>);

			_add_root();
		}

		bool receive(MessageFs &);
};
