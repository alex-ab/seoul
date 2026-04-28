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
	class Avl_entry;
	class Avl_file;

	using namespace Genode;
	using namespace File_system;

	typedef File_system::Packet_descriptor Packet;
	typedef File_system::Session           Session;

	struct Open_dir {
		Dir_handle  dir  { 0 };
		String<File_system::MAX_PATH_LEN> path { };
	};
}


class Seoul::Avl_entry : public Genode::Avl_node<Seoul::Avl_entry>
{
	private:

		uint64_t        const _key;
		Seoul::Open_dir       _dir;

		/*
		 * Noncopyable
		 */
		Avl_entry             (Avl_entry const &);
		Avl_entry &operator = (Avl_entry const &);

	public:

		Avl_entry(uint64_t key, String<MAX_PATH_LEN> &path)
		: _key(key), _dir { .path = path } { }

		bool higher(Avl_entry *e) const { return e->_key > _key; }

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

		void with_dir(auto const &fn) { fn(_dir); }
};


class Seoul::Avl_file : public Genode::Avl_node<Seoul::Avl_file>
{
	private:

		uint64_t const _key;

		struct {
			File_handle          handle { 0 };
			Dir_handle           dir    { 0 };
			unsigned long        dir_inode { 0 };
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

		unsigned const           fs_id { 1 }; /* change if multiple instantiated */

		unsigned const           root_nodeid { 1 }; /* XXX ever the case ? */

		size_t const _packet_max { tx.bulk_buffer_size() / Session::TX_QUEUE_SIZE };

		Avl_tree<Avl_file>  _files     { };
		Avl_tree<Avl_entry> _dirs      { };
		Allocator_avl       _open_dirs { &heap };

		Signal_handler<Filesystem> _handler { env.ep(), *this, &Filesystem::_handle_submit };

		void _handle_submit() { _handle_ack(); }

		void _handle_ack();
		void _handle_packet_stream();

		void _create    (MessageFs &);
		void _lookup    (MessageFs &);
		void _sync      (MessageFs &);
		void _unlink    (MessageFs &);
		void _get_attr  (MessageFs &);
		void _open_file (MessageFs &);
		void _read_file (MessageFs &);
		void _close_file(MessageFs &);
		void _close_dir (MessageFs &);

		void _handle_read_file(Packet &);
		bool _handle_read_dir (Packet &);

		enum { MAX_PACKETS = 64 };

		Bit_allocator<MAX_PACKETS> _idx_alloc            { };
		Packet                     _packets[MAX_PACKETS] { };

		/* read_dir operation in flight */
		struct {
			Packet   pkg;
			unsigned expect;
			unsigned queued;
			unsigned missing;
		} _pending { };

		Constructible<MessageFsCommit> _read_file_pending { };
		Constructible<MessageFsCommit> _read_dir_pending  { };
		Constructible<MessageFsCommit> _sync_pending      { };

		bool _queued_sync      { };
		bool _queued_file_read { };

		bool with_new_packet(auto const &fn)
		{
#if 1
			Packet packet { };

			bool ok = fn(packet);

			return ok;
#else
			return _idx_alloc.alloc().convert<bool>([&](auto const &id) {
				auto &packet = _packets[id];

				bool ok = fn(packet);

				if (!ok)
					_idx_alloc.free(id);

				return ok;
			}, [](auto) { return false; });
#endif
		}

		bool with_packet(auto &packet, auto const &fn)
		{
#if 1
			bool ok = fn(packet);

			if (!ok)
				return ok;

			tx.release_packet(packet);

			return ok;
#else
			for (unsigned idx = 0; idx < MAX_PACKETS; idx++) {
				auto &p = _packets[idx];

				if (p.handle()    != packet.handle() ||
				    p.operation() != packet.operation() ||
				    p.offset()    != packet.offset() ||
				    p.size()      != packet.size())
					continue;

				bool ok = fn(packet);

				if (!ok)
					return ok;

				_idx_alloc.free(idx);

				tx.release_packet(packet);

				return ok;
			}

			error(__func__, " unknown packet ", packet.handle());

			return false;
#endif
		}

		void with_open_dir(MessageFs const &msg, auto &entry, auto const &fn)
		{
			if (msg.nodeid > root_nodeid && !entry.dir.value) {
				if (entry.dir.value)
					warning(__func__, " nodeid=", msg.nodeid, " ", entry.path.string(), " already open ?");

				Dir_handle h { fs.dir(entry.path.string(), false) };

				entry.dir = h;

				fn();
			} else
				fn();
		}

		void with_dir_entry(uint64_t key, auto const &fn_hit, auto const &fn_unknown)
		{
			if (_dirs.first())
				_dirs.first()->with_entry(key, fn_hit, fn_unknown);
			else
				fn_unknown();
		}

		void with_file_entry(uint64_t key, auto const &fn_hit, auto const &fn_unknown)
		{
			if (_files.first())
				_files.first()->with_entry(key, fn_hit, fn_unknown);
			else
				fn_unknown();
		}

		void with_each_file(auto const &fn) { _files.for_each(fn); }

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

					packet = Packet(p, file_handle, Packet::READ, size, pos);

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

					packet = Packet(p, fh, Packet::SYNC, 0);

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
			         msg.buffer.size);
		}

	public:

		Filesystem(Env &env, Motherboard &mb) : mb(mb), env(env)
		{
			if (root.value != 0) {
				error("Filesystem offline - unexpected state");
				return;
			}

			fs.sigh(_handler);

			mb.bus_fs.add(this, receive_static<MessageFs>);

			String<MAX_PATH_LEN> p { "" };
			auto e = new (heap) Avl_entry(root_nodeid, p);
			e->with_dir([&](auto &dir) { dir.dir = root; });
			_dirs.insert(e);
		}

		bool receive(MessageFs &);
};
