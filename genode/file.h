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


#include <util/bit_allocator.h>
#include <file_system_session/connection.h>


namespace Seoul {
	class Filesystem;

	using namespace Genode;
	using namespace File_system;

	typedef File_system::Packet_descriptor Packet;
	typedef File_system::Session           Session;
}


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

		Signal_handler<Filesystem> _handler { env.ep(), *this, &Filesystem::_handle_submit };

		void _handle_submit() { _handle_ack(); }

		void _handle_ack()
		{
			Genode::Mutex::Guard guard(mutex);

			while (true) {
				auto packet = _pending.pkg.size() ? _pending.pkg
				                                  : tx.try_get_acked_packet();

				if (_pending.pkg.size())
					_pending.pkg = { };

				if (!packet.size())
					break;
#if 0
		Node_handle handle()    const { return _handle;   }
		Opcode      operation() const { return _op;       }
		seek_off_t  position()  const { return _op != Opcode::WRITE_TIMESTAMP ? _position : 0; }
		size_t      length()    const { return _op != Opcode::WRITE_TIMESTAMP ? _length : 0;   }
		bool        succeeded() const { return _success;  }
#endif

				switch (packet.operation()) {
				case Packet::READ_READY:
					error(__func__, " READ READY");
					break;

				case Packet::READ:
				{
					bool wakeup = false;

					bool verbose = false;

					if (verbose)
						log(__func__, " READ ",
						    packet.succeeded() ? "succeeded" : "failure",
						    " handle=", packet.handle(),
						    " len=", packet.length(), "/?", sizeof(Directory_entry),
						    " node handle ", packet.handle().value,
						    " offset=", packet.offset());

					bool const ok = with_packet(packet, [&](auto) {

						if (!packet.succeeded() || !_read_dir_pending.constructed()) {
							error("queue read dir failure");
							return false;
						}

						auto &msg = _read_dir_pending->fs_delayed;

						auto count   = packet.length() / sizeof(Directory_entry);
						auto entries = reinterpret_cast<Directory_entry *>(tx.packet_content(packet));

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

						auto more = queue_read_dir(dir, _pending.expect,
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

					/* error message already before, if appropriate */
					if (!ok)
						return;

					continue;
				}
				case Packet::WRITE:
					error(__func__, " WRITE");
					break;

				case Packet::SYNC:
					error(__func__, " SYNC");
					break;

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

		struct {
			Dir_handle  dir  { 0 };
			String<128> path { };
		} _nodeid_dirs[64] { };

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

		Constructible<MessageFsCommit> _read_dir_pending { };

		bool with_new_packet(auto const &fn)
		{
			return _idx_alloc.alloc().convert<bool>([&](auto const &id) {
				auto &packet = _packets[id];

				bool ok = fn(packet);

				if (!ok)
					_idx_alloc.free(id);

				return ok;
			}, [](auto) { return false; });
		}

		bool with_packet(auto &packet, auto const &fn)
		{
			for (unsigned idx = 0; idx < MAX_PACKETS; idx++) {
				auto &p = _packets[idx];

				if (p.offset() != packet.offset() || p.size() != packet.size())
					continue;

				bool ok = fn(packet);

				if (!ok)
					return ok;

				_idx_alloc.free(idx);

				tx.release_packet(packet);

				return ok;
			}

			return false;
		}

		void with_open_dir(MessageFs const &msg, auto &entry, auto const &fn)
		{
			if (msg.nodeid > root_nodeid && !entry.dir.value) {
				Dir_handle h { fs.dir(entry.path.string(), false) };

				entry.dir = h;

				fn();
			} else
				fn();
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

			auto &entry = _nodeid_dirs[root_nodeid];

			entry.dir  = root;
			entry.path = "";
		}

		bool receive(MessageFs &msg)
		{
			if (fs_id != msg.fs_id)
				return false;

			/* XXX dynamic lookup structure required */
			if (msg.nodeid >= sizeof(_nodeid_dirs) / sizeof(_nodeid_dirs[0])) {
				error("nodeid=", msg.nodeid, "out of range - "
				      "msg.type=", unsigned(msg.type));
				msg.fail();
				return true;
			}

			bool wakeup = false;

			switch (msg.type) {
			case MessageFs::OPEN_DIR:
			{
				Genode::Mutex::Guard guard(mutex);

				bool verbose = false;

				if (verbose)
					log(" File::OPEN_DIR nodeid=", msg.nodeid);

				auto &entry = _nodeid_dirs[msg.nodeid];

				try {
					with_open_dir(msg, entry, [&]() {
						if (verbose)
							log (" File::OPEN_DIR nodeid=", msg.nodeid, " -> fh=", entry.dir.value, " '", entry.path, "'");

						msg.fh = entry.dir.value;
					});
				} catch (...) {
					error("open dir failed '", entry.path, "'");
					msg.fail();
				}
				break;
			}
			case MessageFs::CLOSE_DIR:
			{
				if (msg.fh == root.value) /* keep root dir open */
					break;

				Genode::Mutex::Guard guard(mutex);

				auto &entry = _nodeid_dirs[msg.nodeid];

				if (entry.dir.value != msg.fh)
					warning("close_dir: fh does not match inode");

				Dir_handle h { msg.fh };

				try {
					fs.close(h);
					entry.dir.value = 0;
				} catch (...) {
					error("closing dir failed");
				}
				break;
			}
			case MessageFs::READ_DIR:
			{
				{
					Genode::Mutex::Guard guard(mutex);

					if (_nodeid_dirs[msg.nodeid].dir.value != msg.fh)
						warning("read_dir: fh does not match inode");

					if (_read_dir_pending.constructed()) {
						/*
						 * mb.bus_fs_commit.send(*_read_dir_pending)
						 * passes here. Commit collected dirs and finish
						 * current read_dir
						 */
						msg.buffer = _read_dir_pending->fs_delayed.buffer;

						_read_dir_pending.destruct();

						return true;
					}

					Dir_handle const dir { msg.fh };

					/* continuation of case A shortage */
					if (_pending.queued) {
						_read_dir_pending.construct(fs_id, msg.nodeid, msg);

						Signal_transmitter(_handler).submit();

						return true;
					}

					_pending.expect  = fs.num_entries(dir);
					_pending.queued  = queue_read_dir(dir, _pending.expect);
					_pending.missing = _pending.expect - _pending.queued;

					if (!_pending.queued) {
						msg.empty_read_dir();
						break;
					}

					_read_dir_pending.construct(fs_id, msg.nodeid, msg);
				}

				tx.wakeup();

				break;
			}
			case MessageFs::GET_ATTR:
			{
				try {
					auto &entry = _nodeid_dirs[msg.nodeid];

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
					error(" File::GET_ATTR exception");
				}
				break;
			}
			case MessageFs::LOOKUP:
			{
				auto * path = reinterpret_cast<char const *>(msg.buffer.start);

				bool verbose = false;

				try {
					auto &parent_dir = _nodeid_dirs[msg.nodeid];

					with_open_dir(msg, parent_dir, [&]() {

						Dir_handle parent = parent_dir.dir;

						if (verbose)
							log(" File::LOOKUP parent inode=", msg.nodeid,
							    " fh=", parent.value, " path='", path, "'");

						/* STAT_ONLY leads to already_open exception for files XXX */
						File_handle h = fs.file(parent, path, READ_ONLY, false);

						Status status = fs.status(h);

						if (verbose)
							log(" File::LOOKUP -> inode=", status.inode,
							    " size=", status.size,
							    " time=", status.modification_time.ms_since_1970);

						if (status.directory()) {

							String<128> g_path { parent_dir.path, "/", path };

							if (verbose)
								log(" File::LOOKUP - dir '", g_path,
								    "' -> nodeid=", status.inode);

							if (status.inode >= sizeof(_nodeid_dirs) / sizeof(_nodeid_dirs[0])) {
								error(" File::LOOKUP - inode too large ", status.inode);
								return;
							}

							auto &dir = _nodeid_dirs[status.inode];

							if (dir.dir.value)
								error("already open ? dir nodeid=", status.inode, " already fh=", dir.dir.value, " '", dir.path, "' vs '", g_path, "'");

							dir.path = g_path;
						}

						msg.add_status(status.inode, status.size,
						               status.modification_time.ms_since_1970,
						               status.directory(), status.symlink());

						msg.writeable  = status.rwx.writeable;
						msg.readable   = status.rwx.readable;
						msg.executable = status.rwx.executable;

						fs.close(h);
					});
				} catch (...) {
					msg.fail();
					error("exception ", __LINE__);
				}
				break;
			}
			default:
				Logging::panic("unsupported call");
				break;
			}

			if (wakeup)
				tx.wakeup();

			return true;
		}

		unsigned queue_read_dir(Dir_handle const &dir_handle,
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
};
