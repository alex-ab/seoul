#pragma once

#include <base/mutex.h>

namespace Seoul {

	class Lock : public Genode::Mutex {

		public:

			void unprotect(auto const &fn) { release(); fn(); acquire(); }
	};
}
