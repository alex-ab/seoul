/*
 * \brief  Seoul component for Genode
 * \author Alexander Boettcher
 * \author Norman Feske
 * \author Markus Partheymueller
 * \author Benjamin Lamowski
 * \date   2011-11-18
 */

/*
 * Copyright (C) 2026 Alexander Boettcher
 *
 * This file is distributed under the terms of the GNU General Public License
 * version 2.
 */

#pragma once

enum Config_flags {
	CONFIG_SEOUL_MAP_SMALL        = 0x01,
	CONFIG_SEOUL_RDTSC_EXIT       = 0x02,
	CONFIG_SEOUL_VCPU_SAME_CPU    = 0x04,
	CONFIG_SEOUL_CPUID_NATIVE     = 0x08,
	CONFIG_SEOUL_MEMORY_VERBOSE   = 0x10,
	CONFIG_SEOUL_TRACK_EXITS      = 0x20,
	CONFIG_SEOUL_NO_GUI_HEURISTIC = 0x40,
};
