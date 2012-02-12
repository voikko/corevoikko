/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2012 Harri Pitkänen <hatapitk@iki.fi>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *********************************************************************************/

#ifndef LIBVOIKKO_FST_TRANSITION_H
#define LIBVOIKKO_FST_TRANSITION_H

#include <stdint.h>

namespace libvoikko { namespace fst {

struct transinfo_t {
	uint32_t targetState : 24;
	uint8_t moreTransitions : 8;
};

struct Transition {
	uint16_t symIn;
	uint16_t symOut;
	transinfo_t transInfo;
};

struct OverflowCell {
	uint32_t moreTransitions;
	uint32_t padding;
};

} }

#endif
