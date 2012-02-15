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

#ifndef LIBVOIKKO_FST_CONFIGURATION_H
#define LIBVOIKKO_FST_CONFIGURATION_H

#include <cstddef>
#include <string>
#include <stdint.h>

namespace libvoikko { namespace fst {
	
	struct Configuration {
		const int bufferSize;
		int stackDepth;
		int inputDepth;
		uint32_t * stateIndexStack;
		uint32_t * currentTransitionStack;
		uint16_t * inputSymbolStack;
		uint16_t * outputSymbolStack;
		/** Length of entire input string in characters */
		int inputLength;
		Configuration(int bufferSize);
		~Configuration();
	};

} }

#endif
