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

#include "fst/Configuration.hpp"
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <cstring>

using namespace std;

namespace libvoikko { namespace fst {
	
	Configuration::Configuration(int bufferSize) :
		bufferSize(bufferSize),
		stackDepth(0),
		inputDepth(0),
		stateIndexStack(new uint32_t[bufferSize]),
		currentTransitionStack(new uint32_t[bufferSize]),
		inputSymbolStack(new uint16_t[bufferSize]),
		outputSymbolStack(new uint16_t[bufferSize]),
		inputLength(0)
		{ }
	
	Configuration::~Configuration() {
		delete[] stateIndexStack;
		delete[] currentTransitionStack;
		delete[] inputSymbolStack;
		delete[] outputSymbolStack;
	}
} }
