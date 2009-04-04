/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2008 Harri Pitkänen <hatapitk@iki.fi>
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

#ifndef VOIKKO_GRAMMAR_CACHESETUP_H
#define VOIKKO_GRAMMAR_CACHESETUP_H

#include "voikko_defs.h"
#include "grammar/CacheEntry.hpp"

namespace libvoikko {

/**
 * Grammar checker cache. Currently the cache can hold only one paragraph.
 */
class voikko_gc_cache {
	public:
	/** Constructs an empty cache */
	voikko_gc_cache();
	
	/** Clears the cache */
	void clear();
	
	/** Null terminated string containing the paragraph text. */
	wchar_t * paragraph;
	/** First error in linked list. */
	grammar::CacheEntry * firstError;
};

/**
 * Clears grammar checker error cache.
 */
void gc_clear_cache(int handle);

}

#endif
