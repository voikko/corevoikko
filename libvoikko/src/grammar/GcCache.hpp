/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2008 - 2010 Harri Pitkänen <hatapitk@iki.fi>
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

#ifndef VOIKKO_GRAMMAR_GCCACHE_HPP
#define VOIKKO_GRAMMAR_GCCACHE_HPP

#include "grammar/CacheEntry.hpp"

namespace libvoikko { namespace grammar {

/**
 * Grammar checker cache. Currently the cache can hold only one paragraph.
 */
class GcCache {
	public:
	/** Constructs an empty cache */
	GcCache();
	
	/** Clears the cache */
	void clear();
	
	/** Null terminated string containing the paragraph text. */
	wchar_t * paragraph;
	
	/** First error in linked list. */
	grammar::CacheEntry * firstError;
};

} }

#endif
