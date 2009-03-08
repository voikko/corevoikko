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

#ifndef VOIKKO_GRAMMAR_CACHE_H
#define VOIKKO_GRAMMAR_CACHE_H

#include "voikko_defs.h"

namespace libvoikko {

/**
 * Returns a pointer to a cached grammar error or null, if there are no cached
 * results for given paragraph.
 */
const voikko_grammar_error * gc_error_from_cache(int handle, const wchar_t * text,
                             size_t startpos, int skiperrors);

/**
 * Performs grammar checking on the entire paragraph and stores the results
 * to cache.
 */
void gc_paragraph_to_cache(int handle, const wchar_t * text, size_t textlen);

/**
 * Appends an entry to the grammar checker error cache.
 */
void gc_cache_append_error(int /*handle*/, voikko_gc_cache_entry * new_entry);

/**
 * Create a new empty grammar checker error cache entry.
 * @param suggestions number of suggestions that will be added to this entry
 */
voikko_gc_cache_entry * gc_new_cache_entry(int suggestions);

}

#endif
