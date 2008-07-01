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

#ifndef VOIKKO_GCCACHE_H
#define VOIKKO_GCCACHE_H

#include "voikko_defs.h"

/**
 * Grammar checker cache entry.
 */
struct voikko_gc_cache_entry_s {
	/** Grammar error */
	voikko_grammar_error error;
	/** Next error in linked list */
	struct voikko_gc_cache_entry_s * next_error;
};
typedef struct voikko_gc_cache_entry_s voikko_gc_cache_entry;

/**
 * Grammar checker cache. Currently the cache can hold only one paragraph.
 */
typedef struct {
	/** Null terminated string containing the paragraph text. */
	wchar_t * paragraph;
	/** First error in linked list. */
	voikko_gc_cache_entry * first_error;
} voikko_gc_cache;


/**
 * Initialise grammar checker cache.
 */
void init_gc_cache(voikko_gc_cache * gc_cache);


/**
 * Clears grammar checker error cache.
 */
void gc_clear_cache(int handle);


/**
 * Returns a pointer to a cached grammar error or null, if there are no cached
 * results for given paragraph.
 */
const voikko_grammar_error * gc_error_from_cache(int handle, const wchar_t * text, size_t startpos);

/**
 * Performs grammar checking on the entire paragraph and stores the results
 * to cache.
 */
void gc_paragraph_to_cache(int handle, const wchar_t * text, size_t textlen);

#endif
