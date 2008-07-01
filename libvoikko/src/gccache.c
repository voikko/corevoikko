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

#include "gccache.h"
#include "voikko_setup.h"
#include <string.h>
#include <stdlib.h>

void init_gc_cache(voikko_gc_cache * gc_cache) {
	memset(gc_cache, 0, sizeof(voikko_gc_cache));
}

const voikko_grammar_error * gc_error_from_cache(int handle, const wchar_t * text, size_t startpos) {
	if (!voikko_options.gc_cache.paragraph) return 0;	
	if (wcscmp(voikko_options.gc_cache.paragraph, text) != 0) return 0;
	voikko_gc_cache_entry * e = voikko_options.gc_cache.first_error;
	while (1) {
		if (!e) {
			// This will be initialized to zero meaning "no errors"
			static const voikko_grammar_error no_error;
			return &no_error;
		}
		if (e->error.startpos >= startpos)
			return &e->error;
		e = e->next_error;
	}
}

void gc_clear_cache(int handle) {
	if (voikko_options.gc_cache.paragraph)
		free(voikko_options.gc_cache.paragraph);
	voikko_gc_cache_entry * entry = voikko_options.gc_cache.first_error;
	while (entry) {
		voikko_gc_cache_entry * next = entry->next_error;
		free(entry);
		entry = next;
	}
	init_gc_cache(&voikko_options.gc_cache);
}

/**
 * Appends an entry to the grammar checker error cache.
 */
void gc_cache_append_error(int handle, voikko_gc_cache_entry * new_entry) {
	// TODO: errors should be inserted in correct order
	voikko_gc_cache_entry * entry = voikko_options.gc_cache.first_error;
	if (!entry) {
		voikko_options.gc_cache.first_error = new_entry;
		return;
	}
	while (entry) {
		if (!entry->next_error) {
			entry->next_error = new_entry;
			return;
		}
		entry = entry->next_error;
	}
}

/**
 * Create a new empty grammar checker error cache entry.
 */
voikko_gc_cache_entry * gc_new_cache_entry() {
	voikko_gc_cache_entry * e = calloc(1, sizeof(voikko_gc_cache_entry));
	return e;
}

void gc_paragraph_to_cache(int handle, const wchar_t * text, size_t textlen) {
	gc_clear_cache(handle);
	voikko_options.gc_cache.paragraph = malloc((textlen + 1) * sizeof(wchar_t));
	if (!voikko_options.gc_cache.paragraph) return;
	memcpy(voikko_options.gc_cache.paragraph, text, textlen * sizeof(wchar_t));
	voikko_options.gc_cache.paragraph[textlen] = L'\0';
	size_t pos = 0;
	while (1) {
		wchar_t * errorpos = wcsstr(text + pos, L"joten kuten");
		if (errorpos) {
			voikko_gc_cache_entry * e = gc_new_cache_entry();
			if (!e) return;
			e->error.error_code = 1; //FIXME: GCERR_WRITE_TOGETHER
			e->error.startpos = errorpos - text;
			e->error.errorlen = 11;
			gc_cache_append_error(handle, e);
			pos = e->error.startpos + e->error.errorlen;
		}
		else break;
	}
}
