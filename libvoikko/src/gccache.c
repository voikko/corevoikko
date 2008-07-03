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
#include "gcerror.h"
#include "gcanalysis.h"
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

/**
 * GC errors from static list of incorrect patterns
 */
void gc_static_replacements(int handle, const gc_sentence * sentence) {
	for (int i = 0; i < sentence->token_count - 2; i++) {
		gc_token t = sentence->tokens[i];
		if (t.type != TOKEN_WORD) continue;
		if (wcscmp(t.str, L"joten")) continue;
		t = sentence->tokens[i+1];
		if (t.type != TOKEN_WHITESPACE) continue;
		t = sentence->tokens[i+2];
		if (t.type != TOKEN_WORD) continue;
		if (wcscmp(t.str, L"kuten")) continue;
		voikko_gc_cache_entry * e = gc_new_cache_entry();
		if (!e) return;
		e->error.error_code = GCERR_WRITE_TOGETHER;
		e->error.startpos = sentence->tokens[i].pos;
		e->error.errorlen = 10 + sentence->tokens[i+1].tokenlen;
		gc_cache_append_error(handle, e);
	}
}

/**
 * GC errors due to wrong context independent use of punctuation or whitespace
 * within a sentence.
 */
void gc_local_punctuation(int handle, const gc_sentence * sentence) {
	for (int i = 0; i < sentence->token_count; i++) {
		gc_token t = sentence->tokens[i];
		if (t.type != TOKEN_WHITESPACE) continue;
		if (t.tokenlen == 1) continue;
		voikko_gc_cache_entry * e = gc_new_cache_entry();
		if (!e) return;
		e->error.error_code = GCERR_EXTRA_WHITESPACE;
		e->error.startpos = sentence->tokens[i].pos;
		e->error.errorlen = sentence->tokens[i].tokenlen;
		gc_cache_append_error(handle, e);
	}
}

void gc_paragraph_to_cache(int handle, const wchar_t * text, size_t textlen) {
	gc_clear_cache(handle);
	voikko_options.gc_cache.paragraph = malloc((textlen + 1) * sizeof(wchar_t));
	if (!voikko_options.gc_cache.paragraph) return;
	memcpy(voikko_options.gc_cache.paragraph, text, textlen * sizeof(wchar_t));
	voikko_options.gc_cache.paragraph[textlen] = L'\0';
	gc_paragraph * para = gc_analyze_paragraph(handle, text, textlen);
	if (!para) return;
	for (int i = 0; i < para->sentence_count; i++) {
		gc_static_replacements(handle, para->sentences[i]);
		gc_local_punctuation(handle, para->sentences[i]);
	}
	free_gc_paragraph(para);
}
