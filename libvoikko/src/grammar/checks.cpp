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

#include "grammar/checks.hpp"
#include "grammar/error.hpp"
#include "grammar/cachesetup.hpp"
#include "setup/setup.hpp"
#include "utils/utils.hpp"
#include <cstdlib>
#include <cstring>
#include <wctype.h>

namespace libvoikko {

/**
 * Appends an entry to the grammar checker error cache.
 */
void gc_cache_append_error(int handle, voikko_gc_cache_entry * new_entry) {
	voikko_gc_cache_entry * entry = voikko_options.gc_cache.first_error;
	if (!entry) {
		voikko_options.gc_cache.first_error = new_entry;
		return;
	}
	if (entry->error.startpos > new_entry->error.startpos) {
		new_entry->next_error = voikko_options.gc_cache.first_error;
		voikko_options.gc_cache.first_error = new_entry;
		return;
	}
	while (1) {
		if (!entry->next_error) {
			entry->next_error = new_entry;
			return;
		}
		if (entry->error.startpos <= new_entry->error.startpos &&
		    entry->next_error->error.startpos > new_entry->error.startpos) {
			new_entry->next_error = entry->next_error;
			entry->next_error = new_entry;
			return;
		}
		entry = entry->next_error;
	}
}

/**
 * Create a new empty grammar checker error cache entry.
 * @param suggestions number of suggestions that will be added to this entry
 */
voikko_gc_cache_entry * gc_new_cache_entry(int suggestions) {
	// TODO: C allocation
	voikko_gc_cache_entry * e = (voikko_gc_cache_entry *) calloc(1, sizeof(voikko_gc_cache_entry));
	if (!e) return 0;
	if (suggestions > 0) {
		// TODO: C allocation
		e->error.suggestions = (char **) calloc(suggestions + 1, sizeof(char *));
		if (!e->error.suggestions) {
			free(e);
			return 0;
		}
	}
	return e;
}

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
		voikko_gc_cache_entry * e = gc_new_cache_entry(1);
		if (!e) return;
		e->error.error_code = GCERR_WRITE_TOGETHER;
		e->error.startpos = sentence->tokens[i].pos;
		e->error.errorlen = 10 + sentence->tokens[i+1].tokenlen;
		e->error.suggestions[0] = new char[11];
		if (e->error.suggestions[0]) {
			strcpy(e->error.suggestions[0], "jotenkuten");
		}
		gc_cache_append_error(handle, e);
	}
}

void gc_local_punctuation(int handle, const gc_sentence * sentence) {
	voikko_gc_cache_entry * e;
	for (int i = 0; i < sentence->token_count; i++) {
		gc_token t = sentence->tokens[i];
		switch (t.type) {
		case TOKEN_WHITESPACE:
			if (t.tokenlen > 1) {
				e = gc_new_cache_entry(0);
				if (!e) return;
				e->error.error_code = GCERR_EXTRA_WHITESPACE;
				e->error.startpos = sentence->tokens[i].pos;
				e->error.errorlen = sentence->tokens[i].tokenlen;
				gc_cache_append_error(handle, e);
			}
			else if (i + 1 < sentence->token_count) {
				gc_token t2 = sentence->tokens[i+1];
				if (t2.type != TOKEN_PUNCTUATION ||
				    t2.str[0] != L',') continue;
				e = gc_new_cache_entry(0);
				if (!e) return;
				e->error.error_code = GCERR_SPACE_BEFORE_PUNCTUATION;
				e->error.startpos = sentence->tokens[i].pos;
				e->error.errorlen = 2;
				gc_cache_append_error(handle, e);
			}
			break;
		case TOKEN_PUNCTUATION:
			if (i == 0) {
				if (t.str[0] == L'(' || t.str[0] == L')' ||
				    t.str[0] == L'"' || t.str[0] == L'\'') continue;
				e = gc_new_cache_entry(0);
				if (!e) return;
				e->error.error_code = GCERR_INVALID_SENTENCE_STARTER;
				if (sentence->tokens[i].pos == 0) {
					e->error.startpos = 0;
					e->error.errorlen = 1;
				}
				else {
					e->error.startpos = sentence->tokens[i].pos - 1;
					e->error.errorlen = 2;
				}
				gc_cache_append_error(handle, e);
				continue;
			}
			if (t.str[0] == L',' && i + 1 < sentence->token_count) {
				gc_token t2 = sentence->tokens[i+1];
				if (t2.type != TOKEN_PUNCTUATION ||
				    t2.str[0] != L',') continue;
				e = gc_new_cache_entry(0);
				if (!e) return;
				e->error.error_code = GCERR_EXTRA_COMMA;
				e->error.startpos = sentence->tokens[i].pos;
				e->error.errorlen = 2;
				gc_cache_append_error(handle, e);
			}
			break;
		case TOKEN_NONE:
		case TOKEN_WORD:
		case TOKEN_UNKNOWN:
			break;
		}
	}
}

void gc_character_case(int handle, const gc_sentence * sentence) {
	int first_word_seen = 0;
	for (int i = 0; i < sentence->token_count; i++) {
		gc_token t = sentence->tokens[i];
		if (t.type != TOKEN_WORD) continue;
		if (!first_word_seen) {
			first_word_seen = 1;
			if (!iswupper(t.str[0]) && !iswdigit(t.str[0])) {
				voikko_gc_cache_entry * e = gc_new_cache_entry(0);
				if (!e) return;
				e->error.error_code = GCERR_WRITE_FIRST_UPPERCASE;
				e->error.startpos = t.pos;
				e->error.errorlen = t.tokenlen;
				gc_cache_append_error(handle, e);
			}
			continue;
		}
		if (!t.is_valid_word) continue;
		if (!t.first_letter_lcase) continue;
		if (t.possible_sentence_start) continue;
		if (!iswupper(t.str[0])) continue;
		voikko_gc_cache_entry * e = gc_new_cache_entry(0);
		if (!e) return;
		e->error.error_code = GCERR_WRITE_FIRST_LOWERCASE;
		e->error.startpos = t.pos;
		e->error.errorlen = t.tokenlen;
		gc_cache_append_error(handle, e);
	}
}

void gc_repeating_words(int handle, const gc_sentence * sentence) {
	for (int i = 0; i < sentence->token_count - 2; i++) {
		if (sentence->tokens[i].type != TOKEN_WORD) continue;
		if (sentence->tokens[i + 1].type != TOKEN_WHITESPACE) {
			i++;
			continue;
		}
		if (sentence->tokens[i + 2].type != TOKEN_WORD) {
			i += 2;
			continue;
		}
		if (wcscmp(sentence->tokens[i].str, sentence->tokens[i + 2].str)) {
			i++;
			continue;
		}
		voikko_gc_cache_entry * e = gc_new_cache_entry(1);
		if (!e) return;
		e->error.error_code = GCERR_REPEATING_WORD;
		e->error.startpos = sentence->tokens[i].pos;
		e->error.errorlen = sentence->tokens[i].tokenlen +
		                    sentence->tokens[i + 1].tokenlen +
		                    sentence->tokens[i + 2].tokenlen;
		e->error.suggestions[0] = voikko_ucs4tocstr(sentence->tokens[i].str,
		                          "UTF-8", sentence->tokens[i].tokenlen);
		gc_cache_append_error(handle, e);
	}
}

void gc_end_punctuation(int handle, const gc_paragraph * paragraph) {
	if (voikko_options.accept_titles_in_gc && paragraph->sentence_count == 1) return;
	if (voikko_options.accept_unfinished_paragraphs_in_gc) return;
	
	gc_sentence * sentence = paragraph->sentences[paragraph->sentence_count - 1];
	gc_token * token = sentence->tokens + (sentence->token_count - 1);
	if (token->type == TOKEN_PUNCTUATION) return;
	voikko_gc_cache_entry * e = gc_new_cache_entry(0);
	if (!e) return;
	e->error.error_code = GCERR_TERMINATING_PUNCTUATION_MISSING;
	e->error.startpos = token->pos;
	e->error.errorlen = token->tokenlen;
	gc_cache_append_error(handle, e);
}

}
