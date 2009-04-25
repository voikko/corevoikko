/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2008 - 2009 Harri Pitkänen <hatapitk@iki.fi>
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
#include "grammar/cache.hpp"
#include "setup/setup.hpp"
#include "utils/utils.hpp"
#include <cstdlib>
#include <cstring>
#include <wctype.h>

using namespace libvoikko::grammar;

namespace libvoikko {

void gc_local_punctuation(int handle, const Sentence * sentence) {
	CacheEntry * e;
	for (size_t i = 0; i < sentence->tokenCount; i++) {
		Token t = sentence->tokens[i];
		switch (t.type) {
		case TOKEN_WHITESPACE:
			if (t.tokenlen > 1) {
				e = new CacheEntry(1);
				e->error.error_code = GCERR_EXTRA_WHITESPACE;
				e->error.startpos = sentence->tokens[i].pos;
				e->error.errorlen = sentence->tokens[i].tokenlen;
				e->error.suggestions[0] = new char[2];
				strcpy(e->error.suggestions[0], " ");
				gc_cache_append_error(handle, e);
			}
			else if (i + 1 < sentence->tokenCount) {
				Token t2 = sentence->tokens[i+1];
				if (t2.type != TOKEN_PUNCTUATION ||
				    t2.str[0] != L',') continue;
				e = new CacheEntry(1);
				e->error.error_code = GCERR_SPACE_BEFORE_PUNCTUATION;
				e->error.startpos = sentence->tokens[i].pos;
				e->error.errorlen = 2;
				e->error.suggestions[0] = new char[2];
				e->error.suggestions[0][0] = t2.str[0];
				e->error.suggestions[0][1] = L'\0';
				gc_cache_append_error(handle, e);
			}
			break;
		case TOKEN_PUNCTUATION:
			if (i == 0) {
				if (wcschr(L"()\"”»'", t.str[0])) {
					continue;
				}
				e = new CacheEntry(0);
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
			if (t.str[0] == L',' && i + 1 < sentence->tokenCount) {
				Token t2 = sentence->tokens[i+1];
				if (t2.type != TOKEN_PUNCTUATION ||
				    t2.str[0] != L',') continue;
				e = new CacheEntry(1);
				e->error.error_code = GCERR_EXTRA_COMMA;
				e->error.startpos = sentence->tokens[i].pos;
				e->error.errorlen = 2;
				e->error.suggestions[0] = new char[2];
				strcpy(e->error.suggestions[0], ",");
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

void gc_punctuation_of_quotations(int handle, const Sentence * sentence) {
	for (size_t i = 0; i + 2 < sentence->tokenCount; i++) {
		if (sentence->tokens[i].type != TOKEN_PUNCTUATION) {
			continue;
		}
		if (sentence->tokens[i + 1].type != TOKEN_PUNCTUATION) {
			continue;
		}
		if (!wcschr(L"\"»\u201d", sentence->tokens[i + 1].str[0])) {
			continue;
		}
		if (sentence->tokens[i + 2].type != TOKEN_PUNCTUATION) {
			continue;
		}
		if (sentence->tokens[i + 2].str[0] != L',') {
			continue;
		}
		
		wchar_t quoteChar = sentence->tokens[i + 1].str[0];
		CacheEntry * e;
		switch (sentence->tokens[i].str[0]) {
		case L'.':
			e = new CacheEntry(1);
			e->error.error_code = GCERR_INVALID_PUNCTUATION_AT_END_OF_QUOTATION;
			e->error.startpos = sentence->tokens[i].pos;
			e->error.errorlen = 3;
			e->error.suggestions[0] = new char[3];
			e->error.suggestions[0][0] = quoteChar;
			e->error.suggestions[0][1] = L',';
			e->error.suggestions[0][2] = L'\0';
			gc_cache_append_error(handle, e);
			break;
		case L'!':
		case L'?':
			e = new CacheEntry(1);
			e->error.error_code = GCERR_INVALID_PUNCTUATION_AT_END_OF_QUOTATION;
			e->error.startpos = sentence->tokens[i].pos;
			e->error.errorlen = 3;
			e->error.suggestions[0] = new char[3];
			e->error.suggestions[0][0] = (sentence->tokens[i].str[0] == L'!' ? '!' : '?');
			e->error.suggestions[0][1] = quoteChar;
			e->error.suggestions[0][2] = L'\0';
			gc_cache_append_error(handle, e);
			break;
		}
	}
}

void gc_character_case(int handle, const Sentence * sentence, bool isFirstInParagraph) {
	// Check if the sentence is written fully in upper case letters.
	// If it is, no character case errors should be reported.
	bool onlyUpper = true;
	for (size_t i = 0; i < sentence->tokenCount; i++) {
		Token t = sentence->tokens[i];
		if (t.type != TOKEN_WORD) continue;
		for (size_t j = 0; j < t.tokenlen; j++) {
			if (iswlower(t.str[j])) {
				onlyUpper = false;
				break;
			}
		}
		if (!onlyUpper) {
			break;
		}
	}
	if (onlyUpper) {
		return;
	}
	
	// If the sentence contains a tab character as a word separator, it is
	// likely that it is actually not a sentence. Maybe it would be best to
	// disable all checks in that case, but character case checks are more
	// likely to be wrong than others.
	for (size_t i = 0; i < sentence->tokenCount; i++) {
		Token t = sentence->tokens[i];
		if (t.type != TOKEN_WHITESPACE) {
			continue;
		}
		for (size_t j = 0; j < t.tokenlen; j++) {
			if (t.str[j] == L'\t') {
				return;
			}
		}
	}
	
	int first_word_seen = 0;
	for (size_t i = 0; i < sentence->tokenCount; i++) {
		Token t = sentence->tokens[i];
		if (t.type != TOKEN_WORD) continue;
		if (!first_word_seen) {
			first_word_seen = 1;
			bool needCheckingOfFirstUppercase = (!isFirstInParagraph || !voikko_options.accept_bulleted_lists_in_gc);
			if (needCheckingOfFirstUppercase && !iswupper(t.str[0]) && !iswdigit(t.str[0])) {
				CacheEntry * e = new CacheEntry(1);
				e->error.error_code = GCERR_WRITE_FIRST_UPPERCASE;
				e->error.startpos = t.pos;
				e->error.errorlen = t.tokenlen;
				wchar_t * suggestion = new wchar_t[t.tokenlen];
				suggestion[0] = towupper(t.str[0]);
				wcsncpy(suggestion + 1, t.str + 1, t.tokenlen - 1);
				e->error.suggestions[0] = voikko_ucs4tocstr(suggestion, "UTF-8", t.tokenlen);
				delete[] suggestion;
				gc_cache_append_error(handle, e);
			}
			continue;
		}
		if (!t.isValidWord) continue;
		if (!t.firstLetterLcase) continue;
		if (t.possibleSentenceStart) continue;
		if (t.tokenlen == 1) continue;
		if (!iswupper(t.str[0])) continue;
		CacheEntry * e = new CacheEntry(1);
		e->error.error_code = GCERR_WRITE_FIRST_LOWERCASE;
		e->error.startpos = t.pos;
		e->error.errorlen = t.tokenlen;
		wchar_t * suggestion = new wchar_t[t.tokenlen];
		suggestion[0] = towlower(t.str[0]);
		wcsncpy(suggestion + 1, t.str + 1, t.tokenlen - 1);
		e->error.suggestions[0] = voikko_ucs4tocstr(suggestion, "UTF-8", t.tokenlen);
		delete[] suggestion;
		gc_cache_append_error(handle, e);
	}
}

void gc_repeating_words(int handle, const Sentence * sentence) {
	for (size_t i = 0; i + 2 < sentence->tokenCount; i++) {
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
		CacheEntry * e = new CacheEntry(1);
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

void gc_end_punctuation(int handle, const Paragraph * paragraph) {
	if (voikko_options.accept_titles_in_gc && paragraph->sentenceCount == 1) return;
	if (voikko_options.accept_unfinished_paragraphs_in_gc) return;
	if (voikko_options.accept_bulleted_lists_in_gc) return;
	
	Sentence * sentence = paragraph->sentences[paragraph->sentenceCount - 1];
	Token * token = sentence->tokens + (sentence->tokenCount - 1);
	if (token->type == TOKEN_PUNCTUATION) return;
	CacheEntry * e = new CacheEntry(0);
	e->error.error_code = GCERR_TERMINATING_PUNCTUATION_MISSING;
	e->error.startpos = token->pos;
	e->error.errorlen = token->tokenlen;
	gc_cache_append_error(handle, e);
}

}
