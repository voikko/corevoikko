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

// TODO: C linkage
extern "C" {
#include "voikko_defs.h"
#include "voikko_setup.h"
#include "voikko_utils.h"
#include "voikko_spell.h"
}
#include <cstdlib>
#include <wctype.h>

namespace libvoikko {

/**
 * Returns 1 if given word ending with a dot can be interpreted
 * as a single word, 0 if the dot does not belong to the word.
 */
int dot_part_of_word(const wchar_t * text, size_t len) {
	if (len < 2) return 0;
	
	// ordinal numbers
	int only_numbers = 1;
	for (size_t i = 0; i < len - 1; i++) {
		if (!iswdigit(text[i])) {
			only_numbers = 0;
			break;
		}
	}
	if (only_numbers) return 1;
	
	// abbreviations
	if (voikko_do_spell(text, len) != SPELL_FAILED) return 1;
	return 0;
}

VOIKKOEXPORT enum voikko_sentence_type voikko_next_sentence_start_ucs4(int handle,
                          const wchar_t * text, size_t textlen, size_t * sentencelen) {
	enum voikko_token_type token = TOKEN_WORD;
	size_t slen = 0;
	size_t tokenlen;
	size_t previous_token_start = 0;
	enum voikko_token_type previous_token_type = TOKEN_NONE;
	int end_found = 0;
	int end_dotword = 0;
	int possible_end_punctuation = 0;
	while (token != TOKEN_NONE && textlen > slen) {
		int ignore_dot_saved = voikko_options.ignore_dot;
		voikko_options.ignore_dot = 0;
		token = voikko_next_token_ucs4(handle, text + slen,
		                               textlen - slen, &tokenlen);
		voikko_options.ignore_dot = ignore_dot_saved;
		if (end_found) {
			if (token != TOKEN_WHITESPACE) {
				*sentencelen = slen;
				if (end_dotword || possible_end_punctuation) return SENTENCE_POSSIBLE;
				else return SENTENCE_PROBABLE;
			}
		}
		else if (token == TOKEN_PUNCTUATION) {
			wchar_t punct = text[slen];
			if (wcschr(L"!?", punct)) end_found = 1;
			else if (punct == L'.' && tokenlen == 3) {
				end_found = 1;
				possible_end_punctuation = 1;
			}
			else if (punct == L'.') {
				end_found = 1;
				if (slen != 0 &&
				    previous_token_type == TOKEN_WORD &&
				    dot_part_of_word(text + previous_token_start,
				      slen - previous_token_start + 1)) {
					end_dotword = 1;
				}
			}
			else if (punct == L':') {
				end_found = 1;
				possible_end_punctuation = 1;
			}
		}
		previous_token_start = slen;
		previous_token_type = token;
		slen += tokenlen;
	}
	*sentencelen = textlen;
	return SENTENCE_NONE;
}

VOIKKOEXPORT enum voikko_sentence_type voikko_next_sentence_start_cstr(int handle, const char * text,
                          size_t textlen, size_t * sentencelen) {
	wchar_t * text_ucs4;
	enum voikko_sentence_type result;
	if (text == 0) return SENTENCE_NONE;
	text_ucs4 = voikko_cstrtoucs4(text, voikko_options.encoding, textlen);
	if (text_ucs4 == 0) return SENTENCE_NONE;
	result = voikko_next_sentence_start_ucs4(handle, text_ucs4, wcslen(text_ucs4),
	                                         sentencelen);
	free(text_ucs4);
	return result;
}

}
