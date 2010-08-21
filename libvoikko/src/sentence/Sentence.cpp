/* Libvoikko: Library of Finnish language tools
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

#include "sentence/Sentence.hpp"
#include "character/SimpleChar.hpp"
#include "tokenizer/Tokenizer.hpp"
#include "character/charset.hpp"

using namespace libvoikko::character;

namespace libvoikko { namespace sentence {

/**
 * Returns true if given word ending with a dot can be interpreted
 * as a single word, false if the dot does not belong to the word.
 */
static bool dot_part_of_word(voikko_options_t * voikkoOptions, const wchar_t * text, size_t len) {
	if (len < 2) return false;
	
	// ordinal numbers and dates
	bool onlyNumbersOrDots = true;
	for (size_t i = 0; i < len - 1; i++) {
		if (text[i] != L'.' && !SimpleChar::isDigit(text[i])) {
			onlyNumbersOrDots = false;
			break;
		}
	}
	if (onlyNumbersOrDots) {
		return true;
	}
	
	// abbreviations
	if (voikkoOptions->speller->spell(text, len) != spellchecker::SPELL_FAILED) {
		return true;
	}
	return false;
}

voikko_sentence_type Sentence::next(voikko_options_t * options,
		const wchar_t * text, size_t textlen, size_t * sentencelen) {
	voikko_token_type token = TOKEN_WORD;
	size_t slen = 0;
	size_t tokenlen;
	size_t previous_token_start = 0;
	voikko_token_type previous_token_type = TOKEN_NONE;
	bool end_found = false;
	bool in_quotation = false;
	bool end_dotword = false;
	bool possible_end_punctuation = false;
	while (token != TOKEN_NONE && textlen > slen) {
		int ignore_dot_saved = options->ignore_dot;
		options->ignore_dot = 0;
		token = tokenizer::Tokenizer::nextToken(options, text + slen,
		                               textlen - slen, &tokenlen);
		options->ignore_dot = ignore_dot_saved;
		if (end_found && !in_quotation) {
			if (token != TOKEN_WHITESPACE) {
				*sentencelen = slen;
				if (end_dotword || possible_end_punctuation) return SENTENCE_POSSIBLE;
				else return SENTENCE_PROBABLE;
			}
		}
		else if (token == TOKEN_PUNCTUATION) {
			wchar_t punct = text[slen];
			if (wcschr(L"!?", punct)) {
				end_found = true;
				if (in_quotation) {
					possible_end_punctuation = true;
				}
			}
			else if ((punct == L'.' && tokenlen == 3) || punct == L'\u2026') {
				// ellipsis
				end_found = true;
				possible_end_punctuation = true;
			}
			else if (punct == L'.') {
				end_found = true;
				if (slen != 0 &&
				    previous_token_type == TOKEN_WORD &&
				    dot_part_of_word(options, text + previous_token_start,
				      slen - previous_token_start + 1)) {
					end_dotword = true;
				}
			}
			else if (punct == L':') {
				end_found = true;
				possible_end_punctuation = true;
			}
			else if (isFinnishQuotationMark(punct) || punct == L'\u201C') {
				in_quotation = !in_quotation;
				if (!in_quotation && slen + 1 < textlen && text[slen + 1] == L',') {
					// Comma immediately after ending quote suggests that
					// the sentence most likely did not end here.
					end_found = false;
					possible_end_punctuation = false;
				}
			}
		}
		previous_token_start = slen;
		previous_token_type = token;
		slen += tokenlen;
	}
	*sentencelen = textlen;
	return SENTENCE_NONE;
}

} }
