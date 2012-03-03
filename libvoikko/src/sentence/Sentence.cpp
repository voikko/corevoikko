/* The contents of this file are subject to the Mozilla Public License Version 
 * 1.1 (the "License"); you may not use this file except in compliance with 
 * the License. You may obtain a copy of the License at 
 * http://www.mozilla.org/MPL/
 * 
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 * 
 * The Original Code is Libvoikko: Library of natural language processing tools.
 * The Initial Developer of the Original Code is Harri Pitkänen <hatapitk@iki.fi>.
 * Portions created by the Initial Developer are Copyright (C) 2008 - 2010
 * the Initial Developer. All Rights Reserved.
 * 
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
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
	if (len < 2) {
		return false;
	}
	
	// Initials: Pertti K.
	if (len == 2 && SimpleChar::isUpper(text[0])) {
		return true;
	}
	
	// ordinal numbers and dates
	bool onlyNumbersOrDots = true;
	for (size_t i = 0; i < len - 1; i++) {
		// '-' may be used in expressions denoting ordinal range: "24.-26. joulukuuta"
		if (text[i] != L'.' && text[i] != L'-' && !SimpleChar::isDigit(text[i])) {
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
