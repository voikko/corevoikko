/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2007 - 2009 Harri Pitkänen <hatapitk@iki.fi>
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

#include "voikko_defs.h"
#include "setup/setup.hpp"
#include "utils/utils.hpp"
#include "character/charset.hpp"
#include <wchar.h>
#include <cstdlib>

namespace libvoikko {

size_t word_length(const wchar_t * text, size_t textlen) {
	size_t wlen = 0;
	bool processing_number = false;
	
	size_t adot;
	if (voikko_options.ignore_dot) adot = 1;
	else adot = 0;
	
	while (wlen < textlen) {
		switch (get_char_type(text[wlen])) {
			case CHAR_LETTER:
				processing_number = false;
				wlen++;
				break;
			case CHAR_DIGIT:
				processing_number = true;
				wlen++;
				break;
			case CHAR_WHITESPACE:
			case CHAR_UNKNOWN:
				return wlen;
			case CHAR_PUNCTUATION:
				switch (text[wlen]) {
					case L'\'':
					case L'\u2019': /* RIGHT SINGLE QUOTATION MARK */
					case L':':
						if (wlen + 1 == textlen) return wlen;
						if (get_char_type(text[wlen+1]) ==
						    CHAR_LETTER) break;
						return wlen;
					case L'-':
					case L'\u00AD': /* SOFT HYPHEN */
					case L'\u2010': /* HYPHEN */
					case L'\u2011': /* NON-BREAKING HYPHEN */
						if (wlen + 1 == textlen) return wlen + 1;
						if (wcschr(L"\"»\u201d,", text[wlen+1]))
							return wlen + 1;
						switch (get_char_type(text[wlen+1])) {
							case CHAR_LETTER:
							case CHAR_DIGIT:
								break;
							case CHAR_WHITESPACE:
							case CHAR_UNKNOWN:
								return wlen + 1;
							case CHAR_PUNCTUATION:
								return wlen;
						}
						break;
					case L'.':
						if (wlen + 1 == textlen) return wlen + adot;
						switch (get_char_type(text[wlen+1])) {
							case CHAR_LETTER:
							case CHAR_DIGIT:
								break;
							case CHAR_WHITESPACE:
							case CHAR_UNKNOWN:
							case CHAR_PUNCTUATION:
								return wlen + adot;
						}
						break;
					case L',':
						if (!processing_number) return wlen;
						if (wlen + 1 == textlen) return wlen;
						if (get_char_type(text[wlen+1]) ==
						    CHAR_DIGIT) break;
						return wlen;
						
					default:
						return wlen;
				}
				wlen++;
		}
	}
	return textlen;
}

VOIKKOEXPORT enum voikko_token_type voikko_next_token_ucs4(int /*handle*/, const wchar_t * text, size_t textlen,
                                                           size_t * tokenlen) {
	if (textlen == 0) {
		*tokenlen = 0;
		return TOKEN_NONE;
	}
	switch (get_char_type(text[0])) {
		case CHAR_LETTER:
		case CHAR_DIGIT:
			*tokenlen = word_length(text, textlen);
			return TOKEN_WORD;
		case CHAR_WHITESPACE:
			for (size_t i = 1; i < textlen; i++) {
				if (get_char_type(text[i]) != CHAR_WHITESPACE) {
					*tokenlen = i;
					return TOKEN_WHITESPACE;
				}
			}
			*tokenlen = textlen;
			return TOKEN_WHITESPACE;
		case CHAR_PUNCTUATION:
			if (wcschr(L"-\u2010\u2011", text[0])) {
				if (textlen == 1) {
					*tokenlen = 1;
					return TOKEN_PUNCTUATION;
				}
				size_t wlen = word_length(text + 1, textlen - 1);
				if (wlen == 0) {
					*tokenlen = 1;
					return TOKEN_PUNCTUATION;
				}
				*tokenlen = wlen + 1;
				return TOKEN_WORD;
			}
			else if (textlen >= 3 && text[0] == L'.' && text[1] == L'.'
			         && text[2] == L'.') {
				*tokenlen = 3;
				return TOKEN_PUNCTUATION;
			}
			*tokenlen = 1;
			return TOKEN_PUNCTUATION;
		case CHAR_UNKNOWN:
			*tokenlen = 1;
			return TOKEN_UNKNOWN;
	}
	return TOKEN_NONE; // unreachable
}

VOIKKOEXPORT enum voikko_token_type voikko_next_token_cstr(int handle, const char * text, size_t textlen,
                                                           size_t * tokenlen) {
	if (text == 0) return TOKEN_NONE;
	wchar_t * text_ucs4 = voikko_cstrtoucs4(text, voikko_options.encoding, textlen);
	if (text_ucs4 == 0) return TOKEN_NONE;
	voikko_token_type result =
		voikko_next_token_ucs4(handle, text_ucs4, wcslen(text_ucs4), tokenlen);
	delete[] text_ucs4;
	return result;
}

}
