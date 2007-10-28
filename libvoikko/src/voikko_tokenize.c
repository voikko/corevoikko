/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2007 Harri Pitkänen <hatapitk@iki.fi>
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
#include "voikko_setup.h"
#include "voikko_charset.h"
#include "voikko_utils.h"
#include <wchar.h>
#include <stdlib.h>

size_t word_length(const wchar_t * text, size_t textlen) {
	size_t wlen = 0;
	int processing_number = 0;
	
	size_t adot;
	if (voikko_options.ignore_dot) adot = 1;
	else adot = 0;
	
	while (wlen < textlen) {
		switch (get_char_type(text[wlen])) {
			case CHAR_LETTER:
				processing_number = 0;
				wlen++;
				break;
			case CHAR_DIGIT:
				processing_number = 1;
				wlen++;
				break;
			case CHAR_WHITESPACE:
			case CHAR_UNKNOWN:
				return wlen;
			case CHAR_PUNCTUATION:
				switch (text[wlen]) {
					case L'\'':
					case L':':
						if (wlen + 1 == textlen) return wlen;
						if (get_char_type(text[wlen+1]) ==
						    CHAR_LETTER) break;
						return wlen;
					case L'-':
						if (wlen + 1 == textlen) return wlen + 1;
						if (wcschr(L"\",", text[wlen+1]))
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

enum voikko_token_type voikko_next_token_ucs4(const wchar_t * text, size_t textlen, size_t * tokenlen) {
	size_t i;
	size_t wlen;
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
			for (i = 1; i < textlen; i++) {
				if (get_char_type(text[i]) != CHAR_WHITESPACE) {
					*tokenlen = i;
					return TOKEN_WHITESPACE;
				}
			}
			*tokenlen = textlen;
			return TOKEN_WHITESPACE;
		case CHAR_PUNCTUATION:
			if (text[0] == L'-') {
				if (textlen == 1) {
					*tokenlen = 1;
					return TOKEN_PUNCTUATION;
				}
				wlen = word_length(text + 1, textlen - 1);
				if (wlen == 0) {
					*tokenlen = 1;
					return TOKEN_PUNCTUATION;
				}
				*tokenlen = wlen + 1;
				return TOKEN_WORD;
			}
			*tokenlen = 1;
			return TOKEN_PUNCTUATION;
		case CHAR_UNKNOWN:
			*tokenlen = 1;
			return TOKEN_UNKNOWN;
	}
	return TOKEN_NONE; // unreachable
}

enum voikko_token_type voikko_next_token_cstr(const char * text, size_t textlen, size_t * tokenlen) {
	wchar_t * text_ucs4;
	enum voikko_token_type result;
	if (text == 0) return TOKEN_NONE;
	text_ucs4 = voikko_cstrtoucs4(text, voikko_options.encoding, textlen);
	if (text_ucs4 == 0) return TOKEN_NONE;
	result = voikko_next_token_ucs4(text_ucs4, wcslen(text_ucs4), tokenlen);
	free(text_ucs4);
	return result;
}
