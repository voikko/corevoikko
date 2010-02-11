/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2006 - 2009 Harri Pitkänen <hatapitk@iki.fi>
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

#ifndef VOIKKO_CHARACTER_CHARSET_H
#define VOIKKO_CHARACTER_CHARSET_H

#include <cstddef>

namespace libvoikko {

enum char_type {CHAR_UNKNOWN, CHAR_LETTER, CHAR_DIGIT, CHAR_WHITESPACE, CHAR_PUNCTUATION};

/** Returns character type for given character
 * @param c character to check
 * @return the character type
 */
char_type get_char_type(wchar_t c);

/** 
 * Checks if a character is a Finnish quotation mark.
 * @param c character to check
 * @return true if the character is a Finnish quotation mark, false otherwise.
 */
bool isFinnishQuotationMark(wchar_t c);

/** Normalises an unicode string according to our conventions
 * @param word string to normalise
 * @param len length of the string
 * @return the normalised string or null if normalisation failed
 */
wchar_t * voikko_normalise(const wchar_t * word, size_t len);

/** Formats modified string to more closely match the original
 * @param orig original string
 * @param orig_len length of the original string
 * @param modified pointer to the modified string. The string may be
 *        relocated if it needs to be expanded, otherwise it is modified
 *        in place.
 * @param modified_len length of the modified string
 */
void voikko_cset_reformat(const wchar_t * orig, size_t orig_len, wchar_t ** modified, size_t modified_len);

/**
 * Converts a wide character to lowercase equivalent. This function only converts
 * "simple" characters that have one-to-one mapping between upper and lower case
 * forms and the conversion is not language dependent. More or less this is a
 * locale independent "towlower".
 */
wchar_t simpleLower(wchar_t input);

}

#endif
