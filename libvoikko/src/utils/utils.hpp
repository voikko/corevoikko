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

#ifndef VOIKKO_UTILS_UTILS_H
#define VOIKKO_UTILS_UTILS_H

#include <cstddef>

#ifdef DEBUG
#define LOG(x) printf x
#else
#define LOG(X)
#endif

#define VOIKKO_CONSONANTS L"bcdfghjklmnpqrstvwxz\u0161\u017e"
#define VOIKKO_VOWELS L"aeiouy\u00e4\u00f6"

namespace libvoikko {

enum casetype {CT_NO_LETTERS, CT_ALL_LOWER, CT_FIRST_UPPER, CT_COMPLEX, CT_ALL_UPPER};

/** Returns the type of given string wrt the use of character cases.
 * @param word string to check
 * @param nchars length of the string
 * @return character case type of the string
 */
enum casetype voikko_casetype(const wchar_t * word, size_t nchars);

/** Changes a string to use defined character case
 * @param charcase character case
 * @param word string to change
 * @param nchars length of the string
 */
void voikko_set_case(enum casetype charcase, wchar_t * word, size_t nchars);

/** Detect special non-word strings such as URLs and email addresses
 * @param word string to check
 * @param nchars length of the string
 * @return true if string is a non-word, otherwise false
 */
bool voikko_is_nonword(const wchar_t * word, size_t nchars);

}

#endif
