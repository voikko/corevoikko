/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2006 Harri Pitkänen <hatapitk@iki.fi>
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

#ifndef VOIKKO_UTILS_H
#define VOIKKO_UTILS_H

#include <stddef.h>

#ifdef DEBUG
#define LOG(x) printf x
#else
#define LOG(X)
#endif

#define VOIKKO_CONSONANTS L"bcdfghjklmnpqrstvwxz\u0161\u017e"
#define VOIKKO_VOWELS L"aeiouy\u00e4\u00f6"

enum casetype {CT_NO_LETTERS, CT_ALL_LOWER, CT_FIRST_UPPER, CT_COMPLEX, CT_ALL_UPPER};

/** Converts a multibyte string to a wide character string
 * @param word multibyte string to convert
 * @param encoding iconv name of the multibyte encoding
 * @param len length of the multibyte string in bytes or 0, if the string is null terminated
 * @return a newly allocated null terminated wide character string or null, if
 *         conversion failed.
 */
wchar_t * voikko_cstrtoucs4(const char * word, const char * encoding, size_t len);

/** Converts a wide character string to a multibyte string
 * @param word wide character string to convert
 * @param encoding iconv name of the multibyte encoding
 * @param len number of characters in the wide character string or 0, if the string is null terminated
 * @return a newly allocated null terminated multibyte string or null, if
 *         conversion failed.
 */
char * voikko_ucs4tocstr(const wchar_t * word, const char * encoding, size_t len);

/** Simple string hashing algorithm
 * @param word string to hash
 * @param len length of the word
 * @param order order of the resulting hash value
 * @return integer from range [0, 2^order - 1]
 */
int voikko_hash(const wchar_t * word, size_t len, int order);

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

#endif
