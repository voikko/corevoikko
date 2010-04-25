/* Libvoikko: Library of Finnish language tools
 * Copyright (C) 2009 - 2010 Harri Pitkänen <hatapitk@iki.fi>
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

#ifndef VOIKKO_UTILS_STRINGUTILS
#define VOIKKO_UTILS_STRINGUTILS

#include <cstring>

namespace libvoikko { namespace utils {

class StringUtils {

	public:
	
	/**
	 * Creates an UCS4 string from a null terminated UTF-8 string.
	 * Returns a null pointer if memory allocation fails or input
	 * string is not valid UTF-8.
	 */
	static wchar_t * ucs4FromUtf8(const char * const original);
	static wchar_t * ucs4FromUtf8(const char * const original, size_t byteCount);
	
	/**
	 * Creates an UTF-8 string from a null terminated UCS4 string.
	 * Returns a null pointer if memory allocation fails or input
	 * string contains invalid codepoints.
	 */
	static char * utf8FromUcs4(const wchar_t * const original);
	static char * utf8FromUcs4(const wchar_t * const original, size_t wlen);
	
	/**
	 * Makes a copy of a string.
	 */
	static wchar_t * copy(const wchar_t * const original);

	/**
	 * Delete a C++ allocated char string array.
	 */
	static void deleteCStringArray(char ** stringArray);
	
	/**
	 * Convert a C++ allocated char string to use
	 * malloc for memory allocation.
	 */
	static void convertCStringToMalloc(char * & cString);
	
	/**
	 * Convert a C++ allocated char string array to use
	 * malloc for memory allocation.
	 */
	static void convertCStringArrayToMalloc(char ** & stringArray);

	/**
	 * Creates a null terminated string where some special characters that cannot
	 * be sent to malaga have been stripped.
	 */
	static wchar_t * stripSpecialCharsForMalaga(wchar_t * & original, size_t origLength);
	
	/**
	 * Checks if given null terminated string is a positive integer.
	 */
	static bool isInteger(const wchar_t * word);
	
	/**
	 * Checks if given null terminated string is a roman numeral.
	 */
	static bool isRomanNumeral(const wchar_t * word);
	
	/**
	 * Checks if given null terminated string is a possible list item such as
	 * 1, 2, a, b, A, B, i, ii, iii ...
	 */
	static bool isPossibleListItem(const wchar_t * word);
};

} }

#endif
