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
 * Portions created by the Initial Developer are Copyright (C) 2009 - 2010
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
	 * Makes a copy of a string.
	 */
	static char * copy(const char * const original);

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
	 * Checks if given null terminated string may be a chapter number (3, 3.4, 3.65.3, ...).
	 */
	static bool isChapterNumber(const wchar_t * word);
	
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
