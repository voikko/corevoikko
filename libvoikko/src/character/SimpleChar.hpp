/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2010 Harri Pitkänen <hatapitk@iki.fi>
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

#ifndef VOIKKO_CHARACTER_SIMPLE_CHAR_H
#define VOIKKO_CHARACTER_SIMPLE_CHAR_H

#include <cwchar>

namespace libvoikko { namespace character {

/**
 * Utility functions for language independent character operations.
 */
class SimpleChar {

	public:
		/**
		 * Converts a wide character to lowercase equivalent. This function only converts
		 * "simple" characters that have one-to-one mapping between upper and lower case
		 * forms.
		 */
		static wchar_t lower(wchar_t input);
		
		/**
		 * Converts a wide character to uppercase equivalent. This function only converts
		 * "simple" characters that have one-to-one mapping between upper and lower case
		 * forms.
		 */
		static wchar_t upper(wchar_t input);
		
		/**
		 * Checks if a character is an upper case letter.
		 */
		static bool isUpper(wchar_t input);
		
		/**
		 * Checks if a character is an lower case letter.
		 */
		static bool isLower(wchar_t input);
		
		/**
		 * Checks if a character is a digit.
		 */
		static bool isDigit(wchar_t input);
		
		/**
		 * Checks if a character is a whitespace character.
		 */
		static bool isWhitespace(wchar_t input);
};

} }

#endif
