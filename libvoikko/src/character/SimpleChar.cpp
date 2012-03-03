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
 * Portions created by the Initial Developer are Copyright (C) 2010
 * the Initial Developer. All Rights Reserved.
 * 
 * Contributor(s):
 *   Sjur Moshagen
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

#include "character/SimpleChar.hpp"

namespace libvoikko { namespace character {

wchar_t SimpleChar::lower(wchar_t input) {
	// Basic Latin
	if (input >= 0x41 && input <= 0x5A) {
		// A-Z
		return input + 0x20;
	}
	// Latin-1 Supplement
	if (input >= 0xC0 && input <= 0xD6) {
		// À-Ö
		return input + 0x20;
	}
	if (input >= 0xD8 && input <= 0xDE) {
		// Ø-Þ
		return input + 0x20;
	}
	// Latin Extended-A
	if (input >= 0x0100 && input <= 0x0136 && input % 2 == 0) {
		// Ā-Ķ
		return input + 1;
	}
	if (input >= 0x0139 && input <= 0x0147 && input % 2 == 1) {
		// Ĺ-Ň
		return input + 1;
	}
	if (input >= 0x014A && input <= 0x0176 && input % 2 == 0) {
		// Ŋ-Ŷ
		return input + 1;
	}
	if (input >= 0x0179 && input <= 0x017D && input % 2 == 1) {
		// Ź-Ž
		return input + 1;
	}
	// Cyrillic
	if (input >= 0x0400 && input <= 0x040F) {
		// Cyrillic Ѐ-Џ
		return input + 0x50;
	}
	if (input >= 0x0410 && input <= 0x042F) {
		// Cyrillic А-Я
		return input + 0x20;
	}
	if (input >= 0x0460 && input <= 0x0480 && input % 2 == 0) {
		// Cyrillic Ѡ-Ҁ
		return input + 1;
	}
	if (input >= 0x048A && input <= 0x0522 && input % 2 == 0) {
		// Cyrillic Ҋ-Ԣ
		return input + 1;
	}
	// TODO: other Unicode character ranges are not yet mapped
	return input;
}

wchar_t SimpleChar::upper(wchar_t input) {
	// Basic Latin
	if (input >= 0x61 && input <= 0x7A) {
		// A-Z
		return input - 0x20;
	}
	// Latin-1 Supplement
	if (input >= 0xE0 && input <= 0xF6) {
		// À-Ö
		return input - 0x20;
	}
	if (input >= 0xF8 && input <= 0xFE) {
		// Ø-þ
		return input - 0x20;
	}
	// Latin Extended-A
	if (input >= 0x0101 && input <= 0x0137 && input % 2 == 1) {
		// Ā-Ķ
		return input - 1;
	}
	if (input >= 0x013A && input <= 0x0148 && input % 2 == 0) {
		// Ĺ-Ň
		return input - 1;
	}
	if (input >= 0x014B && input <= 0x0177 && input % 2 == 1) {
		// Ŋ-Ŷ
		return input - 1;
	}
	if (input >= 0x017A && input <= 0x017E && input % 2 == 0) {
		// Ź-Ž
		return input - 1;
	}
	// Cyrillic
	if (input >= 0x0450 && input <= 0x045F) {
		// Cyrillic ѐ-џ
		return input - 0x50;
	}
	if (input >= 0x0430 && input <= 0x044F) {
		// Cyrillic а-я
		return input - 0x20;
	}
	if (input >= 0x0461 && input <= 0x0481 && input % 2 == 1) {
		// Cyrillic ѡ-ҁ
		return input - 1;
	}
	if (input >= 0x048B && input <= 0x0523 && input % 2 == 1) {
		// Cyrillic ҋ-ԣ
		return input - 1;
	}
	// TODO: other Unicode character ranges are not yet mapped
	return input;
}

bool SimpleChar::isUpper(wchar_t input) {
	return input != lower(input);
}

bool SimpleChar::isLower(wchar_t input) {
	return input != upper(input);
}

bool SimpleChar::isDigit(wchar_t input) {
	return (input >= 0x30 && input <= 0x39);
}

bool SimpleChar::isWhitespace(wchar_t input) {
	return (input >= 0x09 && input <= 0x0D) ||
	       input == 0x20 ||
	       input == 0x85 ||
	       input == 0xA0 ||
	       input == 0x1680 ||
	       input == 0x180E ||
	       (input >= 0x2000 && input <= 0x200A) ||
	       input == 0x2028 ||
	       input == 0x2029 ||
	       input == 0x202F ||
	       input == 0x205F ||
	       input == 0x3000;
}

bool SimpleChar::equalsIgnoreCase(const wchar_t * str1, const wchar_t * str2) {
	if (wcslen(str1) != wcslen(str2)) {
		return false;
	}
	for (size_t i = 0; str1[i] != L'\0'; i++) {
		if (lower(str1[i]) != lower(str2[i])) {
			return false;
		}
	}
	return true;
}

} }
