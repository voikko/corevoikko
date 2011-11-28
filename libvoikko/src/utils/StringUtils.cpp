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

#include "utils/StringUtils.hpp"
#include "utf8/utf8.hpp"
#include <cstring>
#include <cstdlib>
#include <cwchar>

using namespace std;

namespace libvoikko { namespace utils {

wchar_t * StringUtils::ucs4FromUtf8(const char * const original) {
	return ucs4FromUtf8(original, strlen(original));
}
	
wchar_t * StringUtils::ucs4FromUtf8(const char * const original, size_t byteCount) {
	wchar_t * ucs4Buffer = 0;
	try {
		size_t chars = utf8::distance(original, original + byteCount);
		ucs4Buffer = new wchar_t[chars + 1];
		const char * origPtr = original;
		for (size_t i = 0; i < chars; i++) {
			// Using unchecked function because validity was already
			// checked with utf8::distance.
			ucs4Buffer[i] = utf8::unchecked::next(origPtr);
		}
		ucs4Buffer[chars] = L'\0';
		return ucs4Buffer;
	} catch (...) {
		// invalid UTF-8 sequence or not enough memory
		delete[] ucs4Buffer;
		return 0;
	}
}

char * StringUtils::utf8FromUcs4(const wchar_t * const original) {
	return utf8FromUcs4(original, wcslen(original));
}

char * StringUtils::utf8FromUcs4(const wchar_t * const original, size_t wlen) {
	char * utf8Buffer;
	try {
		utf8Buffer = new char[wlen * 6 + 1];
	} catch (...) {
		// not enough memory
		return 0;
	}
	try {
		char * utfPtr = utf8Buffer;
		for (size_t i = 0; i < wlen; i++) {
			utfPtr = utf8::append(original[i], utfPtr);
		}
		*utfPtr = '\0';
		return utf8Buffer;
	} catch (...) {
		// invalid codepoint
		delete[] utf8Buffer;
		return 0;
	}
}

wchar_t * StringUtils::copy(const wchar_t * const original) {
	size_t len = wcslen(original);
	wchar_t * copied = new wchar_t[len + 1];
	wcscpy(copied, original);
	return copied;
}

void StringUtils::deleteCStringArray(char ** stringArray) {
	if (stringArray) {
		for (char ** p = stringArray; *p; p++) {
			delete[] *p;
		}
		delete[] stringArray;
	}
}

void StringUtils::convertCStringToMalloc(char * & cString) {
	if (!cString) {
		return;
	}
	
	size_t strLen = strlen(cString);
	char * newString = (char *) malloc((strLen + 1) * sizeof(char));
	if (!newString) {
		return;
	}
	strcpy(newString, cString);
	delete[] cString;
	cString = newString;
}

void StringUtils::convertCStringArrayToMalloc(char ** & stringArray) {
	if (!stringArray) {
		return;
	}
	
	size_t numberOfStrings;
	for (numberOfStrings = 0; stringArray[numberOfStrings] != 0; numberOfStrings++) {
	}
	
	char ** newArray = (char **) malloc((numberOfStrings + 1) * sizeof(char *));
	if (!newArray) {
		return;
	}
	
	for (size_t i = 0; i < numberOfStrings; i++) {
		size_t strLen = strlen(stringArray[i]);
		newArray[i] = (char *) malloc((strLen + 1) * sizeof(char));
		if (!newArray[i]) {
			return;
		}
		strcpy(newArray[i], stringArray[i]);
	}
	newArray[numberOfStrings] = 0;
	
	for (size_t i = 0; i < numberOfStrings; i++) {
		delete[] stringArray[i];
	}
	delete[] stringArray;
	
	stringArray = newArray;
}

wchar_t * StringUtils::stripSpecialCharsForMalaga(wchar_t * & original, size_t origLength) {
	size_t wordLength = 0;
	wchar_t * wordBuffer = new wchar_t[origLength + 1];
	for (size_t i = 0; i < origLength; i++) {
		if (original[i] != L'\u00AD') {
			// not a soft hyphen
			wordBuffer[wordLength++] = original[i];
		}
	}
	wordBuffer[wordLength] = L'\0';
	return wordBuffer;
}

bool StringUtils::isInteger(const wchar_t * word) {
	for (size_t i = 0; word[i] != L'\0'; i++) {
		if (word[i] < 0x30 || word[i] > 0x39) {
			return false;
		}
	}
	return true;
}

bool StringUtils::isChapterNumber(const wchar_t * word) {
	bool dotLast = false;
	for (size_t i = 0; word[i] != L'\0'; i++) {
		if (word[i] == L'.') {
			if (i == 0 || dotLast) {
				return false;
			}
			dotLast = true;
		}
		else if (word[i] < 0x30 || word[i] > 0x39) {
			return false;
		}
		else {
			dotLast = false;
		}
	}
	return !dotLast;
}

bool StringUtils::isRomanNumeral(const wchar_t * word) {
	// TODO: does not handle all possibilities, does not validate
	for (size_t i = 0; word[i] != L'\0'; i++) {
		if (!wcschr(L"iIvVxX", word[i])) {
			return false;
		}
	}
	return true;
}


bool StringUtils::isPossibleListItem(const wchar_t * word) {
	if (wcslen(word) == 1) {
		return true;
	}
	if (isChapterNumber(word)) {
		return true;
	}
	if (isRomanNumeral(word)) {
		return true;
	}
	return false;
}

} }
