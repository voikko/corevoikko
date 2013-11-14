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

char * StringUtils::copy(const char * const original) {
	size_t len = strlen(original);
	char * copied = new char[len + 1];
	strcpy(copied, original);
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
		if (!std::wcschr(L"iIvVxX", word[i])) {
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
