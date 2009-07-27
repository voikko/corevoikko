/* Libvoikko: Library of Finnish language tools
 * Copyright (C) 2009 Harri Pitkänen <hatapitk@iki.fi>
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
#include "utils/utils.hpp"
#include <cstring>
#include <cstdlib>
#include "wchar.h"

using namespace std;

namespace libvoikko { namespace utils {

wchar_t * StringUtils::ucs4FromUtf8(const char * const original) {
	return voikko_cstrtoucs4(original, "UTF-8", strlen(original));
}

char * StringUtils::utf8FromUcs4(const wchar_t * const original) {
	return utf8FromUcs4(original, wcslen(original));
}

char * StringUtils::utf8FromUcs4(const wchar_t * const original, size_t wlen) {
	return voikko_ucs4tocstr(original, "UTF-8", wlen);
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

} }
