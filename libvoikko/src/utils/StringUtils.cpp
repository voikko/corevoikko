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
#include <cstring>
#include <cstdlib>

using namespace std;

namespace libvoikko { namespace utils {

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
	for (numberOfStrings = 0; stringArray[numberOfStrings] != 0; numberOfStrings++);
	
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
	
	for (size_t i = 0; i < numberOfStrings; i++) {
		delete[] stringArray[i];
	}
	delete[] stringArray;
	
	stringArray = newArray;
}

} }
