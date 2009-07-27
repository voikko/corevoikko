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

#include "morphology/Analysis.hpp"
#include "utils/StringUtils.hpp"

using namespace libvoikko::utils;

namespace libvoikko { namespace morphology {

Analysis::Analysis() : keys(0) {
}

Analysis::~Analysis() {
	deleteKeys();
	std::map<std::string, wchar_t *>::iterator it = attributes.begin();
	while (it != attributes.end()) {
		delete[] it++->second;
	}
}

void Analysis::addAttribute(const char * key, wchar_t * value) {
	attributes.insert(std::make_pair(std::string(key), value));
	recreateKeys();
}

const char ** Analysis::getKeys() const {
	return const_cast<const char **>(keys);
}

const wchar_t * Analysis::getValue(const char * key) const {
	std::map<std::string, wchar_t *>::const_iterator valueI =
	    attributes.find(std::string(key));
	if (valueI == attributes.end()) {
		return 0;
	}
	else {
		return valueI->second;
	}
}

void Analysis::deleteKeys() {
	delete[] keys;
	keys = 0;
}

void Analysis::recreateKeys() {
	deleteKeys();
	keys = new const char*[attributes.size() + 1];
	std::map<std::string, wchar_t *>::const_iterator it = attributes.begin();
	size_t i = 0;
	while (it != attributes.end()) {
		keys[i++] = it++->first.c_str();
	}
	keys[i] = 0;
}

} }
