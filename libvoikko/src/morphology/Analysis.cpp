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
 * Portions created by the Initial Developer are Copyright (C) 2009
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
