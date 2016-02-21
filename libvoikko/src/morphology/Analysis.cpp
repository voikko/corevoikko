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
#include <array>

using namespace libvoikko::utils;

namespace libvoikko { namespace morphology {

static constexpr std::array<const char *,21> KEY_TO_STRING { {
	"BASEFORM",
	"CLASS",
	"COMPARISON",
	"FOCUS",
	"FSTOUTPUT",
	"KYSYMYSLIITE",
	"MALAGA_VAPAA_JALKIOSA",
	"MOOD",
	"NEGATIVE",
	"NUMBER",
	"PARTICIPLE",
	"PERSON",
	"POSSESSIVE",
	"POSSIBLE_GEOGRAPHICAL_NAME",
	"REQUIRE_FOLLOWING_VERB",
	"SIJAMUOTO",
	"STRUCTURE",
	"TENSE",
	"WEIGHT",
	"WORDBASES",
	"WORDIDS"
} };

static const std::map<std::string, Analysis::Key> STRING_TO_KEY = {
	{"BASEFORM", Analysis::Key::BASEFORM},
	{"CLASS", Analysis::Key::CLASS},
	{"COMPARISON", Analysis::Key::COMPARISON},
	{"FOCUS", Analysis::Key::FOCUS},
	{"FSTOUTPUT", Analysis::Key::FSTOUTPUT},
	{"KYSYMYSLIITE", Analysis::Key::KYSYMYSLIITE},
	{"MALAGA_VAPAA_JALKIOSA", Analysis::Key::MALAGA_VAPAA_JALKIOSA},
	{"MOOD", Analysis::Key::MOOD},
	{"NEGATIVE", Analysis::Key::NEGATIVE},
	{"NUMBER", Analysis::Key::NUMBER},
	{"PARTICIPLE", Analysis::Key::PARTICIPLE},
	{"PERSON", Analysis::Key::PERSON},
	{"POSSESSIVE", Analysis::Key::POSSESSIVE},
	{"POSSIBLE_GEOGRAPHICAL_NAME", Analysis::Key::POSSIBLE_GEOGRAPHICAL_NAME},
	{"REQUIRE_FOLLOWING_VERB", Analysis::Key::REQUIRE_FOLLOWING_VERB},
	{"SIJAMUOTO", Analysis::Key::SIJAMUOTO},
	{"STRUCTURE", Analysis::Key::STRUCTURE},
	{"TENSE", Analysis::Key::TENSE},
	{"WEIGHT", Analysis::Key::WEIGHT},
	{"WORDBASES", Analysis::Key::WORDBASES},
	{"WORDIDS", Analysis::Key::WORDIDS}
};

Analysis::Analysis() : keys(0) {
}

Analysis::~Analysis() {
	deleteKeys();
	std::map<Key, wchar_t *>::iterator it = attributes.begin();
	while (it != attributes.end()) {
		if (!constAttributes[static_cast<int>(it->first)]) {
			delete[] it->second;
		}
		++it;
	}
}

void Analysis::addAttribute(Key key, wchar_t * value) {
	attributes.insert(std::make_pair(key, value));
}

void Analysis::addConstAttribute(Key key, const wchar_t * value) {
	attributes.insert(std::make_pair(key, const_cast<wchar_t *>(value)));
	constAttributes.set(static_cast<int>(key));
}

void Analysis::removeAttribute(Key key) {
	std::map<Key, wchar_t *>::iterator valueI = attributes.find(key);
	if (valueI != attributes.end()) {
		if (constAttributes[static_cast<int>(valueI->first)]) {
			constAttributes[static_cast<int>(valueI->first)] = false;
		}
		else {
			delete[] valueI->second;
		}
		attributes.erase(valueI);
	}
}

const char ** Analysis::getKeys() const {
	return const_cast<const char **>(keys);
}

std::vector<Analysis::Key> Analysis::getInternalKeys() const {
	std::vector<Analysis::Key> keys;
	for (auto keyAndValue: attributes) {
		keys.push_back(keyAndValue.first);
	}
	return keys;
}

const wchar_t * Analysis::getValue(Analysis::Key key) const {
	std::map<Key, wchar_t *>::const_iterator valueI =
	    attributes.find(key);
	if (valueI == attributes.end()) {
		return nullptr;
	}
	else {
		return valueI->second;
	}
}

const wchar_t * Analysis::getValueS(const char * key) const {
	std::map<std::string, Analysis::Key>::const_iterator keyI = STRING_TO_KEY.find(std::string(key));
	if (keyI == STRING_TO_KEY.end()) {
		return nullptr;
	}
	else {
		return this->getValue(keyI->second);
	}
}

void Analysis::deleteKeys() {
	delete[] keys;
	keys = 0;
}

void Analysis::seal() {
	deleteKeys();
	keys = new const char*[attributes.size() + 1];
	std::map<Key, wchar_t *>::const_iterator it = attributes.begin();
	size_t i = 0;
	while (it != attributes.end()) {
		keys[i++] = KEY_TO_STRING[(int)it++->first];
	}
	keys[i] = 0;
}

} }
