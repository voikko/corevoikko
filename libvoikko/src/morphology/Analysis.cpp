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

static const std::map<std::string, voikko_mor_analysis::Key> STRING_TO_KEY = {
	{"BASEFORM", voikko_mor_analysis::Key::BASEFORM},
	{"CLASS", voikko_mor_analysis::Key::CLASS},
	{"COMPARISON", voikko_mor_analysis::Key::COMPARISON},
	{"FOCUS", voikko_mor_analysis::Key::FOCUS},
	{"FSTOUTPUT", voikko_mor_analysis::Key::FSTOUTPUT},
	{"KYSYMYSLIITE", voikko_mor_analysis::Key::KYSYMYSLIITE},
	{"MALAGA_VAPAA_JALKIOSA", voikko_mor_analysis::Key::MALAGA_VAPAA_JALKIOSA},
	{"MOOD", voikko_mor_analysis::Key::MOOD},
	{"NEGATIVE", voikko_mor_analysis::Key::NEGATIVE},
	{"NUMBER", voikko_mor_analysis::Key::NUMBER},
	{"PARTICIPLE", voikko_mor_analysis::Key::PARTICIPLE},
	{"PERSON", voikko_mor_analysis::Key::PERSON},
	{"POSSESSIVE", voikko_mor_analysis::Key::POSSESSIVE},
	{"POSSIBLE_GEOGRAPHICAL_NAME", voikko_mor_analysis::Key::POSSIBLE_GEOGRAPHICAL_NAME},
	{"REQUIRE_FOLLOWING_VERB", voikko_mor_analysis::Key::REQUIRE_FOLLOWING_VERB},
	{"SIJAMUOTO", voikko_mor_analysis::Key::SIJAMUOTO},
	{"STRUCTURE", voikko_mor_analysis::Key::STRUCTURE},
	{"TENSE", voikko_mor_analysis::Key::TENSE},
	{"WEIGHT", voikko_mor_analysis::Key::WEIGHT},
	{"WORDBASES", voikko_mor_analysis::Key::WORDBASES},
	{"WORDIDS", voikko_mor_analysis::Key::WORDIDS}
};

voikko_mor_analysis::voikko_mor_analysis() : keys(0) {
}

voikko_mor_analysis::~voikko_mor_analysis() {
	deleteKeys();
	std::map<Key, wchar_t *>::iterator it = attributes.begin();
	while (it != attributes.end()) {
		if (!constAttributes[static_cast<int>(it->first)]) {
			delete[] it->second;
		}
		++it;
	}
}

void voikko_mor_analysis::addAttribute(Key key, wchar_t * value) {
	attributes.insert(std::make_pair(key, value));
}

void voikko_mor_analysis::addConstAttribute(Key key, const wchar_t * value) {
	attributes.insert(std::make_pair(key, const_cast<wchar_t *>(value)));
	constAttributes.set(static_cast<int>(key));
}

void voikko_mor_analysis::removeAttribute(Key key) {
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

const char ** voikko_mor_analysis::getKeys() const {
	return const_cast<const char **>(keys);
}

std::vector<voikko_mor_analysis::Key> voikko_mor_analysis::getInternalKeys() const {
	std::vector<voikko_mor_analysis::Key> keys;
	for (auto keyAndValue: attributes) {
		keys.push_back(keyAndValue.first);
	}
	return keys;
}

const wchar_t * voikko_mor_analysis::getValue(voikko_mor_analysis::Key key) const {
	std::map<Key, wchar_t *>::const_iterator valueI =
	    attributes.find(key);
	if (valueI == attributes.end()) {
		return nullptr;
	}
	else {
		return valueI->second;
	}
}

const wchar_t * voikko_mor_analysis::getValueS(const char * key) const {
	std::map<std::string, voikko_mor_analysis::Key>::const_iterator keyI = STRING_TO_KEY.find(std::string(key));
	if (keyI == STRING_TO_KEY.end()) {
		return nullptr;
	}
	else {
		return this->getValue(keyI->second);
	}
}

void voikko_mor_analysis::deleteKeys() {
	delete[] keys;
	keys = 0;
}

void voikko_mor_analysis::seal() {
	deleteKeys();
	keys = new const char*[attributes.size() + 1];
	std::map<Key, wchar_t *>::const_iterator it = attributes.begin();
	size_t i = 0;
	while (it != attributes.end()) {
		keys[i++] = KEY_TO_STRING[(int)it++->first];
	}
	keys[i] = 0;
}
