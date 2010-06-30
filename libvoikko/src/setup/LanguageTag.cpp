/* Libvoikko: Library of Finnish language tools
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

#include "setup/LanguageTag.hpp"

using namespace std;

namespace libvoikko { namespace setup {

LanguageTag::LanguageTag() :
	language(""),
	privateUse("") {
}

LanguageTag::LanguageTag(const LanguageTag & languageTag) :
	language(languageTag.language),
	privateUse(languageTag.privateUse) {
}

const string & LanguageTag::getLanguage() const {
	return language;
}

void LanguageTag::setLanguage(const string & language) {
	size_t splitPos = language.find("_");
	if (splitPos != string::npos) {
		// if geographical area (such as FI in fi_FI) is given, discard i
		this->language = language.substr(0, splitPos);
	} else {
		this->language = language;
	}
}

const string & LanguageTag::getPrivateUse() const {
	return privateUse;
}

void LanguageTag::setPrivateUse(const string & privateUse) {
	this->privateUse = privateUse;
}

string LanguageTag::toBcp47() const {
	string tag = this->language;
	if (!this->privateUse.empty()) {
		tag.append("-x-");
		tag.append(this->privateUse);
	}
	return tag;
}

bool operator<(const LanguageTag & l1, const LanguageTag & l2) {
	if (l1.language != l2.language) {
		return l1.language < l2.language;
	}
	return l1.privateUse < l2.privateUse;
}

} }
