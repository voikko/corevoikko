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
 * Portions created by the Initial Developer are Copyright (C) 2010 - 2015
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

#include "setup/LanguageTag.hpp"

using namespace std;

namespace libvoikko { namespace setup {

LanguageTag::LanguageTag() :
	language(""),
	script(""),
	privateUse("") {
}

LanguageTag::LanguageTag(const LanguageTag & languageTag) :
	language(languageTag.language),
	script(languageTag.script),
	privateUse(languageTag.privateUse) {
}

static void lowercaseTagPart(string & tagPart) {
	for (size_t i = 0; i < tagPart.size(); ++i) {
		char current = tagPart.at(i);
		if (current >= 65 && current <= 90) {
			tagPart[i] = current + 32;
		}
	}
}

const string & LanguageTag::getLanguage() const {
	return language;
}

void LanguageTag::setLanguage(const string & language) {
	size_t splitPos = language.find("_");
	if (splitPos != string::npos) {
		// if geographical area (such as FI in fi_FI) is given, discard it
		this->language = language.substr(0, splitPos);
	} else {
		this->language = language;
	}
	lowercaseTagPart(this->language);
}

const string & LanguageTag::getScript() const {
	return script;
}

void LanguageTag::setScript(const string & script) {
	if (script.length() == 4) {
		this->script = script;
	}
}

const string & LanguageTag::getPrivateUse() const {
	return privateUse;
}

void LanguageTag::setPrivateUse(const string & privateUse) {
	this->privateUse = privateUse;
	for (size_t hyphenPos = this->privateUse.find("-"); hyphenPos != string::npos;
	     hyphenPos = this->privateUse.find("-")) {
		this->privateUse.erase(hyphenPos, 1);
	}
	lowercaseTagPart(this->privateUse);
}

void LanguageTag::setLanguageAndScript(const string & languageAndScript) {
	size_t splitPos = languageAndScript.find("-");
	if (splitPos != string::npos) {
		// TODO: the trailing part is script only if it is four letters long
		// We don't support other components for now so we just accept is as script anyway.
		setLanguage(languageAndScript.substr(0, splitPos));
		setScript(languageAndScript.substr(splitPos + 1));
	}
	else {
		setLanguage(languageAndScript);
	}
}

void LanguageTag::setBcp47(const string & bcp) {
	size_t splitPos = bcp.find("-x-");
	if (splitPos != string::npos) {
		setLanguageAndScript(bcp.substr(0, splitPos));
		setPrivateUse(bcp.substr(splitPos + 3));
	}
	else {
		setLanguageAndScript(bcp);
	}
}

string LanguageTag::toBcp47() const {
	string tag = this->language;
	if (!this->script.empty()) {
		tag.append("-");
		tag.append(this->script);
	}
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
	if (l1.script != l2.script) {
		return l1.script < l2.script;
	}
	return l1.privateUse < l2.privateUse;
}

} }
