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
 * Portions created by the Initial Developer are Copyright (C) 2008 - 2010
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

#include "setup/Dictionary.hpp"

using namespace std;

namespace libvoikko { namespace setup {

Dictionary::Dictionary() :
	morPath(),
	morBackend(),
	spellBackend(),
	suggestionBackend(),
	hyphenatorBackend(),
	language(),
	description(),
	isDefaultDict(false) {
}

Dictionary::Dictionary(const string & morPath, const string & morBackend,
                       const string & spellBackend,
                       const string & suggestionBackend,
                       const string & hyphenatorBackend,
                       const LanguageTag & language,
                       const string & description) :
	morPath(morPath),
	morBackend(morBackend),
	spellBackend(spellBackend),
	suggestionBackend(suggestionBackend),
	hyphenatorBackend(hyphenatorBackend),
	language(language),
	description(description),
	isDefaultDict(false) {
}
Dictionary::Dictionary(const string & morPath, const string & grammarPath, const string & morBackend,
                       const string & grammarBackend,
                       const string & spellBackend,
                       const string & suggestionBackend,
                       const string & hyphenatorBackend,
                       const LanguageTag & language,
                       const string & description) :
	morPath(morPath),
	grammarPath(grammarPath),
	morBackend(morBackend),
	grammarBackend(grammarBackend),
	spellBackend(spellBackend),
	suggestionBackend(suggestionBackend),
	hyphenatorBackend(hyphenatorBackend),
	language(language),
	description(description),
	isDefaultDict(false) {
}

Dictionary::Dictionary(const Dictionary & dictionary) :
	morPath(dictionary.morPath),
	morBackend(dictionary.morBackend),
	spellBackend(dictionary.spellBackend),
	suggestionBackend(dictionary.suggestionBackend),
	hyphenatorBackend(dictionary.hyphenatorBackend),
	language(dictionary.language),
	description(dictionary.description),
	isDefaultDict(dictionary.isDefaultDict) {
}

const string & Dictionary::getMorPath() const {
	return morPath;
}

const string & Dictionary::getMorBackend() const {
	return morBackend;
}

const string & Dictionary::getSpellBackend() const {
	return spellBackend;
}

const string & Dictionary::getGrammarBackend() const {
	return grammarBackend;
}
const string & Dictionary::getSuggestionBackend() const {
	return suggestionBackend;
}

const string & Dictionary::getHyphenatorBackend() const {
	return hyphenatorBackend;
}

const LanguageTag & Dictionary::getLanguage() const {
	return language;
}

const string & Dictionary::getDescription() const {
	return description;
}

bool Dictionary::isValid() const {
	return !language.getLanguage().empty();
}

bool Dictionary::isDefault() const {
	return isDefaultDict;
}

void Dictionary::setDefault(bool isDefault) {
	this->isDefaultDict = isDefault;
}

bool operator<(const Dictionary & d1, const Dictionary & d2) {
	return d1.language < d2.language;
}

} }
