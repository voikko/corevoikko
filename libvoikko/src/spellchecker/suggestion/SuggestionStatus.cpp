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

#include "spellchecker/suggestion/SuggestionStatus.hpp"

namespace libvoikko { namespace spellchecker { namespace suggestion {

SuggestionStatus::SuggestionStatus(const wchar_t * word, size_t wlen, size_t maxSuggestions) :
	word(word),
	wlen(wlen),
	maxCost(0),
	maxSuggestions(maxSuggestions),
	suggestionCount(0),
	currentCost(0) {
	suggestions = new Suggestion[maxSuggestions + 1];
}

SuggestionStatus::~SuggestionStatus() {
	for (size_t i = 0; i < suggestionCount; i++) {
		delete[] suggestions[i].word;
	}
	delete[] suggestions;
}

bool SuggestionStatus::shouldAbort() const {
	if (suggestionCount == maxSuggestions) {
		return true;
	}
	if (currentCost < maxCost) {
		return false;
	}
	if (suggestionCount == 0 && currentCost < 2 * maxCost) {
		// If no suggestions have been found, we allow the search
		// to take twice as long as usual.
		return false;
	}
	return true;
}

void SuggestionStatus::charge() {
	currentCost++;
}

void SuggestionStatus::setMaxCost(size_t maxCost) {
	this->maxCost = maxCost;
}

void SuggestionStatus::addSuggestion(const wchar_t * newSuggestion, int priority) {
	if (suggestionCount < maxSuggestions) {
		int finalPriority = priority * (suggestionCount + 5);
		//std::wcerr << L"Suggestion " << newSuggestion;
		//std::wcerr << L", priority " << priority;
		//std::wcerr << L", final priority " << finalPriority << std::endl;
		suggestions[suggestionCount].word = newSuggestion;
		suggestions[suggestionCount].priority = finalPriority;
		suggestionCount++;
	}
}

const Suggestion * SuggestionStatus::getSuggestions() const {
	return suggestions;
}

void SuggestionStatus::sortSuggestions() {
	/* Sort the suggestions by priority using insertion sort */
	int j;
	Suggestion current;
	for (size_t i = 0; i < suggestionCount; i++) {
		current = suggestions[i];
		for (j = i - 1; j >= 0 && suggestions[j].priority > current.priority; j--) {
			suggestions[j + 1] = suggestions[j];
		}
		suggestions[j + 1] = current;
	}
}

size_t SuggestionStatus::getSuggestionCount() const {
	return suggestionCount;
}

const wchar_t * SuggestionStatus::getWord() const {
	return word;
}

size_t SuggestionStatus::getWordLength() const {
	return wlen;
}

}}}
