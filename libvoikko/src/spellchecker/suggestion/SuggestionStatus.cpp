/* Libvoikko: Finnish spellchecker and hyphenator library
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

#include "spellchecker/suggestion/SuggestionStatus.hpp"

namespace libvoikko { namespace spellchecker { namespace suggestion {

SuggestionStatus::SuggestionStatus(int handle, const wchar_t * word, size_t wlen, size_t maxSuggestions, size_t maxCost) :
	handle(handle),
	word(word),
	wlen(wlen),
	maxCost(maxCost),
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

void SuggestionStatus::addSuggestion(const wchar_t * newSuggestion, int priority) {
	if (suggestionCount < maxSuggestions) {
		suggestions[suggestionCount].word = newSuggestion;
		suggestions[suggestionCount].priority = priority * (suggestionCount + 5);
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
