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

#ifndef VOIKKO_SPELLCHECKER_SUGGESTION_SUGGESTION_STATUS_H
#define VOIKKO_SPELLCHECKER_SUGGESTION_SUGGESTION_STATUS_H

#include "spellchecker/suggestion/Suggestion.hpp"
#include <cstddef>

namespace libvoikko { namespace spellchecker { namespace suggestion {

class SuggestionStatus {
	public:
		SuggestionStatus(int handle, const wchar_t * word, size_t wlen, size_t maxSuggestions, size_t maxCost);
		~SuggestionStatus();
		bool shouldAbort() const;
		void charge();
		
		/**
		 * Adds a new suggestion with given priority. The ownership of suggestion
		 * string is transferred to this object.
		 */
		void addSuggestion(const wchar_t * newSuggestion, int priority);
		
		/**
		 * Returns the array containing the generated suggestions.
		 */
		const Suggestion * getSuggestions() const;
		
		/**
		 * Sort suggestions by priority.
		 */
		void sortSuggestions();
		
		/**
		 * Returns the number of generated suggestions.
		 */
		size_t getSuggestionCount() const;
		
		/**
		 * Returns the word for which the suggestions are being created.
		 */
		const wchar_t * getWord() const;
		
		/**
		 * Returns the length of the word for which the suggestions are being created.
		 */
		size_t getWordLength() const;
	
	private:
		SuggestionStatus(SuggestionStatus const & other);
		SuggestionStatus & operator = (const SuggestionStatus & other);
		
		/** handle */
		const int handle;
		
		/** string to find suggestions for */
		const wchar_t * const word;
		
		/** length of word */
		const size_t wlen;
		
		/** Maximum number of suggestions that can be collected */
		size_t maxSuggestions;
		
		/** Current count of collected suggestions */
		size_t suggestionCount;
		
		/** number of allowed malaga calls */
		size_t maxCost;
		
		/** Array of suggestions */
		Suggestion * suggestions;
	};

}}}
#endif
