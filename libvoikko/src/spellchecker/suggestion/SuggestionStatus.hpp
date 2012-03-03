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

#ifndef VOIKKO_SPELLCHECKER_SUGGESTION_SUGGESTION_STATUS_H
#define VOIKKO_SPELLCHECKER_SUGGESTION_SUGGESTION_STATUS_H

#include "spellchecker/suggestion/Suggestion.hpp"
#include <cstddef>

namespace libvoikko { namespace spellchecker { namespace suggestion {

class SuggestionStatus {
	public:
		SuggestionStatus(const wchar_t * word, size_t wlen, size_t maxSuggestions);
		~SuggestionStatus();
		
		/**
		 * Returns true if no more suggestions should be generated.
		 */
		bool shouldAbort() const;
		
		/**
		 * Increases the counter of calls made to morphological
		 * analysis component (or other CPU-heavy operations).
		 * Single call should roughly correspond to cost of one
		 * morphological analysis operation.
		 */
		void charge();

		/** Set maximum computational cost. */
		void setMaxCost(size_t maxCost);
		
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
		
		/** string to find suggestions for */
		const wchar_t * const word;
		
		/** length of word */
		const size_t wlen;
		
		/**
		 * Maximum number of times the morphological analysis
		 * (or an operation with computationally equivalent cost)
		 * may be performed.
		 */
		size_t maxCost;
		
		/** Maximum number of suggestions that can be collected */
		size_t maxSuggestions;
		
		/** Current count of collected suggestions */
		size_t suggestionCount;
		
		/** Current cost of suggestion search */
		size_t currentCost;
		
		/** Array of suggestions */
		Suggestion * suggestions;
	};

}}}
#endif
