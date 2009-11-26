/* Libvoikko: Library of Finnish language tools
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

#ifndef VOIKKO_HYPHENATOR_ANALYZER_TO_FINNISH_HYPHENATOR_ADAPTER
#define VOIKKO_HYPHENATOR_ANALYZER_TO_FINNISH_HYPHENATOR_ADAPTER

#include "hyphenator/Hyphenator.hpp"
#include "morphology/Analyzer.hpp"

namespace libvoikko { namespace hyphenator {

/**
 * Adapter that uses an existing Analyzer for Finnish hyphenation. The
 * analyzer must remain operational until this adapter has been terminated.
 */
class AnalyzerToFinnishHyphenatorAdapter : public Hyphenator {
	public:
		AnalyzerToFinnishHyphenatorAdapter(morphology::Analyzer * analyzer);
		char * hyphenate(const wchar_t * word, size_t wlen);
		void terminate();
		void setUglyHyphenation(bool uglyHyphenation);
		void setHyphenateUnknown(bool hyphenateUnknown);
		void setIntersectCompoundLevel(int level);
		void setMinHyphenatedWordLength(int length);
		void setIgnoreDot(bool ignoreDot);
	private:
		morphology::Analyzer * const analyzer;
		bool uglyHyphenation;
		bool hyphenateUnknown;
		int intersectCompoundLevel;
		size_t minHyphenatedWordLength;
		bool ignoreDot;
		
		/**
		 * Creates an array of hyphenation buffers for given word.
		 * @param word word to analyse
		 * @param len length of the word
		 * @param dotRemoved pointer to a bool that will be set to true
		 * if trailing dot is ignored. Otherwise it will be set to false.
		 * @return array of hyphenation buffers that correspond to different
		 * ways how word could be split
		 */
		char ** splitCompounds(const wchar_t * word, size_t len, bool * dotRemoved);

		/**
		 * Hyphenates a compound word.
		 * @param word word to hyphenate
		 * @param hyphenation buffer to write the results to. It is assumed that
		 * compound word borders have already been marked on the buffer.
		 * @param len length of the word to hyphenate
		 */
		void compoundHyphenation(const wchar_t * word, char * hyphenation, size_t len) const;

		/**
		 * Calculates the intersection of hyphenation points.
		 * @param hyphenations array of hyphenation buffers
		 * @return hyphenation buffer that contains the intersection of given hyphenations
		 */
		char * intersectHyphenations(char ** hyphenations) const;

		/**
		 * Sets the known hyphenation points (compound word borders) according to given
		 * morphological analysis.
		 * @param analysis morphological analysis of the word
		 * @param buffer hyphenation buffer to store the results to
		 * @param len length of the buffer
		 */
		void interpretAnalysis(const morphology::Analysis * analysis, char * buffer,
		                       size_t len) const;

		/**
		 * Checks if given word can be safely hyphenated using standard hyphenation rules
		 * @param word word to check
		 * @param nchars number of characters in the word
		 * @return true if the word should be hyphenated with rule based hyphenator, otherwise false.
		 */
		bool allowRuleHyphenation(const wchar_t * word, size_t nchars) const;

		/**
		 * Removes hyphenation buffers that are considered unnecessary to analyse.
		 * @param hyphenations list of hyphenation buffers. It is assumed that compound
		 * word borders have already been marked on the buffer.
		 * @param len length of the word
		 */
		void removeExtraHyphenations(char ** hyphenations, size_t len) const;

		/**
		 * Performs rule-based hyphenation.
		 * @param word word to hyphenate
		 * @param hyphenationPoints hyphenation buffer where the results will be stored
		 * @param nchars number of characters in the word
		 */
		void ruleHyphenation(const wchar_t * word, char * hyphenationPoints,
		                     size_t nchars) const;

		/**
		 * Checks if the proposed hyphenation point is valid.
		 * @param word word to hyphenate
		 * @param hyphenation_points hyphenation buffer containing the existing hyphenation points
		 * @param new_hyphen_pos position of the proposed new hyphenation point
		 * @param nchars number of characters in the word
		 * @return true if the proposed hyphenation point is valid (will not result in
		 * syllables without any vowels), false if it is invalid.
		 */
		bool isGoodHyphenPosition(const wchar_t * word, const char * hyphenationPoints,
		                          size_t newHyphenPos, size_t nchars) const;

};

} }

#endif
