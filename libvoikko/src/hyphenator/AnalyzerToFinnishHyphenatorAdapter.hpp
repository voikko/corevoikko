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
		int minHyphenatedWordLength;
		bool ignoreDot;
};

} }

#endif
