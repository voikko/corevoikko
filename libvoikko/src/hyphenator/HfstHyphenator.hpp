/* Libvoikko: Library of Finnish language tools
 * Copyright (C) 2010 Flammie Pirinen <flammie@iki.fi>
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

#ifndef VOIKKO_HYPHENATOR_HFST_HYPHENATOR
#define VOIKKO_HYPHENATOR_HFST_HYPHENATOR

#include "hyphenator/Hyphenator.hpp"
#include "morphology/Analyzer.hpp"
#include "setup/DictionaryException.hpp"
#include <map>
#include <string>
#include <hfst2/hfst.h>
#include <hfst2/FlagDiacritics.h>

namespace libvoikko { namespace hyphenator {

/**
 * Morphological analyzer that uses Hfst to analyze words.
 */
class HfstHyphenator : public Hyphenator {
	public:
		HfstHyphenator(const std::string & directoryName) throw(setup::DictionaryException);
		char* hyphenate(const wchar_t* word, size_t wlen);
		char* hyphenate(const char* word);
		void setUglyHyphenation(bool ugliness);
		void setHyphenateUnknown(bool unknown);
		void setMinHyphenatedWordLength(int wlen);
		void setIgnoreDot(bool);
		void terminate();
	private:
		HWFST::KeyTable * keyTable;
		HWFST::TransducerHandle hyphenation;
		HWFST::TransducerHandle dictionary;
		FlagDiacriticTable flagTable;
		HWFST::KeySet flags;
};

} }

#endif
