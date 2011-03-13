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

#ifndef VOIKKO_SPELLCHECKER_FINNISH_SPELLER_TWEAKS_WRAPPER
#define VOIKKO_SPELLCHECKER_FINNISH_SPELLER_TWEAKS_WRAPPER

#include "spellchecker/Speller.hpp"
#include "morphology/Analyzer.hpp"
#include "setup/setup.hpp"
#include "hyphenator/AnalyzerToFinnishHyphenatorAdapter.hpp"

namespace libvoikko { namespace spellchecker {

/**
 * Wrapper that tweaks the results from a speller to work around some limitations
 * with Finnish Malaga morphology. In practice this should not be used with
 * other languages or morphologies.
 */
class FinnishSpellerTweaksWrapper : public Speller {
	public:
		FinnishSpellerTweaksWrapper(Speller * speller, morphology::Analyzer * analyzer, voikko_options_t * voikkoOptions);
		spellresult spell(const wchar_t * word, size_t wlen);
		void terminate();
	private:
		spellresult spellWithoutSoftHyphen(const wchar_t * word, size_t wlen);
		Speller * const speller;
		morphology::Analyzer * const analyzer;
		hyphenator::AnalyzerToFinnishHyphenatorAdapter * hyphenator;
		const voikko_options_t * const voikkoOptions;
};

} }

#endif
