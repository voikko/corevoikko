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
 * Portions created by the Initial Developer are Copyright (C) 2010
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
		FinnishSpellerTweaksWrapper(Speller * speller, morphology::Analyzer * analyzer, VoikkoHandle * voikkoOptions);
		spellresult spell(const wchar_t * word, size_t wlen);
		void terminate();
	private:
		spellresult spellWithoutSoftHyphen(const wchar_t * word, size_t wlen);
		Speller * const speller;
		morphology::Analyzer * const analyzer;
		hyphenator::AnalyzerToFinnishHyphenatorAdapter * hyphenator;
		const VoikkoHandle * const voikkoOptions;
};

} }

#endif
