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

#include "porting.h"
#include "spellchecker/SpellerFactory.hpp"
#include "spellchecker/AnalyzerToSpellerAdapter.hpp"
#include "spellchecker/FinnishSpellerTweaksWrapper.hpp"
#include "spellchecker/FixedResultSpeller.hpp"

#ifdef HAVE_HFST
#include "spellchecker/HfstSpeller.hpp"
#endif

#ifdef HAVE_VFST
#include "spellchecker/VfstSpeller.hpp"
#endif

using namespace std;

namespace libvoikko { namespace spellchecker {

Speller * SpellerFactory::getSpeller(voikko_options_t * voikkoOptions,
	                             const setup::Dictionary & dictionary)
	                              throw(setup::DictionaryException) {
	morphology::Analyzer * currentAnalyzer = voikkoOptions->morAnalyzer;
	// FIXME: add proper parsing and possibility to combine components freely
	// Take care of proper memory management (who has to delete what).
	string spellBackend = dictionary.getSpellBackend().getBackend();
	if (spellBackend == "AnalyzerToSpellerAdapter(currentAnalyzer)") {
		return new AnalyzerToSpellerAdapter(currentAnalyzer);
	} else if (spellBackend == "FinnishSpellerTweaksWrapper(AnalyzerToSpellerAdapter(currentAnalyzer),currentAnalyzer)") {
		return new FinnishSpellerTweaksWrapper(new AnalyzerToSpellerAdapter(currentAnalyzer), currentAnalyzer, voikkoOptions);
	} else if (spellBackend == "AllOk") {
		return new FixedResultSpeller(SPELL_OK);
	} else if (spellBackend == "AllError") {
		return new FixedResultSpeller(SPELL_FAILED);
	}
	#ifdef HAVE_HFST
	if (spellBackend == "hfst") {
		return new HfstSpeller(dictionary.getMorBackend().getPath());
	}
	#endif
	#ifdef HAVE_VFST
	if (spellBackend == "vfst") {
		return new VfstSpeller(dictionary.getMorBackend().getPath());
	}
	#endif
	throw setup::DictionaryException("Failed to create speller because backend configuration could not be parsed");
}

} }
