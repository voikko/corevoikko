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

#include "porting.h"
#include "spellchecker/suggestion/SuggestionGeneratorFactory.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorNull.hpp"
#include "spellchecker/suggestion/SuggestionStrategyOcr.hpp"
#include "spellchecker/suggestion/SuggestionStrategyTyping.hpp"
#include "setup/setup.hpp"

#ifdef HAVE_HFST
#include "spellchecker/HfstSuggestion.hpp"
#endif

using namespace std;

namespace libvoikko { namespace spellchecker { namespace suggestion {

SuggestionGenerator * SuggestionGeneratorFactory::getSuggestionGenerator(
	                             voikko_options_t * voikkoOptions,
	                             SuggestionType suggestionType)
	                              throw(setup::DictionaryException) {
	string backend = voikkoOptions->dictionary.getSuggestionBackend();
	if (backend == "FinnishSuggestionStrategy(currentAnalyzer)") {
		if (suggestionType == SUGGESTION_TYPE_OCR) {
			return new SuggestionStrategyOcr(voikkoOptions->morAnalyzer, 1500);
		}
		else {
			return new SuggestionStrategyTyping(voikkoOptions->morAnalyzer, 590);
		}
	} else if (backend == "null") {
		return new SuggestionGeneratorNull();
	}
	#ifdef HAVE_HFST
	if (backend == "hfst") {
		return new HfstSuggestion(voikkoOptions->dictionary.getMorPath(), voikkoOptions);
	}
	#endif
	throw setup::DictionaryException("Failed to create suggestion generator because of unknown suggestion generator backend");
}

} } }
