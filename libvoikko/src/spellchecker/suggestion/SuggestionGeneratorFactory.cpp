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
			return new SuggestionStrategyOcr(voikkoOptions->morAnalyzer, 1000);
		}
		else {
			return new SuggestionStrategyTyping(voikkoOptions->morAnalyzer, 450);
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
