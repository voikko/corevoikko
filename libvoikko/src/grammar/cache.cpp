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
 * Portions created by the Initial Developer are Copyright (C) 2008 - 2011
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

#include "utils/utils.hpp"
#include "grammar/cachesetup.hpp"
#include "grammar/GrammarChecker.hpp"
#include "grammar/cache.hpp"
#include "grammar/FinnishAnalysis.hpp"
#include "grammar/FinnishRuleEngine.hpp"
#include "grammar/HfstAnalysis.hpp"
#include "grammar/CgRuleEngine.hpp"
#include <cstring>
#include <cstdlib>

//#include "grammar/FinnishRuleEngine/CapitalizationCheck.hpp"
//#include "grammar/FinnishRuleEngine/checks.hpp"
//#include "grammar/FinnishRuleEngine/MissingVerbCheck.hpp"
//#include "grammar/FinnishRuleEngine/NegativeVerbCheck.hpp"
//#include "grammar/FinnishRuleEngine/CompoundVerbCheck.hpp"
//#include "grammar/FinnishRuleEngine/SidesanaCheck.hpp"
//#ifdef HAVE_MALAGA
//	#include "autocorrect/AutoCorrect.hpp"
//#endif


using namespace libvoikko::grammar;

//using namespace libvoikko::autocorrect;

namespace libvoikko {

// This will be initialized to zero meaning "no errors"
static const voikko_grammar_error no_grammar_error = voikko_grammar_error();

const voikko_grammar_error * gc_error_from_cache(voikko_options_t * voikkoOptions, const wchar_t * text,
                             size_t startpos, int skiperrors) {
	if (!voikkoOptions->grammarChecker->gc_cache.paragraph) {
		return 0;
	}
	if (wcscmp(voikkoOptions->grammarChecker->gc_cache.paragraph, text) != 0) {
		return 0;
	}
	CacheEntry * e = voikkoOptions->grammarChecker->gc_cache.firstError;
	int preverrors = 0;
	while (e) {
		if (preverrors >= skiperrors &&
		    e->error.startpos >= startpos) {
			return &e->error;
		}
		preverrors++;
		e = e->nextError;
	}
	return &no_grammar_error;
}

void gc_paragraph_to_cache(voikko_options_t * voikkoOptions, const wchar_t * text, size_t textlen) {
	gc_clear_cache(voikkoOptions);
	voikkoOptions->grammarChecker->gc_cache.paragraph = new wchar_t[textlen + 1];
	if (!voikkoOptions->grammarChecker->gc_cache.paragraph) {
		return;
	}
	memcpy(voikkoOptions->grammarChecker->gc_cache.paragraph, text, textlen * sizeof(wchar_t));
	voikkoOptions->grammarChecker->gc_cache.paragraph[textlen] = L'\0';
	//FinnishAnalysis analyser;
	HfstAnalysis analyser;
	Paragraph * para = analyser.analyse_paragraph(voikkoOptions, text, textlen);
	if (!para) {
		return;
	}
	
	// If paragraph is a single sentence without any whitespace, do not try to
	// do grammar checking on it. This could be an URL or something equally
	// strange.
	if (para->sentenceCount == 1) {
		Sentence * sentence = para->sentences[0];
		bool hasWhitespace = false;
		for (size_t i = 0; i < sentence->tokenCount; i++) {
			if (sentence->tokens[i].type == TOKEN_WHITESPACE) {
				hasWhitespace = true;
				break;
			}
		}
		if (!hasWhitespace) {
			// If this is a single word sentence, we should check it, otherwise
			// it makes no sense to try.
			if (sentence->tokenCount > 2 || sentence->tokenCount == 0 ||
			    sentence->tokens[0].type != TOKEN_WORD) {
				delete para;
				return;
			}
		}
	}
	
	//check::CapitalizationCheck capitalizationCheck;
	//check::NegativeVerbCheck negativeVerbCheck;
	//check::CompoundVerbCheck compoundVerbCheck;
	//check::SidesanaCheck sidesanaCheck;
	//check::MissingVerbCheck missingVerbCheck;
	FinnishRuleEngine checks;
	CgRuleEngine cgchecks;
	cgchecks.load(std::string("/home/fran/.voikko/4/se-x-standard/sme-gramchk.bin"));
/*	for (size_t i = 0; i < para->sentenceCount; i++) {
#ifdef HAVE_MALAGA
		// TODO: Autocorrect data should be moved to a separate data file (VFST) in
		// later format revisions. Old implementation is only available to support
		// v2 dictionary format.
		AutoCorrect::autoCorrect(voikkoOptions, para->sentences[i]);
#endif
//		gc_local_punctuation(voikkoOptions, para->sentences[i]);
//		gc_punctuation_of_quotations(voikkoOptions, para->sentences[i]);
//		gc_repeating_words(voikkoOptions, para->sentences[i]);
//		negativeVerbCheck.check(voikkoOptions, para->sentences[i]);
//		compoundVerbCheck.check(voikkoOptions, para->sentences[i]);
//		sidesanaCheck.check(voikkoOptions, para->sentences[i]);
//		missingVerbCheck.check(voikkoOptions, para->sentences[i]);
		checks.check(voikkoOptions, para->sentences[i]);
	}
	//capitalizationCheck.check(voikkoOptions, para);
	//gc_end_punctuation(voikkoOptions, para);
*/
	checks.check(voikkoOptions, para);
	cgchecks.check(voikkoOptions, para);

	delete para;
}

void gc_cache_append_error(voikko_options_t * voikkoOptions, CacheEntry * new_entry) {
	CacheEntry * entry = voikkoOptions->grammarChecker->gc_cache.firstError;
	if (!entry) {
		voikkoOptions->grammarChecker->gc_cache.firstError = new_entry;
		return;
	}
	if (entry->error.startpos > new_entry->error.startpos) {
		new_entry->nextError = voikkoOptions->grammarChecker->gc_cache.firstError;
		voikkoOptions->grammarChecker->gc_cache.firstError = new_entry;
		return;
	}
	while (1) {
		if (!entry->nextError) {
			entry->nextError = new_entry;
			return;
		}
		if (entry->error.startpos <= new_entry->error.startpos &&
		    entry->nextError->error.startpos > new_entry->error.startpos) {
			new_entry->nextError = entry->nextError;
			entry->nextError = new_entry;
			return;
		}
		entry = entry->nextError;
	}
}

}
