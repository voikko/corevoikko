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
 * Portions created by the Initial Developer are Copyright (C) 2006 - 2009
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
#include "setup/setup.hpp"
#include "setup/DictionaryFactory.hpp"
#ifdef HAVE_GETPWUID_R
#include <pwd.h>
#endif // HAVE_GETPWUID_R
#include "morphology/AnalyzerFactory.hpp"
#include "spellchecker/SpellerFactory.hpp"
#include "grammar/GrammarCheckerFactory.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorFactory.hpp"
#include "hyphenator/HyphenatorFactory.hpp"
#include "grammar/cachesetup.hpp"
#include <cstring>
#include <sys/stat.h>
#include <cstdlib>
#include <string>
#include <iostream>

#ifdef WIN32
#include <windows.h>
#else
#include <unistd.h>
#endif

using namespace std;

namespace libvoikko {

using namespace setup;

static int setGrammarOption(voikko_options_t * handle, int value, int * option) {
		if (value && !(*option)) {
			*option = 1;
			gc_clear_cache(handle);
		}
		else if (!value && (*option)) {
			*option = 0;
			gc_clear_cache(handle);
		}
		return 1;
}

VOIKKOEXPORT int voikkoSetBooleanOption(voikko_options_t * options, int option, int value) {
	switch (option) {
		case VOIKKO_OPT_IGNORE_DOT:
			if (value) {
				options->ignore_dot = 1;
				options->hyphenator->setIgnoreDot(true);
			}
			else {
				options->ignore_dot = 0;
				options->hyphenator->setIgnoreDot(false);
			}
			return 1;
		case VOIKKO_OPT_IGNORE_NUMBERS:
			options->ignore_numbers = (value ? 1 : 0);
			return 1;
		case VOIKKO_OPT_IGNORE_UPPERCASE:
			options->ignore_uppercase = (value ? 1 : 0);
			return 1;
		case VOIKKO_OPT_ACCEPT_FIRST_UPPERCASE:
			options->accept_first_uppercase = (value ? 1 : 0);
			return 1;
		case VOIKKO_OPT_ACCEPT_ALL_UPPERCASE:
			options->accept_all_uppercase = (value ? 1 : 0);
			return 1;
		case VOIKKO_OPT_NO_UGLY_HYPHENATION:
			if (value) {
				options->hyphenator->setUglyHyphenation(false);
			}
			else {
				options->hyphenator->setUglyHyphenation(true);
			}
			return 1;
		case VOIKKO_OPT_OCR_SUGGESTIONS:
			{
			spellchecker::suggestion::SuggestionType type = (value ? 
				spellchecker::suggestion::SUGGESTION_TYPE_OCR :
				spellchecker::suggestion::SUGGESTION_TYPE_STD);
			delete options->suggestionGenerator;
			options->suggestionGenerator =
				spellchecker::suggestion::SuggestionGeneratorFactory::getSuggestionGenerator(options, type);
			return 1;
			}
		case VOIKKO_OPT_IGNORE_NONWORDS:
			options->ignore_nonwords = (value ? 1 : 0);
			return 1;
		case VOIKKO_OPT_ACCEPT_EXTRA_HYPHENS:
			options->accept_extra_hyphens = (value ? 1 : 0);
			return 1;
		case VOIKKO_OPT_ACCEPT_MISSING_HYPHENS:
			options->accept_missing_hyphens = (value ? 1 : 0);
			return 1;
		case VOIKKO_OPT_ACCEPT_TITLES_IN_GC:
			return setGrammarOption(options, value,
			       &(options->accept_titles_in_gc));
		case VOIKKO_OPT_ACCEPT_UNFINISHED_PARAGRAPHS_IN_GC:
			return setGrammarOption(options, value,
			       &(options->accept_unfinished_paragraphs_in_gc));
		case VOIKKO_OPT_ACCEPT_BULLETED_LISTS_IN_GC:
			return setGrammarOption(options, value,
			       &(options->accept_bulleted_lists_in_gc));
		case VOIKKO_OPT_HYPHENATE_UNKNOWN_WORDS:
			if (value) {
				options->hyphenator->setHyphenateUnknown(true);
			}
			else {
				options->hyphenator->setHyphenateUnknown(false);
			}
			return 1;
	}
	return 0;
}

VOIKKOEXPORT int voikkoSetIntegerOption(voikko_options_t * options, int option, int value) {
	switch (option) {
		case VOIKKO_MIN_HYPHENATED_WORD_LENGTH:
			options->hyphenator->setMinHyphenatedWordLength(value);
			return 1;
		case VOIKKO_SPELLER_CACHE_SIZE:
			if (options->spellerCache) {
				if (options->spellerCache->getSizeParam() != value) {
					delete options->spellerCache;
					if (value >= 0) {
						options->spellerCache = new spellchecker::SpellerCache(value);
					} else {
						options->spellerCache = 0;
					}
				}
			} else {
				if (value >= 0) {
					options->spellerCache = new spellchecker::SpellerCache(value);
				}
			}
			return 1;
	}
	return 0;
}

VOIKKOEXPORT voikko_options_t * voikkoInit(const char ** error, const char * langcode,
                                   const char * path) {
	if (!langcode) {
		*error = "Language must not be null";
		return 0;
	}
	voikko_options_t * options = new voikko_options_t();
	options->ignore_dot = 0;
	options->ignore_numbers = 0;
	options->ignore_uppercase = 0;
	options->ignore_nonwords = 1;
	options->accept_first_uppercase = 1;
	options->accept_all_uppercase = 1;
	options->accept_extra_hyphens = 0;
	options->accept_missing_hyphens = 0;
	options->accept_titles_in_gc = 0;
	options->accept_unfinished_paragraphs_in_gc = 0;
	options->accept_bulleted_lists_in_gc = 0;
	options->morAnalyzer = 0;
	options->grammarChecker = 0;
	options->speller = 0;
	options->suggestionGenerator = 0;
	options->hyphenator = 0;
	options->hfst = 0;
	
	try {
		options->morAnalyzer = 0;
		options->speller = 0;
		Dictionary dict;
		if (path) {
			dict = DictionaryFactory::load(string(langcode), string(path));
		}
		else {
			dict = DictionaryFactory::load(string(langcode));
		}
		cerr << "setup::voikkoInit " << dict.getMorBackend() << " " <<  dict.getGrammarBackend()  << endl;
		options->dictionary = dict;
		options->morAnalyzer = morphology::AnalyzerFactory::getAnalyzer(dict);
		options->speller = spellchecker::SpellerFactory::getSpeller(options, dict);
		options->grammarChecker = grammar::GrammarCheckerFactory::getGrammarChecker(options, dict);
		options->suggestionGenerator =
			spellchecker::suggestion::SuggestionGeneratorFactory::getSuggestionGenerator(options,
				spellchecker::suggestion::SUGGESTION_TYPE_STD);
		options->hyphenator = hyphenator::HyphenatorFactory::getHyphenator(options, dict);
	}
	catch (DictionaryException & e) {
		if (options->hyphenator) {
			options->hyphenator->terminate();
			delete options->hyphenator;
			options->hyphenator = 0;
		}
		delete options->suggestionGenerator;
		options->suggestionGenerator = 0;
		if (options->speller) {
			options->speller->terminate();
			delete options->speller;
			options->speller = 0;
		}
		if (options->morAnalyzer) {
			options->morAnalyzer->terminate();
			delete options->morAnalyzer;
			options->morAnalyzer = 0;
		}
		*error = e.what();
		delete options;
		return 0;
	}
	
	options->spellerCache = new spellchecker::SpellerCache(0);
	*error = 0;
	return options;
}

VOIKKOEXPORT void voikkoTerminate(voikko_options_t * handle) {
	handle->hyphenator->terminate();
	delete handle->hyphenator;
	delete handle->suggestionGenerator;
	handle->speller->terminate();
	delete handle->speller;
	handle->morAnalyzer->terminate();
	delete handle->morAnalyzer;
	delete handle->spellerCache;
	gc_clear_cache(handle);
	delete handle;
}

}
