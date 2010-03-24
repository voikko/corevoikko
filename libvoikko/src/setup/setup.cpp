/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2006 - 2009 Harri Pitkänen <hatapitk@iki.fi>
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
#include "setup/setup.hpp"
#include "setup/DictionaryLoader.hpp"
#ifdef HAVE_GETPWUID_R
#include <pwd.h>
#endif // HAVE_GETPWUID_R
#include "morphology/AnalyzerFactory.hpp"
#include "spellchecker/SpellerFactory.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorFactory.hpp"
#include "hyphenator/HyphenatorFactory.hpp"
#include "grammar/cachesetup.hpp"
#include <cstring>
#include <sys/stat.h>
#include <cstdlib>
#include <string>

#ifdef WIN32
#include <windows.h>
#else
#include <unistd.h>
#endif

using namespace std;

namespace libvoikko {

using namespace setup;

voikko_options_t voikko_options;

int voikko_handle_count;

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
	}
	return 0;
}

VOIKKOEXPORT voikko_options_t * voikkoInit(const char ** error, const char * langcode,
                                   int cache_size, const char * path) {
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
	options->cache_size = cache_size;
	options->morAnalyzer = 0;
	
	if (langcode) {
		try {
			options->morAnalyzer = 0;
			options->speller = 0;
			Dictionary dict;
			if (path) {
				dict = DictionaryLoader::load(string(langcode), string(path));
			}
			else {
				dict = DictionaryLoader::load(string(langcode));
			}
			options->dictionary = dict;
			options->morAnalyzer = morphology::AnalyzerFactory::getAnalyzer(dict);
			options->speller = spellchecker::SpellerFactory::getSpeller(options, dict);
			options->suggestionGenerator =
				spellchecker::suggestion::SuggestionGeneratorFactory::getSuggestionGenerator(options,
					spellchecker::suggestion::SUGGESTION_TYPE_STD);
			options->hyphenator = hyphenator::HyphenatorFactory::getHyphenator(options, dict);
		}
		catch (DictionaryException e) {
			if (options->hyphenator) {
				options->hyphenator->terminate();
				delete options->hyphenator;
				options->hyphenator = 0;
			}
			if (options->suggestionGenerator) {
				delete options->suggestionGenerator;
				options->suggestionGenerator = 0;
			}
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
	}
	
	if (cache_size >= 0) {
		options->cache = new wchar_t[6544 << cache_size];
		options->cache_meta = new char[1008 << cache_size];
		memset(options->cache_meta, 0, 1008 << cache_size);
		memset(options->cache, 0, 6544 * sizeof(wchar_t) << cache_size);
	} else {
		options->cache = 0;
	}
	*error = 0;
	return options;
}

VOIKKOEXPORT void voikkoTerminate(voikko_options_t * handle) {
	handle->hyphenator->terminate();
	delete handle->hyphenator;
	handle->hyphenator = 0;
	delete handle->suggestionGenerator;
	handle->suggestionGenerator = 0;
	handle->speller->terminate();
	delete handle->speller;
	handle->speller = 0;
	handle->morAnalyzer->terminate();
	delete handle->morAnalyzer;
	handle->morAnalyzer = 0;
	/*int c = 0;
	for (int i = 0; i < 1*1008; i++) if (handle->cache_meta[i] == '.') c++;
	printf("Cache slots used: %d\n", c);*/
	if (handle->cache) {
		delete[] handle->cache;
		delete[] handle->cache_meta;
	}
	gc_clear_cache(handle);
}

}
