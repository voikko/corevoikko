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

#include "voikko_defs.h"
#include "setup/setup.hpp"
#include "setup/DictionaryLoader.hpp"
#ifdef HAVE_GETPWUID_R
#include <pwd.h>
#endif // HAVE_GETPWUID_R
#include "morphology/AnalyzerFactory.hpp"
#include "spellchecker/SpellerFactory.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorFactory.hpp"
#include "hyphenator/HyphenatorFactory.hpp"
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

static int setGrammarOption(int handle, int value, int * option) {
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

VOIKKOEXPORT int voikko_set_bool_option(int handle, int option, int value) {
	voikko_options_t * options = &voikko_options;
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
			if (value) voikko_options.ignore_numbers = 1;
			else voikko_options.ignore_numbers = 0;
			return 1;
		case VOIKKO_OPT_IGNORE_UPPERCASE:
			if (value) voikko_options.ignore_uppercase = 1;
			else voikko_options.ignore_uppercase = 0;
			return 1;
		case VOIKKO_OPT_ACCEPT_FIRST_UPPERCASE:
			if (value) voikko_options.accept_first_uppercase = 1;
			else voikko_options.accept_first_uppercase = 0;
			return 1;
		case VOIKKO_OPT_ACCEPT_ALL_UPPERCASE:
			if (value) voikko_options.accept_all_uppercase = 1;
			else voikko_options.accept_all_uppercase = 0;
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
			delete voikko_options.suggestionGenerator;
			voikko_options.suggestionGenerator =
				spellchecker::suggestion::SuggestionGeneratorFactory::getSuggestionGenerator(&voikko_options, type);
			return 1;
			}
		case VOIKKO_OPT_IGNORE_NONWORDS:
			if (value) voikko_options.ignore_nonwords = 1;
			else voikko_options.ignore_nonwords = 0;
			return 1;
		case VOIKKO_OPT_ACCEPT_EXTRA_HYPHENS:
			if (value) voikko_options.accept_extra_hyphens = 1;
			else voikko_options.accept_extra_hyphens = 0;
			return 1;
		case VOIKKO_OPT_ACCEPT_MISSING_HYPHENS:
			if (value) voikko_options.accept_missing_hyphens = 1;
			else voikko_options.accept_missing_hyphens = 0;
			return 1;
		case VOIKKO_OPT_ACCEPT_TITLES_IN_GC:
			return setGrammarOption(handle, value,
			       &(voikko_options.accept_titles_in_gc));
		case VOIKKO_OPT_ACCEPT_UNFINISHED_PARAGRAPHS_IN_GC:
			return setGrammarOption(handle, value,
			       &(voikko_options.accept_unfinished_paragraphs_in_gc));
		case VOIKKO_OPT_ACCEPT_BULLETED_LISTS_IN_GC:
			return setGrammarOption(handle, value,
			       &(voikko_options.accept_bulleted_lists_in_gc));
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

VOIKKOEXPORT int voikko_set_int_option(int /*handle*/, int option, int value) {
	voikko_options_t * options = &voikko_options;
	switch (option) {
		case 5: // deprecated option VOIKKO_INTERSECT_COMPOUND_LEVEL
			return 1;
		case VOIKKO_MIN_HYPHENATED_WORD_LENGTH:
			options->hyphenator->setMinHyphenatedWordLength(value);
			return 1;
	}
	return 0;
}

VOIKKOEXPORT const char * voikko_init_with_path(int * handle, const char * langcode,
                                   int cache_size, const char * path) {
	/* FIXME: Temporary hack needed for MT unsafe malaga library */
	if (voikko_handle_count++ > 0) return "Maximum handle count exceeded";
	
	voikko_options.ignore_dot = 0;
	voikko_options.ignore_numbers = 0;
	voikko_options.ignore_uppercase = 0;
	voikko_options.ignore_nonwords = 1;
	voikko_options.accept_first_uppercase = 1;
	voikko_options.accept_all_uppercase = 1;
	voikko_options.accept_extra_hyphens = 0;
	voikko_options.accept_missing_hyphens = 0;
	voikko_options.accept_titles_in_gc = 0;
	voikko_options.accept_unfinished_paragraphs_in_gc = 0;
	voikko_options.accept_bulleted_lists_in_gc = 0;
	voikko_options.cache_size = cache_size;
	voikko_options.morAnalyzer = 0;
	
	if (langcode) {
		try {
			voikko_options.morAnalyzer = 0;
			voikko_options.speller = 0;
			Dictionary dict;
			if (path) {
				dict = DictionaryLoader::load(string(langcode), string(path));
			}
			else {
				dict = DictionaryLoader::load(string(langcode));
			}
			voikko_options.dictionary = dict;
			voikko_options.morAnalyzer = morphology::AnalyzerFactory::getAnalyzer(dict);
			voikko_options.speller = spellchecker::SpellerFactory::getSpeller(&voikko_options, dict);
			voikko_options.suggestionGenerator =
				spellchecker::suggestion::SuggestionGeneratorFactory::getSuggestionGenerator(&voikko_options,
					spellchecker::suggestion::SUGGESTION_TYPE_STD);
			voikko_options.hyphenator = hyphenator::HyphenatorFactory::getHyphenator(&voikko_options, dict);
		}
		catch (DictionaryException e) {
			if (voikko_options.hyphenator) {
				voikko_options.hyphenator->terminate();
				delete voikko_options.hyphenator;
				voikko_options.hyphenator = 0;
			}
			if (voikko_options.suggestionGenerator) {
				delete voikko_options.suggestionGenerator;
				voikko_options.suggestionGenerator = 0;
			}
			if (voikko_options.speller) {
				voikko_options.speller->terminate();
				delete voikko_options.speller;
				voikko_options.speller = 0;
			}
			if (voikko_options.morAnalyzer) {
				voikko_options.morAnalyzer->terminate();
				delete voikko_options.morAnalyzer;
				voikko_options.morAnalyzer = 0;
			}
			voikko_handle_count--;
			return e.what();
		}
	}
	
	if (cache_size >= 0) {
		voikko_options.cache = new wchar_t[6544 << cache_size];
		if (voikko_options.cache) {
			voikko_options.cache_meta = new char[1008 << cache_size];
			if (voikko_options.cache_meta)
				memset(voikko_options.cache_meta, 0, 1008 << cache_size);
			else {
				delete[] voikko_options.cache;
				voikko_options.cache = 0;
			}
			memset(voikko_options.cache, 0, 6544 * sizeof(wchar_t) << cache_size);
		}
	}
	else voikko_options.cache = 0;
	*handle = voikko_handle_count;
	return 0;
}

VOIKKOEXPORT const char * voikko_init(int * handle, const char * langcode, int cache_size) {
	return voikko_init_with_path(handle, langcode, cache_size, 0);
}

VOIKKOEXPORT int voikko_terminate(int handle) {
	if (handle == 1 && voikko_handle_count > 0) {
		voikko_handle_count--;
		voikko_options.hyphenator->terminate();
		delete voikko_options.hyphenator;
		voikko_options.hyphenator = 0;
		delete voikko_options.suggestionGenerator;
		voikko_options.suggestionGenerator = 0;
		voikko_options.speller->terminate();
		delete voikko_options.speller;
		voikko_options.speller = 0;
		voikko_options.morAnalyzer->terminate();
		delete voikko_options.morAnalyzer;
		voikko_options.morAnalyzer = 0;
		/*int c = 0;
		for (int i = 0; i < 1*1008; i++) if (voikko_options.cache_meta[i] == '.') c++;
		printf("Cache slots used: %d\n", c);*/
		if (voikko_options.cache) {
			delete[] voikko_options.cache;
			delete[] voikko_options.cache_meta;
		}
		gc_clear_cache(handle);
		return 1;
	}
	else return 0;
}

}
