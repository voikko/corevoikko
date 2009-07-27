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
#include <malaga.h>
#include <cstring>
#include <sys/stat.h>
#include <unistd.h>
#include <cstdlib>
#include <string>

#ifdef WIN32
#include <windows.h>
#endif

using namespace std;

namespace libvoikko {

using namespace setup;

voikko_options_t voikko_options;

int voikko_handle_count;

VOIKKOEXPORT int voikko_set_bool_option(int /*handle*/, int option, int value) {
	switch (option) {
		case VOIKKO_OPT_IGNORE_DOT:
			if (value) voikko_options.ignore_dot = 1;
			else voikko_options.ignore_dot = 0;
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
			if (value) voikko_options.no_ugly_hyphenation = 1;
			else voikko_options.no_ugly_hyphenation = 0;
			return 1;
		case VOIKKO_OPT_OCR_SUGGESTIONS:
			if (value) voikko_options.suggestion_type = ST_OCR;
			else voikko_options.suggestion_type = ST_STD;
			return 1;
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
			if (value) voikko_options.accept_titles_in_gc = 1;
			else voikko_options.accept_titles_in_gc = 0;
			return 1;
		case VOIKKO_OPT_ACCEPT_UNFINISHED_PARAGRAPHS_IN_GC:
			if (value) voikko_options.accept_unfinished_paragraphs_in_gc = 1;
			else voikko_options.accept_unfinished_paragraphs_in_gc = 0;
			return 1;
		case VOIKKO_OPT_ACCEPT_BULLETED_LISTS_IN_GC:
			if (value) voikko_options.accept_bulleted_lists_in_gc = 1;
			else voikko_options.accept_bulleted_lists_in_gc = 0;
			return 1;
		case VOIKKO_OPT_HYPHENATE_UNKNOWN_WORDS:
			if (value) voikko_options.hyphenate_unknown_words = 1;
			else voikko_options.hyphenate_unknown_words = 0;
			return 1;
	}
	return 0;
}

VOIKKOEXPORT int voikko_set_int_option(int /*handle*/, int option, int value) {
	switch (option) {
		case VOIKKO_INTERSECT_COMPOUND_LEVEL:
			voikko_options.intersect_compound_level = value;
			return 1;
		case VOIKKO_MIN_HYPHENATED_WORD_LENGTH:
			voikko_options.min_hyphenated_word_length = value;
			return 1;
	}
	return 0;
}

VOIKKOEXPORT int voikko_set_string_option(int /*handle*/, int option, const char * value) {
	switch (option) {
		case VOIKKO_OPT_ENCODING:
			if (!value) return 0;
			#ifdef HAVE_ICONV
			iconv_t toext = iconv_open(value, INTERNAL_CHARSET);
			if (toext == (iconv_t) -1) {
				return 0;
			}
			iconv_t fromext = iconv_open(INTERNAL_CHARSET, value);
			if (fromext == (iconv_t) -1) {
				iconv_close(toext);
				return 0;
			}
			iconv_close(voikko_options.iconv_ucs4_ext);
			voikko_options.iconv_ucs4_ext = toext;
			iconv_close(voikko_options.iconv_ext_ucs4);
			voikko_options.iconv_ext_ucs4 = fromext;
			#endif
			voikko_options.encoding = value;
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
	voikko_options.no_ugly_hyphenation = 0;
	voikko_options.accept_extra_hyphens = 0;
	voikko_options.accept_missing_hyphens = 0;
	voikko_options.accept_titles_in_gc = 0;
	voikko_options.hyphenate_unknown_words = 1;
	voikko_options.intersect_compound_level = 1;
	voikko_options.min_hyphenated_word_length = 2;
	voikko_options.encoding = "UTF-8";
	voikko_options.cache_size = cache_size;
	voikko_options.suggestion_type = ST_STD;
	
	#ifdef HAVE_ICONV
	/* Initialise converters */
	voikko_options.iconv_ucs4_utf8 = iconv_open("UTF-8", INTERNAL_CHARSET);
	if (voikko_options.iconv_ucs4_utf8 == (iconv_t) -1) {
		return "iconv_open(\"UTF-8\", \"" INTERNAL_CHARSET "\") failed";
	}
	voikko_options.iconv_utf8_ucs4 = iconv_open(INTERNAL_CHARSET, "UTF-8");
	if (voikko_options.iconv_utf8_ucs4 == (iconv_t) -1) {
		iconv_close(voikko_options.iconv_ucs4_utf8);
		return "iconv_open(\"" INTERNAL_CHARSET "\", \"UTF-8\") failed";
	}
	voikko_options.iconv_ucs4_ext = iconv_open(voikko_options.encoding, INTERNAL_CHARSET);
	if (voikko_options.iconv_ucs4_ext == (iconv_t) -1) {
		iconv_close(voikko_options.iconv_utf8_ucs4);
		iconv_close(voikko_options.iconv_ucs4_utf8);
		return "iconv_open(voikko_options.encoding, \"" INTERNAL_CHARSET "\") failed";
	}
	voikko_options.iconv_ext_ucs4 = iconv_open(INTERNAL_CHARSET, voikko_options.encoding);
	if (voikko_options.iconv_ext_ucs4 == (iconv_t) -1) {
		iconv_close(voikko_options.iconv_ucs4_ext);
		iconv_close(voikko_options.iconv_utf8_ucs4);
		iconv_close(voikko_options.iconv_ucs4_utf8);
		return "iconv_open(\"" INTERNAL_CHARSET "\", voikko_options.encoding) failed";
	}
	#endif
	
	if (langcode) {
		try {
			if (path) {
				DictionaryLoader::load(string(langcode), string(path));
			}
			else {
				DictionaryLoader::load(string(langcode));
			}
		}
		catch (DictionaryException e) {
			#ifdef HAVE_ICONV
			iconv_close(voikko_options.iconv_ext_ucs4);
			iconv_close(voikko_options.iconv_ucs4_ext);
			iconv_close(voikko_options.iconv_utf8_ucs4);
			iconv_close(voikko_options.iconv_ucs4_utf8);
			#endif
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
		#ifdef HAVE_ICONV
		iconv_close(voikko_options.iconv_ext_ucs4);
		iconv_close(voikko_options.iconv_ucs4_ext);
		iconv_close(voikko_options.iconv_utf8_ucs4);
		iconv_close(voikko_options.iconv_ucs4_utf8);
		#endif
		terminate_libmalaga();
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
