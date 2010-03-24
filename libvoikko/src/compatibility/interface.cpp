/* Libvoikko: Finnish spellchecker and hyphenator library
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

/**
 * Compatibility implementations for deprecated public API.
 */

#include "porting.h"
#include "setup/setup.hpp"
#include "utils/StringUtils.hpp"
#include "voikko.h"
#include <cstring>
#include <cstdlib>

namespace libvoikko { namespace compatibility {

VOIKKOEXPORT const char * voikko_init_with_path(int * handle, const char * langcode,
                                   int cache_size, const char * path) {
	if (voikko_handle_count++ > 0) {
		return "Maximum handle count exceeded";
	}
	const char * error;
	voikko_options_t * theHandle =
			reinterpret_cast<voikko_options_t *>(voikkoInit(&error, langcode, cache_size, path));
	if (theHandle) {
		*handle = 1;
		voikko_options = *theHandle;
		delete theHandle;
		return 0;
	} else {
		voikko_handle_count--;
		*handle = 0;
		return error;
	}
}

VOIKKOEXPORT const char * voikko_init(int * handle, const char * langcode, int cache_size) {
	return voikko_init_with_path(handle, langcode, cache_size, 0);
}

VOIKKOEXPORT int voikko_terminate(int handle) {
	if (handle == 1 && voikko_handle_count > 0) {
		voikko_handle_count--;
		voikkoTerminate(reinterpret_cast<VoikkoHandle *>(&voikko_options));
		return 1;
	} else {
		return 0;
	}
}

VOIKKOEXPORT int voikko_set_bool_option(int /*handle*/, int option, int value) {
	return voikkoSetBooleanOption(reinterpret_cast<VoikkoHandle *>(&voikko_options), option, value);
}

VOIKKOEXPORT int voikko_set_int_option(int /*handle*/, int option, int value) {
	if (option == 5) {
		// deprecated option VOIKKO_INTERSECT_COMPOUND_LEVEL
		return 1;
	}
	return voikkoSetIntegerOption(reinterpret_cast<VoikkoHandle *>(&voikko_options), option, value);
}

VOIKKOEXPORT int voikko_set_string_option(int /*handle*/, int option, const char * value) {
	// If deprecated VOIKKO_OPT_ENCODING is used and value is "UTF-8" return success.
	// Otherwise return failure.
	if (!value || option != 2) {
		return 0;
	}
	if (strcmp(value, "UTF-8") == 0) {
		return 1;
	}
	return 0;
}

VOIKKOEXPORT int voikko_spell_cstr(int /*handle*/, const char * word) {
	return voikkoSpellCstr(reinterpret_cast<VoikkoHandle *>(&voikko_options), word);
}

VOIKKOEXPORT int voikko_spell_ucs4(int /*handle*/, const wchar_t * word) {
	return voikkoSpellUcs4(reinterpret_cast<VoikkoHandle *>(&voikko_options), word);
}

VOIKKOEXPORT char ** voikko_suggest_cstr(int /*handle*/, const char * word) {
	char ** suggestions = voikkoSuggestCstr(reinterpret_cast<VoikkoHandle *>(&voikko_options), word);
	utils::StringUtils::convertCStringArrayToMalloc(suggestions);
	return suggestions;
}

VOIKKOEXPORT wchar_t ** voikko_suggest_ucs4(int /*handle*/, const wchar_t * word) {
	return voikkoSuggestUcs4(reinterpret_cast<VoikkoHandle *>(&voikko_options), word);
}

VOIKKOEXPORT char * voikko_hyphenate_cstr(int /*handle*/, const char * word) {
	char * hyphenation = voikkoHyphenateCstr(reinterpret_cast<VoikkoHandle *>(&voikko_options), word);
	utils::StringUtils::convertCStringToMalloc(hyphenation);
	return hyphenation;
}

VOIKKOEXPORT char * voikko_hyphenate_ucs4(int /*handle*/, const wchar_t * word) {
	char * hyphenation = voikkoHyphenateUcs4(reinterpret_cast<VoikkoHandle *>(&voikko_options), word);
	utils::StringUtils::convertCStringToMalloc(hyphenation);
	return hyphenation;
}

VOIKKOEXPORT void voikko_free_suggest_cstr(char ** suggest_result) {
	if (suggest_result) {
		for (char ** p = suggest_result; *p; p++) {
			free(*p);
		}
		free(suggest_result);
	}
}

VOIKKOEXPORT void voikko_free_hyphenate(char * hyphenate_result) {
	free(hyphenate_result);
}

VOIKKOEXPORT enum voikko_token_type voikko_next_token_ucs4(int /*handle*/, const wchar_t * text,
		size_t textlen, size_t * tokenlen) {
	return voikkoNextTokenUcs4(reinterpret_cast<VoikkoHandle *>(&voikko_options), text, textlen, tokenlen);
}

VOIKKOEXPORT enum voikko_token_type voikko_next_token_cstr(int /*handle*/, const char * text,
		size_t textlen, size_t * tokenlen) {
	return voikkoNextTokenCstr(reinterpret_cast<VoikkoHandle *>(&voikko_options), text, textlen, tokenlen);
}

VOIKKOEXPORT enum voikko_sentence_type voikko_next_sentence_start_ucs4(int /*handle*/,
		const wchar_t * text, size_t textlen, size_t * sentencelen) {
	return voikkoNextSentenceStartUcs4(reinterpret_cast<VoikkoHandle *>(&voikko_options), text, textlen, sentencelen);
}

VOIKKOEXPORT enum voikko_sentence_type voikko_next_sentence_start_cstr(int /*handle*/,
                          const char * text, size_t textlen, size_t * sentencelen) {
	return voikkoNextSentenceStartCstr(reinterpret_cast<VoikkoHandle *>(&voikko_options), text, textlen, sentencelen);
}

VOIKKOEXPORT voikko_grammar_error voikko_next_grammar_error_ucs4(int /*handle*/, const wchar_t * text,
		 size_t textlen, size_t startpos, int skiperrors) {
	voikko_grammar_error gError;
	voikko_grammar_error * error = reinterpret_cast<voikko_grammar_error *>(voikkoNextGrammarErrorUcs4(reinterpret_cast<VoikkoHandle *>(&voikko_options), text, textlen, startpos, skiperrors));
	if (error) {
		// TODO: pick the attributes separately, do not assume that voikko_frammar_error == VoikkoGrammarError
		gError = *error;
		// TODO: delete using public API
		delete error;
	} else {
		gError.errorlen = 0;
		gError.error_code = 0;
		gError.error_description = 0;
		gError.error_level = 0;
		gError.startpos = 0;
		gError.suggestions = 0;
	}
	return gError;
}

VOIKKOEXPORT voikko_grammar_error voikko_next_grammar_error_cstr(int handle, const char * text,
                     size_t textlen, size_t startpos, int skiperrors) {
	if (text == 0 || textlen == 0) {
		return voikko_next_grammar_error_ucs4(handle, 0, 0, 0, 0);
	}
	
	wchar_t * text_ucs4 =
	    utils::StringUtils::ucs4FromUtf8(text, textlen);
	if (text_ucs4 == 0) {
		return voikko_next_grammar_error_ucs4(handle, 0, 0, 0, 0);
	}
	
	size_t wtextlen = wcslen(text_ucs4);
	voikko_grammar_error e = voikko_next_grammar_error_ucs4(handle, text_ucs4,
	                         wtextlen, startpos, skiperrors);
	delete[] text_ucs4;
	
	return e;
}

} }
