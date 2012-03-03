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

/**
 * Compatibility implementations for deprecated public API.
 */

#include "porting.h"
#include "utils/StringUtils.hpp"
#include "voikko.h"
#include <cstring>
#include <cstdlib>
#include <cwchar>

namespace libvoikko { namespace compatibility {

/** Only MAX_HANDLES - 1 handles are actually available because handle 0 is not used. */
static const int MAX_HANDLES = 5;
static VoikkoHandle ** handles = 0;

/**
 * Converts "something" to "fi-x-somethin-g"
 */
static char * convertVariantToBCP47(const char * variant) {
	size_t variantLen = strlen(variant); // this must be at least 1
	char * language = new char[2 * variantLen + 5];
	strcpy(language, "fi-x");
	size_t pos = 4;
	for (size_t i = 0; i < variantLen; i++) {
		if (i % 8 == 0) {
			language[pos++] = '-';
		}
		language[pos++] = variant[i];
	}
	language[pos] = '\0';
	return language;
}

/**
 * Find a slot for new instance handle. Not thread safe.
 * @return index of available instance slot or -1 if all slots are full.
 */
static int findFreeSlotForHandle() {
	if (!handles) {
		handles = new VoikkoHandle*[MAX_HANDLES];
		memset(handles, 0, MAX_HANDLES * sizeof(VoikkoHandle*));
	}
	// Slot 0 is not used in order to ensure compatibility with old behaviour
	// as much as possible.
	for (int i = 1; i < MAX_HANDLES; i++) {
		if (!handles[i]) {
			return i;
		}
	}
	return -1;
}

/**
 * Free memory reserved for handles if no handles exist anymore.
 */
static void freeHandlesIfPossible() {
	for (int i = 1; i < MAX_HANDLES; i++) {
		if (handles[i]) {
			return;
		}
	}
	delete[] handles;
	handles = 0;
}

VOIKKOEXPORT const char * voikko_init_with_path(int * handle, const char * langcode,
                                   int cache_size, const char * path) {
	int handleIndex = findFreeSlotForHandle();
	if (handleIndex < 0) {
		return "Maximum handle count exceeded";
	}
	const char * error;
	if (langcode) {
		if (strcmp("", langcode) == 0 ||
		    strcmp("default", langcode) == 0 || strcmp("fi_FI", langcode) == 0) {
			handles[handleIndex] = voikkoInit(&error, "fi", path);
		} else {
			char * language = convertVariantToBCP47(langcode);
			handles[handleIndex] = voikkoInit(&error, language, path);
			delete[] language;
		}
	} else {
		return "Null language code is not allowed";
	}
	
	if (handles[handleIndex]) {
		voikkoSetIntegerOption(handles[handleIndex], VOIKKO_SPELLER_CACHE_SIZE, cache_size);
		*handle = handleIndex;
		return 0;
	} else {
		*handle = 0;
		return error;
	}
}

VOIKKOEXPORT const char * voikko_init(int * handle, const char * langcode, int cache_size) {
	return voikko_init_with_path(handle, langcode, cache_size, 0);
}

VOIKKOEXPORT int voikko_terminate(int handle) {
	if (handle >= 1 && handle < MAX_HANDLES && handles[handle]) {
		voikkoTerminate(handles[handle]);
		handles[handle] = 0;
		freeHandlesIfPossible();
		return 1;
	} else {
		return 0;
	}
}

VOIKKOEXPORT int voikko_set_bool_option(int handle, int option, int value) {
	return voikkoSetBooleanOption(handles[handle], option, value);
}

VOIKKOEXPORT int voikko_set_int_option(int handle, int option, int value) {
	if (option == 5) {
		// deprecated option VOIKKO_INTERSECT_COMPOUND_LEVEL
		return 1;
	}
	return voikkoSetIntegerOption(handles[handle], option, value);
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

VOIKKOEXPORT int voikko_spell_cstr(int handle, const char * word) {
	return voikkoSpellCstr(handles[handle], word);
}

VOIKKOEXPORT int voikko_spell_ucs4(int handle, const wchar_t * word) {
	return voikkoSpellUcs4(handles[handle], word);
}

VOIKKOEXPORT char ** voikko_suggest_cstr(int handle, const char * word) {
	char ** suggestions = voikkoSuggestCstr(handles[handle], word);
	utils::StringUtils::convertCStringArrayToMalloc(suggestions);
	return suggestions;
}

VOIKKOEXPORT wchar_t ** voikko_suggest_ucs4(int handle, const wchar_t * word) {
	return voikkoSuggestUcs4(handles[handle], word);
}

VOIKKOEXPORT char * voikko_hyphenate_cstr(int handle, const char * word) {
	char * hyphenation = voikkoHyphenateCstr(handles[handle], word);
	utils::StringUtils::convertCStringToMalloc(hyphenation);
	return hyphenation;
}

VOIKKOEXPORT char * voikko_hyphenate_ucs4(int handle, const wchar_t * word) {
	char * hyphenation = voikkoHyphenateUcs4(handles[handle], word);
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

VOIKKOEXPORT enum voikko_token_type voikko_next_token_ucs4(int handle, const wchar_t * text,
		size_t textlen, size_t * tokenlen) {
	return voikkoNextTokenUcs4(handles[handle], text, textlen, tokenlen);
}

VOIKKOEXPORT enum voikko_token_type voikko_next_token_cstr(int handle, const char * text,
		size_t textlen, size_t * tokenlen) {
	return voikkoNextTokenCstr(handles[handle], text, textlen, tokenlen);
}

VOIKKOEXPORT enum voikko_sentence_type voikko_next_sentence_start_ucs4(int handle,
		const wchar_t * text, size_t textlen, size_t * sentencelen) {
	return voikkoNextSentenceStartUcs4(handles[handle], text, textlen, sentencelen);
}

VOIKKOEXPORT enum voikko_sentence_type voikko_next_sentence_start_cstr(int handle,
                          const char * text, size_t textlen, size_t * sentencelen) {
	return voikkoNextSentenceStartCstr(handles[handle], text, textlen, sentencelen);
}

VOIKKOEXPORT voikko_grammar_error voikko_next_grammar_error_ucs4(int handle, const wchar_t * text,
		 size_t textlen, size_t startpos, int skiperrors) {
	voikko_grammar_error gError;
	gError.error_level = 0;
	gError.error_description = 0;
	VoikkoGrammarError * error = voikkoNextGrammarErrorUcs4(handles[handle], text, textlen, startpos, skiperrors);
	if (error) {
		gError.error_code = voikkoGetGrammarErrorCode(error);
		gError.startpos = voikkoGetGrammarErrorStartPos(error);
		gError.errorlen = voikkoGetGrammarErrorLength(error);
		
		// Use C allocation for suggestions to maintain compatibility with some
		// broken applications before libvoikko 1.5.
		const char ** suggestions = voikkoGetGrammarErrorSuggestions(error);
		if (suggestions) {
			int suggCount = 0;
			for (const char ** s = suggestions; *s; s++) {
				++suggCount;
			}
			gError.suggestions = static_cast<char **>(malloc((suggCount + 1) * sizeof(char *)));
			for (int i = 0; i < suggCount; i++) {
				gError.suggestions[i] = static_cast<char *>(malloc((strlen(suggestions[i]) + 1) * sizeof(char)));
				strcpy(gError.suggestions[i], suggestions[i]);
			}
			gError.suggestions[suggCount] = 0;
		} else {
			gError.suggestions = 0;
		}
		voikkoFreeGrammarError(error);
	} else {
		gError.error_code = 0;
		gError.startpos = 0;
		gError.errorlen = 0;
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

VOIKKOEXPORT voikko_mor_analysis ** voikko_analyze_word_ucs4(
		int handle, const wchar_t * word) {
	return voikkoAnalyzeWordUcs4(handles[handle], word);
}

VOIKKOEXPORT voikko_mor_analysis ** voikko_analyze_word_cstr(
		int handle, const char * word) {
	return voikkoAnalyzeWordCstr(handles[handle], word);
}

} }
