/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2006 - 2010 Harri Pitkänen <hatapitk@iki.fi>
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
#include "utils/utils.hpp"
#include "utils/StringUtils.hpp"
#include "character/charset.hpp"
#include "spellchecker/suggestion/SuggestionStatus.hpp"
#include "spellchecker/suggestion/SuggestionGenerator.hpp"
#include <cstdlib>
#include <cstring>
#include <cwchar>

#define MAX_SUGGESTIONS 5

using namespace libvoikko::spellchecker::suggestion;

namespace libvoikko {

wchar_t ** getSuggestions(SuggestionStatus & status, bool addDot) {
	const Suggestion * const originalSuggestions = status.getSuggestions();
	size_t returnedSuggestionCount = MAX_SUGGESTIONS < status.getSuggestionCount() ?
	                                 MAX_SUGGESTIONS : status.getSuggestionCount();
	wchar_t ** suggestions = new wchar_t*[returnedSuggestionCount + 1];
	for (size_t i = 0; i < returnedSuggestionCount; i++) {
		size_t sugglen = wcslen(originalSuggestions[i].word);
		wchar_t * buffer = new wchar_t[sugglen + 1 + (addDot ? 1 : 0)];
		wcsncpy(buffer, originalSuggestions[i].word, sugglen);
		if (addDot) {
			buffer[sugglen] = L'.';
			buffer[sugglen+1] = L'\0';
		}
		else {
			buffer[sugglen] = L'\0';
		}
		suggestions[i] = buffer;
	}
	suggestions[returnedSuggestionCount] = 0;
	return suggestions;
}

VOIKKOEXPORT void voikko_free_suggest_ucs4(wchar_t ** suggest_result) {
	if (suggest_result) {
		for (wchar_t ** p = suggest_result; *p; p++) {
			delete[] *p;
		}
		delete[] suggest_result;
	}
}

VOIKKOEXPORT void voikko_free_suggest_cstr(char ** suggest_result) {
	// C deallocation is used here to maintain compatibility with some
	// broken applications before libvoikko 1.5.
	if (suggest_result) {
		for (char ** p = suggest_result; *p; p++) {
			free(*p);
		}
		free(suggest_result);
	}
}

VOIKKOEXPORT wchar_t ** voikko_suggest_ucs4(int handle, const wchar_t * word) {
	bool add_dots = false;
	if (word == 0) return 0;
	size_t wlen = wcslen(word);
	if (wlen <= 1 || wlen > LIBVOIKKO_MAX_WORD_CHARS) return 0;
	
	wchar_t * nword = voikko_normalise(word, wlen);
	if (nword == 0) {
		return 0;
	}
	wlen = wcslen(nword);
	
	if (voikko_options.ignore_dot) {
		if (wlen == 2) {
			delete[] nword;
			return 0;
		}
		if (nword[wlen-1] == L'.') {
			nword[--wlen] = L'\0';
			add_dots = true;
		}
	}
	
	SuggestionStatus status(handle, nword, wlen, MAX_SUGGESTIONS * 3);
	
	SuggestionGenerator * generator = voikko_options.suggestionGenerator;
	generator->generate(&status);
	
	if (status.getSuggestionCount() == 0) {
		delete[] nword;
		return 0;
	}
	
	status.sortSuggestions();

	wchar_t ** suggestions = getSuggestions(status, add_dots);

	/* Change the character case to match the original word */
	enum casetype origcase = voikko_casetype(nword, wlen);
	size_t suglen;
	if (origcase == CT_FIRST_UPPER) {
		size_t i = 0;
		while (suggestions[i] != 0) {
			suglen = wcslen(suggestions[i]);
			if (voikko_casetype(suggestions[i], suglen) == CT_ALL_LOWER)
				voikko_set_case(CT_FIRST_UPPER, suggestions[i], suglen);
			i++;
		}
	}
	if (origcase == CT_ALL_UPPER) {
		size_t i = 0;
		while (suggestions[i] != 0) {
			suglen = wcslen(suggestions[i]);
			voikko_set_case(CT_ALL_UPPER, suggestions[i], suglen);
			i++;
		}
	}

	/* Undo character set normalisation */
	for (size_t i = 0; suggestions[i] != 0;) {
		suglen = wcslen(suggestions[i]);
		voikko_cset_reformat(word, wlen, &(suggestions[i]), suglen);
		i++;
	}

	delete[] nword;
	return suggestions;
}

VOIKKOEXPORT char ** voikko_suggest_cstr(int handle, const char * word) {
	if (word == 0 || word[0] == '\0') return 0;
	size_t len = strlen(word);
	if (len > LIBVOIKKO_MAX_WORD_CHARS) return 0;
	wchar_t * word_ucs4 = utils::StringUtils::ucs4FromUtf8(word, len);
	if (word_ucs4 == 0) return 0;
	wchar_t ** suggestions_ucs4 = voikko_suggest_ucs4(handle, word_ucs4);
	delete[] word_ucs4;
	if (suggestions_ucs4 == 0) return 0;
	int scount = 0;
	while (suggestions_ucs4[scount] != 0) scount++;
	
	char ** suggestions = new char*[scount + 1];
	if (suggestions == 0) {
		voikko_free_suggest_ucs4(suggestions_ucs4);
		return 0;
	}
	
	int j = 0;
	for (int i = 0; i < scount; i++) {
		char * suggestion = utils::StringUtils::utf8FromUcs4(suggestions_ucs4[i]);
		if (suggestion == 0) continue; /* suggestion cannot be encoded */
		suggestions[j++] = suggestion;
	}
	voikko_free_suggest_ucs4(suggestions_ucs4);
	if (j == 0) {
		delete[] suggestions;
		return 0;
	}
	for (; j <= scount; j++) {
		suggestions[j] = 0;
	}
	
	// Convert to C allocation to maintain compatibility with some
	// broken applications before libvoikko 1.5.
	utils::StringUtils::convertCStringArrayToMalloc(suggestions);
	return suggestions;
}

}
