/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2006 Harri Pitkänen <hatapitk@iki.fi>
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

#include "voikko_hyphenate.h"
#include "voikko_setup.h"
#include "voikko_utils.h"
#include "voikko.h"
#include <wchar.h>
#include <stdlib.h>
#include <wctype.h>

const wchar_t * SPLIT_VOWELS[] = { L"ae", L"ao", L"ea", L"eo", L"ia", L"io", L"oa", L"oe",
                                   L"ua", L"ue", L"ye", L"eä", L"eö", L"iä", L"iö", L"yä",
                                   L"äe", L"öe" };

void voikko_simple_hyphenation(const wchar_t * word, char * hyphenation_points) {
	int nchars = wcslen(word);
	wchar_t * word_copy;
	int i;
	int j;
	
	if (nchars == 0) return;
	
	word_copy = malloc((nchars + 1) * sizeof(wchar_t));
	for (i = 0; i < nchars; i++) {
		word_copy[i] = towlower(word[i]);
		hyphenation_points[i] = ' ';
	}
	word_copy[nchars] = '\0';
	hyphenation_points[nchars] = '\0';
	
	if (nchars == 1) {
		free(word_copy);
		return;
	}
	
	/* no hyphenation allowed at ^C-CV */
	if (wcschr(VOIKKO_CONSONANTS, word_copy[0]) && wcschr(VOIKKO_CONSONANTS, word_copy[1])) i = 2;
	else i = 1;
	
	/* -CV */
	for (; i <= nchars - 2; i++) {
		if (wcschr(VOIKKO_CONSONANTS, word_copy[i]) && wcschr(VOIKKO_VOWELS, word_copy[i+1]))
			hyphenation_points[i] = '-';
	}
	
	/* V-V */
	for (i = 0; i < nchars - 1; i++) {
		if (hyphenation_points[i+1] != ' ') continue;
		if (!wcschr(VOIKKO_VOWELS, word_copy[i])) continue;
		if (!wcschr(VOIKKO_VOWELS, word_copy[i+1])) continue;
		for (j = 0; j < 18; j++) {
			if (wcsncmp(&word_copy[i], SPLIT_VOWELS[j], 2) == 0) {
				hyphenation_points[i+1] = '-';
				break;
			}
		}
	}
	free(word_copy);
}

char * voikko_hyphenate_ucs4(const wchar_t * word) {
	char * hyphenation;
	hyphenation = malloc(wcslen(word) + 1);
	hyphenation[wcslen(word)] = '\0';
	voikko_simple_hyphenation(word, hyphenation);
	return hyphenation;
}

char * voikko_hyphenate_cstr(const char * word) {
	wchar_t * word_ucs4;
	char * result;
	if (word == 0) return 0;
	word_ucs4 = voikko_cstrtoucs4(word, voikko_options.encoding);
	if (word_ucs4 == 0) return 0;
	result = voikko_hyphenate_ucs4(word_ucs4);
	free(word_ucs4);
	return result;
}
