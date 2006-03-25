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

char * voikko_simple_hyphenation(const wchar_t * word) {
	int nchars = wcslen(word);
	wchar_t * word_copy;
	int i;
	char * hyphenation_points;
	
	if (nchars == 0) return 0;
	
	word_copy = malloc((nchars + 1) * sizeof(wchar_t));
	hyphenation_points = malloc(nchars + 1);
	for (i = 0; i < nchars; i++) {
		word_copy[i] = towlower(word[i]);
		hyphenation_points[i] = L' ';
	}
	word_copy[nchars] = '\0';
	hyphenation_points[nchars] = L'\0';
	
	if (nchars == 1) {
		free(word_copy);
		return hyphenation_points;
	}
	for (i = 1; i <= nchars - 2; i++) {
		if (wcschr(VOIKKO_CONSONANTS, word_copy[i]) && wcschr(VOIKKO_VOWELS, word_copy[i+1]))
			hyphenation_points[i] = '-';
	}
	free(word_copy);
	return hyphenation_points;
}

char * voikko_hyphenate_ucs4(const wchar_t * word) {
	return voikko_simple_hyphenation(word);
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
