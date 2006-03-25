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

#include "voikko.h"
#include "voikko_utils.h"
#include "voikko_setup.h"
#include <stdlib.h>
#include <wchar.h>


wchar_t ** voikko_suggest_ucs4(const wchar_t * word) {
	wchar_t ** suggestions;
	wchar_t * suggestion;
	if (word == 0) return 0;
	switch (word[0]) {
		case L'h':
			suggestions = malloc(2 * sizeof(wchar_t *));
			suggestion = malloc(8 * sizeof(wchar_t));
			wcscpy(suggestion, L"hevonen");
			suggestions[0] = suggestion;
			suggestions[1] = 0;
			return suggestions;
		case L'a':
			suggestions = malloc(3 * sizeof(wchar_t *));
			suggestion = malloc(5 * sizeof(wchar_t));
			wcscpy(suggestion, L"auto");
			suggestions[0] = suggestion;
			suggestion = malloc(7 * sizeof(wchar_t));
			wcscpy(suggestion, L"ameeba");
			suggestions[1] = suggestion;
			suggestions[2] = 0;
			return suggestions;
	}
	return 0;
}

char ** voikko_suggest_cstr(const char * word) {
	wchar_t * word_ucs4;
	wchar_t ** suggestions_ucs4;
	char ** suggestions;
	int i;
	int scount;
	char * suggestion;
	if (word == 0 || word[0] == '\0') return 0;
	word_ucs4 = voikko_cstrtoucs4(word, voikko_options.encoding);
	if (word_ucs4 == 0) return 0;
	suggestions_ucs4 = voikko_suggest_ucs4(word_ucs4);
	free(word_ucs4);
	if (suggestions_ucs4 == 0) return 0;
	scount = 0;
	while (suggestions_ucs4[scount] != 0) scount++;
	suggestions = malloc((scount + 1) * sizeof(char *));
	for (i = 0; i < scount; i++) {
		suggestion = voikko_ucs4tocstr(suggestions_ucs4[i], voikko_options.encoding);
		if (suggestion == 0) return 0; /* suggestion cannot be encoded */
		suggestions[i] = suggestion;
		free(suggestions_ucs4[i]);
	}
	suggestions[scount] = 0;
	free(suggestions_ucs4);
	return suggestions;
}
