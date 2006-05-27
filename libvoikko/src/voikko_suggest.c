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

#include "voikko_defs.h"
#include "voikko_utils.h"
#include "voikko_setup.h"
#include "voikko_suggest.h"
#include "voikko_spell.h"
#include <stdlib.h>
#include <wchar.h>
#include <wctype.h>
#include <malaga.h>

#define MAX_SUGGESTIONS 10
#define COST_LIMIT 250

void voikko_suggest_correct_case(int handle, wchar_t *** suggestions, int * max_suggestions,
                                 const wchar_t * word, size_t wlen, int * cost) {
	enum spellresult sres;
	wchar_t * newsugg;
	value_t analysis;
	const char * analysis_str;
	char * malaga_buffer;
	size_t i, j;
	if (*max_suggestions == 0) return;
	sres = voikko_do_spell(word, wlen);
	(*cost)++;
	switch (sres) {
		case SPELL_FAILED:
			return;
		case SPELL_OK:
			newsugg = malloc((wlen + 1) * sizeof(wchar_t));
			wcscpy(newsugg, word);
			**suggestions = newsugg;
			(*suggestions)++;
			(*max_suggestions)--;
			return;
		case SPELL_CAP_FIRST:
			newsugg = malloc((wlen + 1) * sizeof(wchar_t));
			newsugg[0] = towupper(word[0]);
			wcsncpy(newsugg + 1, word + 1, wlen - 1);
			newsugg[wlen] = L'\0';
			**suggestions = newsugg;
			(*suggestions)++;
			(*max_suggestions)--;
			return;
		case SPELL_CAP_ERROR:
			malaga_buffer = voikko_ucs4tocstr(word, "UTF-8");
			analyse_item(malaga_buffer, MORPHOLOGY);
			free(malaga_buffer);
			(*cost)++;
			analysis = first_analysis_result();
			if (!analysis) return;
			analysis_str = get_value_string(analysis);
			newsugg = malloc((wlen + 1) * sizeof(wchar_t));
			wcscpy(newsugg, word);
			j = 0;
			for (i = 0; i < wlen; i++) {
				while (analysis_str[j] == '=') j++;
				if (analysis_str[j] == '\0') break;
				if (analysis_str[j] == 'i' || analysis_str[j] == 'j')
					newsugg[i] = towupper(newsugg[i]);
				else if (analysis_str[j] == 'p' || analysis_str[j] == 'q')
					newsugg[i] = towlower(newsugg[i]);
				j++;
			}
			free((char *) analysis_str);
			**suggestions = newsugg;
			(*suggestions)++;
			(*max_suggestions)--;
			return;
	}
}

const wchar_t * BACK_VOWELS =  L"aouAOU";
const wchar_t * FRONT_VOWELS = L"\u00e4\u00f6y\u00c4\u00d6Y";

void voikko_suggest_vowel_change(int handle, wchar_t *** suggestions, int * max_suggestions,
                                 const wchar_t * word, size_t wlen, int * cost) {
	size_t i;
	int j;
	int k;
	int mask = 0;
	int vcount = 0;
	int pat = 1;
	wchar_t * buffer;
	for (i = 0; i < wlen; i++)
		for (j = 0; j < 6; j++)
			if (word[i] == BACK_VOWELS[j] ||
			    word[i] == FRONT_VOWELS[j]) {
				vcount++;
				mask <<= 1;
				mask++;
				break;
			}
	if (vcount == 0 || vcount > 7) return;
	buffer = malloc((wlen + 1) * sizeof(wchar_t));
	while ((pat & mask) != 0) {
		i = 0;
		wcscpy(buffer, word);
		for (j = 0; j < vcount; j++) {
			while (!wcschr(BACK_VOWELS,  buffer[i]) &&
			       !wcschr(FRONT_VOWELS, buffer[i])) i++;
			if (pat & (1 << j)) {
				for (k = 0; k < 6; k++) {
					if (buffer[i] == BACK_VOWELS[k]) {
						buffer[i] = FRONT_VOWELS[k];
						break;
					}
					if (buffer[i] == FRONT_VOWELS[k]) {
						buffer[i] = BACK_VOWELS[k];
						break;
					}
				}
			}
			i++;
		}
		if (*max_suggestions == 0) {
			free(buffer);
			return;
		}
		voikko_suggest_correct_case(handle, suggestions, max_suggestions,
		                            buffer, wlen, cost);
		pat++;
	}
	free(buffer);
}

void voikko_suggest_word_split(int handle, wchar_t *** suggestions, int * max_suggestions,
                               const wchar_t * word, size_t wlen, int * cost) {
	size_t splitind;
	wchar_t * part1;
	wchar_t * suggestion;
	enum spellresult part1_res, part2_res;
	part1 = malloc((wlen + 1) * sizeof(wchar_t));
	wcscpy(part1, word);

	for (splitind = wlen - 1; splitind > 0; splitind--) {
		part1[splitind] = L'\0';
		part1_res = voikko_do_spell(part1, splitind);
		(*cost)++;
		if (part1_res == SPELL_OK || part1_res == SPELL_CAP_FIRST) {
			part2_res = voikko_do_spell(word + splitind, wlen - splitind);
			(*cost)++;
			if (part2_res == SPELL_OK || part2_res == SPELL_CAP_FIRST) {
				suggestion = malloc((wlen + 2) * sizeof(wchar_t));
				wcsncpy(suggestion, word, splitind);
				if (part1_res == SPELL_CAP_FIRST)
					suggestion[0] = towupper(suggestion[0]);
				suggestion[splitind] = L' ';
				wcsncpy(suggestion + (splitind + 1), word + splitind,
				        wlen - splitind + 1);
				if (part2_res == SPELL_CAP_FIRST)
					suggestion[splitind+1] = towupper(suggestion[splitind+1]);
				**suggestions = suggestion;
				(*suggestions)++;
				(*max_suggestions)--;
			}
		}
	}

	free(part1);
}

/* ä=\u00e4, ö=\u00f6, å=\u00e5 */

const wchar_t * REPLACE_ORIG =
	L"aiitesnulkko\u00e4mrrvppyhjjddd\u00f6gggffbbcwwxzq\u00e5\u00e5\u00e5\u00e5aitesnul"  L"ko\u00e4"
	L"mrvpyhjd\u00f6gfbcwxzq\u00e5";
const wchar_t * REPLACE_REPLACEMENT = 
	L"suorramiklgi\u00f6netbbotjhktsf\u00e4fhkgdpnvevcxao"  L"p"  L"\u00e4\u00f6ekysdhj\u00f6jpp"
	L"kdglhuiel"  L"tvvkasaka";
const int REPL_COUNT = 73;

void voikko_suggest_replacement(int handle, wchar_t *** suggestions, int * max_suggestions,
                                const wchar_t * word, size_t wlen, int * cost) {
	int i;
	wchar_t * pos;
	wchar_t * buffer = malloc((wlen + 1) * sizeof(wchar_t));
	wcsncpy(buffer, word, wlen + 1);
	for (i = 0; i < REPL_COUNT; i++) {
		for (pos = wcschr(buffer, REPLACE_ORIG[i]); pos != 0; pos = wcschr(pos+1, REPLACE_ORIG[i])) {
			*pos = REPLACE_REPLACEMENT[i];
			voikko_suggest_correct_case(handle, suggestions, max_suggestions,
			                            buffer, wlen, cost);
			if (*max_suggestions == 0) break;
			*pos = REPLACE_ORIG[i];
		}
		if (*max_suggestions == 0) break;
		for (pos = wcschr(buffer, towupper(REPLACE_ORIG[i])); pos != 0;
		     pos = wcschr(pos + 1, towupper(REPLACE_ORIG[i]))) {
			*pos = towupper(REPLACE_REPLACEMENT[i]);
			voikko_suggest_correct_case(handle, suggestions, max_suggestions,
			                            buffer, wlen, cost);
			if (*max_suggestions == 0) break;
			*pos = towupper(REPLACE_ORIG[i]);
		}
		if (*max_suggestions == 0) break;
	}
	free(buffer);
}

wchar_t ** voikko_suggest_ucs4(int handle, const wchar_t * word) {
	wchar_t ** suggestions;
	wchar_t ** free_sugg;
	size_t wlen;
	int cost;
	int suggestions_left;
	if (word == 0) return 0;
	wlen = wcslen(word);
	if (wlen <= 1) return 0;
	
	suggestions = calloc(MAX_SUGGESTIONS + 1, sizeof(wchar_t *));
	free_sugg = suggestions;
	suggestions_left = MAX_SUGGESTIONS;
	cost = 0;
	
	voikko_suggest_correct_case(handle, &free_sugg, &suggestions_left, word, wlen, &cost);
	if (suggestions_left != MAX_SUGGESTIONS) return suggestions;
	voikko_suggest_vowel_change(handle, &free_sugg, &suggestions_left, word, wlen, &cost);
	if (cost < COST_LIMIT && suggestions_left > 0)
		voikko_suggest_word_split(handle, &free_sugg, &suggestions_left, word, wlen, &cost);
	if (cost < COST_LIMIT && suggestions_left > 0)
		voikko_suggest_replacement(handle, &free_sugg, &suggestions_left, word, wlen, &cost);
	if (suggestions_left == MAX_SUGGESTIONS) {
		free(suggestions);
		return 0;
	}
	return suggestions;
}

char ** voikko_suggest_cstr(int handle, const char * word) {
	wchar_t * word_ucs4;
	wchar_t ** suggestions_ucs4;
	char ** suggestions;
	int i;
	int scount;
	char * suggestion;
	if (word == 0 || word[0] == '\0') return 0;
	word_ucs4 = voikko_cstrtoucs4(word, voikko_options.encoding);
	if (word_ucs4 == 0) return 0;
	suggestions_ucs4 = voikko_suggest_ucs4(handle, word_ucs4);
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
