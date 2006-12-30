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
#include "voikko_charset.h"
#include <stdlib.h>
#include <wchar.h>
#include <wctype.h>
#include <malaga.h>

#define MAX_SUGGESTIONS 5
#define COST_LIMIT 250

static int abort_search(int * max_suggestions, int * cost) {
	if (*max_suggestions == 0 || *cost >= COST_LIMIT) return 1;
	else return 0;
}


void voikko_suggest_correct_case(int handle, wchar_t *** suggestions, int * max_suggestions,
                                 const wchar_t * word, size_t wlen, int * cost, int ** prios) {
	enum spellresult sres;
	wchar_t * newsugg;
	value_t analysis;
	const char * analysis_str;
	char * malaga_buffer;
	size_t i, j;
	int prio;
	if (*max_suggestions == 0) return;
	sres = voikko_spell_with_priority(word, wlen, &prio);
	(*cost)++;
	switch (sres) {
		case SPELL_FAILED:
			return;
		case SPELL_OK:
			newsugg = malloc((wlen + 1) * sizeof(wchar_t));
			if (newsugg == 0) return;
			wcscpy(newsugg, word);
			**suggestions = newsugg;
			**prios = prio;
			(*suggestions)++;
			(*prios)++;
			(*max_suggestions)--;
			return;
		case SPELL_CAP_FIRST:
			newsugg = malloc((wlen + 1) * sizeof(wchar_t));
			if (newsugg == 0) return;
			newsugg[0] = towupper(word[0]);
			wcsncpy(newsugg + 1, word + 1, wlen - 1);
			newsugg[wlen] = L'\0';
			**suggestions = newsugg;
			**prios = prio;
			(*suggestions)++;
			(*prios)++;
			(*max_suggestions)--;
			return;
		case SPELL_CAP_ERROR:
			malaga_buffer = voikko_ucs4tocstr(word, "UTF-8");
			if (malaga_buffer == 0) return;
			analyse_item(malaga_buffer, MORPHOLOGY);
			free(malaga_buffer);
			(*cost)++;
			analysis = first_analysis_result();
			if (!analysis) return;
			analysis_str = get_value_string(analysis);
			newsugg = malloc((wlen + 1) * sizeof(wchar_t));
			if (newsugg == 0) return;
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
			**prios = prio;
			(*suggestions)++;
			(*prios)++;
			(*max_suggestions)--;
			return;
	}
}

const wchar_t * BACK_VOWELS =  L"aouAOU";
const wchar_t * FRONT_VOWELS = L"\u00e4\u00f6y\u00c4\u00d6Y";

void voikko_suggest_vowel_change(int handle, wchar_t *** suggestions, int * max_suggestions,
                                 const wchar_t * word, size_t wlen, int * cost, int ** prios) {
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
	if (buffer == 0) return;
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
		if (abort_search(max_suggestions, cost)) {
			free(buffer);
			return;
		}
		voikko_suggest_correct_case(handle, suggestions, max_suggestions,
		                            buffer, wlen, cost, prios);
		pat++;
	}
	free(buffer);
}

void voikko_suggest_word_split(int handle, wchar_t *** suggestions, int * max_suggestions,
                               const wchar_t * word, size_t wlen, int * cost, int ** prios) {
	size_t splitind;
	wchar_t * part1;
	wchar_t * suggestion;
	int prio_part;
	int prio_total;
	enum spellresult part1_res, part2_res;
	part1 = malloc((wlen + 1) * sizeof(wchar_t));
	if (part1 == 0) return;
	wcsncpy(part1, word, wlen);
	part1[wlen] = L'\0';

	for (splitind = wlen - 2; splitind >= 2; splitind--) {
		/* Do not split a word if there is a hyphen before the last character of part1
		   or after the first character of part2. Do not suggest splitting immediately
		   before or after a hyphen either. */
		if (word[splitind-2] == L'-' || word[splitind-1] == L'-' ||
		    word[splitind]   == L'-' || word[splitind+1] == L'-') continue;
		part1[splitind] = L'\0';
		part1_res = voikko_spell_with_priority(part1, splitind, &prio_total);
		(*cost)++;
		if (part1_res == SPELL_OK || part1_res == SPELL_CAP_FIRST) {
			part2_res = voikko_spell_with_priority(word + splitind, wlen - splitind, &prio_part);
			prio_total += prio_part;
			(*cost)++;
			if (part2_res == SPELL_OK || part2_res == SPELL_CAP_FIRST) {
				suggestion = malloc((wlen + 2) * sizeof(wchar_t));
				if (suggestion == 0) break;
				wcsncpy(suggestion, word, splitind);
				if (part1_res == SPELL_CAP_FIRST)
					suggestion[0] = towupper(suggestion[0]);
				suggestion[splitind] = L' ';
				wcsncpy(suggestion + (splitind + 1), word + splitind,
				        wlen - splitind + 1);
				if (part2_res == SPELL_CAP_FIRST)
					suggestion[splitind+1] = towupper(suggestion[splitind+1]);
				**suggestions = suggestion;
				**prios = prio_total;
				(*suggestions)++;
				(*prios)++;
				(*max_suggestions)--;
			}
		}
		if (abort_search(max_suggestions, cost)) break;
	}

	free(part1);
}

/* ä=\u00e4, ö=\u00f6, å=\u00e5, š=\u0161, ž=\u017e, é=\u00e9, â=\u00e2 */

const wchar_t * REPLACE_ORIG =
	L"aiites"  L"snulkko\u00e4mrrvppyhjjddd\u00f6gggffbbcwwxz"  L"zq\u00e5\u00e5\u00e5\u00e5aitesnul"
	L"ko\u00e4mrvpyhjd\u00f6gfbcwxzq\u00e5a"  L"e"  L"a";
const wchar_t * REPLACE_REPLACEMENT = 
	L"suorr\u0161amiklgi\u00f6netbbotjhktsf\u00e4fhkgdpnvevc\u017exao"  L"p"  L"\u00e4\u00f6ekysdhj\u00f6"
	L"jpp"  L"kdglhuiel"  L"tvvkasaka"  L"\u00e5\u00e9\u00e2";

void voikko_suggest_replacement(int handle, wchar_t *** suggestions, int * max_suggestions,
                                const wchar_t * word, size_t wlen, int * cost, int ** prios, int start, int end) {
	int i;
	wchar_t * pos;
	wchar_t * buffer = malloc((wlen + 1) * sizeof(wchar_t));
	if (buffer == 0) return;
	wcsncpy(buffer, word, wlen);
	buffer[wlen] = L'\0';
	for (i = start; i <= end; i++) {
		for (pos = wcschr(buffer, REPLACE_ORIG[i]); pos != 0; pos = wcschr(pos+1, REPLACE_ORIG[i])) {
			*pos = REPLACE_REPLACEMENT[i];
			voikko_suggest_correct_case(handle, suggestions, max_suggestions,
			                            buffer, wlen, cost, prios);
			if (abort_search(max_suggestions, cost)) break;
			*pos = REPLACE_ORIG[i];
		}
		if (abort_search(max_suggestions, cost)) break;
		for (pos = wcschr(buffer, towupper(REPLACE_ORIG[i])); pos != 0;
		     pos = wcschr(pos + 1, towupper(REPLACE_ORIG[i]))) {
			*pos = towupper(REPLACE_REPLACEMENT[i]);
			voikko_suggest_correct_case(handle, suggestions, max_suggestions,
			                            buffer, wlen, cost, prios);
			if (abort_search(max_suggestions, cost)) break;
			*pos = towupper(REPLACE_ORIG[i]);
		}
		if (abort_search(max_suggestions, cost)) break;
	}
	free(buffer);
}

void voikko_suggest_deletion(int handle, wchar_t *** suggestions, int * max_suggestions,
                             const wchar_t * word, size_t wlen, int * cost, int ** prios) {
	size_t i;
	wchar_t * buffer = malloc(wlen * sizeof(wchar_t));
	if (buffer == 0) return;
	for (i = 0; i < wlen && !abort_search(max_suggestions, cost); i++) {
		if (i == 0 || towlower(word[i]) != towlower(word[i-1])) {
			wcsncpy(buffer, word, i);
			wcsncpy(buffer + i, word + (i + 1), wlen - i);
			voikko_suggest_correct_case(handle, suggestions, max_suggestions,
			                            buffer, wlen - 1, cost, prios);
		}
	}
	free(buffer);
}

void voikko_suggest_insert_special(int handle, wchar_t *** suggestions, int * max_suggestions,
                                   const wchar_t * word, size_t wlen, int * cost, int ** prios) {
	size_t j;
	wchar_t * buffer = malloc((wlen + 2) * sizeof(wchar_t));
	if (buffer == 0) return;
	wcsncpy(buffer + 1, word, wlen);
	buffer[wlen+1] = L'\0';
	
	/* suggest adding '-' */
	for (j = 2; j <= wlen - 2 && !abort_search(max_suggestions, cost); j++) {
		/* Do not add hyphen if there already is another nearby */
		if (word[j-2] == L'-' || word[j-1] == L'-' || word[j] == L'-' || word[j+1] == L'-')
			continue;
		wcsncpy(buffer, word, j);
		buffer[j] = L'-';
		voikko_suggest_correct_case(handle, suggestions, max_suggestions,
		                            buffer, wlen + 1, cost, prios);
	}
	/* suggest character duplication */
	wcsncpy(buffer + 1, word, wlen + 1);
	for (j = 0; j < wlen && !abort_search(max_suggestions, cost); j++) {
		buffer[j] = word[j];
		if (j < wlen - 1 && word[j] == word[j+1]) { /* Do not duplicate if there already are two same letters */
			j++;
			continue;
		}
		if (word[j] == L'-' || word[j] == L'\'') continue; /* These should not be duplicated */
		voikko_suggest_correct_case(handle, suggestions, max_suggestions,
		                            buffer, wlen + 1, cost, prios);
	}
	free(buffer);
}

const wchar_t * INS_CHARS = L"aitesnulko\u00e4mrvpyhjd\u00f6gfbcw:xzq\u00e5";

void voikko_suggest_insertion(int handle, wchar_t *** suggestions, int * max_suggestions,
                              const wchar_t * word, size_t wlen, int * cost, int ** prios, int start, int end) {
	int i;
	size_t j;
	wchar_t * buffer = malloc((wlen + 2) * sizeof(wchar_t));
	if (buffer == 0) return;
	for (i = start; i <= end; i++) {
		buffer[0] = word[0];
		wcsncpy(buffer + 1, word, wlen);
		buffer[wlen+1] = L'\0';
		for (j = 0; j < wlen && !abort_search(max_suggestions, cost); j++) {
			if (j != 0) buffer[j-1] = word[j-1];
			if (INS_CHARS[i] == towlower(word[j])) continue; /* avoid duplicates */
			if (j > 0 && INS_CHARS[i] == towlower(word[j-1])) continue; /* avoid duplicates */
			buffer[j] = INS_CHARS[i];
			voikko_suggest_correct_case(handle, suggestions, max_suggestions,
			                            buffer, wlen + 1, cost, prios);
		}
		if (abort_search(max_suggestions, cost)) break;
		if (INS_CHARS[i] == word[wlen-1]) continue;
		buffer[wlen-1] = word[wlen-1];
		buffer[wlen] = INS_CHARS[i];
		voikko_suggest_correct_case(handle, suggestions, max_suggestions,
		                            buffer, wlen + 1, cost, prios);
	}
	free(buffer);
}

void voikko_suggest_swap(int handle, wchar_t *** suggestions, int * max_suggestions,
                         const wchar_t * word, size_t wlen, int * cost, int ** prios) {
	size_t max_distance;
	size_t i;
	size_t j;
	int k;
	wchar_t * buffer;
	if (wlen <= 8) max_distance = 10;
	else max_distance = 50 / wlen;
	if (max_distance == 0) return;
	buffer = malloc((wlen + 1) * sizeof(wchar_t));
	if (buffer == 0) return;
	wcsncpy(buffer, word, wlen);
	buffer[wlen] = L'\0';
	for (i = 0; i < wlen && !abort_search(max_suggestions, cost); i++) {
		for (j = i + 1; j < wlen && !abort_search(max_suggestions, cost); j++) {
			if (j - i > max_distance) break;
			/* do not suggest the same word */
			if (towlower(buffer[i]) == towlower(buffer[j])) continue;
			/* do not suggest swapping front and back vowels that have already been
			   tested earlier */
			for (k = 0; k < 3; k++) {
				if ((towlower(buffer[i]) == BACK_VOWELS[k] &&
				     towlower(buffer[j]) == FRONT_VOWELS[k]) ||
				    (towlower(buffer[i]) == FRONT_VOWELS[k] &&
				     towlower(buffer[j]) == BACK_VOWELS[k])) break;
			}
			if (k < 3) continue;
			buffer[i] = word[j];
			buffer[j] = word[i];
			voikko_suggest_correct_case(handle, suggestions, max_suggestions,
			                            buffer, wlen, cost, prios);
			buffer[i] = word[i];
			buffer[j] = word[j];
		}
	}
	free(buffer);
}

int voikko_suggest_add_dots(wchar_t ** suggestions) {
	int i;
	size_t sugglen;
	wchar_t * buffer;
	for (i = 0; suggestions[i] != 0; i++) {
		sugglen = wcslen(suggestions[i]);
		buffer = realloc(suggestions[i], (sugglen + 2) * sizeof(wchar_t));
		if (!buffer) return 0; /* Stop if allocation fails */
		buffer[sugglen] = L'.';
		buffer[sugglen+1] = L'\0';
		suggestions[i] = buffer;
	}
	return 1;
}

wchar_t ** voikko_suggest_ucs4(int handle, const wchar_t * word) {
	wchar_t ** suggestions;
	int * prios;
	int * free_prio;
	wchar_t ** free_sugg;
	wchar_t * nword;
	size_t wlen;
	int cost;
	int suggestions_left;
	int add_dots = 0;
	if (word == 0) return 0;
	wlen = wcslen(word);
	if (wlen <= 1 || wlen > LIBVOIKKO_MAX_WORD_CHARS) return 0;
	
	nword = voikko_normalise(word, wlen);
	if (nword == 0) return 0;
	wlen = wcslen(nword);
	
	if (voikko_options.ignore_dot) {
		if (wlen == 2) {
			free(nword);
			return 0;
		}
		if (nword[wlen-1] == L'.') {
			nword[--wlen] = L'\0';
			add_dots = 1;
		}
	}
	
	suggestions = calloc(MAX_SUGGESTIONS * 3 + 1, sizeof(wchar_t *));
	if (suggestions == 0) {
		free(nword);
		return 0;
	}
	prios = malloc(MAX_SUGGESTIONS * 3 * sizeof(int));
	if (prios == 0) {
		free(suggestions);
		free(nword);
		return 0;
	}
	free_sugg = suggestions;
	free_prio = prios;
	suggestions_left = MAX_SUGGESTIONS * 3;
	cost = 0;
	
	voikko_suggest_correct_case(handle, &free_sugg, &suggestions_left, nword, wlen, &cost, &free_prio);
	if (suggestions_left != MAX_SUGGESTIONS * 3) {
		free(prios);
		free(nword);
		if (add_dots) voikko_suggest_add_dots(suggestions);
		return suggestions;
	}
	voikko_suggest_vowel_change(handle, &free_sugg, &suggestions_left, nword, wlen, &cost, &free_prio);
	if (cost < COST_LIMIT && suggestions_left > 0)
		voikko_suggest_replacement(handle, &free_sugg, &suggestions_left, nword, wlen, &cost, &free_prio, 0, 50);
	if (cost < COST_LIMIT && suggestions_left > 0)
		voikko_suggest_deletion(handle, &free_sugg, &suggestions_left, nword, wlen, &cost, &free_prio);
	if (cost < COST_LIMIT && suggestions_left > 0)
		voikko_suggest_insert_special(handle, &free_sugg, &suggestions_left, nword, wlen, &cost, &free_prio);
	if (cost < COST_LIMIT && suggestions_left > 0)
		voikko_suggest_word_split(handle, &free_sugg, &suggestions_left, nword, wlen, &cost, &free_prio);
	if (cost < COST_LIMIT && suggestions_left > 0)
		voikko_suggest_insertion(handle, &free_sugg, &suggestions_left, nword, wlen, &cost, &free_prio, 0, 5);
	if (cost < COST_LIMIT && suggestions_left > 0)
		voikko_suggest_swap(handle, &free_sugg, &suggestions_left, nword, wlen, &cost, &free_prio);
	if (cost < COST_LIMIT && suggestions_left > 0)
		voikko_suggest_replacement(handle, &free_sugg, &suggestions_left, nword, wlen, &cost, &free_prio, 51, 77);
	if (cost < COST_LIMIT && suggestions_left > 0)
		voikko_suggest_insertion(handle, &free_sugg, &suggestions_left, nword, wlen, &cost, &free_prio, 6, 10);
	if (cost < COST_LIMIT && suggestions_left > 0)
		voikko_suggest_insertion(handle, &free_sugg, &suggestions_left, nword, wlen, &cost, &free_prio, 11, 15);
	if (cost < COST_LIMIT && suggestions_left > 0)
		voikko_suggest_insertion(handle, &free_sugg, &suggestions_left, nword, wlen, &cost, &free_prio, 16, 20);
	if (cost < COST_LIMIT && suggestions_left > 0)
		voikko_suggest_insertion(handle, &free_sugg, &suggestions_left, nword, wlen, &cost, &free_prio, 21, 25);
	if (cost < COST_LIMIT && suggestions_left > 0)
		voikko_suggest_insertion(handle, &free_sugg, &suggestions_left, nword, wlen, &cost, &free_prio, 26, 29);

	if (suggestions_left == MAX_SUGGESTIONS * 3) {
		free(suggestions);
		free(prios);
		free(nword);
		return 0;
	}
	
	/* Sort the suggestions by priority using insertion sort */
	int i, j;
	wchar_t * current_sugg;
	int current_prio;
	for (i = 0; suggestions + i < free_sugg; i++) {
		current_sugg = suggestions[i];
		current_prio = prios[i];
		for (j = i - 1; j >= 0 && prios[j] > current_prio; j--) {
			suggestions[j + 1] = suggestions[j];
			prios[j + 1] = prios[j];
		}
		suggestions[j + 1] = current_sugg;
		prios[j + 1] = current_prio;
	}
	free(prios);

	/* Remove extra suggestions */
	for (i = MAX_SUGGESTIONS; suggestions[i] != 0; i++) {
		free(suggestions[i]);
		suggestions[i] = 0;
	}

	/* Change the character case to match the original word */
	enum casetype origcase = voikko_casetype(nword, wlen);
	size_t suglen;
	if (origcase == CT_FIRST_UPPER) {
		i = 0;
		while (suggestions[i] != 0) {
			suglen = wcslen(suggestions[i]);
			if (voikko_casetype(suggestions[i], suglen) == CT_ALL_LOWER)
				voikko_set_case(CT_FIRST_UPPER, suggestions[i], suglen);
			i++;
		}
	}
	if (origcase == CT_ALL_UPPER) {
		i = 0;
		while (suggestions[i] != 0) {
			suglen = wcslen(suggestions[i]);
			voikko_set_case(CT_ALL_UPPER, suggestions[i], suglen);
			i++;
		}
	}
	
	free(nword);
	if (add_dots) voikko_suggest_add_dots(suggestions);
	return suggestions;
}

char ** voikko_suggest_cstr(int handle, const char * word) {
	wchar_t * word_ucs4;
	wchar_t ** suggestions_ucs4;
	char ** suggestions;
	int i;
	int j;
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
	suggestions = calloc(scount + 1, sizeof(char *));
	if (suggestions == 0) {
		free(suggestions_ucs4);
		return 0;
	}
	j = 0;
	for (i = 0; i < scount; i++) {
		suggestion = voikko_ucs4tocstr(suggestions_ucs4[i], voikko_options.encoding);
		free(suggestions_ucs4[i]);
		if (suggestion == 0) continue; /* suggestion cannot be encoded */
		suggestions[j++] = suggestion;
	}
	suggestions[scount] = 0;
	free(suggestions_ucs4);
	if (j == 0) {
		free(suggestions);
		return 0;
	}
	else return suggestions;
}
