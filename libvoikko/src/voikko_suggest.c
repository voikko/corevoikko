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

static int abort_search(voikko_sugg_status_t * s) {
	if ((*s).max_suggestions == 0 || (*s).max_cost <= 0) return 1;
	else return 0;
}

static void charge(voikko_sugg_status_t * s) {
	((*s).max_cost)--;
}

void voikko_suggest_correct_case(voikko_sugg_status_t * s, const wchar_t * buffer, size_t buflen) {
	enum spellresult sres;
	wchar_t * newsugg;
	value_t analysis;
	const char * analysis_str;
	char * malaga_buffer;
	size_t i, j, wlen;
	int prio;
	const wchar_t * word;
	if (abort_search(s)) return;
	if (buffer == 0) {
		word = (*s).word;
		wlen = (*s).wlen;
	}
	else {
		word = buffer;
		wlen = buflen;
	}
	sres = voikko_spell_with_priority(word, wlen, &prio);
	charge(s);
	switch (sres) {
		case SPELL_FAILED:
			return;
		case SPELL_OK:
			newsugg = malloc((wlen + 1) * sizeof(wchar_t));
			if (newsugg == 0) return;
			wcscpy(newsugg, word);
			*((*s).suggestions) = newsugg;
			*((*s).prios) = prio;
			((*s).suggestions)++;
			((*s).prios)++;
			((*s).max_suggestions)--;
			return;
		case SPELL_CAP_FIRST:
			newsugg = malloc((wlen + 1) * sizeof(wchar_t));
			if (newsugg == 0) return;
			newsugg[0] = towupper(word[0]);
			wcsncpy(newsugg + 1, word + 1, wlen - 1);
			newsugg[wlen] = L'\0';
			*((*s).suggestions) = newsugg;
			*((*s).prios) = prio;
			((*s).suggestions)++;
			((*s).prios)++;
			((*s).max_suggestions)--;
			return;
		case SPELL_CAP_ERROR:
			malaga_buffer = voikko_ucs4tocstr(word, "UTF-8", wlen);
			if (malaga_buffer == 0) return;
			analyse_item(malaga_buffer, MORPHOLOGY);
			free(malaga_buffer);
			charge(s);
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
			*((*s).suggestions) = newsugg;
			*((*s).prios) = prio;
			((*s).suggestions)++;
			((*s).prios)++;
			((*s).max_suggestions)--;
			return;
	}
}

const wchar_t * BACK_VOWELS =  L"aouAOU";
const wchar_t * FRONT_VOWELS = L"\u00e4\u00f6y\u00c4\u00d6Y";

void voikko_suggest_vowel_change(voikko_sugg_status_t * s) {
	size_t i;
	int j;
	int k;
	int mask = 0;
	int vcount = 0;
	int pat = 1;
	wchar_t * buffer;
	for (i = 0; i < (*s).wlen; i++)
		for (j = 0; j < 6; j++)
			if (((*s).word)[i] == BACK_VOWELS[j] ||
			    ((*s).word)[i] == FRONT_VOWELS[j]) {
				vcount++;
				mask <<= 1;
				mask++;
				break;
			}
	if (vcount == 0 || vcount > 7) return;
	buffer = malloc(((*s).wlen + 1) * sizeof(wchar_t));
	if (buffer == 0) return;
	while ((pat & mask) != 0) {
		i = 0;
		wcscpy(buffer, (*s).word);
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
		if (abort_search(s)) {
			free(buffer);
			return;
		}
		voikko_suggest_correct_case(s, buffer, (*s).wlen);
		pat++;
	}
	free(buffer);
}

void voikko_suggest_word_split(voikko_sugg_status_t * s) {
	size_t splitind;
	wchar_t * part1;
	wchar_t * suggestion;
	int prio_part;
	int prio_total;
	enum spellresult part1_res, part2_res;
	part1 = malloc(((*s).wlen + 1) * sizeof(wchar_t));
	if (part1 == 0) return;
	wcsncpy(part1, (*s).word, (*s).wlen);
	part1[(*s).wlen] = L'\0';

	for (splitind = (*s).wlen - 2; splitind >= 2; splitind--) {
		/* Do not split a word if there is a hyphen before the last character of part1
		   or after the first character of part2. Do not suggest splitting immediately
		   before or after a hyphen either. */
		if (((*s).word)[splitind-2] == L'-' || ((*s).word)[splitind-1] == L'-' ||
		    ((*s).word)[splitind]   == L'-' || ((*s).word)[splitind+1] == L'-') continue;
		part1[splitind] = L'\0';
		part1_res = voikko_spell_with_priority(part1, splitind, &prio_total);
		charge(s);
		if (part1_res == SPELL_OK || part1_res == SPELL_CAP_FIRST) {
			part2_res = voikko_spell_with_priority((*s).word + splitind, (*s).wlen - splitind, &prio_part);
			prio_total += prio_part;
			charge(s);
			if (part2_res == SPELL_OK || part2_res == SPELL_CAP_FIRST) {
				suggestion = malloc(((*s).wlen + 2) * sizeof(wchar_t));
				if (suggestion == 0) break;
				wcsncpy(suggestion, (*s).word, splitind);
				if (part1_res == SPELL_CAP_FIRST)
					suggestion[0] = towupper(suggestion[0]);
				suggestion[splitind] = L' ';
				wcsncpy(suggestion + (splitind + 1), (*s).word + splitind,
				        (*s).wlen - splitind + 1);
				if (part2_res == SPELL_CAP_FIRST)
					suggestion[splitind+1] = towupper(suggestion[splitind+1]);
				*((*s).suggestions) = suggestion;
				*((*s).prios) = prio_total;
				((*s).suggestions)++;
				((*s).prios)++;
				((*s).max_suggestions)--;
			}
		}
		if (abort_search(s)) break;
	}

	free(part1);
}

/* ä=\u00e4, ö=\u00f6, å=\u00e5, š=\u0161, ž=\u017e, é=\u00e9, â=\u00e2 */


const wchar_t * STD_REPL_ORIG =
	L"aiittes"  L"snulkko\u00e4mrrvppyhjjddd\u00f6gggffbbccwwxz"  L"zq\u00e5\u00e5\u00e5\u00e5aitesnul"
	L"ko\u00e4mrvpyhjd\u00f6gfbcwxzq\u00e5a"  L"e"  L"a"  L"ks";
const wchar_t * STD_REPL_REPL = 
	L"suordr\u0161amiklgi\u00f6netbbotjhktsf\u00e4fhkgdpnvsevc\u017exao"  L"p"  L"\u00e4\u00f6ekysdhj\u00f6"
	L"jpp"  L"kdglhuiel"  L"tvvkasaka"  L"\u00e5\u00e9\u00e2cc";

const wchar_t * OCR_REPL_ORIG =
	L"0liuoa"  L"\u00e4o"  L"\u00f6s"  L"\u0161z"  L"\u017ee"  L"\u00e9a"  L"\u00e2pbefqonmuvocbh";
const wchar_t * OCR_REPL_REPL =
	L"oilou\u00e4a"  L"\u00f6o"  L"\u0161s"  L"\u017ez"  L"\u00e9e"  L"\u00e2a"  L"bpfeoqmnvucohb";

void voikko_suggest_replacement(voikko_sugg_status_t * s, const wchar_t * from, const wchar_t * to, int count) {
	int i;
	wchar_t * pos;
	wchar_t * buffer = malloc(((*s).wlen + 1) * sizeof(wchar_t));
	if (buffer == 0) return;
	wcsncpy(buffer, (*s).word, (*s).wlen);
	buffer[(*s).wlen] = L'\0';
	for (i = 0; i < count; i++) {
		for (pos = wcschr(buffer, from[i]); pos != 0; pos = wcschr(pos+1, from[i])) {
			*pos = to[i];
			voikko_suggest_correct_case(s, buffer, (*s).wlen);
			if (abort_search(s)) break;
			*pos = from[i];
		}
		if (abort_search(s)) break;
		for (pos = wcschr(buffer, towupper(from[i])); pos != 0;
		     pos = wcschr(pos + 1, towupper(from[i]))) {
			*pos = towupper(to[i]);
			voikko_suggest_correct_case(s, buffer, (*s).wlen);
			if (abort_search(s)) break;
			*pos = towupper(from[i]);
		}
		if (abort_search(s)) break;
	}
	free(buffer);
}

void voikko_suggest_deletion(voikko_sugg_status_t * s) {
	size_t i;
	wchar_t * buffer = malloc((*s).wlen * sizeof(wchar_t));
	if (buffer == 0) return;
	for (i = 0; i < (*s).wlen && !abort_search(s); i++) {
		if (i == 0 || towlower(((*s).word)[i]) != towlower(((*s).word)[i-1])) {
			wcsncpy(buffer, (*s).word, i);
			wcsncpy(buffer + i, (*s).word + (i + 1), (*s).wlen - i);
			voikko_suggest_correct_case(s, buffer, (*s).wlen - 1);
		}
	}
	free(buffer);
}

void voikko_suggest_insert_special(voikko_sugg_status_t * s) {
	size_t j;
	wchar_t * buffer = malloc(((*s).wlen + 2) * sizeof(wchar_t));
	if (buffer == 0) return;
	wcsncpy(buffer + 1, (*s).word, (*s).wlen);
	buffer[(*s).wlen+1] = L'\0';
	
	/* suggest adding '-' */
	for (j = 2; j <= (*s).wlen - 2 && !abort_search(s); j++) {
		/* Do not add hyphen if there already is another nearby */
		if (((*s).word)[j-2] == L'-' || ((*s).word)[j-1] == L'-' ||
		    ((*s).word)[j] ==   L'-' || ((*s).word)[j+1] == L'-')
			continue;
		wcsncpy(buffer, (*s).word, j);
		buffer[j] = L'-';
		voikko_suggest_correct_case(s, buffer, (*s).wlen + 1);
	}
	/* suggest character duplication */
	wcsncpy(buffer + 1, (*s).word, (*s).wlen + 1);
	for (j = 0; j < (*s).wlen && !abort_search(s); j++) {
		buffer[j] = ((*s).word)[j];
		/* Do not duplicate if there already are two same letters */
		if (j < (*s).wlen - 1 && ((*s).word)[j] == ((*s).word)[j+1]) {
			j++;
			continue;
		}
		/* These should not be duplicated */
		if (((*s).word)[j] == L'-' || ((*s).word)[j] == L'\'') continue;
		voikko_suggest_correct_case(s, buffer, (*s).wlen + 1);
	}
	free(buffer);
}

const wchar_t * INS_CHARS = L"aitesnulko\u00e4mrvpyhjd\u00f6gfbcw:xzq\u00e5";

void voikko_suggest_insertion(voikko_sugg_status_t * s, int start, int end) {
	int i;
	size_t j;
	wchar_t * buffer = malloc(((*s).wlen + 2) * sizeof(wchar_t));
	if (buffer == 0) return;
	for (i = start; i <= end; i++) {
		buffer[0] = ((*s).word)[0];
		wcsncpy(buffer + 1, (*s).word, (*s).wlen);
		buffer[(*s).wlen+1] = L'\0';
		for (j = 0; j < (*s).wlen && !abort_search(s); j++) {
			if (j != 0) buffer[j-1] = ((*s).word)[j-1];
			if (INS_CHARS[i] == towlower(((*s).word[j]))) continue; /* avoid duplicates */
			if (j > 0 && INS_CHARS[i] == towlower(((*s).word[j-1]))) continue; /* avoid duplicates */
			buffer[j] = INS_CHARS[i];
			voikko_suggest_correct_case(s, buffer, (*s).wlen + 1);
		}
		if (abort_search(s)) break;
		if (INS_CHARS[i] == ((*s).word)[(*s).wlen-1]) continue;
		buffer[(*s).wlen-1] = ((*s).word)[(*s).wlen-1];
		buffer[(*s).wlen] = INS_CHARS[i];
		voikko_suggest_correct_case(s, buffer, (*s).wlen + 1);
	}
	free(buffer);
}

void voikko_suggest_swap(voikko_sugg_status_t * s) {
	size_t max_distance;
	size_t i;
	size_t j;
	int k;
	wchar_t * buffer;
	if ((*s).wlen <= 8) max_distance = 10;
	else max_distance = 50 / (*s).wlen;
	if (max_distance == 0) return;
	buffer = malloc(((*s).wlen + 1) * sizeof(wchar_t));
	if (buffer == 0) return;
	wcsncpy(buffer, (*s).word, (*s).wlen);
	buffer[(*s).wlen] = L'\0';
	for (i = 0; i < (*s).wlen && !abort_search(s); i++) {
		for (j = i + 1; j < (*s).wlen && !abort_search(s); j++) {
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
			buffer[i] = ((*s).word)[j];
			buffer[j] = ((*s).word)[i];
			voikko_suggest_correct_case(s, buffer, (*s).wlen);
			buffer[i] = ((*s).word)[i];
			buffer[j] = ((*s).word)[j];
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
	voikko_sugg_status_t status;
	wchar_t ** suggestions;
	int * prios;
	wchar_t * nword;
	size_t wlen;
	int add_dots;
	 
	add_dots = 0;
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
	
	status.suggestions = suggestions;
	status.prios = prios;
	status.max_suggestions = MAX_SUGGESTIONS * 3;
	status.max_cost = COST_LIMIT;
	if (voikko_options.suggestion_type == ST_OCR) status.max_cost = status.max_cost * 3;
	status.handle = handle;
	status.word = nword;
	status.wlen = wlen;
	
	voikko_suggest_correct_case(&status, 0, 0);
	if (status.max_suggestions != MAX_SUGGESTIONS * 3) {
		free(prios);
		free(nword);
		if (add_dots) voikko_suggest_add_dots(suggestions);
		return suggestions;
	}
	if (voikko_options.suggestion_type == ST_OCR) {
		voikko_suggest_replacement(&status, OCR_REPL_ORIG, OCR_REPL_REPL, 31);
	}
	else {
		voikko_suggest_vowel_change(&status);
		if (!abort_search(&status)) voikko_suggest_replacement(&status, STD_REPL_ORIG, STD_REPL_REPL, 51);
		if (!abort_search(&status)) voikko_suggest_deletion(&status);
		if (!abort_search(&status)) voikko_suggest_insert_special(&status);
		if (!abort_search(&status)) voikko_suggest_word_split(&status);
		if (!abort_search(&status)) voikko_suggest_insertion(&status, 0, 5);
		if (!abort_search(&status)) voikko_suggest_swap(&status);
		if (!abort_search(&status)) voikko_suggest_replacement(&status, STD_REPL_ORIG + 51, STD_REPL_REPL + 51, 30);
		if (!abort_search(&status)) voikko_suggest_insertion(&status, 6, 29);
	}

	if (status.max_suggestions == MAX_SUGGESTIONS * 3) {
		free(suggestions);
		free(prios);
		free(nword);
		return 0;
	}
	
	/* Sort the suggestions by priority using insertion sort */
	int i, j;
	wchar_t * current_sugg;
	int current_prio;
	for (i = 0; suggestions + i < status.suggestions; i++) {
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
	word_ucs4 = voikko_cstrtoucs4(word, voikko_options.encoding, 0);
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
		suggestion = voikko_ucs4tocstr(suggestions_ucs4[i], voikko_options.encoding, 0);
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
