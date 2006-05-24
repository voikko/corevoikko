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
#include "voikko_hyphenate.h"
#include "voikko_setup.h"
#include "voikko_utils.h"
#include <wchar.h>
#include <stdlib.h>
#include <wctype.h>
#include <string.h>

const wchar_t * SPLIT_VOWELS[] = { L"ae", L"ao", L"ea", L"eo", L"ia", L"io", L"oa", L"oe",
                                   L"ua", L"ue", L"ye", L"e\u00e4", L"e\u00f6", L"i\u00e4",
                                   L"i\u00f6", L"y\u00e4", L"\u00e4e", L"\u00f6e" };
const wchar_t * LONG_CONSONANTS[] = { L"shtsh", L"\u0161t\u0161", L"tsh", L"t\u0161", L"zh" };

void voikko_simple_hyphenation(const wchar_t * word, char * hyphenation_points, size_t nchars) {
	wchar_t * word_copy;
	size_t i;
	size_t j;
	size_t k;
	
	if (nchars <= 1) return;
	
	/* TODO: the following is not enough if we later want to prevent hyphenation at single
	 * points, not only in whole word segments. */
	if (hyphenation_points[0] == 'X') return;
	
	word_copy = malloc((nchars + 1) * sizeof(wchar_t));
	for (i = 0; i < nchars; i++) {
		word_copy[i] = towlower(word[i]);
	}
	word_copy[nchars] = '\0';
	
	/* at least one vowel is required before the first hyphen */
	i = 0;
	while (word_copy[i] != L'\0' && wcschr(VOIKKO_CONSONANTS, word_copy[i])) i++;
	
	/* -CV */
	for (; i <= nchars - 2; i++) {
		if (wcschr(VOIKKO_CONSONANTS, word_copy[i]) && wcschr(VOIKKO_VOWELS, word_copy[i+1]))
			hyphenation_points[i] = '-';
	}
	
	/* 'V */
	for (i = 1; i < nchars - 1; i++) {
		if (word_copy[i] == L'\'' && wcschr(VOIKKO_VOWELS, word_copy[i+1]))
			hyphenation_points[i] = '=';
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
	
	/* long consonants */
	for (i = 1; i < nchars - 1; i++) {
		for (j = 0; j < 5; j++) {
			if (i + wcslen(LONG_CONSONANTS[j]) < nchars &&
			    wcsncmp(&word_copy[i], LONG_CONSONANTS[j], wcslen(LONG_CONSONANTS[j])) == 0) {
				for (k = i + 1; k <= i + wcslen(LONG_CONSONANTS[j]); k++) {
					if (hyphenation_points[k] == '-') {
						hyphenation_points[k] = ' ';
						hyphenation_points[i] = '-';
					}
				}
			}
		}
	}
	
	free(word_copy);
	
	if (voikko_options.no_ugly_hyphenation) {
		hyphenation_points[1] = ' ';
		hyphenation_points[nchars-1] = ' ';
	}
}

void voikko_interpret_analysis(value_t analysis, char * buffer, size_t len) {
	const char * analysis_string;
	const char * analysis_ptr;
	size_t i;
	analysis_string = get_value_string(analysis);
	memset(buffer, ' ', len);
	analysis_ptr = analysis_string;
	if (*analysis_ptr == '=') analysis_ptr++;
	for (i = 0; i < len; i++) {
		if (analysis_ptr[0] == '\0') break;
		if (analysis_ptr[0] == '-' && analysis_ptr[1] == '=') {
			buffer[i] = '=';
			analysis_ptr += 2;
			continue;
		}
		if (analysis_ptr[0] == '=') {
			buffer[i] = '-';
			analysis_ptr += 2;
			continue;
		}
		if (analysis_ptr[0] == 'j' || analysis_ptr[0] == 'q') buffer[i] = 'X';
		analysis_ptr++;
	}
	free((char *) analysis_string);
}

char ** voikko_split_compounds(const wchar_t * word) {
	char * word_utf8;
	value_t analysis_result;
	int analysis_count;
	char ** all_results;
	char * result;
	size_t word_len;
	size_t i;
	all_results = malloc((LIBVOIKKO_MAX_ANALYSIS_COUNT + 1) * sizeof(char *));
	word_len = wcslen(word);
	all_results[LIBVOIKKO_MAX_ANALYSIS_COUNT] = 0;
	word_utf8 = voikko_ucs4tocstr(word, "UTF-8");
	
	analyse_item(word_utf8, MORPHOLOGY);
	analysis_count = 0;
	analysis_result = first_analysis_result();
	while (analysis_result) {
		result = malloc(word_len + 1);
		result[word_len] = '\0';
		voikko_interpret_analysis(analysis_result, result, word_len);
		all_results[analysis_count] = result;
		if (++analysis_count == LIBVOIKKO_MAX_ANALYSIS_COUNT) break;
		analysis_result = next_analysis_result();
	}
	if (analysis_count == 0) {
		result = malloc(word_len + 1);
		memset(result, ' ', word_len);
		for (i = 0; i < word_len; i++)
			if (word[i] == L'-') result[i] = '=';
		result[word_len] = '\0';
		all_results[0] = result;
		analysis_count++;
	}
	all_results[analysis_count] = 0;
	free(word_utf8);

	voikko_remove_extra_hyphenations(all_results, word_len, voikko_options.intersect_compound_level);

	return all_results;
}

char * voikko_intersect_hyphenations(char ** hyphenations) {
	size_t len;
	size_t i;
	char * intersection;
	char ** current_ptr;
	len = strlen(hyphenations[0]);
	intersection = malloc(len + 1);
	strcpy(intersection, hyphenations[0]);
	for (i = 0; i < len; i++) if (intersection[i] == 'X') intersection[i] = ' ';
	current_ptr = &hyphenations[1];
	while (*current_ptr != 0) {
		for (i = 0; i < len; i++) {
			if ((*current_ptr)[i] == ' ' || (*current_ptr)[i] == 'X')
				intersection[i] = ' ';
		}
		current_ptr++;
	}
	return intersection;
}

void voikko_compound_hyphenation(const wchar_t * word, char * hyphenation) {
	size_t len;
	size_t start;
	size_t end;
	len = wcslen(word);
	start = 0;
	while (start < len && hyphenation[start] == '=') start++;
	end = start + 1;
	while (end < len) {
		if (hyphenation[end] != ' ' && hyphenation[end] != 'X') {
			voikko_simple_hyphenation(&word[start], &hyphenation[start], end-start);
			if (hyphenation[end] == '=') start = end + 1;
			else start = end;
			end = start + 1;
		}
		else end++;
	}
	if (end == len && start < end)
		voikko_simple_hyphenation(&word[start], &hyphenation[start], end-start);
}

void voikko_remove_extra_hyphenations(char ** hyphenations, size_t len, int intersect_compound_level) {
	int min_parts = 0;
	int current_parts;
	size_t i;
	int hyphenation_count = 0;
	int j = 0;
	char ** current_buffer = hyphenations;
	while (*current_buffer != 0) {
		hyphenation_count++;
		current_parts = 1;
		for (i = 0; i < len; i++)
			if ((*current_buffer)[i] != ' ' && (*current_buffer)[i] != 'X') current_parts++;
		if (min_parts == 0 || min_parts > current_parts) min_parts = current_parts;
		current_buffer++;
	}
	if (min_parts > intersect_compound_level) return; /* nothing to do */
	/* delete items from array where current_parts > intersect_compound_level */
	while (j < hyphenation_count) {
		current_buffer = hyphenations + j;
		current_parts = 1;
		for (i = 0; i < len; i++)
			if ((*current_buffer)[i] != ' ' && (*current_buffer)[i] != 'X') current_parts++;
		if (current_parts > intersect_compound_level) {
			free(hyphenations[j]);
			hyphenations[j] = hyphenations[--hyphenation_count];
			hyphenations[hyphenation_count] = 0;
		}
		else j++;
	}
	/* TODO: remove indentically split words */
}

char * voikko_hyphenate_ucs4(int handle, const wchar_t * word) {
	char ** hyphenations;
	char * hyphenation;
	int i;
	hyphenations = voikko_split_compounds(word);
	/*i=0; while (hyphenations[i] != 0) printf("hyph='%s'\n", hyphenations[i++]);*/
	i = 0;
	while (hyphenations[i] != 0) voikko_compound_hyphenation(word, hyphenations[i++]);
	/*i=0; while (hyphenations[i] != 0) printf("hyph='%s'\n", hyphenations[i++]);*/
	hyphenation = voikko_intersect_hyphenations(hyphenations);
	i = 0;
	while (hyphenations[i] != 0) free(hyphenations[i++]);
	free(hyphenations);
	/*printf("hyphenation = '%s'\n", hyphenation);*/
	return hyphenation;
}

char * voikko_hyphenate_cstr(int handle, const char * word) {
	wchar_t * word_ucs4;
	char * result;
	if (word == 0) return 0;
	word_ucs4 = voikko_cstrtoucs4(word, voikko_options.encoding);
	if (word_ucs4 == 0) return 0;
	result = voikko_hyphenate_ucs4(handle, word_ucs4);
	free(word_ucs4);
	return result;
}
