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
#include <string.h>

const wchar_t * SPLIT_VOWELS[] = { L"ae", L"ao", L"ea", L"eo", L"ia", L"io", L"oa", L"oe",
                                   L"ua", L"ue", L"ye", L"e\u00e4", L"e\u00f6", L"i\u00e4",
                                   L"i\u00f6", L"y\u00e4", L"\u00e4e", L"\u00f6e" };

void voikko_simple_hyphenation(const wchar_t * word, char * hyphenation_points, int nchars) {
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

void voikko_interpret_analysis(value_t analysis, char * buffer, int len) {
	char * analysis_string;
	wchar_t * analysis_w;
	wchar_t * analysis_ptr;
	int i;
	analysis_string = get_value_string(analysis);
	analysis_w = voikko_cstrtoucs4(analysis_string, "UTF-8");
	memset(buffer, ' ', len);
	analysis_ptr = analysis_w;
	if (*analysis_ptr == '=') analysis_ptr++;
	for (i = 0; i < len; i++) {
		if (analysis_ptr[0] == L'\0') break;
		if (analysis_ptr[0] == L'-' && analysis_ptr[1] == L'=') {
			buffer[i] = '=';
			analysis_ptr += 2;
			continue;
		}
		if (analysis_ptr[0] == L'=') {
			buffer[i] = '-';
			analysis_ptr += 2;
			continue;
		}
		analysis_ptr++;
	}
	free(analysis_w);
	free(analysis_string);
}

char ** voikko_split_compounds(const wchar_t * word) {
	char * word_utf8;
	value_t analysis_result;
	int analysis_count;
	char ** all_results;
	char * result;
	int word_len;
	int i;
	int j;
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

	/* Check if there is a non-compound alternative
	 * => that will override all other combinations */
	for (i = 0; all_results[i] != 0; i++) {
		if (!strchr(all_results[i], '-') && !strchr(all_results[i], '=')) {
			for (j = 0; all_results[j] != 0; j++)
				if (j != i) free(all_results[j]);
			all_results[0] = all_results[i];
			all_results[1] = 0;
			break;
		}
	}

	return all_results;
}

char * voikko_intersect_hyphenations(char ** hyphenations) {
	int len;
	int i;
	char * intersection;
	char ** current_ptr;
	len = strlen(hyphenations[0]);
	intersection = malloc(len + 1);
	strcpy(intersection, hyphenations[0]);
	current_ptr = &hyphenations[1];
	while (*current_ptr != 0) {
		for (i = 0; i < len; i++) {
			if ((*current_ptr)[i] == ' ') intersection[i] = ' ';
		}
		current_ptr++;
	}
	return intersection;
}

void voikko_compound_hyphenation(const wchar_t * word, char * hyphenation) {
	int len;
	int start;
	int end;
	len = wcslen(word);
	start = 0;
	while (start < len) {
		for (end = start; end < len; end++) {
			if (hyphenation[end] == '-') {
				if (start == end) {
					start++;
					break;
				}
				voikko_simple_hyphenation(&word[start], &hyphenation[start], end-start);
				start = end;
				break;
			}
			if (hyphenation[end] == '=') {
				if (start == end) {
					start++;
					break;
				}
				voikko_simple_hyphenation(&word[start], &hyphenation[start], end-start);
				start = end + 1;
				break;
			}
		}
		if (start < len && end == len) {
			voikko_simple_hyphenation(&word[start], &hyphenation[start], end-start);
			break;
		}
	}
}

char * voikko_hyphenate_ucs4(const wchar_t * word) {
	char ** hyphenations;
	char * hyphenation;
	int i;
	hyphenations = voikko_split_compounds(word);
	/*i=0; while (hyphenations[i] != 0) printf("hyph='%s'\n", hyphenations[i++]);*/
	i = 0;
	while (hyphenations[i] != 0) voikko_compound_hyphenation(word, hyphenations[i++]);
	hyphenation = voikko_intersect_hyphenations(hyphenations);
	i = 0;
	while (hyphenations[i] != 0) free(hyphenations[i++]);
	free(hyphenations);
	/*printf("hyphenation = '%s'\n", hyphenation);*/
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
