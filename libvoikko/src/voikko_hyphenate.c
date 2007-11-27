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


int voikko_is_good_hyphen_position(const wchar_t * word, const char * hyphenation_points,
                                      size_t new_hyphen_pos, size_t nchars) {
	int has_vowel;
	size_t i;
	
	// Check for out of bounds hyphen
	if (new_hyphen_pos == 0 || new_hyphen_pos + 1 >= nchars) return 0;
	
	has_vowel = 0;
	for (i = new_hyphen_pos - 1; hyphenation_points[i] != '-' && hyphenation_points[i] != '='; i--) {
		if (i == 0) break;
		if (wcschr(VOIKKO_VOWELS, word[i])) has_vowel = 1;
	}
	if (has_vowel == 0) return 0;
	
	has_vowel = 0;
	for (i = new_hyphen_pos; i < nchars &&
	     hyphenation_points[i] != '-' && hyphenation_points[i] != '='; i++) {
		if (wcschr(VOIKKO_VOWELS, word[i])) has_vowel = 1;
	}
	if (has_vowel == 0) return 0;
	
	return 1;
}

int voikko_allow_rule_hyphenation(const wchar_t * word, size_t nchars) {
	// Word is too short
	if (nchars <= 1) return 0;
	
	if (voikko_options.no_ugly_hyphenation) {
		// non-word strings are not hyphenated
		if (voikko_is_nonword(word, nchars)) return 0;
	
		// Word ends with number (not safe to be hyphenated)
		if (word[nchars - 1] >= L'0' && word[nchars - 1] <= L'9') return 0;
	}
	
	return 1;
}

const wchar_t * SPLIT_VOWELS[] = { L"ae", L"ao", L"ea", L"eo", L"ia", L"io", L"oa", L"oe",
                                   L"ua", L"ue", L"ye", L"e\u00e4", L"e\u00f6", L"i\u00e4",
                                   L"i\u00f6", L"y\u00e4", L"\u00e4e", L"\u00f6e" };
const wchar_t * LONG_CONSONANTS[] = { L"shtsh", L"\u0161t\u0161", L"tsh", L"t\u0161", L"zh" };
const wchar_t * SPLIT_AFTER[] = { L"ie", L"ai" };

void voikko_rule_hyphenation(const wchar_t * word, char * hyphenation_points, size_t nchars) {
	wchar_t * word_copy;
	size_t i;
	size_t j;
	size_t k;
	
	if (!voikko_allow_rule_hyphenation(word, nchars)) return;
	
	/* TODO: the following is not enough if we later want to prevent hyphenation at single
	 * points, not only in whole word segments. */
	if (hyphenation_points[0] == 'X') return;
	
	word_copy = malloc((nchars + 1) * sizeof(wchar_t));
	if (word_copy == 0) return;
	
	for (i = 0; i < nchars; i++) {
		word_copy[i] = towlower(word[i]);
	}
	word_copy[nchars] = '\0';
	
	/* at least one vowel is required before the first hyphen */
	i = 0;
	while (word_copy[i] != L'\0' && wcschr(VOIKKO_CONSONANTS, word_copy[i])) i++;
	
	/* -CV (not after special characters, hyphenating "vast'edes" as "vast'e-des" is ugly) */
	for (; i <= nchars - 2; i++) {
		if (wcschr(VOIKKO_CONSONANTS, word_copy[i]) && wcschr(VOIKKO_VOWELS, word_copy[i+1])
		    && !wcschr(L"/.:&%\'", word_copy[i-1])
		    && (i <= 1 || !voikko_options.no_ugly_hyphenation || word_copy[i-2] != L'\''))
			hyphenation_points[i] = '-';
	}
	
	/* 'V */
	for (i = 1; i < nchars - 1; i++) {
		if (word_copy[i] == L'\'' && wcschr(VOIKKO_VOWELS, word_copy[i+1]))
			hyphenation_points[i] = '=';
	}
	
	/* Split before and after long vowels */
	for (i = 0; i < nchars - 1; i++) {
		if (wcschr(VOIKKO_VOWELS, word_copy[i]) && word_copy[i] == word_copy[i+1]) {
			if (voikko_is_good_hyphen_position(word_copy, hyphenation_points, i, nchars))
				hyphenation_points[i] = '-';
			if (voikko_is_good_hyphen_position(word_copy, hyphenation_points, i+2, nchars))
				hyphenation_points[i+2] = '-';
		}
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
	
	if (voikko_options.no_ugly_hyphenation) {
		hyphenation_points[1] = ' ';
		hyphenation_points[nchars-1] = ' ';
		for (i = 0; i <= nchars - 2; i++) {
			if (wcschr(VOIKKO_VOWELS, word_copy[i]) && wcschr(VOIKKO_VOWELS, word_copy[i+1]))
				hyphenation_points[i+1] = ' ';
		}
	}
	else if (nchars >= 3) {
		/* VV-V */
		for (i = 0; i < nchars - 3; i++) {
			for (j = 0; j < 2; j++) {
				if (hyphenation_points[i+1] != '-' &&
				    wcsncmp(word_copy + i,  SPLIT_AFTER[j], 2) == 0 &&
				    wcschr(VOIKKO_VOWELS, word_copy[i+2]) &&
				    voikko_is_good_hyphen_position(word_copy, hyphenation_points, i+2, nchars)) {
					hyphenation_points[i+2] = '-';
				}
			}
		}
	}
	
	free(word_copy);
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

char ** voikko_split_compounds(const wchar_t * word, size_t len, int * dot_removed) {
	char * word_utf8;
	value_t analysis_result;
	int analysis_count;
	char ** all_results;
	char * result;
	size_t utf8_len;
	size_t i;
	
	all_results = malloc((LIBVOIKKO_MAX_ANALYSIS_COUNT + 1) * sizeof(char *));
	if (all_results == 0) return 0;
	all_results[LIBVOIKKO_MAX_ANALYSIS_COUNT] = 0;
	word_utf8 = voikko_ucs4tocstr(word, "UTF-8", len);
	if (word_utf8 == 0) {
		free(all_results);
		return 0;
	}
	utf8_len = strlen(word_utf8);
	
	analyse_item(word_utf8, MORPHOLOGY);
	analysis_count = 0;
	analysis_result = first_analysis_result();
	
	/** We may have to remove the trailing dot before hyphenation */
	if (!analysis_result && voikko_options.ignore_dot && len > 1 &&
	    word_utf8[utf8_len - 1] == '.') {
		word_utf8[utf8_len - 1] = '\0';
		utf8_len--;
		*dot_removed = 1;
		analyse_item(word_utf8, MORPHOLOGY);
		analysis_result = first_analysis_result();
	}
	else *dot_removed = 0;
	
	/** Iterate over all analyses and add results to all_results */
	while (analysis_result) {
		result = malloc(len + 1);
		if (result == 0) break;
		result[len] = '\0';
		voikko_interpret_analysis(analysis_result, result, len - *dot_removed);
		if (*dot_removed) result[len - 1] = ' ';
		all_results[analysis_count] = result;
		if (++analysis_count == LIBVOIKKO_MAX_ANALYSIS_COUNT) break;
		analysis_result = next_analysis_result();
	}
	
	/** If the word could not be parsed, assume that it does not contain any
	    morpheme borders that we should know about (unless there is a hyphen,
	    which tells us where the border is). If the entire word seems impossible
	    to hyphenate, do not split it. */
	if (analysis_count == 0) {
		result = malloc(len + 1);
		if (result == 0) {
			free(all_results);
			return 0;
		}
		memset(result, ' ', len);
		if (voikko_allow_rule_hyphenation(word, len)) {
			for (i = 0; i < len; i++)
				if (word[i] == L'-') result[i] = '=';
		}
		result[len] = '\0';
		all_results[0] = result;
		analysis_count++;
	}
	all_results[analysis_count] = 0;
	free(word_utf8);

	voikko_remove_extra_hyphenations(all_results, len, voikko_options.intersect_compound_level);

	return all_results;
}

char * voikko_intersect_hyphenations(char ** hyphenations) {
	size_t len;
	size_t i;
	char * intersection;
	char ** current_ptr;
	len = strlen(hyphenations[0]);
	intersection = malloc(len + 1);
	if (intersection == 0) return 0;

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

void voikko_compound_hyphenation(const wchar_t * word, char * hyphenation, size_t len) {
	size_t start;
	size_t end;
	start = 0;
	while (start < len && hyphenation[start] == '=') start++;
	end = start + 1;
	while (end < len) {
		if (hyphenation[end] != ' ' && hyphenation[end] != 'X') {
			if (end - start >= voikko_options.min_hyphenated_word_length)
				voikko_rule_hyphenation(&word[start], &hyphenation[start], end-start);
			if (hyphenation[end] == '=') start = end + 1;
			else start = end;
			end = start + 1;
		}
		else end++;
	}
	if (end == len && start < end && end - start >= voikko_options.min_hyphenated_word_length)
		voikko_rule_hyphenation(&word[start], &hyphenation[start], end-start);
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
	/* delete items from array where current_parts > min_parts */
	while (j < hyphenation_count) {
		current_buffer = hyphenations + j;
		current_parts = 1;
		for (i = 0; i < len; i++)
			if ((*current_buffer)[i] != ' ' && (*current_buffer)[i] != 'X') current_parts++;
		if (current_parts > min_parts) {
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
	int dot_removed;
	size_t wlen;
	
	if (word == 0) return 0;
	wlen = wcslen(word);
	
	/* Short words may not need to be hyphenated at all */
	if (wlen < voikko_options.min_hyphenated_word_length) {
		hyphenation = malloc(wlen + 1);
		if (!hyphenation) return 0;
		memset(hyphenation, ' ', wlen);
		hyphenation[wlen] = '\0';
		return hyphenation;
	}
	
	ENTER_V
	hyphenations = voikko_split_compounds(word, wlen, &dot_removed);
	if (hyphenations == 0) {
		EXIT_V
		return 0;
	}
	
	/*i=0; while (hyphenations[i] != 0) printf("hyph='%s'\n", hyphenations[i++]);*/
	i = 0;
	while (hyphenations[i] != 0)
		voikko_compound_hyphenation(word, hyphenations[i++], wlen - dot_removed);
	/*i=0; while (hyphenations[i] != 0) printf("hyph='%s'\n", hyphenations[i++]);*/
	hyphenation = voikko_intersect_hyphenations(hyphenations);
	if (hyphenation == 0) {
		free(hyphenations);
		EXIT_V
		return 0;
	}

	i = 0;
	while (hyphenations[i] != 0) free(hyphenations[i++]);
	free(hyphenations);
	/*printf("hyphenation = '%s'\n", hyphenation);*/
	EXIT_V
	return hyphenation;
}

char * voikko_hyphenate_cstr(int handle, const char * word) {
	wchar_t * word_ucs4;
	char * result;
	if (word == 0) return 0;
	word_ucs4 = voikko_cstrtoucs4(word, voikko_options.encoding, 0);
	if (word_ucs4 == 0) return 0;
	result = voikko_hyphenate_ucs4(handle, word_ucs4);
	free(word_ucs4);
	return result;
}
