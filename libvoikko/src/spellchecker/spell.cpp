/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2006 - 2008 Harri Pitkänen <hatapitk@iki.fi>
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
#include "character/charset.hpp"
#include "spellchecker/spell.hpp"
#include <cstdlib>
#include <cstring>
#include <malaga.h>
#include <wchar.h>
#include <wctype.h>

namespace libvoikko {

enum spellresult voikko_match_word_and_analysis(const wchar_t * word, size_t len, const char * analysis_str) {
	size_t i, j;
	char captype; /* 'i' = uppercase letter, 'p' = lowercase letter, 'v' = punctuation */
	enum spellresult result = SPELL_OK;
	j = 0;
	for (i = 0; i < len; i++) {
		while (analysis_str[j] == '=') j++;
		if (analysis_str[j] == '\0') break;
		
		if (iswupper(word[i])) captype = 'i';
		else if (iswlower(word[i])) captype = 'p';
		else captype = 'v';
		
		if (captype == 'p' && (analysis_str[j] == 'i' || analysis_str[j] == 'j')) {
			if (i == 0) result = SPELL_CAP_FIRST;
			else result = SPELL_CAP_ERROR;
		}
		if (captype == 'i' && (analysis_str[j] == 'p' || analysis_str[j] == 'q')) {
			result = SPELL_CAP_ERROR;
		}
		if (result == SPELL_CAP_ERROR) break;
		j++;
	}
	return result;
}

enum spellresult voikko_spell_with_priority(const wchar_t * word, size_t len, int * prio) {
	enum spellresult result;
	enum spellresult best_result;
	size_t j;
	value_t analysis;
	char * analysis_str;

	char * malaga_buffer = voikko_ucs4tocstr(word, "UTF-8", len);
	if (malaga_buffer == 0) return SPELL_FAILED;
	analyse_item(malaga_buffer, MORPHOLOGY);
	delete[] malaga_buffer;
	if (prio != 0) *prio = 0;
	
	analysis = first_analysis_result();
	if (!analysis) return SPELL_FAILED;
	
	best_result = SPELL_FAILED;
	do {
		analysis_str = get_value_string(analysis);
		result = voikko_match_word_and_analysis(word, len, analysis_str);
		if (best_result == SPELL_FAILED || best_result > result) {
			best_result = result;
			if (prio != 0) {
				*prio = 0;
				for (j = 0; analysis_str[j] != '\0'; j++)
					if (analysis_str[j] == '=') (*prio)++;
			}
		}
		free(analysis_str);
		if (best_result == SPELL_OK) break;
		analysis = next_analysis_result();
	} while (analysis);
	
	if (prio != 0) {
		if (best_result == SPELL_CAP_FIRST) (*prio) += 1;
		else if (best_result == SPELL_CAP_ERROR) (*prio) += 2;
	}
	return best_result;
}

enum spellresult voikko_do_spell(const wchar_t * word, size_t len) {
	wchar_t * hyphen_pos;
	size_t leading_len;
	char * malaga_buffer;
	value_t current_analysis;
	char * analysis_str;
	enum spellresult result_with_border = SPELL_FAILED;
	enum spellresult result_without_border = SPELL_FAILED;
	enum spellresult spres;
	size_t i, j;
	
	enum spellresult result = voikko_spell_with_priority(word, len, 0);
	if (result != SPELL_OK && len > 3) hyphen_pos = wmemchr(word + 1, L'-', len - 2);
	else hyphen_pos = 0;
	
	if (hyphen_pos) { /* Check optional hyphens */
		leading_len = hyphen_pos - word;
		wchar_t * buffer = new wchar_t[len];
		if (!buffer) return result;
		wcsncpy(buffer, word, leading_len);
		wcsncpy(buffer + leading_len, hyphen_pos + 1, len - leading_len - 1);
		buffer[len - 1] = L'\0';
		
		if (voikko_options.accept_extra_hyphens && leading_len > 1 &&
		    buffer[leading_len] != L'-') {
			/* All hyphens are optional */
			/* FIXME: deep recursion */
			spres = voikko_do_spell(buffer, len - 1);
			if (spres == SPELL_OK) {
				delete[] buffer;
				return spres;
			}
		}
		
		/* Leading part ends with the same VC pair as the trailing part starts ('pop-opisto') */
		if (leading_len >= 2 && len - leading_len >= 3) {
			wint_t vctest1 = towlower(word[leading_len - 2]);
			wint_t vctest2 = towlower(word[leading_len - 1]);
			if (wcschr(VOIKKO_VOWELS, vctest1) &&
			    wcschr(VOIKKO_CONSONANTS, vctest2) &&
			    towlower(word[leading_len + 1]) == vctest1 &&
			    towlower(word[leading_len + 2]) == vctest2) {
				spres = voikko_spell_with_priority(buffer, len - 1, 0);
				if (result == SPELL_FAILED || result > spres) {
					delete[] buffer;
					return spres;
				}
			}
		}
		
		/* Ambiguous compound ('syy-silta', 'syys-ilta') */
		malaga_buffer = voikko_ucs4tocstr(buffer, "UTF-8", 0);
		if (!malaga_buffer) {
			delete[] buffer;
			return result;
		}
		analyse_item(malaga_buffer, MORPHOLOGY);
		delete[] malaga_buffer;
		
		current_analysis = first_analysis_result();
		if (!current_analysis) {
			delete[] buffer;
			return result;
		}
		
		do {
			analysis_str = get_value_string(current_analysis);
			j = 0;
			for (i = 0; i < leading_len; i++) {
				while (analysis_str[j] == '=') j++;
				if (analysis_str[j] == '\0') break;
				j++;
			}
			if (i == leading_len) {
				spres = voikko_match_word_and_analysis(buffer, len - 1, analysis_str);
				if (analysis_str[j] == '=' && (result_with_border == SPELL_FAILED ||
				    result_with_border > spres)) result_with_border = spres;
				if (analysis_str[j] != '=' && (result_without_border == SPELL_FAILED ||
				    result_without_border > spres)) result_without_border = spres;
			}
			free(analysis_str);
			current_analysis = next_analysis_result();
		} while (current_analysis);
		
		delete[] buffer;
		if (result_with_border != SPELL_FAILED && result_without_border != SPELL_FAILED &&
		    (result == SPELL_FAILED || result > result_with_border)) return result_with_border;
	}
	
	return result;
}

enum spellresult voikko_do_spell_ignore_hyphens(const wchar_t * word, size_t len) {
	enum spellresult spres = voikko_do_spell(word, len);
	if (spres != SPELL_FAILED) return spres;
	
	// Hyphens were already present, so we cannot do anything more
	if (len < 2 || (word[0] == L'-' && word[len - 1] == L'-')) return SPELL_FAILED;
	
	wchar_t * buffer = new wchar_t[len + 2];
	if (!buffer) return SPELL_FAILED;
	size_t newlen = len + 1;
	if (word[0] == L'-') {
		wcsncpy(buffer, word, len);
		buffer[len] = L'-';
	}
	else {
		buffer[0] = L'-';
		wcsncpy(buffer + 1, word, len);
		if (word[len - 1] != L'-') {
			buffer[len + 1] = L'-';
			newlen++;
		}
	}
	spres = voikko_do_spell(buffer, newlen);
	delete[] buffer;
	return spres;
}

VOIKKOEXPORT int voikko_spell_cstr(int handle, const char * word) {
	wchar_t * word_ucs4;
	int result;
	if (word == 0 || word[0] == '\0') return VOIKKO_SPELL_OK;
	size_t len = strlen(word);
	if (len > LIBVOIKKO_MAX_WORD_CHARS) return 0;
	word_ucs4 = voikko_cstrtoucs4(word, voikko_options.encoding, len);
	if (word_ucs4 == 0) return VOIKKO_CHARSET_CONVERSION_FAILED;
	result = voikko_spell_ucs4(handle, word_ucs4);
	delete[] word_ucs4;
	return result;
}

/* A small result cache:
 * word_length order elements wchars offset
 * 1           4     16       16     0
 * 2           5     32       64     16
 * 3           6     64       192    80
 * 4           7     128      512    272
 * 5           7     128      640    784
 * 6           7     128      768    1424
 * 7           7     128      896    2192
 * 8           7     128      1024   3088
 * 9           7     128      1152   4112
 * 10          7     128      1280   5264
 * total size      sizeof(wchar_t) * 6544
 */

const int VOIKKO_HASH_ORDERS[]   =  {0, 3+0,  5+0,  6+0,   7+0,   7+0,    7+0,    7+0,    7+0,    7+0,    7+0};
const int VOIKKO_CACHE_OFFSETS[] =  {0,   0, 1*16, 1*80, 1*272, 1*784, 1*1424, 1*2192, 1*3088, 1*4112, 1*5264};
const int VOIKKO_META_OFFSETS[]  =  {0,   0, 1*16, 1*48, 1*112, 1*240,  1*368,  1*496,  1*624,  1*752,  1*880};

enum spellresult voikko_cached_spell(const wchar_t * buffer, size_t len) {
	int hashcode;
	int cache_offset;
	int meta_offset;
	enum spellresult result;
	int sparam = voikko_options.cache_size;
	if (voikko_options.cache && len <= 10) { /* check cache */
		hashcode = voikko_hash(buffer, len, VOIKKO_HASH_ORDERS[len] + sparam);
		cache_offset = (VOIKKO_CACHE_OFFSETS[len] << sparam) + hashcode * len;
		meta_offset = (VOIKKO_META_OFFSETS[len] << sparam) + hashcode;
		if (wcsncmp(voikko_options.cache + cache_offset, buffer, len) == 0) {
			/* DEBUG: printf("Cache hit: '%ls'\n", buffer);*/
			if (voikko_options.cache_meta[meta_offset] == 'i') return SPELL_CAP_FIRST;
			else return SPELL_OK;
		}
		/* not in cache */
		if (voikko_options.accept_missing_hyphens)
			result = voikko_do_spell_ignore_hyphens(buffer, len);
		else
			result = voikko_do_spell(buffer, len);
		if (result == SPELL_OK || result == SPELL_CAP_FIRST) {
			wcsncpy(voikko_options.cache + cache_offset, buffer, len);
			voikko_options.cache_meta[meta_offset] = (result == SPELL_OK) ? 'p' : 'i';
		}
		return result;
	}
	/* no cache available */
	if (voikko_options.accept_missing_hyphens)
		return voikko_do_spell_ignore_hyphens(buffer, len);
	else
		return voikko_do_spell(buffer, len);
}


VOIKKOEXPORT int voikko_spell_ucs4(int handle, const wchar_t * word) {
	size_t nchars = wcslen(word);
	int result;
	wchar_t * nword;
	int dot_index;
	enum casetype caps;
	enum spellresult sres;
	if (nchars == 0) return VOIKKO_SPELL_OK;
	if (nchars > LIBVOIKKO_MAX_WORD_CHARS) return VOIKKO_INTERNAL_ERROR;
	ENTER_V
	
	nword = voikko_normalise(word, nchars);
	if (nword == 0) {
		EXIT_V
		return VOIKKO_INTERNAL_ERROR;
	}
	nchars = wcslen(nword);
	
	if (voikko_options.ignore_numbers) {
		for (size_t i = 0; i < nchars; i++) {
			if (iswdigit(nword[i])) {
				delete[] nword;
				EXIT_V
				return VOIKKO_SPELL_OK;
			}
		}
	}
	caps = voikko_casetype(nword, nchars);
	if ((voikko_options.ignore_uppercase && caps == CT_ALL_UPPER) ||
	    (voikko_options.ignore_nonwords && voikko_is_nonword(nword, nchars))) {
		delete[] nword;
		EXIT_V
		return VOIKKO_SPELL_OK;
	}
	
	wchar_t * buffer = new wchar_t[nchars + 1];
	if (buffer == 0) {
		delete[] nword;
		EXIT_V
		return VOIKKO_INTERNAL_ERROR;
	}

	for (size_t i = 0; i < nchars; i++) {
		buffer[i] = towlower(nword[i]);
	}
	buffer[nchars] = L'\0';
	if (voikko_options.ignore_dot && buffer[nchars - 1] == L'.') {
		dot_index = nchars - 1;
		buffer[dot_index] = L'\0';
	}
	else dot_index = -1;
	
	/* Check words that require exact captialisation */
	if (caps == CT_COMPLEX || caps == CT_NO_LETTERS ||
	    (caps == CT_ALL_UPPER && !voikko_options.accept_all_uppercase)) {
		wcsncpy(buffer, nword, nchars);
		buffer[0] = towlower(buffer[0]);
		if (voikko_options.accept_missing_hyphens)
			sres = voikko_do_spell_ignore_hyphens(buffer, nchars);
		else
			sres = voikko_do_spell(buffer, nchars);
		if (sres == SPELL_OK ||
		    (sres == SPELL_CAP_FIRST && voikko_options.accept_first_uppercase && iswupper(nword[0])))
			result = VOIKKO_SPELL_OK;
		else result = VOIKKO_SPELL_FAILED;
		if (result == VOIKKO_SPELL_FAILED && dot_index != -1) { /* remove dot */
			buffer[dot_index] = L'\0';
			if (voikko_options.accept_missing_hyphens)
				sres = voikko_do_spell_ignore_hyphens(buffer, nchars);
			else
				sres = voikko_do_spell(buffer, nchars);
			if (sres == SPELL_OK ||
			    (sres == SPELL_CAP_FIRST && voikko_options.accept_first_uppercase && iswupper(nword[0])))
				result = VOIKKO_SPELL_OK;
		}
		delete[] nword;
		delete[] buffer;
		EXIT_V
		return result;
	}
	
	
	/* Check without trailing dot */
	switch (caps) {
		case CT_ALL_LOWER:
			sres = voikko_cached_spell(buffer, nchars);
			result = (sres == SPELL_OK) ? VOIKKO_SPELL_OK : VOIKKO_SPELL_FAILED;
			break;
		case CT_FIRST_UPPER:
			sres = voikko_cached_spell(buffer, nchars);
			if ((sres == SPELL_OK && voikko_options.accept_first_uppercase) || sres == SPELL_CAP_FIRST)
				result = VOIKKO_SPELL_OK;
			else result = VOIKKO_SPELL_FAILED;
			break;
		case CT_ALL_UPPER:
			assert(voikko_options.accept_all_uppercase);
			sres = voikko_cached_spell(buffer, nchars);
			result = (sres == SPELL_FAILED) ? VOIKKO_SPELL_FAILED : VOIKKO_SPELL_OK;
			break;
		default: /* should not happen */
			result = VOIKKO_INTERNAL_ERROR;
	}
	if (result == VOIKKO_SPELL_OK) {
		delete[] nword;
		delete[] buffer;
		return VOIKKO_SPELL_OK;
	}
	
	/* Check with trailing dot */
	if (dot_index != -1) {
		buffer[dot_index] = L'.';
		switch (caps) {
			case CT_ALL_LOWER:
				sres = voikko_cached_spell(buffer, nchars);
				result = (sres == SPELL_OK) ? VOIKKO_SPELL_OK : VOIKKO_SPELL_FAILED;
				break;
			case CT_FIRST_UPPER:
				sres = voikko_cached_spell(buffer, nchars);
				if ((sres == SPELL_OK && voikko_options.accept_first_uppercase) || sres == SPELL_CAP_FIRST)
					result = VOIKKO_SPELL_OK;
				else result = VOIKKO_SPELL_FAILED;
				break;
			case CT_ALL_UPPER:
				assert(voikko_options.accept_all_uppercase);
				sres = voikko_cached_spell(buffer, nchars);
				result = (sres == SPELL_FAILED) ? VOIKKO_SPELL_FAILED : VOIKKO_SPELL_OK;
			default: /* should not happen */
				result = VOIKKO_INTERNAL_ERROR;
		}
	}
	delete[] nword;
	delete[] buffer;
	return result;
}

}
