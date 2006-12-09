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
#include "voikko_spell.h"
#include "voikko_setup.h"
#include "voikko_utils.h"
#include "voikko_charset.h"
#include <stdlib.h>
#include <string.h>
#include <malaga.h>
#include <wchar.h>
#include <wctype.h>

enum spellresult voikko_spell_with_priority(const wchar_t * word, size_t len, int * prio) {
	enum spellresult result;
	enum spellresult best_result;
	size_t i, j;
	value_t analysis;
	const char * analysis_str;

	/* Capitalisation pattern: 'i' = uppercase letter, 'p' = lowercase letter, 'v' is punctuation */
	char * cappat;

	char * malaga_buffer = voikko_ucs4tocstr(word, "UTF-8");
	if (malaga_buffer == 0) return SPELL_FAILED;
	analyse_item(malaga_buffer, MORPHOLOGY);
	free(malaga_buffer);
	if (prio != 0) *prio = 0;
	
	analysis = first_analysis_result();
	if (!analysis) return SPELL_FAILED;
	cappat = malloc(len);
	if (cappat == 0) return SPELL_FAILED;
	for (i = 0; i < len; i++) {
		if (iswupper(word[i])) cappat[i] = 'i';
		else if (iswlower(word[i])) cappat[i] = 'p';
		else cappat[i] = 'v';
	}
	
	best_result = SPELL_FAILED;
	do {
		result = SPELL_OK;
		j = 0;
		analysis_str = get_value_string(analysis);
		for (i = 0; i < len; i++) {
			while (analysis_str[j] == '=') j++;
			if (analysis_str[j] == '\0') break;
			if (cappat[i] == 'p' && (analysis_str[j] == 'i' || analysis_str[j] == 'j')) {
				if (i == 0) result = SPELL_CAP_FIRST;
				else result = SPELL_CAP_ERROR;
			}
			if (cappat[i] == 'i' && (analysis_str[j] == 'p' || analysis_str[j] == 'q')) {
				result = SPELL_CAP_ERROR;
			}
			if (result == SPELL_CAP_ERROR) break;
			j++;
		}
		if (best_result == SPELL_FAILED || best_result > result) {
			best_result = result;
			if (prio != 0) {
				*prio = 0;
				for (j = 0; analysis_str[j] != '\0'; j++)
					if (analysis_str[j] == '=') (*prio)++;
			}
		}
		free((char *) analysis_str);
		if (best_result == SPELL_OK) break;
		analysis = next_analysis_result();
	} while (analysis);
	free(cappat);
	
	if (prio != 0) {
		if (best_result == SPELL_CAP_FIRST) (*prio) += 1;
		else if (best_result == SPELL_CAP_ERROR) (*prio) += 2;
	}
	return best_result;
}

enum spellresult voikko_do_spell(const wchar_t * word, size_t len) {
	return voikko_spell_with_priority(word, len, 0);
}

int voikko_spell_cstr(int handle, const char * word) {
	wchar_t * word_ucs4;
	int result;
	if (word == 0 || word[0] == '\0') return VOIKKO_SPELL_OK;
	word_ucs4 = voikko_cstrtoucs4(word, voikko_options.encoding);
	if (word_ucs4 == 0) return VOIKKO_CHARSET_CONVERSION_FAILED;
	result = voikko_spell_ucs4(handle, word_ucs4);
	free(word_ucs4);
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
		result = voikko_do_spell(buffer, len);
		if (result == SPELL_OK || result == SPELL_CAP_FIRST) {
			wcsncpy(voikko_options.cache + cache_offset, buffer, len);
			voikko_options.cache_meta[meta_offset] = (result == SPELL_OK) ? 'p' : 'i';
		}
		return result;
	}
	/* no cache available */
	return voikko_do_spell(buffer, len);
}


int voikko_spell_ucs4(int handle, const wchar_t * word) {
	size_t nchars = wcslen(word);
	int i;
	int result;
	wchar_t * nword;
	wchar_t * buffer;
	int dot_index;
	enum casetype caps;
	enum spellresult sres;
	if (nchars == 0) return VOIKKO_SPELL_OK;
	if (nchars > LIBVOIKKO_MAX_WORD_CHARS) return VOIKKO_INTERNAL_ERROR;
	
	nword = voikko_normalise(word, nchars);
	if (nword == 0) return VOIKKO_INTERNAL_ERROR;
	nchars = wcslen(nword);
	
	if (voikko_options.ignore_numbers) {
		for (i = 0; i < nchars; i++) {
			if (iswdigit(nword[i])) {
				free(nword);
				return VOIKKO_SPELL_OK;
			}
		}
	}
	caps = voikko_casetype(nword, nchars);
	if (voikko_options.ignore_uppercase && caps == CT_ALL_UPPER){
		free(nword);
		return VOIKKO_SPELL_OK;
	}
	
	buffer = malloc((nchars + 1) * sizeof(wchar_t));
	if (buffer == 0) {
		free(nword);
		return VOIKKO_INTERNAL_ERROR;
	}

	for (i = 0; i < nchars; i++) buffer[i] = towlower(nword[i]);
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
		sres = voikko_do_spell(buffer, nchars);
		if (sres == SPELL_OK ||
		    (sres == SPELL_CAP_FIRST && voikko_options.accept_first_uppercase && iswupper(nword[0])))
			result = VOIKKO_SPELL_OK;
		else result = VOIKKO_SPELL_FAILED;
		if (result == VOIKKO_SPELL_FAILED && dot_index != -1) { /* remove dot */
			buffer[dot_index] = L'\0';
			sres = voikko_do_spell(buffer, nchars);
			if (sres == SPELL_OK ||
			    (sres == SPELL_CAP_FIRST && voikko_options.accept_first_uppercase && iswupper(nword[0])))
				result = VOIKKO_SPELL_OK;
		}
		free(nword);
		free(buffer);
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
		free(nword);
		free(buffer);
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
	free(nword);
	free(buffer);
	return result;
}
