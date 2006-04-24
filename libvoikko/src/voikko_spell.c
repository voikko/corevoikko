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
#include <stdlib.h>
#include <string.h>
#include <malaga.h>
#include <wchar.h>
#include <wctype.h>

int voikko_do_spell(const char * word) {
	char * buffer = malloc(strlen(word) + 1);
	strcpy(buffer, word);
	analyse_item(buffer, MORPHOLOGY);
	if (first_analysis_result()) {
		free(buffer);
		return VOIKKO_SPELL_OK;
	}
	if (voikko_options.ignore_dot && strlen(word) > 1 && word[strlen(word) - 1] == '.') {
		buffer[strlen(word) - 1] = '\0';
		analyse_item(buffer, MORPHOLOGY);
		if (first_analysis_result()) {
			free(buffer);
			return VOIKKO_SPELL_OK;
		}
	}
	free(buffer);
	return VOIKKO_SPELL_FAILED;
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

int voikko_spell_ucs4(int handle, const wchar_t * word) {
	size_t nchars = wcslen(word);
	size_t i;
	char * word_utf8;
	int result;
	if (voikko_options.ignore_numbers) {
		for (i = 0; i < nchars; i++) {
			if (iswdigit(word[i])) return VOIKKO_SPELL_OK;
		}
	}
	if (voikko_options.ignore_uppercase) {
		for (i = 0; i < nchars; i++)
			if (iswlower(word[i])) break;
		if (i == nchars) return VOIKKO_SPELL_OK;
	}
	word_utf8 = voikko_ucs4tocstr(word, "UTF-8");
	result = voikko_do_spell(word_utf8);
	free(word_utf8);
	return result;
}
