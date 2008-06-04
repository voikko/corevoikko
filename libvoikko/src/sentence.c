/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2008 Harri Pitkänen <hatapitk@iki.fi>
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
#include "voikko_setup.h"
#include "voikko_utils.h"
#include <stdlib.h>

enum voikko_sentence_type voikko_next_sentence_start_ucs4(int handle,
                          const wchar_t * text, size_t textlen, size_t * sentencelen) {
	// TODO
	return SENTENCE_NONE;
}

enum voikko_sentence_type voikko_next_sentence_start_cstr(int handle, const char * text,
                          size_t textlen, size_t * sentencelen) {
	wchar_t * text_ucs4;
	enum voikko_token_type result;
	if (text == 0) return SENTENCE_NONE;
	text_ucs4 = voikko_cstrtoucs4(text, voikko_options.encoding, textlen);
	if (text_ucs4 == 0) return SENTENCE_NONE;
	result = voikko_next_sentence_start_ucs4(handle, text_ucs4, wcslen(text_ucs4),
	                                         sentencelen);
	free(text_ucs4);
	return result;
}
