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

#define GCERR_WRITE_TOGETHER 1

voikko_grammar_error voikko_next_grammar_error_cstr(int handle, const char * text,
                                                   size_t textlen, size_t startpos) {
	voikko_grammar_error e;
	e.error_code = 0;
	e.error_level = 0;
	e.error_description = 0;
	e.startpos = 0;
	e.errorlen = 0;
	e.suggestions = 0;
	if (text == 0 || textlen == 0) return e;
	wchar_t * text_ucs4 = voikko_cstrtoucs4(text, voikko_options.encoding, textlen);
	if (text_ucs4 == 0) return e;
	
	size_t wtextlen = wcslen(text_ucs4);
	if (startpos < wtextlen) {
		wchar_t * errorpos = wcsstr(text_ucs4 + startpos, L"joten kuten");
		if (errorpos) {
			e.error_code = GCERR_WRITE_TOGETHER;
			e.startpos = errorpos - text_ucs4;
			e.errorlen = 11;
		}
	}
	
	free(text_ucs4);
	return e;
}

const char * voikko_error_message_cstr(int error_code, const char * language) {
	switch (error_code) {
		case GCERR_WRITE_TOGETHER: return "Sanat on kirjoitettava yhteen.";
	}
	return "Tuntematon virhe";
}
