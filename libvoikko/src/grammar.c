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
#include <string.h>

#define GCERR_WRITE_TOGETHER 1

void init_gc_error(voikko_grammar_error * gc_error) {
	memset(gc_error, 0, sizeof(gc_error));
}

voikko_grammar_error voikko_next_grammar_error_cstr(int handle, const char * text,
                                                   size_t textlen, size_t startpos) {
	voikko_grammar_error e;
	init_gc_error(&e);
	if (text == 0 || textlen == 0) return e;
	wchar_t * text_ucs4 = voikko_cstrtoucs4(text, voikko_options.encoding, textlen);
	if (text_ucs4 == 0) return e;
	
	size_t wtextlen = wcslen(text_ucs4);
	const voikko_grammar_error * c_error = gc_error_from_cache(handle, text_ucs4, startpos);
	if (!c_error) gc_paragraph_to_cache(handle, text_ucs4, wtextlen);
	c_error = gc_error_from_cache(handle, text_ucs4, startpos);
	
	free(text_ucs4);
	if (c_error) return *c_error;
	else return e;
}

const char * voikko_error_message_cstr(int error_code, const char * language) {
	switch (error_code) {
		case GCERR_WRITE_TOGETHER: return "Sanat on kirjoitettava yhteen.";
	}
	return "Tuntematon virhe";
}
