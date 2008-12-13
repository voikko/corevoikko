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
#include "setup/setup.hpp"
#include "utils/utils.hpp"
#include "grammar/cache.hpp"
#include <cstdlib>
#include <cstring>

namespace libvoikko {

void init_gc_error(voikko_grammar_error * gc_error) {
	memset(gc_error, 0, sizeof(gc_error));
}

VOIKKOEXPORT voikko_grammar_error voikko_next_grammar_error_cstr(int handle, const char * text,
                     size_t textlen, size_t startpos, int skiperrors) {
	if (text == 0 || textlen == 0) {
		return voikko_next_grammar_error_ucs4(handle, 0, 0, 0, 0);
	}
	
	wchar_t * text_ucs4 = voikko_cstrtoucs4(text, voikko_options.encoding, textlen);
	if (text_ucs4 == 0) {
		return voikko_next_grammar_error_ucs4(handle, 0, 0, 0, 0);
	}
	
	size_t wtextlen = wcslen(text_ucs4);
	voikko_grammar_error e = voikko_next_grammar_error_ucs4(handle, text_ucs4,
	                         wtextlen, startpos, skiperrors);
	delete[] text_ucs4;
	
	return e;
}

VOIKKOEXPORT voikko_grammar_error voikko_next_grammar_error_ucs4(int handle, const wchar_t * text_ucs4,
                     size_t wtextlen, size_t startpos, int skiperrors) {
	voikko_grammar_error e;
	init_gc_error(&e);
	if (text_ucs4 == 0 || wtextlen == 0) return e;
	
	const voikko_grammar_error * c_error = gc_error_from_cache(handle, text_ucs4, startpos, skiperrors);
	if (!c_error) {
		gc_paragraph_to_cache(handle, text_ucs4, wtextlen);
		c_error = gc_error_from_cache(handle, text_ucs4, startpos, skiperrors);
	}
	
	if (!c_error) return e;
	
	// Return a deep copy of cached error
	memcpy(&e, c_error, sizeof(voikko_grammar_error));
	if (!c_error->suggestions) return e;
	int sugg_count = 0;
	for (char ** s = c_error->suggestions; *s; s++) {
		sugg_count++;
	}
	e.suggestions = new char*[sugg_count + 1];
	if (!e.suggestions) return e;
	for (int i = 0; i < sugg_count; i++) {
		e.suggestions[i] = new char[strlen(c_error->suggestions[i]) + 1];
		if (!e.suggestions[i]) return e;
		strcpy(e.suggestions[i], c_error->suggestions[i]);
	}
	e.suggestions[sugg_count] = 0;
	return e;
}

}
