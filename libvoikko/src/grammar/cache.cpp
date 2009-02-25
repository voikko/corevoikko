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

#include "setup/setup.hpp"
#include "utils/utils.hpp"
#include "grammar/cachesetup.hpp"
#include "grammar/cache.hpp"
#include "grammar/checks.hpp"
#include "grammar/analysis.hpp"
#include <cstring>
#include <cstdlib>
#include <wctype.h>

using namespace libvoikko::grammar;

namespace libvoikko {

// This will be initialized to zero meaning "no errors"
static const voikko_grammar_error no_grammar_error = voikko_grammar_error();

const voikko_grammar_error * gc_error_from_cache(int /*handle*/, const wchar_t * text,
                             size_t startpos, int skiperrors) {
	if (!voikko_options.gc_cache.paragraph) return 0;	
	if (wcscmp(voikko_options.gc_cache.paragraph, text) != 0) return 0;
	voikko_gc_cache_entry * e = voikko_options.gc_cache.first_error;
	int preverrors = 0;
	while (e) {
		if (preverrors >= skiperrors &&
		    e->error.startpos >= startpos) {
			return &e->error;
		}
		preverrors++;
		e = e->next_error;
	}
	return &no_grammar_error;
}

void gc_paragraph_to_cache(int handle, const wchar_t * text, size_t textlen) {
	gc_clear_cache(handle);
	voikko_options.gc_cache.paragraph = new wchar_t[textlen + 1];
	if (!voikko_options.gc_cache.paragraph) return;
	memcpy(voikko_options.gc_cache.paragraph, text, textlen * sizeof(wchar_t));
	voikko_options.gc_cache.paragraph[textlen] = L'\0';
	Paragraph * para = gc_analyze_paragraph(handle, text, textlen);
	if (!para) return;
	for (int i = 0; i < para->sentenceCount; i++) {
		gc_static_replacements(handle, para->sentences[i]);
		gc_local_punctuation(handle, para->sentences[i]);
		gc_character_case(handle, para->sentences[i]);
		gc_repeating_words(handle, para->sentences[i]);
	}
	gc_end_punctuation(handle, para);
	delete para;
}

}
