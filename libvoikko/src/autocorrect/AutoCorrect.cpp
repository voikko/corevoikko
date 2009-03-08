/* Libvoikko: Library of Finnish language tools
 * Copyright (C) 2009 Harri Pitkänen <hatapitk@iki.fi>
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

#include "autocorrect/AutoCorrect.hpp"
#include "grammar/error.hpp"
#include "grammar/cachesetup.hpp"
#include "grammar/cache.hpp"
#include "TrieNode.hpp"
#include <cstring>
#include <wchar.h>

using namespace libvoikko::grammar;

namespace libvoikko { namespace autocorrect {

#include "autocorrect/data.hpp"

size_t traverse(size_t initial, const wchar_t * str, size_t strlen) {
	size_t current = initial;
	for (size_t i = 0; i < strlen; i++) {
		if (NODES[current].subtreeStart) {
			current = NODES[current].subtreeStart;
			while (NODES[current].label != str[i]) {
				current++;
				if (!NODES[current].label) {
					return 0;
				}
			}
		}
		else {
			return 0;
		}
	}
	return current;
}

void AutoCorrect::autoCorrect(int handle, const libvoikko::grammar::Sentence * sentence) {
	for (size_t i = 0; i + 2 < sentence->tokenCount; i++) {
		Token t = sentence->tokens[i];
		if (t.type != TOKEN_WORD) continue;
		if (wcscmp(t.str, L"joten")) continue;
		t = sentence->tokens[i+1];
		if (t.type != TOKEN_WHITESPACE) continue;
		t = sentence->tokens[i+2];
		if (t.type != TOKEN_WORD) continue;
		if (wcscmp(t.str, L"kuten")) continue;
		voikko_gc_cache_entry * e = gc_new_cache_entry(1);
		if (!e) return;
		e->error.error_code = GCERR_WRITE_TOGETHER;
		e->error.startpos = sentence->tokens[i].pos;
		e->error.errorlen = 10 + sentence->tokens[i+1].tokenlen;
		e->error.suggestions[0] = new char[11];
		if (e->error.suggestions[0]) {
			strcpy(e->error.suggestions[0], "jotenkuten");
		}
		gc_cache_append_error(handle, e);
	}
}

} }
