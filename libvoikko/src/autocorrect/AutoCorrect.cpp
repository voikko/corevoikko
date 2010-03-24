/* Libvoikko: Library of Finnish language tools
 * Copyright (C) 2009 - 2010 Harri Pitkänen <hatapitk@iki.fi>
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
#include "utils/StringUtils.hpp"
#include "TrieNode.hpp"
#include <cstring>
#include <cwchar>

using namespace libvoikko::grammar;
using namespace libvoikko::utils;

namespace libvoikko { namespace autocorrect {

#include "autocorrect/data.hpp"

size_t AutoCorrect::traverse(size_t initial, const wchar_t * str, size_t strlen) {
	size_t current = initial;
	for (size_t i = 0; i < strlen; i++) {
		if (str[i] == L'\u00AD') {
			// skip soft hyphens in input string
			continue;
		}
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

void AutoCorrect::autoCorrect(voikko_options_t * options, const libvoikko::grammar::Sentence * sentence) {
	for (size_t i = 0; i + 2 < sentence->tokenCount; i++) {
		Token t = sentence->tokens[i];
		if (t.type != TOKEN_WORD) {
			continue;
		}
		
		// Is the first word in the trie?
		size_t trieNode = traverse(0, t.str, t.tokenlen);
		if (!trieNode) {
			continue;
		}
		
		// Is the first word alone an error?
		if (NODES[trieNode].replacementIndex) {
			CacheEntry * e = new CacheEntry(1);
			if (!e) return;
			e->error.error_code = GCERR_INVALID_SPELLING;
			e->error.startpos = sentence->tokens[i].pos;
			e->error.errorlen = sentence->tokens[i].tokenlen;
			e->error.suggestions[0] = StringUtils::utf8FromUcs4(
			        REPLACEMENTS[NODES[trieNode].replacementIndex]);
			gc_cache_append_error(options, e);
		}
		
		// Is there a second word (in the sentence and in trie)?
		t = sentence->tokens[i+1];
		if (t.type != TOKEN_WHITESPACE) {
			continue;
		}
		t = sentence->tokens[i+2];
		if (t.type != TOKEN_WORD) {
			continue;
		}
		trieNode = traverse(trieNode, L" ", 1);
		if (!trieNode) {
			continue;
		}
		
		// Is the second word in the trie?
		trieNode = traverse(trieNode, t.str, t.tokenlen);
		if (!trieNode) {
			continue;
		}
		
		// Is the second word an error?
		if (NODES[trieNode].replacementIndex) {
			CacheEntry * e = new CacheEntry(1);
			if (!e) return;
			e->error.error_code = GCERR_INVALID_SPELLING;
			e->error.startpos = sentence->tokens[i].pos;
			e->error.errorlen = sentence->tokens[i+2].pos + sentence->tokens[i+2].tokenlen
			                    - e->error.startpos;
			e->error.suggestions[0] = StringUtils::utf8FromUcs4(
			        REPLACEMENTS[NODES[trieNode].replacementIndex]);
			gc_cache_append_error(options, e);
		}
	}
}

} }
