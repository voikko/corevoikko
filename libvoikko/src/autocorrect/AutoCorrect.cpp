/* The contents of this file are subject to the Mozilla Public License Version 
 * 1.1 (the "License"); you may not use this file except in compliance with 
 * the License. You may obtain a copy of the License at 
 * http://www.mozilla.org/MPL/
 * 
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 * 
 * The Original Code is Libvoikko: Library of natural language processing tools.
 * The Initial Developer of the Original Code is Harri Pitkänen <hatapitk@iki.fi>.
 * Portions created by the Initial Developer are Copyright (C) 2009 - 2011
 * the Initial Developer. All Rights Reserved.
 * 
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *********************************************************************************/

#include "autocorrect/AutoCorrect.hpp"
#include "character/SimpleChar.hpp"
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

/**
 * Traverse trie from node initial through the characters in string str.
 * Returns 0 if there is no such path in the trie, otherwise returns the
 * index of the node at the last character of str.
 */
static size_t traverse(size_t initial, const wchar_t * str, size_t strlen, bool lowerFirst) {
	size_t current = initial;
	for (size_t i = 0; i < strlen; i++) {
		if (str[i] == L'\u00AD') {
			// skip soft hyphens in input string
			continue;
		}
		if (NODES[current].subtreeStart) {
			current = NODES[current].subtreeStart;
			wchar_t inputChar = (lowerFirst && i == 0 ? character::SimpleChar::lower(str[i]) : str[i]);
			while (NODES[current].label != inputChar) {
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

static void printErrorIfFinal(const TrieNode * node, const Token * firstErrorToken, const Token * lastErrorToken,
                              bool lowerFirst, voikko_options_t * options) {
	if (node->replacementIndex) {
		CacheEntry * e = new CacheEntry(1);
		e->error.error_code = GCERR_INVALID_SPELLING;
		e->error.startpos = firstErrorToken->pos;
		e->error.errorlen = lastErrorToken->pos + lastErrorToken->tokenlen - firstErrorToken->pos;
		const wchar_t * replacement = REPLACEMENTS[node->replacementIndex];
		if (lowerFirst) {
			wchar_t * replBuffer = StringUtils::copy(replacement);
			replBuffer[0] = character::SimpleChar::upper(replBuffer[0]);
			e->error.suggestions[0] = StringUtils::utf8FromUcs4(replBuffer);
			delete[] replBuffer;
		} else {
			e->error.suggestions[0] = StringUtils::utf8FromUcs4(replacement);
		}
		gc_cache_append_error(options, e);
	}
}

void AutoCorrect::autoCorrect(voikko_options_t * options, const libvoikko::grammar::Sentence * sentence) {
	for (size_t i = 0; i + 1 < sentence->tokenCount; i++) {
		Token t = sentence->tokens[i];
		if (t.type != TOKEN_WORD) {
			continue;
		}
		
		// Is the first word in the trie?
		bool lowerFirst = false;
		size_t trieNode = traverse(0, t.str, t.tokenlen, lowerFirst);
		if (!trieNode) {
			if (i == 0 && character::SimpleChar::isUpper(t.str[0])) {
				lowerFirst = true;
				trieNode = traverse(0, t.str, t.tokenlen, lowerFirst);
			}
			if (!trieNode) {
				continue;
			}
		}
		
		// Is the first word alone an error?
		printErrorIfFinal(NODES + trieNode, sentence->tokens + i, sentence->tokens + i, lowerFirst, options);
		
		size_t j = i + 1;
		while (j + 1 < sentence->tokenCount) {
			// Is there a second word (in the sentence and in trie)?
			t = sentence->tokens[j];
			if (t.type != TOKEN_WHITESPACE) {
				break;
			}
			t = sentence->tokens[j+1];
			if (t.type != TOKEN_WORD) {
				break;
			}
			trieNode = traverse(trieNode, L" ", 1, false);
			if (!trieNode) {
				break;
			}
			
			// Is the next word in the trie?
			trieNode = traverse(trieNode, t.str, t.tokenlen, false);
			if (!trieNode) {
				break;
			}
			
			// Is the next word an error?
			printErrorIfFinal(NODES + trieNode, sentence->tokens + i, sentence->tokens + (j + 1),
			                  lowerFirst, options);
			
			j += 2;
		}
	}
}

} }
