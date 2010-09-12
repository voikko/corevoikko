/* Libvoikko: Library of Finnish language tools
 * Copyright (C) 2010 Harri Pitkänen <hatapitk@iki.fi>
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

#include "grammar/check/NegativeVerbCheck.hpp"
#include "grammar/error.hpp"
#include "grammar/cache.hpp"

using namespace std;

namespace libvoikko { namespace grammar { namespace check {

void NegativeVerbCheck::check(voikko_options_t * options, const Sentence * sentence) {
	for (size_t i = 0; i + 2 < sentence->tokenCount; i++) {
		const Token * token = sentence->tokens + i;
		if (token->type == TOKEN_WORD &&
		    (sentence->tokens + i + 1)->type == TOKEN_WHITESPACE &&
		    (sentence->tokens + i + 2)->type == TOKEN_WORD) {
			const Token * word2 = sentence->tokens + i + 2;
			if (token->isVerbNegative && word2->isPositiveVerb) {
				CacheEntry * e = new CacheEntry(0);
				e->error.error_code = GCERR_NEGATIVE_VERB_MISMATCH;
				e->error.startpos = token->pos;
				e->error.errorlen = word2->pos + word2->tokenlen - token->pos;
				gc_cache_append_error(options, e);
			}
		}
	}
}

} } }
