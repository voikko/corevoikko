/* Libvoikko: Library of Finnish language tools
 * Copyright (C) 2011 Harri Pitkänen <hatapitk@iki.fi>
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

#include "grammar/check/MissingVerbCheck.hpp"
#include "character/SimpleChar.hpp"
#include "grammar/error.hpp"
#include "grammar/cache.hpp"

using namespace std;

namespace libvoikko { namespace grammar { namespace check {

void MissingVerbCheck::check(voikko_options_t * options, const Sentence * sentence) {
	const Token * firstToken = sentence->tokens;
	if (firstToken->type == TOKEN_PUNCTUATION) {
		return;
	}
	if (options->accept_bulleted_lists_in_gc && sentence->pos == 0 &&
	    (character::SimpleChar::isLower(firstToken->str[0]) || !firstToken->firstLetterLcase)) {
		return;
	}
	int wordCount = 0;
	const Token * lastNonWhitespace = 0;
	for (size_t i = 0; i < sentence->tokenCount; i++) {
		const Token * token = sentence->tokens + i;
		if (token->str[0] == L'\t') {
			return;
		}
		if (token->type == TOKEN_WORD) {
			wordCount++;
			// TODO: Sanastoformaatin versiossa 2 ei ole mahdollista tunnistaa
			// verbiä rakenteista "Kukaan ei _vastannut_." Toistaiseksi siis
			// pelkkä kieltosana riittää täyttämään tämän säännön vaatimukset.
			if (!token->isValidWord || token->possibleMainVerb || token->isVerbNegative) {
				return;
			}
		}
		if (token->type != TOKEN_WHITESPACE) {
			lastNonWhitespace = token;
		}
	}
	if (wordCount < 2 || wcschr(L".!?", lastNonWhitespace->str[0]) == 0) {
		return;
	}
	CacheEntry * e = new CacheEntry(0);
	e->error.error_code = GCERR_MISSING_MAIN_VERB;
	e->error.startpos = sentence->pos;
	e->error.errorlen = lastNonWhitespace->pos + lastNonWhitespace->tokenlen - sentence->pos;
	gc_cache_append_error(options, e);
}

} } }
