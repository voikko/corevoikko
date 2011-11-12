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

#include "grammar/check/SidesanaCheck.hpp"
#include "grammar/error.hpp"
#include "grammar/cache.hpp"

using namespace std;

namespace libvoikko { namespace grammar { namespace check {

void SidesanaCheck::check(voikko_options_t * options, const Sentence * sentence) {
	size_t tokenCount = sentence->tokenCount;
	if (tokenCount >= 2 &&
	    ((sentence->tokens + (tokenCount - 2))->isConjunction) &&
	    (wcscmp((sentence->tokens + (tokenCount - 2))->str, L"vaan") != 0) && // "mitä vaan" ~ "mitä vain"
	    ((sentence->tokens + (tokenCount - 1))->type == TOKEN_PUNCTUATION) &&
	    ((sentence->tokens + (tokenCount - 1))->str[0] == L'.')) {
		CacheEntry * e = new CacheEntry(0);
		e->error.error_code = GCERR_MISPLACED_SIDESANA;
		e->error.startpos = (sentence->tokens + (tokenCount - 2))->pos;
		e->error.errorlen = (sentence->tokens + (tokenCount - 2))->tokenlen;
		gc_cache_append_error(options, e);
	}
}

} } }
