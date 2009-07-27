/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2006 - 2009 Harri Pitkänen <hatapitk@iki.fi>
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

#include "spellchecker/suggestion/SuggestionGeneratorReplacement.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorCaseChange.hpp"
#include <wchar.h>
#include <wctype.h>

namespace libvoikko { namespace spellchecker { namespace suggestion {

SuggestionGeneratorReplacement::SuggestionGeneratorReplacement(
	const wchar_t * replacements) :
	replacements(replacements) { }

void SuggestionGeneratorReplacement::generate(SuggestionStatus * s) const {
	wchar_t * buffer = new wchar_t[s->getWordLength() + 1];
	wcsncpy(buffer, s->getWord(), s->getWordLength());
	buffer[s->getWordLength()] = L'\0';
	for (const wchar_t * i = replacements; *i != L'\0'; i += 2) {
		wchar_t from = *i;
		wchar_t to = *(i + 1);
		for (wchar_t * pos = wcschr(buffer, from); pos != 0;
		     pos = wcschr(pos+1, from)) {
			*pos = to;
			SuggestionGeneratorCaseChange::suggestForBuffer(s, buffer,
			    s->getWordLength());
			if (s->shouldAbort()) break;
			*pos = from;
		}
		if (s->shouldAbort()) break;
		
		/* Only search for upper case letter if it differs from lower case
		   version */
		wchar_t upper_from = towupper(from);
		if (upper_from == from) continue;
		for (wchar_t * pos = wcschr(buffer, upper_from); pos != 0;
		     pos = wcschr(pos + 1, upper_from)) {
			*pos = towupper(to);
			SuggestionGeneratorCaseChange::suggestForBuffer(s, buffer,
			     s->getWordLength());
			if (s->shouldAbort()) break;
			*pos = upper_from;
		}
		if (s->shouldAbort()) break;
	}
	delete[] buffer;
}

}}}
