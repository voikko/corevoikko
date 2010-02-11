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

#include "spellchecker/suggestion/SuggestionGeneratorInsertion.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorCaseChange.hpp"
#include "character/charset.hpp"
#include <cwchar>

namespace libvoikko { namespace spellchecker { namespace suggestion {

SuggestionGeneratorInsertion::SuggestionGeneratorInsertion(
	const wchar_t * characters) :
	characters(characters) { }

void SuggestionGeneratorInsertion::generate(voikko_options_t * voikkoOptions, SuggestionStatus * s) const {
	wchar_t * buffer = new wchar_t[s->getWordLength() + 2];
	for (const wchar_t * ins = characters; *ins != L'\0'; ins++) {
		buffer[0] = s->getWord()[0];
		wcsncpy(buffer + 1, s->getWord(), s->getWordLength());
		buffer[s->getWordLength()+1] = L'\0';
		for (size_t j = 0; j < s->getWordLength() && !s->shouldAbort(); j++) {
			if (j != 0) {
				buffer[j-1] = s->getWord()[j-1];
			}
			if (*ins == simpleLower((s->getWord()[j]))) {
				continue; /* avoid duplicates */
			}
			if (j > 0 && *ins == simpleLower((s->getWord()[j-1]))) {
				continue; /* avoid duplicates */
			}
			buffer[j] = *ins;
			SuggestionGeneratorCaseChange::suggestForBuffer(voikkoOptions,
			    s, buffer, s->getWordLength() + 1);
		}
		if (s->shouldAbort()) {
			break;
		}
		if (*ins == s->getWord()[s->getWordLength()-1]) {
			continue;
		}
		buffer[s->getWordLength()-1] = s->getWord()[s->getWordLength()-1];
		buffer[s->getWordLength()] = *ins;
		SuggestionGeneratorCaseChange::suggestForBuffer(voikkoOptions,
		    s, buffer, s->getWordLength() + 1);
	}
	delete[] buffer;
}

}}}
