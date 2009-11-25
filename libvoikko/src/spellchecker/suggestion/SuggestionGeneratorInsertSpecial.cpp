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

#include "spellchecker/suggestion/SuggestionGeneratorInsertSpecial.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorCaseChange.hpp"
#include <cwchar>
#include <cwctype>

namespace libvoikko { namespace spellchecker { namespace suggestion {

void SuggestionGeneratorInsertSpecial::generate(voikko_options_t * voikkoOptions, SuggestionStatus * s) const {
	wchar_t * buffer = new wchar_t[s->getWordLength() + 2];
	wcsncpy(buffer + 1, s->getWord(), s->getWordLength());
	buffer[s->getWordLength()+1] = L'\0';
	
	/* suggest adding '-' */
	for (size_t j = 2; j <= s->getWordLength() - 2 && !s->shouldAbort(); j++) {
		/* Do not add hyphen if there already is another nearby */
		if (s->getWord()[j-2] == L'-' || s->getWord()[j-1] == L'-' ||
		    s->getWord()[j] ==   L'-' || s->getWord()[j+1] == L'-')
			continue;
		wcsncpy(buffer, s->getWord(), j);
		buffer[j] = L'-';
		SuggestionGeneratorCaseChange::suggestForBuffer(voikkoOptions,
		    s, buffer, s->getWordLength() + 1);
	}
	/* suggest character duplication */
	wcsncpy(buffer + 1, s->getWord(), s->getWordLength() + 1);
	for (size_t j = 0; j < s->getWordLength() && !s->shouldAbort(); j++) {
		buffer[j] = s->getWord()[j];
		/* Do not duplicate if there already are two same letters */
		if (j < s->getWordLength() - 1 && s->getWord()[j] == s->getWord()[j+1]) {
			j++;
			continue;
		}
		/* These should not be duplicated */
		if (s->getWord()[j] == L'-' || s->getWord()[j] == L'\'') continue;
		SuggestionGeneratorCaseChange::suggestForBuffer(voikkoOptions,
		    s, buffer, s->getWordLength() + 1);
	}
	delete[] buffer;
}

}}}
