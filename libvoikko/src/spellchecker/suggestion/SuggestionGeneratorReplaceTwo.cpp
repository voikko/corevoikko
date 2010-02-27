/* Libvoikko: Finnish spellchecker and hyphenator library
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

#include "spellchecker/suggestion/SuggestionGeneratorReplaceTwo.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorCaseChange.hpp"
#include "character/SimpleChar.hpp"
#include <cwchar>

using namespace libvoikko::character;

namespace libvoikko { namespace spellchecker { namespace suggestion {

SuggestionGeneratorReplaceTwo::SuggestionGeneratorReplaceTwo(
	const wchar_t * replacements) :
	replacements(replacements) { }

void SuggestionGeneratorReplaceTwo::generate(voikko_options_t * voikkoOptions, SuggestionStatus * s) const {
	size_t wlen = s->getWordLength();
	wchar_t * buffer = new wchar_t[wlen + 1];
	for (size_t i = 0; i < wlen; ++i) {
		buffer[i] = SimpleChar::lower(s->getWord()[i]);
	}
	buffer[wlen] = L'\0';
	
	for (size_t i = 0; i < wlen - 1; ++i) {
		wchar_t replaced = buffer[i];
		if (replaced != buffer[i + 1]) {
			continue;
		}
		for (const wchar_t * j = replacements; *j != L'\0'; j += 2) {
			if (*j != replaced) {
				continue;
			}
			buffer[i] = *(j + 1);
			buffer[i + 1] = *(j + 1);
			SuggestionGeneratorCaseChange::suggestForBuffer(voikkoOptions->morAnalyzer,
			    s, buffer, wlen);
			if (s->shouldAbort()) {
				break;
			}
		}
		buffer[i] = replaced;
		buffer[i + 1] = replaced;
		if (s->shouldAbort()) {
			break;
		}
		++i;
	}
	
	delete[] buffer;
}

}}}
