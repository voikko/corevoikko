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

#include "spellchecker/suggestion/SuggestionGeneratorVowelChange.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorCaseChange.hpp"
#include <wchar.h>
#include <wctype.h>

namespace libvoikko { namespace spellchecker { namespace suggestion {

static const wchar_t * BACK_VOWELS =  L"aouAOU";
static const wchar_t * FRONT_VOWELS = L"\u00e4\u00f6y\u00c4\u00d6Y";

void SuggestionGeneratorVowelChange::generate(SuggestionStatus * s) const {
	int mask = 0;
	size_t vcount = 0;
	int pat = 1;
	for (size_t i = 0; i < s->getWordLength(); i++)
		for (int j = 0; j < 6; j++)
			if (s->getWord()[i] == BACK_VOWELS[j] ||
			    s->getWord()[i] == FRONT_VOWELS[j]) {
				vcount++;
				mask <<= 1;
				mask++;
				break;
			}
	if (vcount == 0 || vcount > 7) return;
	wchar_t * buffer = new wchar_t[s->getWordLength() + 1];
	while ((pat & mask) != 0) {
		size_t i = 0;
		wcscpy(buffer, s->getWord());
		for (size_t j = 0; j < vcount; j++) {
			while (!wcschr(BACK_VOWELS,  buffer[i]) &&
			       !wcschr(FRONT_VOWELS, buffer[i])) i++;
			if (pat & (1 << j)) {
				for (int k = 0; k < 6; k++) {
					if (buffer[i] == BACK_VOWELS[k]) {
						buffer[i] = FRONT_VOWELS[k];
						break;
					}
					if (buffer[i] == FRONT_VOWELS[k]) {
						buffer[i] = BACK_VOWELS[k];
						break;
					}
				}
			}
			i++;
		}
		if (s->shouldAbort()) {
			delete[] buffer;
			return;
		}
		SuggestionGeneratorCaseChange::suggestForBuffer(s, buffer,
		    s->getWordLength());
		pat++;
	}
	delete[] buffer;
}

}}}
