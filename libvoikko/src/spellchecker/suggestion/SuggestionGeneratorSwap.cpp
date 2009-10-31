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

#include "spellchecker/suggestion/SuggestionGeneratorSwap.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorCaseChange.hpp"
#include <wchar.h>
#include <wctype.h>

namespace libvoikko { namespace spellchecker { namespace suggestion {

static const wchar_t * BACK_VOWELS =  L"aouAOU";
static const wchar_t * FRONT_VOWELS = L"\u00e4\u00f6y\u00c4\u00d6Y";

void SuggestionGeneratorSwap::generate(voikko_options_t * voikkoOptions, SuggestionStatus * s) const {
	size_t max_distance;
	if (s->getWordLength() <= 8) max_distance = 10;
	else max_distance = 50 / s->getWordLength();
	if (max_distance == 0) return;
	wchar_t * buffer = new wchar_t[s->getWordLength() + 1];
	wcsncpy(buffer, s->getWord(), s->getWordLength());
	buffer[s->getWordLength()] = L'\0';
	for (size_t i = 0; i < s->getWordLength() && !s->shouldAbort(); i++) {
		for (size_t j = i + 1; j < s->getWordLength() && !s->shouldAbort(); j++) {
			if (j - i > max_distance) break;
			/* do not suggest the same word */
			if (towlower(buffer[i]) == towlower(buffer[j])) continue;
			/* do not suggest swapping front and back vowels that have already
			   been tested earlier */
			int k;
			for (k = 0; k < 3; k++) {
				if ((towlower(buffer[i]) == (wint_t) BACK_VOWELS[k] &&
				     towlower(buffer[j]) == (wint_t) FRONT_VOWELS[k]) ||
				    (towlower(buffer[i]) == (wint_t) FRONT_VOWELS[k] &&
				     towlower(buffer[j]) == (wint_t) BACK_VOWELS[k])) break;
			}
			if (k < 3) continue;
			buffer[i] = s->getWord()[j];
			buffer[j] = s->getWord()[i];
			SuggestionGeneratorCaseChange::suggestForBuffer(voikkoOptions,
			    s, buffer, s->getWordLength());
			buffer[i] = s->getWord()[i];
			buffer[j] = s->getWord()[j];
		}
	}
	delete[] buffer;
}

}}}
