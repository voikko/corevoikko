/* Libvoikko: Finnish spellchecker and hyphenator library
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

#include "spellchecker/suggestion/SuggestionGeneratorSoftHyphens.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorCaseChange.hpp"
#include <cstdlib>
#include <cwchar>

using namespace libvoikko::morphology;
using namespace std;

namespace libvoikko { namespace spellchecker { namespace suggestion {

SuggestionGeneratorSoftHyphens::SuggestionGeneratorSoftHyphens(Analyzer * morAnalyzer)
		: morAnalyzer(morAnalyzer) {}

void SuggestionGeneratorSoftHyphens::generate(SuggestionStatus * s) const {
	const size_t wlen = s->getWordLength();
	const wchar_t * word = s->getWord();
	const wchar_t * softHyphen = wmemchr(word, L'\u00AD', wlen);
	if (softHyphen) {
		wchar_t * buffer = new wchar_t[wlen];
		size_t j = 0;
		for (size_t i = 0; i < wlen; ++i) {
			if (word[i] != L'\u00AD') {
				buffer[j++] = word[i];
			}
		}
		buffer[j] = L'\0';
		SuggestionGeneratorCaseChange::suggestForBuffer(morAnalyzer, s, buffer, j);
		delete[] buffer;
	}
}

}}}
