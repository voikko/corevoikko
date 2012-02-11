/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2012 Harri Pitkänen <hatapitk@iki.fi>
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

#include "spellchecker/suggestion/SuggestionGeneratorMultiReplacement.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorCaseChange.hpp"
#include <cwchar>
#include "character/SimpleChar.hpp"

using namespace libvoikko::character;

namespace libvoikko { namespace spellchecker { namespace suggestion {

SuggestionGeneratorMultiReplacement::SuggestionGeneratorMultiReplacement(
	const wchar_t * replacements, int replaceCount, morphology::Analyzer * morAnalyzer) :
	replacements(replacements), replaceCount(replaceCount), morAnalyzer(morAnalyzer) { }

void SuggestionGeneratorMultiReplacement::generate(SuggestionStatus * s) const {
	wchar_t * buffer = new wchar_t[s->getWordLength() + 1];
	wcsncpy(buffer, s->getWord(), s->getWordLength());
	buffer[s->getWordLength()] = L'\0';
	doGenerate(s, buffer, buffer, replaceCount);
	delete[] buffer;
}

void SuggestionGeneratorMultiReplacement::doGenerate(SuggestionStatus * s, wchar_t * wordBuffer,
		wchar_t * buffer, int remainingReplacements) const {
	for (const wchar_t * i = replacements; *i != L'\0'; i += 2) {
		wchar_t from = *i;
		wchar_t to = *(i + 1);
		for (wchar_t * pos = wcschr(buffer, from); pos != 0; pos = wcschr(pos+1, from)) {
			*pos = to;
			if (remainingReplacements == 1) {
				SuggestionGeneratorCaseChange::suggestForBuffer(morAnalyzer,
				    s, wordBuffer, s->getWordLength());
			}
			else {
				doGenerate(s, wordBuffer, pos, remainingReplacements - 1);
			}
			if (s->shouldAbort()) {
				return;
			}
			*pos = from;
		}
	}
}

}}}
