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

#include "spellchecker/suggestion/SuggestionGeneratorCaseChange.hpp"
#include "spellchecker/SpellWithPriority.hpp"
#include "character/charset.hpp"
#include "utils/utils.hpp"
#include <cstdlib>
#include <cwchar>
#include <cwctype>

using namespace libvoikko::morphology;
using namespace std;

namespace libvoikko { namespace spellchecker { namespace suggestion {

void SuggestionGeneratorCaseChange::generate(voikko_options_t * voikkoOptions,
	                            SuggestionStatus * s) const {
	suggestForBuffer(voikkoOptions, s, s->getWord(), s->getWordLength());
}

void SuggestionGeneratorCaseChange::suggestForBuffer(voikko_options_t * voikkoOptions,
		SuggestionStatus * s, const wchar_t * word, size_t wlen) {
	wchar_t * newsugg;
	int prio;
	if (s->shouldAbort()) {
		return;
	}
	spellresult sres = SpellWithPriority::spellWithPriority(voikkoOptions, word, wlen, &prio);
	s->charge();
	switch (sres) {
		case SPELL_FAILED:
			return;
		case SPELL_OK:
			newsugg = new wchar_t[wlen + 1];
			wcscpy(newsugg, word);
			s->addSuggestion(newsugg, prio);
			return;
		case SPELL_CAP_FIRST:
			newsugg = new wchar_t[wlen + 1];
			newsugg[0] = towupper(word[0]);
			wcsncpy(newsugg + 1, word + 1, wlen - 1);
			newsugg[wlen] = L'\0';
			s->addSuggestion(newsugg, prio);
			return;
		case SPELL_CAP_ERROR:
			Analyzer * analyzer = voikkoOptions->morAnalyzer;
			list<Analysis *> * analyses = analyzer->analyze(word, wlen);
			s->charge();
			if (analyses->empty()) {
				Analyzer::deleteAnalyses(analyses);
				return;
			}
			const wchar_t * structure =
			    (*analyses->begin())->getValue("STRUCTURE");
			newsugg = new wchar_t[wlen + 1];
			wcscpy(newsugg, word);
			size_t j = 0;
			for (size_t i = 0; i < wlen; i++) {
				while (structure[j] == L'=') {
					j++;
				}
				if (structure[j] == L'\0') {
					break;
				}
				if (structure[j] == L'i' || structure[j] == L'j') {
					newsugg[i] = towupper(newsugg[i]);
				}
				else if (structure[j] == L'p' || structure[j] == L'q') {
					newsugg[i] = simpleLower(newsugg[i]);
				}
				j++;
			}
			Analyzer::deleteAnalyses(analyses);
			s->addSuggestion(newsugg, prio);
			return;
	}
}

}}}
