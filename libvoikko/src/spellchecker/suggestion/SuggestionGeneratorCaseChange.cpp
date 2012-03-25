/* The contents of this file are subject to the Mozilla Public License Version 
 * 1.1 (the "License"); you may not use this file except in compliance with 
 * the License. You may obtain a copy of the License at 
 * http://www.mozilla.org/MPL/
 * 
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 * 
 * The Original Code is Libvoikko: Library of natural language processing tools.
 * The Initial Developer of the Original Code is Harri Pitkänen <hatapitk@iki.fi>.
 * Portions created by the Initial Developer are Copyright (C) 2006 - 2009
 * the Initial Developer. All Rights Reserved.
 * 
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *********************************************************************************/

#include "spellchecker/suggestion/SuggestionGeneratorCaseChange.hpp"
#include "spellchecker/SpellWithPriority.hpp"
#include "character/SimpleChar.hpp"
#include "utils/utils.hpp"
#include <cstdlib>
#include <cwchar>

using namespace libvoikko::morphology;
using namespace libvoikko::character;
using namespace std;

namespace libvoikko { namespace spellchecker { namespace suggestion {

SuggestionGeneratorCaseChange::SuggestionGeneratorCaseChange(Analyzer * morAnalyzer)
		: morAnalyzer(morAnalyzer) {}

void SuggestionGeneratorCaseChange::generate(SuggestionStatus * s) const {
	suggestForBuffer(morAnalyzer, s, s->getWord(), s->getWordLength());
}

void SuggestionGeneratorCaseChange::suggestForBuffer(Analyzer * morAnalyzer,
		SuggestionStatus * s, const wchar_t * word, size_t wlen) {
	wchar_t * newsugg;
	int prio;
	if (s->shouldAbort()) {
		return;
	}
	spellresult sres = SpellWithPriority::spellWithPriority(morAnalyzer, word, wlen, &prio);
	s->charge();
	switch (sres) {
		case SPELL_FAILED:
			return;
		case SPELL_OK:
			newsugg = new wchar_t[wlen + 1];
			wcsncpy(newsugg, word, wlen);
			newsugg[wlen] = L'\0';
			s->addSuggestion(newsugg, prio);
			return;
		case SPELL_CAP_FIRST:
			newsugg = new wchar_t[wlen + 1];
			newsugg[0] = SimpleChar::upper(word[0]);
			wcsncpy(newsugg + 1, word + 1, wlen - 1);
			newsugg[wlen] = L'\0';
			s->addSuggestion(newsugg, prio);
			return;
		case SPELL_CAP_ERROR:
			list<Analysis *> * analyses = morAnalyzer->analyze(word, wlen);
			s->charge();
			if (analyses->empty()) {
				Analyzer::deleteAnalyses(analyses);
				return;
			}
			const wchar_t * structure =
			    (*analyses->begin())->getValue("STRUCTURE");
			newsugg = new wchar_t[wlen + 1];
			wcsncpy(newsugg, word, wlen);
			newsugg[wlen] = L'\0';
			size_t j = 0;
			for (size_t i = 0; i < wlen; i++) {
				while (structure[j] == L'=') {
					j++;
				}
				if (structure[j] == L'\0') {
					break;
				}
				if (structure[j] == L'i' || structure[j] == L'j') {
					newsugg[i] = SimpleChar::upper(newsugg[i]);
				}
				else if (structure[j] == L'p' || structure[j] == L'q') {
					newsugg[i] = SimpleChar::lower(newsugg[i]);
				}
				j++;
			}
			Analyzer::deleteAnalyses(analyses);
			s->addSuggestion(newsugg, prio);
			return;
	}
}

}}}
