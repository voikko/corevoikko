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
 * Portions created by the Initial Developer are Copyright (C) 2006 - 2010
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

#include "spellchecker/suggestion/SuggestionGeneratorSplitWord.hpp"
#include "spellchecker/SpellWithPriority.hpp"
#include <cwchar>
#include "character/SimpleChar.hpp"

using namespace libvoikko::character;

namespace libvoikko { namespace spellchecker { namespace suggestion {

SuggestionGeneratorSplitWord::SuggestionGeneratorSplitWord(morphology::Analyzer * morAnalyzer) :
		morAnalyzer(morAnalyzer) {}

void SuggestionGeneratorSplitWord::generate(SuggestionStatus * s) const {
	int prio_part;
	int prio_total;
	wchar_t * part1 = new wchar_t[s->getWordLength() + 1];
	wcsncpy(part1, s->getWord(), s->getWordLength());
	part1[s->getWordLength()] = L'\0';

	for (size_t splitind = s->getWordLength() - 2; splitind >= 2; splitind--) {
		/* Do not split a word if there is a hyphen before the last character of part1
		   or after the first character of part2. Do not suggest splitting immediately
		   before or after a hyphen either. */
		if (s->getWord()[splitind-2] == L'-' || s->getWord()[splitind-1] == L'-' ||
		    s->getWord()[splitind]   == L'-' || s->getWord()[splitind+1] == L'-') continue;
		part1[splitind] = L'\0';
		spellresult part1_res = SpellWithPriority::spellWithPriority(
		    morAnalyzer, part1, splitind, &prio_total);
		s->charge();
		if (part1_res == SPELL_OK || part1_res == SPELL_CAP_FIRST) {
			spellresult part2_res = SpellWithPriority::spellWithPriority(
			    morAnalyzer, s->getWord() + splitind,
			    s->getWordLength() - splitind, &prio_part);
			prio_total += prio_part;
			s->charge();
			if (part2_res == SPELL_OK || part2_res == SPELL_CAP_FIRST) {
				wchar_t * suggestion = new wchar_t[s->getWordLength() + 2];
				wcsncpy(suggestion, s->getWord(), splitind);
				if (part1_res == SPELL_CAP_FIRST)
					suggestion[0] = SimpleChar::upper(suggestion[0]);
				suggestion[splitind] = L' ';
				wcsncpy(suggestion + (splitind + 1), s->getWord() + splitind,
				        s->getWordLength() - splitind + 1);
				if (part2_res == SPELL_CAP_FIRST)
					suggestion[splitind+1] = SimpleChar::upper(suggestion[splitind+1]);
				s->addSuggestion(suggestion, prio_total);
			}
		}
		if (s->shouldAbort()) break;
	}

	delete[] part1;
}

}}}
