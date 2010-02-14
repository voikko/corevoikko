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

#include "spellchecker/suggestion/SuggestionGeneratorSplitWord.hpp"
#include "spellchecker/SpellWithPriority.hpp"
#include <cwchar>
#include "character/SimpleChar.hpp"

using namespace libvoikko::character;

namespace libvoikko { namespace spellchecker { namespace suggestion {

void SuggestionGeneratorSplitWord::generate(voikko_options_t * voikkoOptions, SuggestionStatus * s) const {
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
		    voikkoOptions, part1, splitind, &prio_total);
		s->charge();
		if (part1_res == SPELL_OK || part1_res == SPELL_CAP_FIRST) {
			spellresult part2_res = SpellWithPriority::spellWithPriority(
			    voikkoOptions, s->getWord() + splitind,
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
