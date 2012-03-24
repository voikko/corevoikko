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
 * Portions created by the Initial Developer are Copyright (C) 2012
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

#include "spellchecker/suggestion/SuggestionGeneratorDeleteTwo.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorCaseChange.hpp"
#include "character/SimpleChar.hpp"
#include <cwchar>

using namespace libvoikko::character;

namespace libvoikko { namespace spellchecker { namespace suggestion {

SuggestionGeneratorDeleteTwo::SuggestionGeneratorDeleteTwo(morphology::Analyzer * morAnalyzer) :
		morAnalyzer(morAnalyzer) {}

void SuggestionGeneratorDeleteTwo::generate(SuggestionStatus * s) const {
	if (s->getWordLength() < 6) {
		return;
	}
	wchar_t * buffer = new wchar_t[s->getWordLength() - 2];
	for (size_t i = 0; i < s->getWordLength() - 3 && !s->shouldAbort(); i++) {
		if (wcsncmp(s->getWord() + i, s->getWord() + (i + 2), 2) == 0) {
			wcsncpy(buffer, s->getWord(), i);
			wcsncpy(buffer + i, s->getWord() + (i + 2), s->getWordLength() - (i + 2));
			SuggestionGeneratorCaseChange::suggestForBuffer(morAnalyzer, s, buffer,
			    s->getWordLength() - 2);
		}
	}
	delete[] buffer;
}

}}}
