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
 * The Initial Developer of the Original Code is Flammie Pirinen <flammie@iki.fi>.
 * Portions created by the Initial Developer are Copyright (C) 2010
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

#include "spellchecker/HfstSuggestion.hpp"
#include "utils/StringUtils.hpp"
#include "utils/utils.hpp"
#include "setup/setup.hpp"
#include "character/SimpleChar.hpp"

#include <fstream>
#include <set>
#include <string>

#include <ZHfstOspeller.h>

using namespace std;
using namespace libvoikko::utils;

using hfst_ol::ZHfstOspeller;

namespace libvoikko { namespace spellchecker { namespace suggestion {

HfstSuggestion::HfstSuggestion(hfst_ol::ZHfstOspeller * speller) throw(setup::DictionaryException) :
	speller_(speller) { }

void HfstSuggestion::generate(SuggestionStatus * s) const {
	s->setMaxCost(s->getMaxSuggestionCount());
	size_t wlen = s->getWordLength();
	char * wordUtf8 = StringUtils::utf8FromUcs4(s->getWord(), wlen);
	set<wstring> allSuggs;
	bool checkUppercasing = (voikko_casetype(s->getWord(), wlen) == CT_FIRST_UPPER);
	hfst_ol::CorrectionQueue corrections = speller_->suggest(wordUtf8);
	while (corrections.size() > 0 && !s->shouldAbort()) {
		const char * sugUtf8 = corrections.top().first.c_str();
		wchar_t * sugU4 = StringUtils::ucs4FromUtf8(sugUtf8, strlen(sugUtf8));
		// HFST speller may return the same suggestion in lower and uppercase form for uppercase words.
		// If this happens we want to drop the second one as it would lead to duplicate suggestion after
		// case correction.
		if (checkUppercasing) {
			wchar_t * uppercased = StringUtils::copy(sugU4);
			uppercased[0] = character::SimpleChar::upper(uppercased[0]);
			pair<set<wstring>::iterator,bool> inserted = allSuggs.insert(wstring(uppercased));
			delete[] uppercased;
			if (!inserted.second) {
				delete[] sugU4;
				corrections.pop();
				continue;
			}
		}
		int weight = (int) (1000.0 * corrections.top().second);
		s->addSuggestion(sugU4, weight + s->getSuggestionCount() + 1);
		s->charge();
		corrections.pop();
	}
	delete[] wordUtf8;
}

void HfstSuggestion::terminate() {
	// do nothing, HfstSpeller manages all resources
}

} } }

// vim: set noexpandtab ts=4:
