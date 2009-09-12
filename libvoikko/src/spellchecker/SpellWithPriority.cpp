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

#include "spellchecker/SpellWithPriority.hpp"
#include "spellchecker/SpellUtils.hpp"
#include "morphology/AnalyzerFactory.hpp"
#include <list>

using namespace std;
using namespace libvoikko::morphology;

namespace libvoikko { namespace spellchecker {

spellresult SpellWithPriority::spellWithPriority(const wchar_t * word, size_t len, int * prio) {
	const Analyzer * analyzer = AnalyzerFactory::getAnalyzer();
	list<Analysis *> * analyses = analyzer->analyze(word, len);
	if (prio != 0) *prio = 0;
	
	if (analyses->empty()) {
		Analyzer::deleteAnalyses(analyses);
		return SPELL_FAILED;
	}
	
	spellresult best_result = SPELL_FAILED;
	list<Analysis *>::const_iterator it = analyses->begin();
	while (it != analyses->end()) {
		const wchar_t * structure = (*it)->getValue("STRUCTURE");
		spellresult result = SpellUtils::matchWordAndAnalysis(word, len, structure);
		if (best_result == SPELL_FAILED || best_result > result) {
			best_result = result;
			if (prio != 0) {
				*prio = 0;
				for (size_t j = 0; structure[j] != L'\0'; j++) {
					if (structure[j] == L'=') {
						(*prio)++;
					}
				}
			}
		}
		if (best_result == SPELL_OK) {
			break;
		}
		it++;
	}
	Analyzer::deleteAnalyses(analyses);
	
	if (prio != 0) {
		if (best_result == SPELL_CAP_FIRST) {
			(*prio) += 1;
		}
		else if (best_result == SPELL_CAP_ERROR) {
			(*prio) += 2;
		}
	}
	return best_result;
}
    
}}
