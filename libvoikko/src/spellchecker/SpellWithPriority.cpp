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

static spellresult handleAnalysis(const wchar_t * word, size_t len, int &prio,
                                  const Analysis * analysis) {
	const wchar_t * structure = analysis->getValue("STRUCTURE");
	spellresult result = SpellUtils::matchWordAndAnalysis(word, len, structure);
	prio = 0;
	for (size_t j = 0; structure[j] != L'\0'; j++) {
		if (structure[j] == L'=') {
			++prio;
		}
	}
	return result;
}
    
spellresult SpellWithPriority::spellWithPriority(const wchar_t * word, size_t len, int * prio) {
	const Analyzer * analyzer = AnalyzerFactory::getAnalyzer();
	list<Analysis *> * analyses = analyzer->analyze(word, len);
	*prio = 0;
	
	if (analyses->empty()) {
		Analyzer::deleteAnalyses(analyses);
		return SPELL_FAILED;
	}
	
	spellresult bestResult = SPELL_FAILED;
	int bestPrio = 0;
	list<Analysis *>::const_iterator it = analyses->begin();
	while (it != analyses->end()) {
		int currentPrio = 0;
		spellresult currentResult = handleAnalysis(word, len, currentPrio, *it);
		if (bestResult == SPELL_FAILED || bestResult > currentResult) {
			bestResult = currentResult;
			bestPrio = currentPrio;
		}
		else if (bestResult == currentResult) {
			if (currentPrio < bestPrio) {
				bestPrio = currentPrio;
			}
		}
		it++;
	}
	Analyzer::deleteAnalyses(analyses);
	*prio = bestPrio;
	
	if (bestResult == SPELL_CAP_FIRST) {
		(*prio) += 1;
	}
	else if (bestResult == SPELL_CAP_ERROR) {
		(*prio) += 2;
	}
	return bestResult;
}
    
}}
