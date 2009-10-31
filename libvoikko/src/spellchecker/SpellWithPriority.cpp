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
#include <list>

using namespace std;
using namespace libvoikko::morphology;

namespace libvoikko { namespace spellchecker {

static int getPriorityFromNounInflection(const Analysis * analysis) {
	const wchar_t * sijamuoto = analysis->getValue("SIJAMUOTO");
	if (!sijamuoto) {
		// unknown sijamuoto
		return 4;
	}
	else if (wcscmp(sijamuoto, L"nimento") == 0) {
		return 2;
	}
	else if (wcscmp(sijamuoto, L"omanto") == 0) {
		return 3;
	}
	else if (wcscmp(sijamuoto, L"osanto") == 0) {
		return 5;
	}
	else if (wcscmp(sijamuoto, L"sisaolento") == 0) {
		return 8;
	}
	else if (wcscmp(sijamuoto, L"sisaeronto") == 0) {
		return 12;
	}
	else if (wcscmp(sijamuoto, L"sisatulento") == 0) {
		return 8;
	}
	else if (wcscmp(sijamuoto, L"ulkoolento") == 0) {
		return 12;
	}
	else if (wcscmp(sijamuoto, L"ulkoeronto") == 0) {
		return 30;
	}
	else if (wcscmp(sijamuoto, L"ulkotulento") == 0) {
		return 20;
	}
	else if (wcscmp(sijamuoto, L"olento") == 0) {
		return 20;
	}
	else if (wcscmp(sijamuoto, L"tulento") == 0) {
		return 20;
	}
	else if (wcscmp(sijamuoto, L"vajanto") == 0) {
		return 60;
	}
	else if (wcscmp(sijamuoto, L"seuranto") == 0) {
		return 60;
	}
	else if (wcscmp(sijamuoto, L"keinonto") == 0) {
		return 20;
	}
	else {
		// unknown sijamuoto
		return 4;
	}
}

static int getPriorityFromWordClassAndInflection(const Analysis * analysis) {
	const wchar_t * wordClass = analysis->getValue("CLASS");
	if (!wordClass) {
		// unknown word class
		return 4;
	}
	if (wcscmp(wordClass, L"nimisana") == 0 ||
	    wcscmp(wordClass, L"laatusana") == 0 ||
	    wcscmp(wordClass, L"nimisana_laatusana") == 0 ||
	    wcscmp(wordClass, L"asemosana") == 0 ||
	    wcscmp(wordClass, L"etunimi") == 0 ||
	    wcscmp(wordClass, L"sukunimi") == 0 ||
	    wcscmp(wordClass, L"paikannimi") == 0 ||
	    wcscmp(wordClass, L"nimi") == 0) {
		return getPriorityFromNounInflection(analysis);
	}
	else {
		// other word classes have no special handling yet
		return 4;
	}
}

static int getPriorityFromStructure(const wchar_t * structure) {
	int countParts = 0;
	for (size_t j = 0; structure[j] != L'\0'; j++) {
		if (structure[j] == L'=') {
			++countParts;
			if (countParts == 5) {
				break;
			}
		}
	}
	return 1 << (3 * (countParts - 1));
}

static int getPriorityFromSpellResult(spellresult result) {
	switch (result) {
		case SPELL_OK:
			return 1;
		case SPELL_CAP_FIRST:
			return 2;
		case SPELL_CAP_ERROR:
			return 3;
		default:
			// should not happen
			return 1;
	}
}

static spellresult handleAnalysis(const wchar_t * word, size_t len, int &prio,
                                  const Analysis * analysis) {
	prio = getPriorityFromWordClassAndInflection(analysis);
	const wchar_t * structure = analysis->getValue("STRUCTURE");
	prio *= getPriorityFromStructure(structure);
	spellresult result = SpellUtils::matchWordAndAnalysis(word, len, structure);
	prio *= getPriorityFromSpellResult(result);
	return result;
}
    
spellresult SpellWithPriority::spellWithPriority(voikko_options_t * voikkoOptions,
	                       const wchar_t * word, size_t len, int * prio) {
	list<Analysis *> * analyses = voikkoOptions->morAnalyzer->analyze(word, len);
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
	
	return bestResult;
}
    
}}
