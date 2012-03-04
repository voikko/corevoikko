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

spellresult SpellWithPriority::spellWithPriority(Analyzer * morAnalyzer,
	                       const wchar_t * word, size_t len, int * prio) {
	list<Analysis *> * analyses = morAnalyzer->analyze(word, len);
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
		++it;
	}
	Analyzer::deleteAnalyses(analyses);
	*prio = bestPrio;
	
	return bestResult;
}
    
}}
