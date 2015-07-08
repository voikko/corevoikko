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
 * Portions created by the Initial Developer are Copyright (C) 2015
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

#include "spellchecker/VfstSuggestion.hpp"
#include "utils/StringUtils.hpp"
#include "setup/setup.hpp"
#include <map>
#include <queue>

using namespace std;
using namespace libvoikko::utils;

namespace libvoikko { namespace spellchecker { namespace suggestion {

static const int BUFFER_SIZE = 2000;

struct WeightedSuggestion {

	wchar_t * suggestion;
	int weight;
	
	bool operator<(const WeightedSuggestion & right) const {
		return weight > right.weight;
	}
};

VfstSuggestion::VfstSuggestion(const fst::WeightedTransducer * acceptor, const string & directoryName) throw(setup::DictionaryException):
                acceptor(acceptor) {
	string errFile = directoryName + "/err.vfst";
	errorModel = new fst::WeightedTransducer(errFile.c_str());
	acceptorConf = new fst::WeightedConfiguration(acceptor->getFlagDiacriticFeatureCount(), BUFFER_SIZE);
	errorModelConf = new fst::WeightedConfiguration(errorModel->getFlagDiacriticFeatureCount(), BUFFER_SIZE);
	acceptorBuffer = new char[BUFFER_SIZE];
	errorModelBuffer = new char[BUFFER_SIZE];
}

void VfstSuggestion::generate(SuggestionStatus * s) const {
	s->setMaxCost(100); // not actually used
	size_t wlen = s->getWordLength();
	char * wordUtf = StringUtils::utf8FromUcs4(s->getWord(), wlen);
	int16_t acceptorWeight;
	int16_t errorModelWeight;
	map<string, int> suggestionWeights;
	if (errorModel->prepare(errorModelConf, wordUtf, wlen)) {
		while (!s->shouldAbort() && errorModel->next(errorModelConf, errorModelBuffer, BUFFER_SIZE, &errorModelWeight)) {
			if (acceptor->prepare(acceptorConf, errorModelBuffer, strlen(errorModelBuffer))) {
				int firstNotReachedPosition;
				if (acceptor->next(acceptorConf, acceptorBuffer, BUFFER_SIZE, &acceptorWeight, &firstNotReachedPosition)) {
					string suggStr(errorModelBuffer);
					int weight = acceptorWeight + errorModelWeight;
					if (suggestionWeights.find(suggStr) != suggestionWeights.end()) {
						suggestionWeights[suggStr] = min(suggestionWeights[suggStr], weight);
					}
					else {
						suggestionWeights[suggStr] = weight;
					}
				}
				else {
					errorModel->backtrackToOutputDepth(errorModelConf, firstNotReachedPosition);
				}
			}
		}
	}
	delete[] wordUtf;
	
	priority_queue<WeightedSuggestion> queue;
	for (map<string, int>::const_iterator it = suggestionWeights.begin(); it != suggestionWeights.end(); ++it) {
		WeightedSuggestion sugg;
		sugg.suggestion = StringUtils::ucs4FromUtf8(it->first.c_str());
		sugg.weight = it->second;
		queue.push(sugg);
	}
	
	while (!queue.empty()) {
		WeightedSuggestion sugg = queue.top();
		queue.pop();
		s->addSuggestion(sugg.suggestion, sugg.weight);
	}
}

void VfstSuggestion::terminate() {
	delete[] errorModelBuffer;
	delete[] acceptorBuffer;
	delete errorModelConf;
	delete acceptorConf;
	errorModel->terminate();
	delete errorModel;
}

} } }
