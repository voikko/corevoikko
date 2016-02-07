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

#include "morphology/VfstAnalyzer.hpp"
#include "setup/DictionaryException.hpp"
#include "utils/StringUtils.hpp"
#include "character/SimpleChar.hpp"
#include "utils/utils.hpp"
#include "voikko_defines.h"
#include <sstream>
#include <iomanip>
#include <cwchar>
#include <cmath>

using namespace libvoikko::character;
using namespace libvoikko::utils;
using namespace libvoikko::fst;

using std::list;
using std::setprecision;
using std::string;
using std::stringstream;

namespace libvoikko { namespace morphology {

static const int BUFFER_SIZE = 2000;
static const int MAX_ANALYSIS_COUNT = 100;

VfstAnalyzer::VfstAnalyzer(const string & directoryName) throw(setup::DictionaryException) {
	string morFile = directoryName + "/mor.vfst";
	// XXX: could handle different types of transducers
	transducer = new WeightedTransducer(morFile.c_str());
	configuration = new WeightedConfiguration(transducer->getFlagDiacriticFeatureCount(), BUFFER_SIZE);
	outputBuffer = new wchar_t[BUFFER_SIZE];
}

list<Analysis *> * VfstAnalyzer::analyze(const char * word, bool fullMorphology) {
	wchar_t * wordUcs4 = StringUtils::ucs4FromUtf8(word);
	list<Analysis *> * result = analyze(wordUcs4, wcslen(wordUcs4), fullMorphology);
	delete[] wordUcs4;
	return result;
}

static double logWeightToProb(int16_t logWeight) {
	return exp(-0.01 * (double) logWeight);
}

list<Analysis *> * VfstAnalyzer::analyze(const wchar_t * word, size_t wlen, bool fullMorphology) {
	if (wlen > LIBVOIKKO_MAX_WORD_CHARS) {
		return new list<Analysis *>();
	}
	
	wchar_t * wordLowerUcs4 = new wchar_t[wlen];
	memcpy(wordLowerUcs4, word, wlen * sizeof(wchar_t));
	voikko_set_case(CT_ALL_LOWER, wordLowerUcs4, wlen);
	
	list<Analysis *> * analysisList = new list<Analysis *>();
	if (transducer->prepare(configuration, wordLowerUcs4, wlen)) {
		int analysisCount = 0;
		int16_t weight;
		while (++analysisCount < MAX_ANALYSIS_COUNT && transducer->next(configuration, outputBuffer, BUFFER_SIZE, &weight)) {
			Analysis * analysis = new Analysis();
			if (fullMorphology) {
				analysis->addAttribute(Analysis::Key::FSTOUTPUT, StringUtils::copy(outputBuffer));
			}
			stringstream ss;
			ss << setprecision(9) << logWeightToProb(weight);
			string weightStr = ss.str();
			analysis->addAttribute(Analysis::Key::WEIGHT, StringUtils::ucs4FromUtf8(weightStr.c_str()));
			analysisList->push_back(analysis);
		}
	}
	
	delete[] wordLowerUcs4;
	return analysisList;
}

void VfstAnalyzer::terminate() {
	delete[] outputBuffer;
	delete configuration;
	transducer->terminate();
	delete transducer;
}

} }
