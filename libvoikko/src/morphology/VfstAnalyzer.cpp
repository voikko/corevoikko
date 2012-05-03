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
#include "voikko_defines.h"

using namespace std;
using namespace libvoikko::utils;
using namespace libvoikko::fst;

namespace libvoikko { namespace morphology {

static const int BUFFER_SIZE = 2000;

VfstAnalyzer::VfstAnalyzer(const string & directoryName) throw(setup::DictionaryException) {
	string morFile = directoryName + "/mor.vfst";
	transducer = new Transducer(morFile.c_str());
	configuration = new Configuration(transducer->getFlagDiacriticFeatureCount(), BUFFER_SIZE);
	outputBuffer = new char[BUFFER_SIZE];
}

list<Analysis *> * VfstAnalyzer::analyze(const wchar_t * word) {
	return analyze(word, wcslen(word));
}

list<Analysis *> * VfstAnalyzer::analyze(const wchar_t * word, size_t wlen) {
	if (wlen > LIBVOIKKO_MAX_WORD_CHARS) {
		return new list<Analysis *>();
	}
	char * wordUtf8 = StringUtils::utf8FromUcs4(word, wlen);
	list<Analysis *> * result = analyze(wordUtf8);
	delete[] wordUtf8;
	return result;
}

static wchar_t * parseStructure(const wchar_t * fstOutput, size_t wlen) {
	wchar_t * structure = new wchar_t[wlen * 2 + 1];
	structure[0] = L'=';
	size_t outputLen = wcslen(fstOutput);
	size_t structurePos = 1;
	size_t charsMissing = wlen;
	for (size_t i = 0; i + 8 < outputLen; i++) {
		if (wcsncmp(fstOutput + i, L"[Xr]", 4) == 0) {
			i += 4;
			while (fstOutput[i] != L'[') {
				structure[structurePos++] = fstOutput[i];
				if (fstOutput[i] != L'=') {
					charsMissing--;
				}
				i++;
			}
		}
	}
	while (charsMissing) {
		structure[structurePos++] = L'p';
		charsMissing--;
	}
	structure[structurePos] = L'\0';
	return structure;
}

list<Analysis *> * VfstAnalyzer::analyze(const char * word) {
	size_t wlen = strlen(word);
	if (wlen > LIBVOIKKO_MAX_WORD_CHARS) {
		return new list<Analysis *>();
	}
	list<Analysis *> * analysisList = new list<Analysis *>();
	if (transducer->prepare(configuration, word, wlen)) {
		wchar_t * wordUcs4 = StringUtils::ucs4FromUtf8(word, wlen);
		size_t ucsLen = wcslen(wordUcs4);
		while (transducer->next(configuration, outputBuffer, BUFFER_SIZE)) {
			Analysis * analysis = new Analysis();
			wchar_t * fstOutput = StringUtils::ucs4FromUtf8(outputBuffer);
			analysis->addAttribute("STRUCTURE", parseStructure(fstOutput, ucsLen));
			analysis->addAttribute("CLASS", utils::StringUtils::copy(L"none"));
			analysis->addAttribute("SIJAMUOTO", utils::StringUtils::copy(L"none"));
			analysis->addAttribute("FSTOUTPUT", fstOutput);
			analysisList->push_back(analysis);
		}
		delete[] wordUcs4;
	}
	return analysisList;
}

void VfstAnalyzer::terminate() {
	delete[] outputBuffer;
	delete configuration;
	transducer->terminate();
	delete transducer;
}

} }
