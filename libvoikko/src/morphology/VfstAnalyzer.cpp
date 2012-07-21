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
#include "utils/utils.hpp"
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

list<Analysis *> * VfstAnalyzer::analyze(const char * word) {
	wchar_t * wordUcs4 = StringUtils::ucs4FromUtf8(word);
	list<Analysis *> * result = analyze(wordUcs4);
	delete[] wordUcs4;
	return result;
}

static wchar_t * parseStructure(const wchar_t * fstOutput, size_t wlen) {
	wchar_t * structure = new wchar_t[wlen * 2 + 1];
	structure[0] = L'=';
	size_t outputLen = wcslen(fstOutput);
	size_t structurePos = 1;
	size_t charsMissing = wlen;
	bool defaultTitleCase = false;
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
		else if (wcsncmp(fstOutput + i, L"[Le", 3) == 0) {
			defaultTitleCase = true;
			i += 4;
		}
	}
	while (charsMissing) {
		if (defaultTitleCase) {
			structure[structurePos++] = L'i';
			defaultTitleCase = false;
		}
		else {
			structure[structurePos++] = L'p';
		}
		charsMissing--;
	}
	structure[structurePos] = L'\0';
	return structure;
}

static wchar_t * parseClass(const wchar_t * fstOutput, size_t fstLen) {
	if (fstLen < 5) {
		return StringUtils::copy(L"none");
	}
	for (size_t i = fstLen - 4; i >= 1; i--) {
		if (wcsncmp(fstOutput + i - 1, L"[L", 2) != 0) {
			continue;
		}
		if (fstOutput[i + 1] == L'n') {
			return StringUtils::copy(L"nimisana");
		}
		if (fstOutput[i + 1] == L'h') {
			return StringUtils::copy(L"huudahdussana");
		}
		if (wcsncmp(fstOutput + i + 1, L"ee", 2) == 0) {
			return StringUtils::copy(L"etunimi");
		}
		if (wcsncmp(fstOutput + i + 1, L"es", 2) == 0) {
			return StringUtils::copy(L"sukunimi");
		}
		if (wcsncmp(fstOutput + i + 1, L"ep", 2) == 0) {
			return StringUtils::copy(L"paikannimi");
		}
		if (wcsncmp(fstOutput + i + 1, L"em", 2) == 0) {
			return StringUtils::copy(L"nimi");
		}
	}
	return StringUtils::copy(L"none");
}

static void parseBasicAttributes(Analysis * analysis, const wchar_t * fstOutput, size_t fstLen) {
	for (size_t i = fstLen - 1; i >= 2; i--) {
		if (fstOutput[i] == L']') {
			size_t j = i;
			while (j >= 1) {
				j--;
				if (fstOutput[j] == L'[') {
					if (fstOutput[j + 1] == L'S') {
						const char * attr = "SIJAMUOTO";
						const wchar_t * muoto = 0;
						size_t sijaLen = i - j - 2;
						if (sijaLen == 1) {
							if (fstOutput[j + 2] == L'n') {
								muoto = L"nimento";
							}
						}
						else if (sijaLen == 3) {
							if (wcsncmp(fstOutput + j + 2, L"ine", 3) == 0) {
								muoto = L"sisaolento";
							}
						}
						if (muoto) {
							analysis->addAttribute(attr, StringUtils::copy(muoto));
						}
					}
					break;
				}
			}
			if (j < 3) {
				return;
			}
			i = j;
		}
	}
}

list<Analysis *> * VfstAnalyzer::analyze(const wchar_t * word, size_t wlen) {
	if (wlen > LIBVOIKKO_MAX_WORD_CHARS) {
		return new list<Analysis *>();
	}
	
	wchar_t * wordLowerUcs4 = new wchar_t[wlen];
	memcpy(wordLowerUcs4, word, wlen * sizeof(wchar_t));
	voikko_set_case(CT_ALL_LOWER, wordLowerUcs4, wlen);
	char * wordLower = StringUtils::utf8FromUcs4(wordLowerUcs4, wlen);
	delete[] wordLowerUcs4;
	
	list<Analysis *> * analysisList = new list<Analysis *>();
	if (transducer->prepare(configuration, wordLower, wlen)) {
		while (transducer->next(configuration, outputBuffer, BUFFER_SIZE)) {
			Analysis * analysis = new Analysis();
			wchar_t * fstOutput = StringUtils::ucs4FromUtf8(outputBuffer);
			size_t fstLen = wcslen(fstOutput);
			analysis->addAttribute("STRUCTURE", parseStructure(fstOutput, wlen));
			analysis->addAttribute("CLASS", parseClass(fstOutput, fstLen));
			parseBasicAttributes(analysis, fstOutput, fstLen);
			analysis->addAttribute("FSTOUTPUT", fstOutput);
			analysisList->push_back(analysis);
		}
	}
	
	delete[] wordLower;
	return analysisList;
}

void VfstAnalyzer::terminate() {
	delete[] outputBuffer;
	delete configuration;
	transducer->terminate();
	delete transducer;
}

} }
