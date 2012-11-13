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
	
	classMap.insert(std::make_pair(L"n", L"nimisana"));
	classMap.insert(std::make_pair(L"l", L"laatusana"));
	classMap.insert(std::make_pair(L"nl", L"nimisana_laatusana"));
	classMap.insert(std::make_pair(L"h", L"huudahdussana"));
	classMap.insert(std::make_pair(L"ee", L"etunimi"));
	classMap.insert(std::make_pair(L"es", L"sukunimi"));
	classMap.insert(std::make_pair(L"ep", L"paikannimi"));
	classMap.insert(std::make_pair(L"em", L"nimi"));
	classMap.insert(std::make_pair(L"t", L"teonsana"));
	
	sijamuotoMap.insert(std::make_pair(L"n", L"nimento"));
	sijamuotoMap.insert(std::make_pair(L"g", L"omanto"));
	sijamuotoMap.insert(std::make_pair(L"p", L"osanto"));
	sijamuotoMap.insert(std::make_pair(L"es", L"olento"));
	sijamuotoMap.insert(std::make_pair(L"tr", L"tulento"));
	sijamuotoMap.insert(std::make_pair(L"ine", L"sisaolento"));
	sijamuotoMap.insert(std::make_pair(L"ela", L"sisaeronto"));
	sijamuotoMap.insert(std::make_pair(L"ill", L"sisatulento"));
	sijamuotoMap.insert(std::make_pair(L"ade", L"ulkoolento"));
	sijamuotoMap.insert(std::make_pair(L"abl", L"ulkoeronto"));
	sijamuotoMap.insert(std::make_pair(L"all", L"ulkotulento"));
	sijamuotoMap.insert(std::make_pair(L"ab", L"vajanto"));
	sijamuotoMap.insert(std::make_pair(L"ko", L"seuranto"));
	sijamuotoMap.insert(std::make_pair(L"in", L"keinonto"));
	
	moodMap.insert(std::make_pair(L"n1", L"A-infinitive"));
	moodMap.insert(std::make_pair(L"n2", L"E-infinitive"));
	moodMap.insert(std::make_pair(L"n3", L"MA-infinitive"));
	moodMap.insert(std::make_pair(L"t", L"indicative"));
	
	numberMap.insert(std::make_pair(L"y", L"singular"));
	numberMap.insert(std::make_pair(L"m", L"plural"));
	
	personMap.insert(std::make_pair(L"1", L"1"));
	personMap.insert(std::make_pair(L"2", L"2"));
	personMap.insert(std::make_pair(L"3", L"3"));
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

static void createDefaultStructure(size_t charsMissing, bool & defaultTitleCase,
                                   wchar_t * structure, size_t & structurePos) {
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
}

static wchar_t * parseStructure(const wchar_t * fstOutput, size_t wlen) {
	wchar_t * structure = new wchar_t[wlen * 2 + 1];
	structure[0] = L'=';
	size_t outputLen = wcslen(fstOutput);
	size_t structurePos = 1;
	size_t charsMissing = wlen;
	size_t charsSeen = 0;
	size_t charsFromDefault = 0;
	bool defaultTitleCase = false;
	for (size_t i = 0; i + 8 < outputLen; i++) {
		if (wcsncmp(fstOutput + i, L"[Bc]", 4) == 0) {
			i += 3;
			if (charsSeen > charsFromDefault) {
				createDefaultStructure(charsSeen - charsFromDefault, defaultTitleCase, structure, structurePos);
				charsMissing -= (charsSeen - charsFromDefault);
			}
			charsSeen = 0;
			charsFromDefault = 0;
			structure[structurePos++] = L'=';
		}
		else if (wcsncmp(fstOutput + i, L"[Xr]", 4) == 0) {
			defaultTitleCase = false;
			i += 4;
			while (fstOutput[i] != L'[') {
				structure[structurePos++] = fstOutput[i];
				if (fstOutput[i] != L'=') {
					charsFromDefault++;
					charsMissing--;
				}
				i++;
			}
			i += 2; // X]
		}
		else if (wcsncmp(fstOutput + i, L"[Le", 3) == 0) {
			defaultTitleCase = true;
			i += 4;
		}
		else if (wcsncmp(fstOutput + i, L"[Xp", 3) == 0) {
			i += 4;
			while (fstOutput[i] != L'[') {
				i++;
			}
			i += 2;
		}
		else if (fstOutput[i] == L'[') {
			while (fstOutput[i] != L']') {
				i++;
			}
		}
		else {
			charsSeen++;
		}
	}
	createDefaultStructure(charsMissing, defaultTitleCase, structure, structurePos);
	structure[structurePos] = L'\0';
	return structure;
}

static wchar_t * getAttributeFromMap(map<wstring, wstring> & theMap, const wchar_t * keyStart, size_t keyLen) {
	map<wstring, wstring>::const_iterator mapIterator = theMap.find(wstring(keyStart, keyLen));
	if (mapIterator == theMap.end()) {
		return 0;
	}
	return StringUtils::copy((*mapIterator).second.c_str());
}

void VfstAnalyzer::parseBasicAttributes(Analysis * analysis, const wchar_t * fstOutput, size_t fstLen) {
	for (size_t i = fstLen - 1; i >= 2; i--) {
		if (fstOutput[i] == L']') {
			size_t j = i;
			while (j >= 1) {
				j--;
				if (fstOutput[j] == L'[') {
					if (fstOutput[j + 1] == L'L') {
						size_t sijaLen = i - j - 2;
						wchar_t * muoto = getAttributeFromMap(classMap, fstOutput + j + 2, sijaLen);
						if (muoto) {
							analysis->addAttribute("CLASS", muoto);
						}
					}
					else if (fstOutput[j + 1] == L'N') {
						size_t sijaLen = i - j - 2;
						wchar_t * muoto = getAttributeFromMap(numberMap, fstOutput + j + 2, sijaLen);
						if (muoto) {
							analysis->addAttribute("NUMBER", muoto);
						}
					}
					else if (fstOutput[j + 1] == L'P') {
						size_t sijaLen = i - j - 2;
						wchar_t * muoto = getAttributeFromMap(personMap, fstOutput + j + 2, sijaLen);
						if (muoto) {
							analysis->addAttribute("PERSON", muoto);
						}
					}
					else if (fstOutput[j + 1] == L'S') {
						size_t sijaLen = i - j - 2;
						wchar_t * muoto = getAttributeFromMap(sijamuotoMap, fstOutput + j + 2, sijaLen);
						if (muoto) {
							analysis->addAttribute("SIJAMUOTO", muoto);
						}
					}
					else if (fstOutput[j + 1] == L'T') {
						size_t sijaLen = i - j - 2;
						wchar_t * muoto = getAttributeFromMap(moodMap, fstOutput + j + 2, sijaLen);
						if (muoto) {
							analysis->addAttribute("MOOD", muoto);
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
	if (transducer->prepare(configuration, wordLower, strlen(wordLower))) {
		while (transducer->next(configuration, outputBuffer, BUFFER_SIZE)) {
			Analysis * analysis = new Analysis();
			wchar_t * fstOutput = StringUtils::ucs4FromUtf8(outputBuffer);
			size_t fstLen = wcslen(fstOutput);
			analysis->addAttribute("STRUCTURE", parseStructure(fstOutput, wlen));
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
