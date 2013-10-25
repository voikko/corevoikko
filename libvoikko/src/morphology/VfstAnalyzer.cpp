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

using namespace std;
using namespace libvoikko::character;
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
	classMap.insert(std::make_pair(L"a", L"lyhenne"));
	classMap.insert(std::make_pair(L"s", L"seikkasana"));
	classMap.insert(std::make_pair(L"u", L"lukusana"));
	classMap.insert(std::make_pair(L"r", L"asemosana"));
	classMap.insert(std::make_pair(L"c", L"sidesana"));
	
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
	sijamuotoMap.insert(std::make_pair(L"sti", L"kerrontosti"));
	sijamuotoMap.insert(std::make_pair(L"ak", L"kohdanto"));
	
	comparisonMap.insert(std::make_pair(L"c", L"comparative"));
	comparisonMap.insert(std::make_pair(L"s", L"superlative"));
	
	moodMap.insert(std::make_pair(L"n1", L"A-infinitive"));
	moodMap.insert(std::make_pair(L"n2", L"E-infinitive"));
	moodMap.insert(std::make_pair(L"n3", L"MA-infinitive"));
	moodMap.insert(std::make_pair(L"t", L"indicative"));
	moodMap.insert(std::make_pair(L"e", L"conditional"));
	moodMap.insert(std::make_pair(L"k", L"imperative"));
	moodMap.insert(std::make_pair(L"m", L"potential"));
	
	numberMap.insert(std::make_pair(L"y", L"singular"));
	numberMap.insert(std::make_pair(L"m", L"plural"));
	
	personMap.insert(std::make_pair(L"1", L"1"));
	personMap.insert(std::make_pair(L"2", L"2"));
	personMap.insert(std::make_pair(L"3", L"3"));
	personMap.insert(std::make_pair(L"4", L"4"));
	
	tenseMap.insert(std::make_pair(L"p", L"present_simple"));
	tenseMap.insert(std::make_pair(L"i", L"past_imperfective"));
	
	focusMap.insert(std::make_pair(L"kin", L"kin"));
	focusMap.insert(std::make_pair(L"kaan", L"kaan"));
	
	possessiveMap.insert(std::make_pair(L"1y", L"1s"));
	possessiveMap.insert(std::make_pair(L"2y", L"2s"));
	possessiveMap.insert(std::make_pair(L"1m", L"1p"));
	possessiveMap.insert(std::make_pair(L"2m", L"2p"));
	possessiveMap.insert(std::make_pair(L"3", L"3"));
	
	negativeMap.insert(std::make_pair(L"t", L"true"));
	negativeMap.insert(std::make_pair(L"f", L"false"));
	negativeMap.insert(std::make_pair(L"b", L"both"));
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
                                   wchar_t * structure, size_t & structurePos, bool isAbbr) {
	while (charsMissing) {
		if (defaultTitleCase) {
			structure[structurePos++] = isAbbr ? L'j' : L'i';
			defaultTitleCase = false;
		}
		else {
			structure[structurePos++] = isAbbr ? L'q' : L'p';
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
	bool isAbbr = false;
	for (size_t i = 0; i + 8 < outputLen; i++) {
		if (wcsncmp(fstOutput + i, L"[Bc]", 4) == 0) {
			if (i == 1) {
				structure[structurePos++] = L'=';
			}
			if (charsSeen > charsFromDefault) {
				createDefaultStructure(charsSeen - charsFromDefault, defaultTitleCase, structure, structurePos, isAbbr);
				charsMissing -= (charsSeen - charsFromDefault);
			}
			if (i != 1) {
				structure[structurePos++] = L'=';
			}
			i += 3;
			charsSeen = 0;
			charsFromDefault = 0;
		}
		else if (wcsncmp(fstOutput + i, L"[Xr]", 4) == 0) {
			defaultTitleCase = false;
			i += 4;
			while (fstOutput[i] != L'[' && charsMissing) {
				structure[structurePos++] = fstOutput[i];
				if (fstOutput[i] != L'=') {
					charsFromDefault++;
					if (fstOutput[i] != L'-') {
						charsMissing--;
					}
				}
				i++;
			}
			i += 2; // X]
		}
		else if (wcsncmp(fstOutput + i, L"[Le", 3) == 0) {
			defaultTitleCase = true;
			isAbbr = false;
			i += 4;
		}
		else if (wcsncmp(fstOutput + i, L"[Lnl", 4) == 0) {
			isAbbr = false;
			i += 4;
		}
		else if (wcsncmp(fstOutput + i, L"[La", 3) == 0) {
			isAbbr = true;
			i += 3;
		}
		else if (wcsncmp(fstOutput + i, L"[L", 2) == 0) {
			isAbbr = false;
			i += 3;
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
		else if (fstOutput[i] == L'-') {
			if (charsSeen > charsFromDefault) {
				createDefaultStructure(charsSeen - charsFromDefault, defaultTitleCase, structure, structurePos, isAbbr);
				charsMissing -= (charsSeen - charsFromDefault);
				structure[structurePos++] = L'-';
				charsSeen = 0;
				charsFromDefault = 0;
			}
			else if (i != 0) {
				charsSeen++;
			}
			if (charsMissing) {
				charsMissing--;
			}
			if (structurePos == 1) {
				structure[0] = L'-';
			}
		}
		else {
			charsSeen++;
		}
	}
	createDefaultStructure(charsMissing, defaultTitleCase, structure, structurePos, isAbbr);
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

static void parseBasicAttribute(Analysis * analysis, const wchar_t * fstOutput, size_t fstLen, size_t i, size_t j,
			        const char * attributeName, map<wstring, wstring> & theMap) {
	size_t sijaLen = i - j - 2;
	wchar_t * muoto = getAttributeFromMap(theMap, fstOutput + j + 2, sijaLen);
	if (muoto) {
		analysis->addAttribute(attributeName, muoto);
	}
}

static bool isValidAnalysis(const wchar_t * fstOutput, size_t len) {
	wchar_t lastChar = L'\0';
	bool boundaryPassed = false;
	bool hyphenPresent = false;
	bool hyphenUnconditionallyAllowed = false;
	for (size_t i = 0; i < len; i++) {
		if (fstOutput[i] == L'[') {
			if (i + 2 >= len) {
				// something wrong with the pattern
				return false;
			}
			if (i + 3 < len && wcsncmp(fstOutput + i + 1, L"Isf", 3) == 0) {
				boundaryPassed = false;
				hyphenUnconditionallyAllowed = true;
			}
			if (fstOutput[i + 1] == L'X') {
				while (i + 3 < len) {
					i++;
					if (wcsncmp(fstOutput + i, L"[X]", 3) == 0) {
						i += 2;
						break;
					}
				}
			}
			else if (wcsncmp(fstOutput + i + 1, L"Bh", 2) == 0) {
				i += 3;
				boundaryPassed = true;
				hyphenPresent = false;
			}
			else {
				while (++i < len && fstOutput[i] != L']') { }
			}
		}
		else if (fstOutput[i] == L'-' && i + 5 < len && wcsncmp(fstOutput + i + 1, L"[Bh]", 4) == 0) {
			boundaryPassed = true;
			hyphenPresent = true;
			i += 4;
		}
		else {
			if (boundaryPassed) {
				if (!hyphenUnconditionallyAllowed) {
					lastChar = SimpleChar::lower(lastChar);
					wchar_t nextChar = SimpleChar::lower(fstOutput[i]);
					bool hyphenRequired = ((lastChar == nextChar) && wcschr(VOIKKO_VOWELS, lastChar));
					if (hyphenRequired != hyphenPresent) {
						return false;
					}
				}
				boundaryPassed = false;
				hyphenUnconditionallyAllowed = false;
			}
			lastChar = fstOutput[i];
		}
	}
	return true;
}

void VfstAnalyzer::parseBasicAttributes(Analysis * analysis, const wchar_t * fstOutput, size_t fstLen) {
	for (size_t i = fstLen - 1; i >= 2; i--) {
		if (fstOutput[i] == L']') {
			size_t j = i;
			while (j >= 1) {
				j--;
				if (fstOutput[j] == L'[') {
					if (fstOutput[j + 1] == L'L') {
						parseBasicAttribute(analysis, fstOutput, fstLen, i, j, "CLASS", classMap);
					}
					else if (fstOutput[j + 1] == L'N') {
						parseBasicAttribute(analysis, fstOutput, fstLen, i, j, "NUMBER", numberMap);
					}
					else if (fstOutput[j + 1] == L'P') {
						parseBasicAttribute(analysis, fstOutput, fstLen, i, j, "PERSON", personMap);
					}
					else if (fstOutput[j + 1] == L'S') {
						parseBasicAttribute(analysis, fstOutput, fstLen, i, j, "SIJAMUOTO", sijamuotoMap);
					}
					else if (fstOutput[j + 1] == L'T') {
						parseBasicAttribute(analysis, fstOutput, fstLen, i, j, "MOOD", moodMap);
					}
					else if (fstOutput[j + 1] == L'A') {
						parseBasicAttribute(analysis, fstOutput, fstLen, i, j, "TENSE", tenseMap);
					}
					else if (fstOutput[j + 1] == L'F') {
						parseBasicAttribute(analysis, fstOutput, fstLen, i, j, "FOCUS", focusMap);
					}
					else if (fstOutput[j + 1] == L'O') {
						parseBasicAttribute(analysis, fstOutput, fstLen, i, j, "POSSESSIVE", possessiveMap);
					}
					else if (fstOutput[j + 1] == L'C') {
						parseBasicAttribute(analysis, fstOutput, fstLen, i, j, "COMPARISON", comparisonMap);
					}
					else if (fstOutput[j + 1] == L'E') {
						parseBasicAttribute(analysis, fstOutput, fstLen, i, j, "NEGATIVE", negativeMap);
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

static void fixStructure(wchar_t * structure, wchar_t * fstOutput, size_t fstLen) {
	bool isDe = false;
	for (size_t j = 0; j + 3 < fstLen; j++) {
		if (wcsncmp(fstOutput + j, L"[Dg]", 4) == 0) {
			for (size_t i = 0; structure[i]; i++) {
				if (structure[i] == L'i') {
					structure[i] = L'p';
				}
			}
		}
		else if (wcsncmp(fstOutput + j, L"[De]", 4) == 0) {
			isDe = true;
		}
		else if (fstOutput[j] == L'-') {
			if (isDe) {
				j++;
				while (j + 4 < fstLen) {
					if (wcsncmp(fstOutput + j, L"[Lep]", 5) == 0) {
						for (size_t i = 0; structure[i]; i++) {
							if (structure[i] == L'i' || structure[i] == L'p') {
								structure[i] = L'i';
								return;
							}
						}
					}
					j++;
				}
			}
			return;
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
			wchar_t * fstOutput = StringUtils::ucs4FromUtf8(outputBuffer);
			size_t fstLen = wcslen(fstOutput);
			if (!isValidAnalysis(fstOutput, fstLen)) {
				delete[] fstOutput;
				continue;
			}
			Analysis * analysis = new Analysis();
			wchar_t * structure = parseStructure(fstOutput, wlen);
			parseBasicAttributes(analysis, fstOutput, fstLen);
			fixStructure(structure, fstOutput, fstLen);
			analysis->addAttribute("STRUCTURE", structure);
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
