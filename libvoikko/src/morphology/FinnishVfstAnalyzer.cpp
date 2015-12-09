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

#include "morphology/FinnishVfstAnalyzer.hpp"
#include "setup/DictionaryException.hpp"
#include "utils/StringUtils.hpp"
#include "character/SimpleChar.hpp"
#include "utils/utils.hpp"
#include "voikko_defines.h"
#include <cassert>

using namespace libvoikko::character;
using namespace libvoikko::utils;
using namespace libvoikko::fst;

using std::string;
using std::wstring;
using std::list;
using std::map;

namespace libvoikko { namespace morphology {

static const int BUFFER_SIZE = 2000;
static const int MAX_ANALYSIS_COUNT = 100;

FinnishVfstAnalyzer::FinnishVfstAnalyzer(const string & directoryName) throw(setup::DictionaryException) {
	string morFile = directoryName + "/mor.vfst";
	transducer = new UnweightedTransducer(morFile.c_str());
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
	classMap.insert(std::make_pair(L"ur", L"lukusana"));
	classMap.insert(std::make_pair(L"r", L"asemosana"));
	classMap.insert(std::make_pair(L"c", L"sidesana"));
	classMap.insert(std::make_pair(L"d", L"suhdesana"));
	classMap.insert(std::make_pair(L"k", L"kieltosana"));
	classMap.insert(std::make_pair(L"p", L"etuliite"));
	
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
	moodMap.insert(std::make_pair(L"n4", L"MINEN-infinitive"));
	moodMap.insert(std::make_pair(L"n5", L"MAINEN-infinitive"));
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
	
	participleMap.insert(std::make_pair(L"v", L"present_active"));
	participleMap.insert(std::make_pair(L"a", L"present_passive"));
	participleMap.insert(std::make_pair(L"u", L"past_active"));
	participleMap.insert(std::make_pair(L"t", L"past_passive"));
	participleMap.insert(std::make_pair(L"m", L"agent"));
	participleMap.insert(std::make_pair(L"e", L"negation"));
}

list<Analysis *> * FinnishVfstAnalyzer::analyze(const char * word, bool fullMorphology) {
	wchar_t * wordUcs4 = StringUtils::ucs4FromUtf8(word);
	list<Analysis *> * result = analyze(wordUcs4, wcslen(wordUcs4), fullMorphology);
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

static inline void decreaseCharsMissing(size_t & charsMissing, size_t charsSeen, size_t charsFromDefault) {
	if (charsSeen - charsFromDefault <= charsMissing) {
		charsMissing -= (charsSeen - charsFromDefault);
	}
	else {
		// lexicon error: something wrong with fstOutput
		assert(false);
		charsMissing = 0;
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
	for (size_t i = 0; i + 0 < outputLen; i++) {
		if (wcsncmp(fstOutput + i, L"[Bc]", 4) == 0 || wcsncmp(fstOutput + i, L"[Bm]", 4) == 0) {
			if (i == 1) {
				structure[structurePos++] = L'=';
			}
			if (charsSeen > charsFromDefault) {
				createDefaultStructure(charsSeen - charsFromDefault, defaultTitleCase, structure, structurePos, isAbbr);
				decreaseCharsMissing(charsMissing, charsSeen, charsFromDefault);
			}
			if (i != 1 && i + 5 < outputLen && structurePos > 0 && structure[structurePos - 1] != L'=') {
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
		else if (wcsncmp(fstOutput + i, L"[Lu", 3) == 0 && i + 5 < outputLen &&
			 (fstOutput[i + 3] == L'r' || StringUtils::isInteger(fstOutput[i + 4]))) {
			isAbbr = true;
			i += 3;
			if (fstOutput[i] == L'r') {
				i++;
			}
		}
		else if (wcsncmp(fstOutput + i, L"[L", 2) == 0) {
			isAbbr = false;
			i += 3;
		}
		else if (wcsncmp(fstOutput + i, L"[Xp", 3) == 0 || wcsncmp(fstOutput + i, L"[Xj", 3) == 0) {
			i += 4;
			while (fstOutput[i] != L'[') {
				i++;
			}
			i += 2;
		}
		else if (wcsncmp(fstOutput + i, L"[Xs", 3) == 0) {
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
				decreaseCharsMissing(charsMissing, charsSeen, charsFromDefault);
				structure[structurePos++] = L'-';
				charsSeen = 0;
				charsFromDefault = 0;
			}
			else if (i != 0) {
				if (charsSeen == charsFromDefault) {
					structure[structurePos++] = L'-';
				}
				else {
					charsSeen++;
				}
			}
			if (charsMissing) {
				charsMissing--;
			}
			if (structurePos == 1) {
				structure[0] = L'-';
			}
		}
		else if (fstOutput[i] == L':') {
			if (isAbbr) {
				if (charsSeen > charsFromDefault) {
					createDefaultStructure(charsSeen - charsFromDefault, defaultTitleCase, structure, structurePos, isAbbr);
					decreaseCharsMissing(charsMissing, charsSeen, charsFromDefault);
					charsSeen = 0;
					charsFromDefault = 0;
				}
				isAbbr = false;
			}
			structure[structurePos++] = L':';
			if (charsMissing) {
				charsMissing--;
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

static void parseBasicAttribute(Analysis * analysis, const wchar_t * fstOutput, size_t i, size_t j,
			        const char * attributeName, map<wstring, wstring> & theMap) {
	if (analysis->getValue(attributeName)) {
		return; // already set
	}
	size_t sijaLen = i - j - 2;
	wchar_t * muoto = getAttributeFromMap(theMap, fstOutput + j + 2, sijaLen);
	if (muoto) {
		analysis->addAttribute(attributeName, muoto);
	}
}

static bool isValidAnalysis(const wchar_t * fstOutput, size_t len) {
	wchar_t beforeLastChar = L'\0';
	wchar_t lastChar = L'\0';
	bool boundaryPassed = false;
	bool hyphenPresent = false;
	bool hyphenUnconditionallyAllowed = false;
	bool hyphenUnconditionallyAllowedJustSet = false;
	bool hyphenRequired = false;
	bool requiredHyphenMissing = false;
	bool startsWithProperNoun = false;
	bool endsWithNonIcaNoun = false;
	for (size_t i = 0; i < len; i++) {
		if (fstOutput[i] == L'[') {
			if (i + 2 >= len) {
				// something wrong with the pattern
				return false;
			}
			if (i + 3 < len && wcsncmp(fstOutput + i + 1, L"Isf", 3) == 0) {
				hyphenUnconditionallyAllowed = true;
				hyphenUnconditionallyAllowedJustSet = true;
			}
			else if (i + 3 < len && wcsncmp(fstOutput + i + 1, L"Icu", 3) == 0) {
				boundaryPassed = false;
				hyphenUnconditionallyAllowed = true;
				hyphenRequired = true;
			}
			else if (i + 3 < len && wcsncmp(fstOutput + i + 1, L"Ica", 3) == 0) {
				requiredHyphenMissing = false;
				endsWithNonIcaNoun = false;
			}
			else if (i + 3 < len && wcsncmp(fstOutput + i + 1, L"Le", 2) == 0) {
				startsWithProperNoun = true; // TODO starts?
				endsWithNonIcaNoun = false;
			}
			else if (i + 2 < len && wcsncmp(fstOutput + i + 1, L"Ln", 2) == 0) {
				endsWithNonIcaNoun = true;
			}
			else if (i + 2 < len && wcsncmp(fstOutput + i + 1, L"Dg", 2) == 0) {
				startsWithProperNoun = false;
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
				if (requiredHyphenMissing) {
					return false;
				}
				if (hyphenRequired) {
					requiredHyphenMissing = true;
				}
			}
			else {
				while (++i < len && fstOutput[i] != L']') { }
			}
		}
		else if (fstOutput[i] == L'-') {
			startsWithProperNoun = false;
			endsWithNonIcaNoun = false;
			if (i + 5 < len && wcsncmp(fstOutput + i + 1, L"[Bh]", 4) == 0) {
				boundaryPassed = true;
				hyphenPresent = true;
				i += 4;
			}
		}
		else {
			if (boundaryPassed) {
				if (lastChar == L'\0' || (beforeLastChar == L'i' && lastChar == L's')) {
					hyphenUnconditionallyAllowed = true;
				}
				if (hyphenRequired) {
					if (hyphenPresent) {
						hyphenRequired = false;
					}
				}
				if (!(hyphenUnconditionallyAllowed && hyphenPresent)) {
					lastChar = SimpleChar::lower(lastChar);
					wchar_t nextChar = SimpleChar::lower(fstOutput[i]);
					bool hyphenRequired = (((lastChar == nextChar) && wcschr(VOIKKO_VOWELS, lastChar)) || (lastChar >= L'0' && lastChar <= L'9'));
					if (hyphenRequired != hyphenPresent) {
						return false;
					}
				}
				boundaryPassed = false;
				if (hyphenUnconditionallyAllowedJustSet) {
					hyphenUnconditionallyAllowedJustSet = false;
				}
				else {
					hyphenUnconditionallyAllowed = false;
				}
			}
			beforeLastChar = lastChar;
			lastChar = fstOutput[i];
		}
	}
	return !requiredHyphenMissing && (!startsWithProperNoun || !endsWithNonIcaNoun);
}

static void addInfoFlag(Analysis * analysis, const wchar_t * outputPosition, const wchar_t * outputBuffer) {
	const wchar_t * className = analysis->getValue("CLASS");
	if (wcsncmp(outputPosition, L"vj", 2) == 0) {
		if (outputBuffer[0] != L'-') {
			analysis->addAttribute("MALAGA_VAPAA_JALKIOSA", StringUtils::copy(L"true"));
		}
	}
	else if (wcsncmp(outputPosition, L"ca", 2) == 0) {
		if (!wcsstr(outputPosition, L"[Bc]") && !wcsstr(outputPosition, L"[Ll]") && (!className || wcsncmp(className, L"nimisana", 8) == 0)) {
			analysis->addAttribute("POSSIBLE_GEOGRAPHICAL_NAME", StringUtils::copy(L"true"));
		}
	}
	else {
		const wchar_t * mood = analysis->getValue("MOOD");
		if ((!mood || (wcscmp(mood, L"E-infinitive") != 0 && wcscmp(mood, L"MINEN-infinitive") != 0 && wcscmp(mood, L"MA-infinitive") != 0)) &&
		    (!className || wcscmp(className, L"teonsana") == 0)) {
			if (wcsncmp(outputPosition, L"ra", 2) == 0) {
				analysis->addAttribute("REQUIRE_FOLLOWING_VERB", StringUtils::copy(L"A-infinitive"));
			}
			else if (wcsncmp(outputPosition, L"rm", 2) == 0) {
				analysis->addAttribute("REQUIRE_FOLLOWING_VERB", StringUtils::copy(L"MA-infinitive"));
			}
		}
	}
}

static bool parseNumeralBaseform(const wchar_t * fstOutput, size_t fstLen, wchar_t * baseform) {
	bool isInXp = false;
	bool isInXr = false;
	bool isInTag = false;
	bool isInDigitSequence = false;
	bool xpPassed = false;
	size_t baseformPos = 0;
	
	for (size_t i = 0; i < fstLen; i++) {
		if (i == 0 && (SimpleChar::isDigit(fstOutput[i]) || fstOutput[i] == L'-')) {
			isInDigitSequence = true;
		}
		if (fstOutput[i] == L'[') {
			if (isInDigitSequence) {
				isInDigitSequence = false;
				xpPassed = true;
			}
			if (i + 2 >= fstLen) {
				// something wrong with the pattern
				return false;
			}
			if (i + 6 < fstLen && (wcsncmp(fstOutput + i, L"[Xp]", 4) == 0 || wcsncmp(fstOutput + i, L"[Xj]", 4) == 0)) {
				i += 3;
				isInXp = true;
			}
			else if (i + 6 < fstLen && wcsncmp(fstOutput + i, L"[Xr]", 4) == 0) {
				// TODO not needed for numerals?
				i += 3;
				isInXr = true;
			}
			else if (i + 4 == fstLen && wcsncmp(fstOutput + i, L"[Bc]", 4) == 0) {
				return false; // incomplete numeral is really a prefix
			}
			else if (i + 6 < fstLen && wcsncmp(fstOutput + i, L"[Bc]", 4) == 0) {
				i += 3;
				xpPassed = false;
			}
			else if (i + 6 < fstLen && (wcsncmp(fstOutput + i, L"[Ln]", 4) == 0 || wcsncmp(fstOutput + i, L"[Ll]", 4) == 0
			         || wcsncmp(fstOutput + i, L"[Lnl]", 5) == 0)) {
				return false; // give up and return to standard algorithm
			}
			else if (wcsncmp(fstOutput + i, L"[X]", 3) == 0) {
				if (isInXp) {
					isInXp = false;
					xpPassed = true;
				}
				isInXr = false;
				i += 2;
			}
			else {
				isInTag = true;
			}
		}
		else if (isInXr) {
			// do nothing
		}
		else if (isInTag) {
			if (fstOutput[i] == L']') {
				isInTag = false;
			}
		}
		else if (isInXp || isInDigitSequence) {
			baseform[baseformPos++] = fstOutput[i];
		}
		else if (!xpPassed || fstOutput[i] == L'-') {
			baseform[baseformPos++] = fstOutput[i];
		}
	}
	baseform[baseformPos] = L'\0';
	return true;
}

static wchar_t * parseBaseform(const wchar_t * fstOutput, size_t fstLen, const wchar_t * structure) {
	wchar_t * baseform = new wchar_t[fstLen + 1];
	size_t baseformPos = 0;
	size_t latestXpStartInFst = 0;
	size_t latestXpStartInBaseform = 0;
	size_t hyphensInLatestXp = 0;
	size_t structurePos = 0;
	size_t structureLen = wcslen(structure);
	bool isInXp = false;
	bool isInXr = false;
	bool isInTag = false;
	bool ignoreNextDe = false;
	bool isDe = false;
	bool classTagSeen = false;
	
	for (size_t i = 0; i < fstLen; i++) {
		if (fstOutput[i] == L'[') {
			if (i + 2 >= fstLen) {
				// something wrong with the pattern
				delete[] baseform;
				return 0;
			}
			if (i + 6 < fstLen && (wcsncmp(fstOutput + i, L"[Xp]", 4) == 0 || wcsncmp(fstOutput + i, L"[Xj]", 4) == 0)) {
				i += 3;
				isInXp = true;
				latestXpStartInFst = i + 1;
				latestXpStartInBaseform = baseformPos;
				hyphensInLatestXp = 0;
			}
			else if (i + 6 < fstLen && wcsncmp(fstOutput + i, L"[Xr]", 4) == 0) {
				i += 3;
				isInXr = true;
			}
			else if (i + 6 < fstLen && wcsncmp(fstOutput + i, L"[Xs]", 4) == 0) {
				i += 3;
				isInXr = true;
			}
			else if (!classTagSeen && i + 6 < fstLen && wcsncmp(fstOutput + i, L"[Lu]", 4) == 0) {
				i += 3;
				classTagSeen = true;
				// we will try completely different rules here and get back if it does not work out
				bool numeralBaseform = parseNumeralBaseform(fstOutput + i + 1, fstLen - i - 1, baseform + baseformPos);
				if (numeralBaseform) {
					return baseform;
				}
			}
			else if (wcsncmp(fstOutput + i, L"[X]", 3) == 0) {
				isInXp = false;
				isInXr = false;
				i += 2;
			}
			else if (wcsncmp(fstOutput + i, L"[De]", 4) == 0) {
				isDe = !ignoreNextDe;
				i += 3;
			}
			else {
				if (fstOutput[i + 1] == L'L') {
					classTagSeen = true;
					isDe = false;
					ignoreNextDe = (i + 3 >= fstLen || (fstOutput[i + 2] != L'l' && wcsncmp(fstOutput + i + 2, L"nl", 2) != 0));
				}
				isInTag = true;
			}
		}
		else if (isInXr) {
			// do nothing
		}
		else if (isInTag) {
			if (fstOutput[i] == L']') {
				isInTag = false;
			}
		}
		else if (isInXp) {
			if (fstOutput[i] == L'-') {
				hyphensInLatestXp++;
			}
		}
		else {
			wchar_t nextChar = fstOutput[i];
			if (nextChar == L'-') {
				if (hyphensInLatestXp > 0) {
					hyphensInLatestXp--;
				}
				else {
					// Compound place name such as "Isolla-Britannialla" needs to have "Isolla" replaced with "Iso". However
					// "-is" is never replaced with "-nen" ("Pohjois-Suomella").
					if (isDe && latestXpStartInFst != 0 && wcsncmp(fstOutput + i - 2, L"is", 2) != 0) {
						for (size_t j = i; j + 4 < fstLen; j++) {
							if (wcsncmp(fstOutput + j, L"[Lep]", 5) == 0) {
								baseformPos = latestXpStartInBaseform;
								for (size_t i = latestXpStartInFst; i < fstLen && fstOutput[i] != L'['; i++) {
									if (fstOutput[i] != L'=') {
										if (i == latestXpStartInFst) {
											baseform[baseformPos++] = SimpleChar::upper(fstOutput[i]);
										}
										else {
											baseform[baseformPos++] = fstOutput[i];
										}
									}
								}
								break;
							}
						}
					}
					latestXpStartInFst = 0;
				}
				isDe = false;
			}
			while (structurePos < structureLen) {
				wchar_t patternChar = structure[structurePos];
				structurePos++;
				if (patternChar != L'=') {
					if (patternChar == L'i' || patternChar == L'j') {
						nextChar = SimpleChar::upper(nextChar);
					}
					break;
				}
			}
			baseform[baseformPos++] = nextChar;
		}
	}
	
	if (latestXpStartInFst != 0) {
		baseformPos = latestXpStartInBaseform;
		for (size_t i = latestXpStartInFst; i < fstLen && fstOutput[i] != L'['; i++) {
			if (fstOutput[i] != L'=') {
				baseform[baseformPos++] = fstOutput[i];
			}
		}
	}
	
	if (baseformPos == 0) {
		delete[] baseform;
		return 0;
	}
	baseform[baseformPos] = L'\0';
	return baseform;
}

void FinnishVfstAnalyzer::duplicateOrgName(Analysis * analysis, const wchar_t * fstOutput, std::list<Analysis *> * analysisList) {
	const wchar_t * oldClass = analysis->getValue("CLASS");
	if (!oldClass || wcscmp(oldClass, L"nimisana") != 0) {
		return;
	}
	size_t fstLen = wcslen(fstOutput);
	if (fstLen < 13) {
		return;
	}
	if (fstOutput[0] == L'-') {
		return;
	}
	if (wcsncmp(fstOutput, L"[La]", 4) == 0) {
		return;
	}
	for (size_t i = fstLen - 5; i >= 8; i--) {
		if (wcsncmp(fstOutput + i, L"[Bc]", 4) == 0) {
			return;
		}
		if (wcsncmp(fstOutput + i, L"[Ion]", 5) == 0) {
			for (size_t j = i - 4; j >= 4; j--) {
				if (wcsncmp(fstOutput + j, L"[Bc]", 4) == 0) {
					Analysis * newAnalysis = new Analysis();
					const char ** keys = analysis->getKeys();
					wchar_t * newStructure = 0;
					for (const char ** keyPtr = keys; *keyPtr; keyPtr++) {
						const char * key = *keyPtr;
						if (strcmp(key, "CLASS") == 0) {
							newAnalysis->addAttribute(key, StringUtils::copy(L"nimi"));
						}
						else if (strcmp(key, "STRUCTURE") == 0) {
							const wchar_t * oldStructure = analysis->getValue(key);
							size_t structureLen = wcslen(oldStructure);
							if (structureLen >= 2) {
								newStructure = StringUtils::copy(oldStructure);
								newStructure[1] = L'i';
								newAnalysis->addAttribute(key, newStructure);
							}
						}
						else if (strcmp(key, "POSSIBLE_GEOGRAPHICAL_NAME") == 0) {
							// skip
						}
						else {
							newAnalysis->addAttribute(key, StringUtils::copy(analysis->getValue(key)));
						}
					}
					if (newStructure) {
						wchar_t * baseform = parseBaseform(fstOutput, fstLen, newStructure);
						if (baseform) {
							newAnalysis->addAttribute("BASEFORM", baseform);
						}
					}
					analysisList->push_back(newAnalysis);
					return;
				}
			}
		}
	}
}

static void debugContentEnd(wchar_t * wordIds, wchar_t * wordBases, wchar_t * xsBuffer, wchar_t * xpBuffer,
                            size_t & idpos, size_t & xspos, size_t & basepos, size_t & xppos) {
	if (xspos > 0) {
		wordIds[idpos++] = L'(';
		wordIds[idpos++] = L'w';
		wcsncpy(wordIds + idpos, xsBuffer, xspos);
		idpos += xspos;
		wordIds[idpos++] = L')';
		xspos = 0;
	}
	if (xppos > 0) {
		wordBases[basepos++] = L'(';
		wcsncpy(wordBases + basepos, xpBuffer, xppos);
		basepos += xppos;
		wordBases[basepos++] = L')';
		xppos = 0;
	}
}

void FinnishVfstAnalyzer::parseDebugAttributes(Analysis * analysis, const wchar_t * fstOutput, size_t fstLen) {
	wchar_t * wordIds = new wchar_t[2 * fstLen + 1];
	wchar_t * wordBases = new wchar_t[2 * fstLen + 1];
	wchar_t * xsBuffer = new wchar_t[fstLen];
	wchar_t * xpBuffer = new wchar_t[fstLen];
	size_t idpos = 0;
	size_t basepos = 0;
	size_t idposLast = 0;
	size_t baseposLast = 0;
	size_t xppos = 0;
	size_t xspos = 0;
	bool inXs = false;
	bool inXp = false;
	bool inXj = false;
	bool inXOther = false;
	bool inContent = false;
	bool inTag = false;
	bool anyXs = false;
	for (size_t i = 0; i < fstLen; i++) {
		if (wcsncmp(fstOutput + i, L"[L", 2) == 0 || wcsncmp(fstOutput + i, L"-[B", 3) == 0) {
			inContent = false;
			inTag = true;
			debugContentEnd(wordIds, wordBases, xsBuffer, xpBuffer, idpos, xspos, basepos, xppos);
			if (fstOutput[i] == L'-') {
				wordIds[idpos++] = L'+';
				wordIds[idpos++] = L'-';
				wordBases[basepos++] = L'+';
				wordBases[basepos++] = L'-';
				i++;
			}
		}
		else if (fstOutput[i] == L'[' && i + 2 < fstLen) {
			if (fstOutput[i + 1] == L'X') {
				if (fstOutput[i + 2] == L's') {
					inXs = true;
					anyXs = true;
					xspos = 0;
					i += 3;
				}
				else if (fstOutput[i + 2] == L'p') {
					inXp = true;
					xppos = 0;
					i += 3;
				}
				else if (fstOutput[i + 2] == L'j') {
					if (inContent) {
						debugContentEnd(wordIds, wordBases, xsBuffer, xpBuffer, idpos, xspos, basepos, xppos);
						idposLast = idpos;
						baseposLast = basepos;
					}
					inXj = true;
					xppos = 0;
					i += 3;
				}
				else if (fstOutput[i + 2] == L']') {
					inXs = false;
					inXp = false;
					inXj = false;
					inXOther = false;
					i += 2;
				}
				else {
					inXOther = true;
					i += 3;
				}
			}
			else {
				inTag = true;
			}
		}
		else if (fstOutput[i] == L']') {
			inTag = false;
		}
		else {
			if (inTag || inXOther) {
				// do nothing
			}
			else if (inXs) {
				xsBuffer[xspos++] = fstOutput[i];
			}
			else if (inXp) {
				xpBuffer[xppos++] = fstOutput[i];
			}
			else if (inXj) {
				if (xppos == 0) {
					xpBuffer[xppos++] = L'+';
				}
				xpBuffer[xppos++] = fstOutput[i];
			}
			else {
				if (!inContent) {
					wordIds[idpos++] = L'+';
					wordBases[basepos++] = L'+';
					idposLast = idpos;
					baseposLast = basepos;
					inContent = true;
				}
				wordIds[idpos++] = fstOutput[i];
				wordBases[basepos++] = fstOutput[i];
			}
		}
	}
	if (xppos > 0) {
		basepos = baseposLast;
		idpos = idposLast;
		int plus = (basepos > 0 && xpBuffer[0] == L'+' && wordBases[basepos - 1] == L'+') ? 1 : 0;
		basepos -= plus;
		idpos -= plus;
		for (size_t i = 0; i < xppos; i++) {
			wchar_t c = xpBuffer[i];
			if (c != L'=') {
				wordBases[basepos++] = c;
				wordIds[idpos++] = c;
			}
		}
		wordBases[basepos++] = L'(';
		wcsncpy(wordBases + basepos, xpBuffer, xppos);
		basepos += xppos;
		wordBases[basepos++] = L')';
		xppos = 0;
	}
	if (xspos > 0) {
		wordIds[idpos++] = L'(';
		wordIds[idpos++] = L'w';
		wcsncpy(wordIds + idpos, xsBuffer, xspos);
		idpos += xspos;
		wordIds[idpos++] = L')';
		xspos = 0;
	}
	wordIds[idpos] = L'\0';
	wordBases[basepos] = L'\0';
	delete[] xsBuffer;
	delete[] xpBuffer;
	if (anyXs) {
		analysis->addAttribute("WORDIDS", wordIds);
	}
	else {
		delete[] wordIds;
	}
	analysis->addAttribute("WORDBASES", wordBases);
}

void FinnishVfstAnalyzer::parseBasicAttributes(Analysis * analysis, const wchar_t * fstOutput, size_t fstLen) {
	bool convertNimiLaatusanaToLaatusana = false;
	bool bcPassed = false;
	for (size_t i = fstLen - 1; i >= 2; i--) {
		if (fstOutput[i] == L']') {
			size_t j = i;
			while (j >= 1) {
				j--;
				if (fstOutput[j] == L'[') {
					if (fstOutput[j + 1] == L'L') {
						if (wcsncmp(fstOutput + (j + 2), L"nl", 2) == 0) {
							const wchar_t * comp = analysis->getValue("COMPARISON");
							if (convertNimiLaatusanaToLaatusana || (comp && (wcscmp(comp, L"comparative") == 0 || wcscmp(comp, L"superlative") == 0)) ||
							    wcsncmp(fstOutput, L"[Lu]", 4) == 0) {
								analysis->addAttribute("CLASS", StringUtils::copy(L"laatusana"));
							}
							else {
								analysis->addAttribute("CLASS", StringUtils::copy(L"nimisana_laatusana"));
							}
						}
						else {
							parseBasicAttribute(analysis, fstOutput, i, j, "CLASS", classMap);
						}
					}
					else if (fstOutput[j + 1] == L'N') {
						const wchar_t * wclass = analysis->getValue("CLASS");
						if (!wclass || (wcscmp(wclass, L"etuliite") != 0 && wcscmp(wclass, L"seikkasana") != 0)) {
							parseBasicAttribute(analysis, fstOutput, i, j, "NUMBER", numberMap);
						}
					}
					else if (fstOutput[j + 1] == L'P') {
						parseBasicAttribute(analysis, fstOutput, i, j, "PERSON", personMap);
					}
					else if (fstOutput[j + 1] == L'S') {
						const wchar_t * wclass = analysis->getValue("CLASS");
						if (!wclass || (wcscmp(wclass, L"etuliite") != 0 && wcscmp(wclass, L"seikkasana") != 0)) {
							parseBasicAttribute(analysis, fstOutput, i, j, "SIJAMUOTO", sijamuotoMap);
							if (j + 5 < fstLen && wcsncmp(fstOutput + (j + 2), L"sti", 3) == 0) {
								convertNimiLaatusanaToLaatusana = true;
							}
						}
					}
					else if (fstOutput[j + 1] == L'T') {
						if (!analysis->getValue("CLASS")) {
							parseBasicAttribute(analysis, fstOutput, i, j, "MOOD", moodMap);
						}
					}
					else if (fstOutput[j + 1] == L'A') {
						parseBasicAttribute(analysis, fstOutput, i, j, "TENSE", tenseMap);
					}
					else if (fstOutput[j + 1] == L'F') {
						if (wcsncmp(fstOutput + (j + 2), L"ko", 2) == 0) {
							analysis->addAttribute("KYSYMYSLIITE", StringUtils::copy(L"true"));
						}
						else {
							parseBasicAttribute(analysis, fstOutput, i, j, "FOCUS", focusMap);
						}
					}
					else if (fstOutput[j + 1] == L'O') {
						parseBasicAttribute(analysis, fstOutput, i, j, "POSSESSIVE", possessiveMap);
					}
					else if (fstOutput[j + 1] == L'C') {
						if (!analysis->getValue("CLASS")) {
							parseBasicAttribute(analysis, fstOutput, i, j, "COMPARISON", comparisonMap);
						}
					}
					else if (fstOutput[j + 1] == L'E') {
						parseBasicAttribute(analysis, fstOutput, i, j, "NEGATIVE", negativeMap);
					}
					else if (fstOutput[j + 1] == L'R') {
						if (!bcPassed) {
							const wchar_t * wclass = analysis->getValue("CLASS");
							// TODO: Checking the end for [Ln] is done to handle -tUAnne ("kuunneltuanne"). This is for compatibility
							// with Malaga implementation. See VISK § 543 (temporaalirakenne) for correct analysis.
							if (!wclass || wcscmp(wclass, L"laatusana") == 0 || wcscmp(fstOutput + (fstLen - 4), L"[Ln]") == 0) {
								parseBasicAttribute(analysis, fstOutput, i, j, "PARTICIPLE", participleMap);
							}
						}
					}
					else if (fstOutput[j + 1] == L'I') {
						addInfoFlag(analysis, fstOutput + (j + 2), fstOutput);
					}
					else if (fstOutput[j + 1] == L'B') {
						if (j >= 5 && fstOutput[j + 2] == L'c') {
							if (!analysis->getValue("CLASS") && (fstOutput[j - 1] == L'-' || wcsncmp(fstOutput + (j - 5), L"-[Bh]", 5) == 0)) {
								analysis->addAttribute("CLASS", StringUtils::copy(L"etuliite"));
							}
							bcPassed = true;
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

static void fixStructure(wchar_t * structure, wchar_t * fstOutput, size_t fstLen) {
	bool isDe = false;
	size_t hyphenCount = 0;
	for (size_t j = 0; j < fstLen; j++) {
		if (j + 3 < fstLen && wcsncmp(fstOutput + j, L"[Dg]", 4) == 0) {
			size_t hyphensInStructure = 0;
			for (size_t i = 0; structure[i]; i++) {
				if (structure[i] == L'i') {
					if (hyphensInStructure == hyphenCount) {
						structure[i] = L'p';
					}
				}
				else if (structure[i] == L'-') {
					hyphensInStructure++;
				}
			}
		}
		else if (j + 3 < fstLen && wcsncmp(fstOutput + j, L"[De]", 4) == 0) {
			isDe = true;
		}
		else if (j + 3 < fstLen && wcsncmp(fstOutput + j, L"[Ln]", 4) == 0) {
			isDe = false;
		}
		else if (fstOutput[j] == L'-') {
			hyphenCount++;
			if (isDe) {
				bool toUpper = (j == fstLen - 1);
				j++;
				while (!toUpper && j + 4 < fstLen) {
					if (wcsncmp(fstOutput + j, L"[Lep]", 5) == 0) {
						toUpper = true;
					}
					j++;
				}
				if (toUpper) {
					for (size_t i = 0; structure[i]; i++) {
						if (structure[i] == L'i' || structure[i] == L'p') {
							structure[i] = L'i';
							return;
						}
					}
				}
			}
		}
	}
}

list<Analysis *> * FinnishVfstAnalyzer::analyze(const wchar_t * word, size_t wlen, bool fullMorphology) {
	list<Analysis *> * analysisList = new list<Analysis *>();
	if (wlen > LIBVOIKKO_MAX_WORD_CHARS) {
		return analysisList;
	}
	
	wchar_t * wordLowerUcs4 = new wchar_t[wlen];
	memcpy(wordLowerUcs4, word, wlen * sizeof(wchar_t));
	voikko_set_case(CT_ALL_LOWER, wordLowerUcs4, wlen);
	char * wordLower = StringUtils::utf8FromUcs4(wordLowerUcs4, wlen);
	delete[] wordLowerUcs4;
	if (!wordLower) {
		return analysisList;
	}
	
	if (transducer->prepare(configuration, wordLower, strlen(wordLower))) {
		int analysisCount = 0;
		while (++analysisCount < MAX_ANALYSIS_COUNT && transducer->next(configuration, outputBuffer, BUFFER_SIZE)) {
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
			const wchar_t * wclass = analysis->getValue("CLASS");
			const wchar_t * sijamuoto = analysis->getValue("SIJAMUOTO");
			const wchar_t * mood = analysis->getValue("MOOD");
			const wchar_t * participle = analysis->getValue("PARTICIPLE");
			if (analysis->getValue("NEGATIVE") && ((wclass && wcscmp(wclass, L"teonsana") != 0) ||
			    (mood && (wcscmp(mood, L"MINEN-infinitive") == 0 || wcscmp(mood, L"E-infinitive") == 0 || wcscmp(mood, L"MA-infinitive") == 0))
			)) {
				analysis->removeAttribute("NEGATIVE");
			}
			if (participle && wcscmp(participle, L"past_passive") == 0 && (!wclass || wcscmp(participle, L"laatusana") != 0)) {
				wclass = L"laatusana";
				analysis->removeAttribute("CLASS");
				analysis->addAttribute("CLASS", StringUtils::copy(wclass));
			}
			if (analysis->getValue("NUMBER") && sijamuoto && wcscmp(sijamuoto, L"kerrontosti") == 0) {
				analysis->removeAttribute("NUMBER");
			}
			if (!analysis->getValue("COMPARISON")) {
				if (wclass && (wcscmp(wclass, L"laatusana") == 0 || wcscmp(wclass, L"nimisana_laatusana") == 0)) {
					analysis->addAttribute("COMPARISON", StringUtils::copy(L"positive"));
				}
			}
			else if (wclass && (wcscmp(wclass, L"nimisana") == 0)) {
				analysis->removeAttribute("COMPARISON");
			}
			analysisList->push_back(analysis);
			duplicateOrgName(analysis, fstOutput, analysisList);
			if (fullMorphology) {
				analysis->addAttribute("FSTOUTPUT", fstOutput);
				wchar_t * baseform = parseBaseform(fstOutput, fstLen, structure);
				if (baseform) {
					analysis->addAttribute("BASEFORM", baseform);
				}
				parseDebugAttributes(analysis, fstOutput, fstLen);
			}
			else {
				delete[] fstOutput;
			}
		}
	}
	
	delete[] wordLower;
	return analysisList;
}

void FinnishVfstAnalyzer::terminate() {
	delete[] outputBuffer;
	delete configuration;
	transducer->terminate();
	delete transducer;
}

} }
