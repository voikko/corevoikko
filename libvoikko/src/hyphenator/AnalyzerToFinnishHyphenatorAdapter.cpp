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
 * Portions created by the Initial Developer are Copyright (C) 2009 - 2011
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

#include "hyphenator/AnalyzerToFinnishHyphenatorAdapter.hpp"
#include "character/SimpleChar.hpp"
#include "utils/StringUtils.hpp"
#include "utils/utils.hpp"
#include "voikko_defines.h"

using namespace libvoikko::morphology;
using namespace libvoikko::character;
using std::list;

namespace libvoikko { namespace hyphenator {

static const wchar_t * const SPLIT_VOWELS[] = { L"ae", L"ao", L"ea", L"eo", L"ia", L"io", L"oa", L"oe",
                                   L"ua", L"ue", L"ye", L"e\u00e4", L"e\u00f6", L"i\u00e4",
                                   L"i\u00f6", L"y\u00e4", L"\u00e4e", L"\u00f6e" };
static const wchar_t * const LONG_CONSONANTS[] = { L"shtsh", L"\u0161t\u0161", L"tsh", L"t\u0161", L"zh" };
static const wchar_t * const SPLIT_AFTER[] = { L"ie", L"ai" };

AnalyzerToFinnishHyphenatorAdapter::AnalyzerToFinnishHyphenatorAdapter(Analyzer * analyzer) :
analyzer(analyzer),
uglyHyphenation(true),
hyphenateUnknown(true),
minHyphenatedWordLength(2),
ignoreDot(false)
{}

char * AnalyzerToFinnishHyphenatorAdapter::hyphenate(const wchar_t * word, size_t wlen) {
	/* Short words may not need to be hyphenated at all */
	if (wlen < minHyphenatedWordLength) {
		char * hyphenation = new char[wlen + 1];
		memset(hyphenation, ' ', wlen);
		hyphenation[wlen] = '\0';
		return hyphenation;
	}
	
	bool dotRemoved = false;
	char ** hyphenations = splitCompounds(word, wlen, &dotRemoved);
	if (hyphenations == 0) {
		return 0;
	}
	
	int i = 0;
	while (hyphenations[i] != 0) {
		compoundHyphenation(word, hyphenations[i++], wlen - (dotRemoved ? 1 : 0));
	}
	
	char * hyphenation = intersectHyphenations(hyphenations);
	if (hyphenation == 0) {
		delete[] hyphenations;
		return 0;
	}

	i = 0;
	while (hyphenations[i] != 0) {
		delete[] hyphenations[i++];
	}
	delete[] hyphenations;
	return hyphenation;
}

static char * unionHyphenations(char ** hyphenations) {
	size_t len = strlen(hyphenations[0]);
	char * intersection = new char[len + 1];

	strcpy(intersection, hyphenations[0]);
	for (size_t i = 0; i < len; i++) {
		if (intersection[i] == 'X') {
			intersection[i] = ' ';
		}
	}
	char ** currentPtr = &hyphenations[1];
	while (*currentPtr != 0) {
		for (size_t i = 0; i < len; i++) {
			if ((*currentPtr)[i] == '-') {
				intersection[i] = '-';
			}
		}
		currentPtr++;
	}
	return intersection;
}

char * AnalyzerToFinnishHyphenatorAdapter::allPossibleHyphenPositions(const wchar_t * word, size_t wlen) {
	/* Short words may not need to be hyphenated at all */
	if (wlen < minHyphenatedWordLength) {
		char * hyphenation = new char[wlen + 1];
		memset(hyphenation, ' ', wlen);
		hyphenation[wlen] = '\0';
		return hyphenation;
	}
	
	bool dotRemoved = false;
	char ** hyphenations = splitCompounds(word, wlen, &dotRemoved);
	if (hyphenations == 0) {
		return 0;
	}
	
	int i = 0;
	while (hyphenations[i] != 0) {
		compoundHyphenation(word, hyphenations[i++], wlen - (dotRemoved ? 1 : 0));
	}
	
	char * hyphenation = unionHyphenations(hyphenations);
	
	i = 0;
	while (hyphenations[i] != 0) {
		delete[] hyphenations[i++];
	}
	delete[] hyphenations;
	return hyphenation;
}

void AnalyzerToFinnishHyphenatorAdapter::terminate() {
}

void AnalyzerToFinnishHyphenatorAdapter::setUglyHyphenation(bool uglyHyphenation) {
	this->uglyHyphenation = uglyHyphenation;
}

void AnalyzerToFinnishHyphenatorAdapter::setHyphenateUnknown(bool hyphenateUnknown) {
	this->hyphenateUnknown = hyphenateUnknown;
}

void AnalyzerToFinnishHyphenatorAdapter::setMinHyphenatedWordLength(int length) {
	this->minHyphenatedWordLength = length;
}

void AnalyzerToFinnishHyphenatorAdapter::setIgnoreDot(bool ignoreDot) {
	this->ignoreDot = ignoreDot;
}

char ** AnalyzerToFinnishHyphenatorAdapter::splitCompounds(const wchar_t * word,
	                size_t len, bool * dotRemoved) {
	const unsigned int MAX_ANALYSIS_COUNT = 31; // TODO: remove hard limit
	char ** allResults = new char*[MAX_ANALYSIS_COUNT + 1];
	allResults[MAX_ANALYSIS_COUNT] = 0;
	char * wordUtf8 = utils::StringUtils::utf8FromUcs4(word, len);
	if (wordUtf8 == 0) {
		delete[] allResults;
		return 0;
	}
	size_t utf8Len = strlen(wordUtf8);
	
	list<Analysis *> * analyses = analyzer->analyze(wordUtf8);
	
	/* We may have to remove the trailing dot before hyphenation */
	if (analyses->empty() && ignoreDot && len > 1 &&
	    wordUtf8[utf8Len - 1] == '.') {
		wordUtf8[utf8Len - 1] = '\0';
		utf8Len--;
		*dotRemoved = true;
		Analyzer::deleteAnalyses(analyses);
		analyses = analyzer->analyze(wordUtf8);
	}
	else {
		*dotRemoved = false;
	}
	
	/* Iterate over all analyses and add results to all_results */
	list<Analysis *>::iterator it = analyses->begin();
	size_t analysisCount = 0;
	while (it != analyses->end()) {
		char * result = new char[len + 1];
		result[len] = '\0';
		interpretAnalysis(*it++, result, len - (*dotRemoved ? 1 : 0));
		if (*dotRemoved) {
			result[len - 1] = ' ';
		}
		allResults[analysisCount] = result;
		if (++analysisCount == MAX_ANALYSIS_COUNT) {
			break;
		}
	}
	Analyzer::deleteAnalyses(analyses);
	
	/* If the word could not be parsed, assume that it does not contain any
	   morpheme borders that we should know about (unless there is a hyphen,
	   which tells us where the border is). If the entire word seems impossible
	   to hyphenate, do not split it. */
	if (analysisCount == 0) {
		char * result = new char[len + 1];
		
		// If unknown words are not allowed to be hyphenated, forbid hypenation
		// at all positions.
		memset(result, hyphenateUnknown ? ' ' : 'X', len);
		
		if (allowRuleHyphenation(word, len)) {
			for (size_t i = 1; i < len - 1; i++) {
				if (word[i] == L'-') {
					result[i] = '=';
				}
			}
		}
		result[len] = '\0';
		allResults[0] = result;
		analysisCount++;
	}
	allResults[analysisCount] = 0;
	delete[] wordUtf8;

	removeExtraHyphenations(allResults, len);

	return allResults;
}

void AnalyzerToFinnishHyphenatorAdapter::compoundHyphenation(
		const wchar_t * word, char * hyphenation, size_t len) const {
	size_t start = 0;
	while (start < len && hyphenation[start] == '=') {
		start++;
	}
	size_t end = start + 1;
	while (end < len) {
		if (hyphenation[end] != ' ' && hyphenation[end] != 'X') {
			if (end >= start + minHyphenatedWordLength) {
				ruleHyphenation(&word[start], &hyphenation[start], end - start);
			}
			if (hyphenation[end] == '=') {
				start = end + 1;
			}
			else {
				start = end;
			}
			end = start + 1;
		}
		else {
			end++;
		}
	}
	if (end == len && start < end && end >= start + minHyphenatedWordLength) {
		ruleHyphenation(&word[start], &hyphenation[start], end - start);
	}
}

char * AnalyzerToFinnishHyphenatorAdapter::intersectHyphenations(char ** hyphenations) const {
	size_t len = strlen(hyphenations[0]);
	char * intersection = new char[len + 1];

	strcpy(intersection, hyphenations[0]);
	for (size_t i = 0; i < len; i++) {
		if (intersection[i] == 'X') {
			intersection[i] = ' ';
		}
	}
	char ** currentPtr = &hyphenations[1];
	while (*currentPtr != 0) {
		for (size_t i = 0; i < len; i++) {
			if ((*currentPtr)[i] == ' ' || (*currentPtr)[i] == 'X') {
				intersection[i] = ' ';
			}
		}
		currentPtr++;
	}
	return intersection;
}

void AnalyzerToFinnishHyphenatorAdapter::interpretAnalysis(const Analysis * analysis,
	         char * buffer, size_t len) const {
	const wchar_t * structure = analysis->getValue("STRUCTURE");
	const wchar_t * structurePtr = structure;
	memset(buffer, ' ', len);
	if (*structurePtr == L'=') {
		structurePtr++;
	}
	for (size_t i = 0; i < len; i++) {
		if (structurePtr[0] == L'\0') {
			break;
		}
		if (structurePtr[0] == L'-' && structurePtr[1] == L'=') {
			if (i != 0) {
				buffer[i] = '=';
			}
			structurePtr += 2;
			continue;
		}
		if (structurePtr[0] == L'=') {
			buffer[i] = '-';
			structurePtr += 2;
			continue;
		}
		if (structurePtr[0] == L'j' || structurePtr[0] == L'q') {
			buffer[i] = 'X';
		}
		structurePtr++;
	}
}

bool AnalyzerToFinnishHyphenatorAdapter::allowRuleHyphenation(const wchar_t * word,
	         size_t nchars) const {
	// Word is too short
	if (nchars <= 1) {
		return false;
	}
	
	if (!uglyHyphenation) {
		// non-word strings are not hyphenated
		if (voikko_is_nonword(word, nchars)) {
			return false;
		}
	
		// Word ends with number (not safe to be hyphenated)
		if (word[nchars - 1] >= L'0' && word[nchars - 1] <= L'9') {
			return false;
		}
	}
	
	return true;
}

void AnalyzerToFinnishHyphenatorAdapter::removeExtraHyphenations(
	    char ** hyphenations, size_t len) const {
	int minParts = 0;
	int hyphenationCount = 0;
	char ** currentBuffer = hyphenations;
	while (*currentBuffer != 0) {
		hyphenationCount++;
		int currentParts = 1;
		for (size_t i = 0; i < len; i++) {
			if ((*currentBuffer)[i] != ' ' && (*currentBuffer)[i] != 'X') {
				currentParts++;
			}
		}
		if (minParts == 0 || minParts > currentParts) {
			minParts = currentParts;
		}
		currentBuffer++;
	}
	if (minParts > 1) {
		return; /* nothing to do */
	}
	
	/* delete items from array where current_parts > min_parts */
	int j = 0;
	while (j < hyphenationCount) {
		currentBuffer = hyphenations + j;
		int currentParts = 1;
		for (size_t i = 0; i < len; i++) {
			if ((*currentBuffer)[i] != ' ' && (*currentBuffer)[i] != 'X') {
				currentParts++;
			}
		}
		if (currentParts > minParts) {
			delete[] hyphenations[j];
			hyphenations[j] = hyphenations[--hyphenationCount];
			hyphenations[hyphenationCount] = 0;
		}
		else {
			j++;
		}
	}
	/* TODO: remove indentically split words */
}

void AnalyzerToFinnishHyphenatorAdapter::ruleHyphenation(const wchar_t * word,
	            char * hyphenationPoints, size_t nchars) const {
	size_t i;
	
	if (!allowRuleHyphenation(word, nchars)) {
		return;
	}
	
	/* TODO: the following is not enough if we later want to prevent hyphenation at single
	 * points, not only in whole word segments. */
	if (hyphenationPoints[0] == 'X') {
		return;
	}
	
	wchar_t * wordCopy = new wchar_t[nchars + 1];
	if (wordCopy == 0) {
		return;
	}
	
	for (i = 0; i < nchars; i++) {
		wordCopy[i] = SimpleChar::lower(word[i]);
	}
	wordCopy[nchars] = '\0';
	
	/* at least one vowel is required before the first hyphen */
	i = 0;
	while (wordCopy[i] != L'\0' && wcschr(VOIKKO_CONSONANTS, wordCopy[i])) {
		i++;
	}
	
	/* -CV (not after special characters, hyphenating "vast'edes" as "vast'e-des" is ugly) */
	for (; i <= nchars - 2; i++) {
		if (wcschr(VOIKKO_CONSONANTS, wordCopy[i]) && wcschr(VOIKKO_VOWELS, wordCopy[i+1])
		    && !wcschr(L"/.:&%\'", wordCopy[i-1])
			&& (i <= 1 || uglyHyphenation || wordCopy[i-2] != L'\'')) {
			hyphenationPoints[i] = '-';
		}
	}
	
	/* 'V */
	for (i = 1; i < nchars - 1; i++) {
		if (wordCopy[i] == L'\'' && wcschr(VOIKKO_VOWELS, wordCopy[i+1])) {
			hyphenationPoints[i] = '=';
		}
	}
	
	/* Split before and after long vowels */
	for (i = 1; i < nchars - 1; i++) {
		if (wcschr(VOIKKO_VOWELS, wordCopy[i]) && wordCopy[i] == wordCopy[i+1]) {
			if (wcschr(VOIKKO_VOWELS, wordCopy[i-1]) &&
				isGoodHyphenPosition(wordCopy, hyphenationPoints, i, nchars)) {
				hyphenationPoints[i] = '-';
			}
			if (isGoodHyphenPosition(wordCopy, hyphenationPoints, i+2, nchars)) {
				hyphenationPoints[i+2] = '-';
			}
		}
	}
	
	/* V-V */
	for (i = 0; i < nchars - 1; i++) {
		if (hyphenationPoints[i+1] != ' ') {
			continue;
		}
		if (!wcschr(VOIKKO_VOWELS, wordCopy[i])) {
			continue;
		}
		if (!wcschr(VOIKKO_VOWELS, wordCopy[i+1])) {
			continue;
		}
		for (size_t j = 0; j < 18; j++) {
			if (wcsncmp(&wordCopy[i], SPLIT_VOWELS[j], 2) == 0) {
				hyphenationPoints[i+1] = '-';
				break;
			}
		}
	}
	
	/* long consonants */
	for (i = 1; i < nchars - 1; i++) {
		for (size_t j = 0; j < 5; j++) {
			if (i + wcslen(LONG_CONSONANTS[j]) < nchars &&
			    wcsncmp(&wordCopy[i], LONG_CONSONANTS[j], wcslen(LONG_CONSONANTS[j])) == 0) {
				for (size_t k = i + 1; k <= i + wcslen(LONG_CONSONANTS[j]); k++) {
					if (hyphenationPoints[k] == '-') {
						hyphenationPoints[k] = ' ';
						hyphenationPoints[i] = '-';
					}
				}
			}
		}
	}
	
	if (!uglyHyphenation) {
		hyphenationPoints[1] = ' ';
		hyphenationPoints[nchars-1] = ' ';
		for (i = 0; i <= nchars - 2; i++) {
			if (wcschr(VOIKKO_VOWELS, wordCopy[i]) && wcschr(VOIKKO_VOWELS, wordCopy[i+1])) {
				hyphenationPoints[i+1] = ' ';
			}
		}
	}
	else if (nchars >= 3) {
		/* VV-V */
		for (i = 0; i < nchars - 3; i++) {
			for (size_t j = 0; j < 2; j++) {
				if (hyphenationPoints[i+1] != '-' &&
				    wcsncmp(wordCopy + i, SPLIT_AFTER[j], 2) == 0 &&
				    wcschr(VOIKKO_VOWELS, wordCopy[i+2]) &&
				    isGoodHyphenPosition(wordCopy, hyphenationPoints, i+2, nchars)) {
					hyphenationPoints[i+2] = '-';
				}
			}
		}
	}
	
	delete[] wordCopy;
}

bool AnalyzerToFinnishHyphenatorAdapter::isGoodHyphenPosition(const wchar_t * word,
	     const char * hyphenationPoints, size_t newHyphenPos, size_t nchars) const {
	// Check for out of bounds hyphen
	if (newHyphenPos == 0 || newHyphenPos + 1 >= nchars) {
		return false;
	}
	
	// Check backwards for vowels
	bool hasVowel = false;
	for (size_t i = newHyphenPos - 1; hyphenationPoints[i] != '-' && hyphenationPoints[i] != '='; i--) {
		if (i == 0) {
			break;
		}
		if (wcschr(VOIKKO_VOWELS, word[i])) {
			hasVowel = true;
		}
	}
	if (!hasVowel) {
		return false;
	}
	
	// Check forward for vowels
	hasVowel = false;
	for (size_t i = newHyphenPos; i < nchars &&
	     hyphenationPoints[i] != '-' && hyphenationPoints[i] != '='; i++) {
		if (word[i] == L'.') {
			break;
		}
		if (wcschr(VOIKKO_VOWELS, word[i])) {
			hasVowel = true;
		}
	}
	if (!hasVowel) {
		return false;
	}
	
	return true;
}


} }
