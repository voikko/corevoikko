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

#include "grammar/FinnishRuleEngine/VfstAutocorrectCheck.hpp"
#include "grammar/error.hpp"
#include "utils/StringUtils.hpp"
#include "character/SimpleChar.hpp"
#include <list>
#include <vector>

using namespace std;

namespace libvoikko { namespace grammar { namespace check {

static const size_t BUFFER_SIZE = 20000;

VfstAutocorrectCheck::VfstAutocorrectCheck(const string & fileName) throw(setup::DictionaryException) {
	transducer = new fst::UnweightedTransducer(fileName.c_str());
	configuration = new fst::Configuration(transducer->getFlagDiacriticFeatureCount(), BUFFER_SIZE);
	inputBuffer = new wchar_t[BUFFER_SIZE + 1];
	outputBuffer = new wchar_t[BUFFER_SIZE + 1];
}

VfstAutocorrectCheck::~VfstAutocorrectCheck() {
	delete[] outputBuffer;
	delete[] inputBuffer;
	delete configuration;
	if (transducer) {
		transducer->terminate();
		delete transducer;
	}
}

void VfstAutocorrectCheck::check(voikko_options_t * options, const Sentence * sentence) {
	if (check(options, sentence, false)) {
		check(options, sentence, true);
	}
}

bool VfstAutocorrectCheck::check(voikko_options_t * options, const Sentence * sentence, bool lowerFirst) {
	list<size_t> lookupPositionsUtf;
	list<size_t> lookupPositionsUcs;
	vector<size_t> ucsOriginalPositions;
	vector<size_t> ucsNormalizedPositions;
	ucsOriginalPositions.push_back(0);
	ucsNormalizedPositions.push_back(0);
	size_t sentenceLengthUtf = 0;
	size_t sentenceLengthUcs = 0;
	for (size_t i = 0; i < sentence->tokenCount; i++) {
		const Token * token = sentence->tokens + i;
		if (token->type == TOKEN_WORD) {
			lookupPositionsUtf.push_back(sentenceLengthUtf);
			lookupPositionsUcs.push_back(sentenceLengthUcs);
		}
		size_t tokenUtfLen;
		if (token->type == TOKEN_WHITESPACE) {
			tokenUtfLen = 1;
			if (sentenceLengthUtf >= BUFFER_SIZE) {
				return false; // sentence is unreasonably long
			}
			inputBuffer[sentenceLengthUtf] = L' ';
			ucsNormalizedPositions.push_back(ucsNormalizedPositions[i] + 1);
		}
		else {
			size_t skippedChars = 0;
			wchar_t * tokenStr;
			if (lowerFirst && i == 0) {
				tokenStr = utils::StringUtils::copy(token->str);
				tokenStr[0] = character::SimpleChar::lower(tokenStr[0]);
			}
			else {
				tokenStr = token->str;
			}
			if (sentenceLengthUtf + token->tokenlen >= BUFFER_SIZE) {
				return false; // sentence is unreasonably long
			}
			for (size_t i = 0; i < token->tokenlen; i++) {
				if (::wcschr(L"\u00AD", tokenStr[i])) {
					skippedChars++;
				}
				else {
					inputBuffer[sentenceLengthUtf + i - skippedChars] = tokenStr[i];
				}
			}
			tokenUtfLen = token->tokenlen - skippedChars;
			if (lowerFirst && i == 0) {
				delete[] tokenStr;
			}
			ucsNormalizedPositions.push_back(ucsNormalizedPositions[i] + token->tokenlen - skippedChars);
		}
		sentenceLengthUtf += tokenUtfLen;
		sentenceLengthUcs += token->tokenlen;
		ucsOriginalPositions.push_back(sentenceLengthUcs);
	}
	list<size_t>::iterator ucsPositions = lookupPositionsUcs.begin();
	bool needLowering = false;
	for (list<size_t>::iterator i = lookupPositionsUtf.begin(); i != lookupPositionsUtf.end(); ++i) {
		size_t position = *i;
		if (lowerFirst && position > 0) {
			break;
		}
		size_t ucsPosition = *(ucsPositions++);
		transducer->prepare(configuration, inputBuffer + position, sentenceLengthUtf - position);
		size_t prefixLength = 0;
		if (transducer->nextPrefix(configuration, outputBuffer, BUFFER_SIZE, &prefixLength)) {
			bool endAtBoundary = false;
			for (vector<size_t>::iterator p = ucsNormalizedPositions.begin(); p != ucsNormalizedPositions.end(); ++p) {
				if (ucsPosition + prefixLength == *p) {
					endAtBoundary = true;
					break;
				}
			}
			if (!endAtBoundary) {
				continue; // end is not at word boundary
			}
			CacheEntry * e = new CacheEntry(1);
			e->error.setErrorCode(GCERR_INVALID_SPELLING);
			size_t startPos = sentence->pos + ucsPosition;
			e->error.setStartPos(startPos);
			size_t lengthCorrection = 0;
			for (size_t n = 0; n < ucsOriginalPositions.size(); n++) {
				size_t oPos = ucsOriginalPositions[n];
				size_t nPos = ucsNormalizedPositions[n];
				if (oPos <= startPos) {
					lengthCorrection = oPos - nPos;
				}
				if (nPos > startPos + prefixLength) {
					lengthCorrection = (ucsOriginalPositions[n - 1] - ucsNormalizedPositions[n - 1]) - lengthCorrection;
					break;
				}
			}
			e->error.setErrorLen(prefixLength + lengthCorrection);
			if (lowerFirst) {
				outputBuffer[0] = character::SimpleChar::upper(outputBuffer[0]);
			}
			e->error.getSuggestions()[0] = utils::StringUtils::utf8FromUcs4(outputBuffer);
			options->grammarChecker->cache.appendError(e);
		}
		else {
			if (!lowerFirst && position == 0 && character::SimpleChar::isUpper(sentence->tokens[0].str[0])) {
				needLowering = true;
			}
		}
	}
	return needLowering;
}

} } }
