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
#include <list>

using namespace std;

namespace libvoikko { namespace grammar { namespace check {

static const int BUFFER_SIZE = 20000;

VfstAutocorrectCheck::VfstAutocorrectCheck(const string & fileName) throw(setup::DictionaryException) {
	transducer = new fst::UnweightedTransducer(fileName.c_str());
	configuration = new fst::Configuration(transducer->getFlagDiacriticFeatureCount(), BUFFER_SIZE);
	inputBuffer = new char[BUFFER_SIZE + 1];
	outputBuffer = new char[BUFFER_SIZE + 1];
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
	list<size_t> lookupPositions;
	size_t sentenceLength = 0;
	for (size_t i = 0; i < sentence->tokenCount; i++) {
		const Token * token = sentence->tokens + i;
		if (token->type == TOKEN_WORD) {
			lookupPositions.push_back(sentenceLength);
		}
		size_t tokenUtfLen = utils::StringUtils::utf8FromUcs4(token->str, token->tokenlen,
		                     inputBuffer + sentenceLength, BUFFER_SIZE - sentenceLength);
		if (tokenUtfLen == BUFFER_SIZE - sentenceLength + 1) {
			return; // sentence is unreasonably long
		}
		sentenceLength += tokenUtfLen;
	}
	for (list<size_t>::iterator i = lookupPositions.begin(); i != lookupPositions.end(); ++i) {
		size_t position = *i;
		if (transducer->prepare(configuration, inputBuffer + position, sentenceLength - position)) {
			if (transducer->next(configuration, outputBuffer, BUFFER_SIZE)) {
				CacheEntry * e = new CacheEntry(1);
				e->error.setErrorCode(GCERR_INVALID_SPELLING);
				e->error.setStartPos(sentence->pos + position);
				e->error.setErrorLen(1); // TODO
				e->error.getSuggestions()[0] = utils::StringUtils::copy(outputBuffer);
				options->grammarChecker->cache.appendError(e);
			}
		}
	}
}

} } }
