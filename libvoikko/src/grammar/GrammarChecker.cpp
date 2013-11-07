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
 * Portions created by the Initial Developer are Copyright (C) 2009
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

#include "setup/setup.hpp"
#include "grammar/GrammarChecker.hpp"

namespace libvoikko { namespace grammar {

// This will be initialized to zero meaning "no errors"
static const VoikkoGrammarError no_grammar_error = VoikkoGrammarError();


GrammarChecker::~GrammarChecker() {
	cache.clear();
}

const VoikkoGrammarError * GrammarChecker::errorFromCache(const wchar_t * text, size_t startpos, int skiperrors) {
	if (!cache.paragraph) {
		return 0;
	}
	if (wcscmp(cache.paragraph, text) != 0) {
		return 0;
	}
	CacheEntry * e = cache.firstError;
	int preverrors = 0;
	while (e) {
		if (preverrors >= skiperrors &&
		    e->error.getStartPos() >= startpos) {
			return &e->error;
		}
		preverrors++;
		e = e->nextError;
	}
	return &no_grammar_error;
}

void GrammarChecker::paragraphToCache(const wchar_t * text, size_t textlen) {
	cache.clear();
	cache.paragraph = new wchar_t[textlen + 1];
	if (!cache.paragraph) {
		return;
	}
	memcpy(cache.paragraph, text, textlen * sizeof(wchar_t));
	cache.paragraph[textlen] = L'\0';
	Paragraph * para = paragraphAnalyser->analyseParagraph(text, textlen);
	if (!para) {
		return;
	}
	
	// If paragraph is a single sentence without any whitespace, do not try to
	// do grammar checking on it. This could be an URL or something equally
	// strange.
	if (para->sentenceCount == 1) {
		Sentence * sentence = para->sentences[0];
		bool hasWhitespace = false;
		for (size_t i = 0; i < sentence->tokenCount; i++) {
			if (sentence->tokens[i].type == TOKEN_WHITESPACE) {
				hasWhitespace = true;
				break;
			}
		}
		if (!hasWhitespace) {
			// If this is a single word sentence, we should check it, otherwise
			// it makes no sense to try.
			if (sentence->tokenCount > 2 || sentence->tokenCount == 0 ||
			    sentence->tokens[0].type != TOKEN_WORD) {
				delete para;
				return;
			}
		}
	}
	
	ruleEngine->check(para);
	delete para;
}

} }
