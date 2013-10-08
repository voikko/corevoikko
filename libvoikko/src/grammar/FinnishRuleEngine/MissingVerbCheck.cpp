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
 * Portions created by the Initial Developer are Copyright (C) 2011 - 2012
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

#include "grammar/FinnishRuleEngine/MissingVerbCheck.hpp"
#include "character/SimpleChar.hpp"
#include "grammar/error.hpp"

using namespace std;

namespace libvoikko { namespace grammar { namespace check {

void MissingVerbCheck::check(voikko_options_t * options, const Sentence * sentence) {
	const Token * firstToken = sentence->tokens;
	if (firstToken->type == TOKEN_PUNCTUATION) {
		return;
	}
	if (options->accept_bulleted_lists_in_gc && sentence->pos == 0 &&
	    (character::SimpleChar::isLower(firstToken->str[0]) || !firstToken->firstLetterLcase)) {
		return;
	}
	int wordCount = 0;
	const Token * lastNonWhitespace = 0;
	bool foundVerbInSentence = false;
	bool foundVerbInCurrentClause = false;
	size_t lastVerbStartToken = 0;
	for (size_t i = 0; i < sentence->tokenCount; i++) {
		const Token * token = sentence->tokens + i;
		if (token->str[0] == L'\t') {
			return;
		}
		if (token->type == TOKEN_WORD) {
			wordCount++;
			// TODO: Sanastoformaatin versiossa 2 ei ole mahdollista tunnistaa
			// verbiä rakenteista "Kukaan ei _vastannut_." Toistaiseksi siis
			// pelkkä kieltosana riittää täyttämään tämän säännön vaatimukset.
			if (!token->isValidWord || token->possibleMainVerb || token->isVerbNegative) {
				foundVerbInSentence = true;
			}
			if (token->possibleConjunction) {
				foundVerbInCurrentClause = false;
			}
			else if (i + 2 < sentence->tokenCount && wcsncmp(token->str, L"siin\u00e4", 5) == 0 &&
			         wcsncmp((sentence->tokens + (i + 2))->str, L"miss\u00e4", 5) == 0) {
				// "siinä missä" voi erottaa lauseita ilman pilkkua. TODO: siistimpi toteutus
				foundVerbInCurrentClause = false;
			}
			else if (token->isMainVerb) {
				if (foundVerbInCurrentClause) {
					// Suppress this error if generic repeating word check applies here
					if (i != lastVerbStartToken + 2 ||
					    token->tokenlen != sentence->tokens[lastVerbStartToken].tokenlen ||
					    wcsncmp(token->str, sentence->tokens[lastVerbStartToken].str, token->tokenlen) != 0) {
						CacheEntry * e = new CacheEntry(0);
						e->error.error_code = GCERR_EXTRA_MAIN_VERB;
						e->error.startpos = sentence->tokens[lastVerbStartToken].pos;
						e->error.errorlen = token->pos + token->tokenlen - sentence->tokens[lastVerbStartToken].pos;
						options->grammarChecker->cache.appendError(e);
					}
					foundVerbInCurrentClause = false;
				}
				else {
					foundVerbInCurrentClause = true;
					lastVerbStartToken = i;
				}
			}
		}
		else if (token->type == TOKEN_PUNCTUATION) {
			foundVerbInCurrentClause = false;
		}
		if (token->type != TOKEN_WHITESPACE) {
			lastNonWhitespace = token;
		}
	}
	if (foundVerbInSentence || wordCount < 2 || wcschr(L".?", lastNonWhitespace->str[0]) == 0) {
		return;
	}
	CacheEntry * e = new CacheEntry(0);
	e->error.error_code = GCERR_MISSING_MAIN_VERB;
	e->error.startpos = sentence->pos;
	e->error.errorlen = lastNonWhitespace->pos + lastNonWhitespace->tokenlen - sentence->pos;
	options->grammarChecker->cache.appendError(e);
}

} } }
