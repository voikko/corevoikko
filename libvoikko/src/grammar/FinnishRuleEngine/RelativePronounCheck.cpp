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
 * Portions created by the Initial Developer are Copyright (C) 2019
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

#include "grammar/FinnishRuleEngine/RelativePronounCheck.hpp"
#include "grammar/error.hpp"

using namespace std;

namespace libvoikko { namespace grammar { namespace check {

static bool isJoka(const Token * t) {
	return t->baseform && wcscmp(t->baseform, L"joka") == 0;
}


static bool isMika(const Token * t) {
	return t->baseform && wcscmp(t->baseform, L"mik\u00e4") == 0;
}

static bool isSpecialExpression(std::vector<const Token *> & tokens, size_t assumedRP) {
	if (tokens.size() - 1 > assumedRP) {
		if (wcscmp(L"vaan", tokens[assumedRP + 1]->str) == 0 && isMika(tokens[assumedRP])) {
			return true; // mikä vaan, minne vaan jne.
		}
	}
	return false;
}

void RelativePronounCheck::check(voikko_options_t * options, const Sentence * sentence) {
	std::vector<const Token *> tokens = sentence->getNonWhitespaceTokens();
	for (size_t i = 1; i < tokens.size(); i++) {
		if ((isJoka(tokens[i]) || isMika(tokens[i])) && wcscmp(tokens[i - 1]->str, L",") != 0 && !isSpecialExpression(tokens, i)) {
			CacheEntry * e = new CacheEntry(0);
			e->error.setErrorCode(GCERR_COMMA_MISSING_BEFORE_RELATIVE_PRONOUN);
			e->error.setStartPos(0);
			e->error.setErrorLen(1);
			options->grammarChecker->cache.appendError(e);
		}
	}
}

} } }
