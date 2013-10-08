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
 * Portions created by the Initial Developer are Copyright (C) 2011
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

#include "grammar/FinnishRuleEngine/SidesanaCheck.hpp"
#include "grammar/error.hpp"

using namespace std;

namespace libvoikko { namespace grammar { namespace check {

void SidesanaCheck::check(voikko_options_t * options, const Sentence * sentence) {
	size_t tokenCount = sentence->tokenCount;
	if ((sentence->tokens + (tokenCount - 1))->type == TOKEN_WHITESPACE) {
		--tokenCount;
	}
	if (tokenCount >= 2 &&
	    ((sentence->tokens + (tokenCount - 2))->isConjunction) &&
	    (wcscmp((sentence->tokens + (tokenCount - 2))->str, L"vaan") != 0) && // "mitä vaan" ~ "mitä vain"
	    ((sentence->tokens + (tokenCount - 1))->type == TOKEN_PUNCTUATION) &&
	    ((sentence->tokens + (tokenCount - 1))->str[0] == L'.')) {
		CacheEntry * e = new CacheEntry(0);
		e->error.error_code = GCERR_MISPLACED_SIDESANA;
		e->error.startpos = (sentence->tokens + (tokenCount - 2))->pos;
		e->error.errorlen = (sentence->tokens + (tokenCount - 2))->tokenlen;
		options->grammarChecker->cache.appendError(e);
	}
}

} } }
