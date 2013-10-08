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
 * Portions created by the Initial Developer are Copyright (C) 2008 - 2011
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

#include "utils/utils.hpp"
#include "grammar/GrammarChecker.hpp"
#include "grammar/cache.hpp"
#include "grammar/Analysis.hpp"
#include <cstring>
#include <cstdlib>

using namespace libvoikko::grammar;

namespace libvoikko {

// This will be initialized to zero meaning "no errors"
static const voikko_grammar_error no_grammar_error = voikko_grammar_error();

const voikko_grammar_error * gc_error_from_cache(voikko_options_t * voikkoOptions, const wchar_t * text,
                             size_t startpos, int skiperrors) {

	if (!voikkoOptions->grammarChecker->cache.paragraph) {
		return 0;
	}
	if (wcscmp(voikkoOptions->grammarChecker->cache.paragraph, text) != 0) {
		return 0;
	}
	CacheEntry * e = voikkoOptions->grammarChecker->cache.firstError;
	int preverrors = 0;
	while (e) {
		if (preverrors >= skiperrors &&
		    e->error.startpos >= startpos) {
			return &e->error;
		}
		preverrors++;
		e = e->nextError;
	}
	return &no_grammar_error;
}


}
