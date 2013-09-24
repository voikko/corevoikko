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
 * Portions created by the Initial Developer are Copyright (C) 2006 - 2010
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

#ifndef VOIKKO_SETUP_SETUP_H
#define VOIKKO_SETUP_SETUP_H

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "morphology/Analyzer.hpp"
#include "spellchecker/Speller.hpp"
#include "spellchecker/SpellerCache.hpp"
#include "grammar/GrammarChecker.hpp"
#include "spellchecker/suggestion/SuggestionGenerator.hpp"
#include "hyphenator/Hyphenator.hpp"
#include "setup/Dictionary.hpp"

// TODO proper abstraction
namespace hfst_ol {
	class ZHfstOspeller;
}

namespace libvoikko {

typedef struct {
	int ignore_dot;
	int ignore_numbers;
	int ignore_uppercase;
	int ignore_nonwords;
	int accept_first_uppercase;
	int accept_all_uppercase;
	int accept_extra_hyphens;
	int accept_missing_hyphens;
	int accept_titles_in_gc;
	int accept_unfinished_paragraphs_in_gc;
	int accept_bulleted_lists_in_gc;
	//grammar::GcCache gc_cache;
	grammar::GrammarChecker * grammarChecker;
	morphology::Analyzer * morAnalyzer;
	spellchecker::Speller * speller;
	spellchecker::SpellerCache * spellerCache;
	spellchecker::suggestion::SuggestionGenerator * suggestionGenerator;
	hyphenator::Hyphenator * hyphenator;
	setup::Dictionary dictionary;
	hfst_ol::ZHfstOspeller* hfst;
} voikko_options_t;

}

#endif
