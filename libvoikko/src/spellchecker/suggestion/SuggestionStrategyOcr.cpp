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
 * Portions created by the Initial Developer are Copyright (C) 2009 - 2012
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

#include "spellchecker/suggestion/SuggestionStrategyOcr.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorReplacement.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorMultiReplacement.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorCaseChange.hpp"

using namespace std;

namespace libvoikko { namespace spellchecker { namespace suggestion {

static const wchar_t * const REPLACEMENTS =
    L"0o" L"li" L"il" L"uo" L"ou"
    L"a\u00e4" L"\u00e4a" L"o\u00f6" L"\u00f6o" L"s\u0161"
    L"\u0161s" L"z\u017e" L"\u017ez" L"e\u00e9" L"\u00e9e"
    L"a\u00e2" L"\u00e2a" L"pb" L"bp" L"ef"
    L"fe" L"qo" L"oq" L"nm" L"mn"
    L"uv" L"vu" L"oc" L"co" L"bh"
    L"hb" L"_a" L"_b" L"_c" L"_d"
    L"_e" L"_f" L"_g" L"_h" L"_i"
    L"_j" L"_k" L"_l" L"_m" L"_n"
    L"_o" L"_p" L"_q" L"_r" L"_s"
    L"_t" L"_u" L"_v" L"_w" L"_x"
    L"_y" L"_z" L"_\u00e4" L"_\u00f6";


SuggestionStrategyOcr::SuggestionStrategyOcr(morphology::Analyzer * morAnalyzer, size_t maxCost) : SuggestionStrategy(maxCost) {
	primaryGenerators.push_back(
		new SuggestionGeneratorCaseChange(morAnalyzer));
	generators.push_back(
		new SuggestionGeneratorReplacement(
		REPLACEMENTS, morAnalyzer));
	generators.push_back(
		new SuggestionGeneratorMultiReplacement(
		REPLACEMENTS, 2, morAnalyzer));
}

}}}
