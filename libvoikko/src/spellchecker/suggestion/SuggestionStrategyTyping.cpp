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

#include "spellchecker/suggestion/SuggestionStrategyTyping.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorCaseChange.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorDeletion.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorInsertion.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorInsertSpecial.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorReplacement.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorReplaceTwo.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorSoftHyphens.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorSplitWord.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorSwap.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorVowelChange.hpp"

using namespace std;

namespace libvoikko { namespace spellchecker { namespace suggestion {

/* ä=\u00e4, ö=\u00f6, å=\u00e5, š=\u0161, ž=\u017e, é=\u00e9, â=\u00e2 */

static const wchar_t * const REPLACEMENTS_1 =
	L".," L"as" L"iu" L"io" L"tr"
	L"td" L"er" L"s\u0161" L"sa" L"nm"
	L"ui" L"lk" L"kl" L"kg" L"oi"
	L"\u00e4\u00f6" L"mn" L"re" L"rt" L"vb"
	L"pb" L"po" L"yt" L"hj" L"jh"
	L"jk" L"dt" L"ds" L"df" L"\u00f6\u00e4"
	L"gf" L"gh" L"gk" L"fg" L"fd"
	L"bp" L"bn" L"cv" L"cs" L"we"
	L"wv" L"xc" L"z\u017e" L"zx" L"qa"
	L"\u00e5o" L"\u00e5p" L"\u00e5\u00e4" L"\u00e5\u00f6" L"ae"
	L"ik" L"ty" L"ea";

static const wchar_t * const REPLACEMENTS_2 =
	L"1q" L"2q" L"2w" L"3w" L"3e"
	L"4e" L"4r" L"5r" L"5t" L"6t"
	L"6y" L"7y" L"7u" L"8u" L"8i"
	L"9i" L"9o" L"0o" L"0p" L"+p";

static const wchar_t * const REPLACEMENTS_3 =
	L"es" L"sd" L"nh" L"uj" L"l\u00f6"
	L"kj" L"op" L"\u00e4p" L"mk" L"rd"
	L"vg" L"pl" L"yh" L"hu" L"ji"
	L"de" L"\u00f6l" L"gt" L"fv" L"bv"
	L"ck" L"wa" L"xs" L"za" L"qk"
	L"\u00e5a" L"a\u00e5" L"e\u00e9" L"a\u00e2" L"kc"
	L"sc" L"ij" L"xz";

static const wchar_t * const REPLACEMENTS_4 =
	L"qw" L"qs" L"wq" L"ws" L"wd"
	L"ed" L"ef" L"rf" L"rg" L"tf"
	L"tg" L"th" L"yg" L"yj" L"uh"
	L"uk" L"il" L"ok" L"ol" L"p\u00f6"
	L"p\u00e4" L"se" L"sx" L"dr" L"bg"
	L"fe" L"fr" L"ft" L"fc" L"gy"
	L"gb" L"gv" L"hy" L"hn" L"hb"
	L"hg" L"ju" L"jm" L"jn" L"ki"
	L"ko" L"km" L"lo" L"lp" L"\u00f6p"
	L"\u00f6\u00e5" L"\u00e4\u00e5" L"zs" L"xd" L"cd"
	L"cf" L"cx" L"vf" L"bh" L"nj"
	L"nb" L"mj" L"ew" L"p\u00e5" L"aq"
	L"sw" L"sz" L"dw" L"dc" L"dx"
	L"vc" L"aw" L"az" L"sq";

static const wchar_t * const REPLACEMENTS_5 =
	L"ao" L"oa";

SuggestionStrategyTyping::SuggestionStrategyTyping(morphology::Analyzer * morAnalyzer, size_t maxCost) : SuggestionStrategy(maxCost) {
	primaryGenerators.push_back(
		new SuggestionGeneratorCaseChange(morAnalyzer));
	primaryGenerators.push_back(
		new SuggestionGeneratorSoftHyphens(morAnalyzer));
	generators.push_back(
		new SuggestionGeneratorVowelChange(morAnalyzer));
	generators.push_back(
		new SuggestionGeneratorReplacement(REPLACEMENTS_1, morAnalyzer));
	generators.push_back(
		new SuggestionGeneratorDeletion(morAnalyzer));
	generators.push_back(
		new SuggestionGeneratorInsertSpecial(morAnalyzer));
	generators.push_back(
		new SuggestionGeneratorSplitWord(morAnalyzer));
	generators.push_back(
		new SuggestionGeneratorReplaceTwo(REPLACEMENTS_1, morAnalyzer));
	generators.push_back(
		new SuggestionGeneratorReplacement(REPLACEMENTS_2, morAnalyzer));
	generators.push_back(
		new SuggestionGeneratorInsertion(L"aitesn", morAnalyzer));
	generators.push_back(
		new SuggestionGeneratorSwap(morAnalyzer));
	generators.push_back(
		new SuggestionGeneratorReplacement(REPLACEMENTS_3, morAnalyzer));
	generators.push_back(
		new SuggestionGeneratorInsertion(
		L"ulko\u00e4mrvpyhjd\u00f6gfbcw:xzq\u00e5'", morAnalyzer));
	generators.push_back(
		new SuggestionGeneratorReplacement(REPLACEMENTS_4, morAnalyzer));
	generators.push_back(
		new SuggestionGeneratorReplaceTwo(REPLACEMENTS_2, morAnalyzer));
	generators.push_back(
		new SuggestionGeneratorReplaceTwo(REPLACEMENTS_3, morAnalyzer));
	generators.push_back(
		new SuggestionGeneratorReplaceTwo(REPLACEMENTS_4, morAnalyzer));
	generators.push_back(
		new SuggestionGeneratorReplacement(REPLACEMENTS_5, morAnalyzer));
}

}}}
