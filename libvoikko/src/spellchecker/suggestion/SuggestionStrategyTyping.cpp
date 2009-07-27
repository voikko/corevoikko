/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2009 Harri Pitkänen <hatapitk@iki.fi>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *********************************************************************************/

#include "spellchecker/suggestion/SuggestionStrategyTyping.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorDeletion.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorInsertion.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorInsertSpecial.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorReplacement.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorSplitWord.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorSwap.hpp"
#include "spellchecker/suggestion/SuggestionGeneratorVowelChange.hpp"

using namespace std;

namespace libvoikko { namespace spellchecker { namespace suggestion {

/* ä=\u00e4, ö=\u00f6, å=\u00e5, š=\u0161, ž=\u017e, é=\u00e9, â=\u00e2 */

SuggestionStrategyTyping::SuggestionStrategyTyping() {
	generators.push_back(
	    new SuggestionGeneratorVowelChange());
	generators.push_back(
		new SuggestionGeneratorReplacement(
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
		L"ik" L"ty"
		));
	generators.push_back(
		new SuggestionGeneratorDeletion());
	generators.push_back(
		new SuggestionGeneratorInsertSpecial());
	generators.push_back(
		new SuggestionGeneratorSplitWord());
	generators.push_back(
		new SuggestionGeneratorReplacement(
		L"1q" L"2q" L"2w" L"3w" L"3e"
		L"4e" L"4r" L"5r" L"5t" L"6t"
		L"6y" L"7y" L"7u" L"8u" L"8i"
		L"9i" L"9o" L"0o" L"0p" L"+p"
		));
	generators.push_back(
		new SuggestionGeneratorInsertion(L"aitesn"));
	generators.push_back(
		new SuggestionGeneratorSwap());
	generators.push_back(
		new SuggestionGeneratorReplacement(
		L"es" L"sd" L"nh" L"uj" L"l\u00f6"
		L"kj" L"op" L"\u00e4p" L"mk" L"rd"
		L"vg" L"pl" L"yh" L"hu" L"ji"
		L"de" L"\u00f6l" L"gt" L"fv" L"bv"
		L"ck" L"wa" L"xs" L"za" L"qk"
		L"\u00e5a" L"a\u00e5" L"e\u00e9" L"a\u00e2" L"kc"
		L"sc" L"ij" L"xz"
		));
	generators.push_back(
		new SuggestionGeneratorInsertion(
		L"ulko\u00e4mrvpyhjd\u00f6gfbcw:xzq\u00e5"));
	generators.push_back(
		new SuggestionGeneratorReplacement(
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
		L"vc" L"aw" L"az" L"sq"
		));
}

}}}
