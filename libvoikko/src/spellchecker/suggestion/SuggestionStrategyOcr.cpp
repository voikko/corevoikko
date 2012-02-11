/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2009 - 2012 Harri Pitkänen <hatapitk@iki.fi>
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
