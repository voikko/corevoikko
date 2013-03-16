/* Libvoikko: Library of Finnish language tools
 * Copyright (C) 2010 Flammie Pirinen <flammie@iki.fi>
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

#include "spellchecker/HfstSuggestion.hpp"
#include "utils/StringUtils.hpp"
#include "setup/setup.hpp"

#include <fstream>

#include <ZHfstOspeller.h>

using namespace std;
using namespace libvoikko::utils;

using hfst_ol::ZHfstOspeller;

namespace libvoikko { namespace spellchecker { namespace suggestion {

HfstSuggestion::HfstSuggestion(hfst_ol::ZHfstOspeller * speller) throw(setup::DictionaryException) :
	speller_(speller) { }

void HfstSuggestion::generate(SuggestionStatus * s) const {
	size_t wlen = s->getWordLength();
	char * wordUtf8 = StringUtils::utf8FromUcs4(s->getWord(), wlen);
	hfst_ol::CorrectionQueue corrections = speller_->suggest(wordUtf8);
	unsigned int correction_count = 0;
	while (corrections.size() > 0 && !s->shouldAbort()) {
		const char * sugUtf8 = corrections.top().first.c_str();
		correction_count++;
		wchar_t * sugU4 = StringUtils::ucs4FromUtf8(sugUtf8, strlen(sugUtf8));
		s->addSuggestion(sugU4, correction_count++);
		corrections.pop();
	}
	delete[] wordUtf8;
}

void HfstSuggestion::terminate() {
	// do nothing, HfstSpeller manages all resources
}

} } }

// vim: set noexpandtab ts=4:
