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
#include <fstream>

using namespace std;
using namespace libvoikko::utils;

namespace libvoikko { namespace spellchecker { namespace suggestion {

HfstSuggestion::HfstSuggestion(const string & directoryName) throw(setup::DictionaryException) {
	string spellerFile = directoryName + "/sug.hfstol";
	string suggerFile = directoryName + "/err.hfstol";
		FILE * error_source = fopen(suggerFile.c_str(), "r");
		FILE * lexiconfile = fopen(spellerFile.c_str(), "r");
		hfst_ol::Transducer * error = 0;
		hfst_ol::Transducer * lexicon = 0;
		try {
				error = new hfst_ol::Transducer(error_source);
				lexicon = new hfst_ol::Transducer(lexiconfile);
		} catch (hfst_ol::HeaderParsingException& e) {
			throw setup::DictionaryException(e.what());
		}
		try {
			speller_ = new hfst_ol::Speller(error, lexicon);
		} catch (hfst_ol::AlphabetTranslationException& e) {
			throw setup::DictionaryException(e.what());
		}
}

void HfstSuggestion::generate(SuggestionStatus* s) const {
	size_t wlen = s->getWordLength();
	char * wordUtf8 = StringUtils::utf8FromUcs4(s->getWord(), wlen);
	hfst_ol::CorrectionQueue corrections = speller_->correct(wordUtf8);
	unsigned int correction_count = 0;
	while (corrections.size() > 0) {
		const char* sugUtf8 = corrections.top().first.c_str();
		corrections.pop();
		correction_count++;
		wchar_t* sugU4 = StringUtils::ucs4FromUtf8(sugUtf8,
												strlen(sugUtf8));
		s->addSuggestion(sugU4, correction_count++);
	}
	delete[] wordUtf8;
}

void HfstSuggestion::terminate() {
	delete speller_;
}

} } }
