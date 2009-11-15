/* Libvoikko: Library of Finnish language tools
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

#include "spellchecker/HfstSpeller.hpp"
#include "utils/StringUtils.hpp"
#include "voikko_defs.h"
#include <fstream>
#include <hfst2/string.h>

using namespace std;
using namespace libvoikko::utils;

namespace libvoikko { namespace spellchecker {

HfstSpeller::HfstSpeller(const string & directoryName) throw(setup::DictionaryException) {
	keyTable = HWFST::create_key_table();
	string spellerFile = directoryName + "/spl.hwfst";
	ifstream spellerStream(spellerFile.c_str());
	if (spellerStream.good()) {
		speller = HWFST::read_transducer(spellerStream, keyTable);
	}
	else {
		throw setup::DictionaryException("Failed to open spl.hwfst");
	}
}
    
spellresult HfstSpeller::spell(const wchar_t * word, size_t wlen) {
	if (wlen > LIBVOIKKO_MAX_WORD_CHARS) {
		return SPELL_FAILED;
	}
	char * wordUtf8 = StringUtils::utf8FromUcs4(word, wlen);
	HWFST::KeyVector * wordPath = HWFST::stringUtf8ToKeyVector(wordUtf8, keyTable);
	HWFST::KeyVector * lookup = HWFST::lookup_first(speller, wordPath);
	spellresult result = (lookup != NULL ? SPELL_OK : SPELL_FAILED);
	delete lookup;
	delete wordPath;
	delete[] wordUtf8;
	return result;
}

void HfstSpeller::terminate() {
	delete keyTable;
	keyTable = 0;
	delete speller;
	speller = 0;
}

} }
