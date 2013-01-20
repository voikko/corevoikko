/* Libvoikko: Library of Finnish language tools
 * Copyright (C) 2009 - 2013 Harri Pitkänen <hatapitk@iki.fi>
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
#include "character/SimpleChar.hpp"
#include "voikko_defines.h"
#include <fstream>
#include <ospell.h>
#include <ol-exceptions.h>

using namespace std;
using namespace libvoikko::character;
using namespace libvoikko::utils;

using hfst_ol::ZHfstOspeller;

namespace libvoikko { namespace spellchecker {

HfstSpeller::HfstSpeller(const string & zhfstFileName) throw(setup::DictionaryException) {
	speller = new ZHfstOspeller();
	try {
		speller->read_zhfst(zhfstFileName.c_str());
	}
	catch (hfst_ol::ZHfstZipReadingError& zhzre) {
		throw setup::DictionaryException("Error reading ZHFST speller");
	}
}

spellresult HfstSpeller::doSpell(const wchar_t * word, size_t wlen) {
	char * wordUtf8 = StringUtils::utf8FromUcs4(word, wlen);
	spellresult result = SPELL_FAILED;
	if (speller->spell(wordUtf8)) {
		result = SPELL_OK;
	}
	return result;
}
 
spellresult HfstSpeller::spell(const wchar_t * word, size_t wlen) {
	spellresult result = doSpell(word, wlen);
	if (result == SPELL_FAILED && SimpleChar::isLower(word[0])) {
		// XXX: This slightly inefficient hack allows us to support SPELL_CAP_FIRST
		wchar_t * modifiedWord = StringUtils::copy(word);
		modifiedWord[0] = SimpleChar::upper(word[0]);
		result = doSpell(modifiedWord, wlen);
		if (result == SPELL_OK) {
			result = SPELL_CAP_FIRST;
		}
		delete[] modifiedWord;
	}
	return result;
}

void HfstSpeller::terminate() {
	if (speller != 0) {
		delete speller;
		speller = 0;
	}
}

} }
// vim: set noexpandtab ts=4:
