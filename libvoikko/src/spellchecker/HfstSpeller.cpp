/* Libvoikko: Library of Finnish language tools
 * Copyright (C) 2009 - 2010 Harri Pitkänen <hatapitk@iki.fi>
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


HfstSpeller::HfstSpeller(const string & directoryName, voikko_options_t* opts)
throw(setup::DictionaryException) {
	speller_ = 0;
	if ((opts != 0) && (opts->hfst != 0)) {
		speller_ = opts->hfst;
	}
	else {
		string spellerFile = directoryName + "/speller.zhfst";
		speller_ = new ZHfstOspeller();
		try {
			speller_->read_zhfst(spellerFile.c_str());
		}
		catch (hfst_ol::ZHfstZipReadingError& zhzre) {
			try {
				speller_->read_legacy(directoryName.c_str());
			}
			catch (hfst_ol::ZHfstLegacyReadingError& zhlre) {
				throw setup::DictionaryException("no usable hfst spellers");
			}
			catch (hfst_ol::AlphabetTranslationException& ate) {
				throw setup::DictionaryException("broken error model detected");
			}
		}
	}
	if (opts != 0)
		opts->hfst = speller_;
}

spellresult HfstSpeller::doSpell(const wchar_t * word, size_t wlen) {
	char * wordUtf8 = StringUtils::utf8FromUcs4(word, wlen);
	spellresult result = SPELL_FAILED;
	if (speller_->spell(wordUtf8)) {
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
	if (speller_ != 0){
		delete speller_;
		speller_ = 0;
	}
}

} }
// vim: set noexpandtab ts=4:
