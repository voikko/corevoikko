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
#include <hfst2/string.h>

using namespace std;
using namespace libvoikko::character;
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
	for (HWFST::Key k = 0; k < keyTable->get_unused_key(); ++k) {
		flagTable.define_diacritic(k, HWFST::get_symbol_name(HWFST::get_key_symbol(k, keyTable)));
		if (flagTable.is_diacritic(k)) {
			flags.insert(k);
		}
	}
}
 
spellresult HfstSpeller::doSpell(const wchar_t * word, size_t wlen) {
	char * wordUtf8 = StringUtils::utf8FromUcs4(word, wlen);
	HWFST::KeyVector * wordPath = HWFST::stringUtf8ToKeyVector(wordUtf8, keyTable);
	HWFST::KeyVectorVector * lookups = HWFST::lookup_all(speller, wordPath, &flags);
	spellresult result = SPELL_FAILED;
	if (lookups != NULL) {
		for (KeyVectorVector::iterator lkv = lookups->begin(); lkv != lookups->end(); ++lkv) {
			KeyVector* hmmlkv = *lkv;
			KeyVector* filtlkv = flagTable.filter_diacritics(hmmlkv);
			if (filtlkv != NULL) {
				result = SPELL_OK;
				delete filtlkv;
				break;
			}
			delete filtlkv;
		}
		delete lookups;
	}
	delete wordPath;
	delete[] wordUtf8;
	return result;
}
 
spellresult HfstSpeller::spell(const wchar_t * word, size_t wlen) {
	if (wlen > LIBVOIKKO_MAX_WORD_CHARS) {
		return SPELL_FAILED;
	}
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
	delete keyTable;
	keyTable = 0;
	HWFST::delete_transducer(speller);
	speller = 0;
}

} }
