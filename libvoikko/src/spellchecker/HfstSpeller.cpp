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
 * Portions created by the Initial Developer are Copyright (C) 2009 - 2013
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

#include "spellchecker/HfstSpeller.hpp"
#include "utils/utils.hpp"
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

HfstSpeller::HfstSpeller(const string & zhfstFileName, voikko_options_t * voikkoOptions) throw(setup::DictionaryException) :
	voikkoOptions(voikkoOptions) {
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
	delete[] wordUtf8;
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
		// This is needed to support VOIKKO_OPT_ACCEPT_ALL_UPPERCASE option
		else if (voikkoOptions->accept_all_uppercase && voikko_casetype(modifiedWord, wlen) == CT_FIRST_UPPER) {
			for (size_t i = 1; i < wlen; i++) {
				modifiedWord[i] = SimpleChar::upper(modifiedWord[i]);
			}
			result = doSpell(modifiedWord, wlen);
			if (result == SPELL_OK) {
				result = SPELL_CAP_ERROR;
			}
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
