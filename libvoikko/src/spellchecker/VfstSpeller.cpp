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
 * Portions created by the Initial Developer are Copyright (C) 2009 - 2012
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

#include "spellchecker/VfstSpeller.hpp"
#include "utils/StringUtils.hpp"
#include "character/SimpleChar.hpp"
#include "voikko_defines.h"

using namespace std;
using namespace libvoikko::character;
using namespace libvoikko::utils;
using namespace libvoikko::fst;

namespace libvoikko { namespace spellchecker {

static const int BUFFER_SIZE = 2000;

VfstSpeller::VfstSpeller(const string & directoryName) throw(setup::DictionaryException) {
	string splFile = directoryName + "/spl.vfst";
	transducer = new Transducer(splFile.c_str());
	configuration = new Configuration(transducer->getFlagDiacriticFeatureCount(), BUFFER_SIZE);
	outputBuffer = new char[BUFFER_SIZE];
}
 
spellresult VfstSpeller::doSpell(const wchar_t * word, size_t wlen) {
	if (wlen > LIBVOIKKO_MAX_WORD_CHARS) {
		return SPELL_FAILED;
	}
	spellresult result = SPELL_FAILED;
	char * wordUtf = StringUtils::utf8FromUcs4(word, wlen);
	if (transducer->prepare(configuration, wordUtf, strlen(wordUtf))) {
		if (transducer->next(configuration, outputBuffer, BUFFER_SIZE)) {
			result = SPELL_OK;
		}
	}
	delete[] wordUtf;
	return result;
}
 
spellresult VfstSpeller::spell(const wchar_t * word, size_t wlen) {
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

void VfstSpeller::terminate() {
	delete[] outputBuffer;
	delete configuration;
	transducer->terminate();
	delete transducer;
}

} }
