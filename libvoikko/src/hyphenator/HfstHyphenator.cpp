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
 * The Initial Developer of the Original Code is Flammie Pirinen <flammie@iki.fi>.
 * Portions created by the Initial Developer are Copyright (C) 2010
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

#include "hyphenator/HfstHyphenator.hpp"
#include "setup/DictionaryException.hpp"
#include "utils/StringUtils.hpp"
#include "voikko_defines.h"
#include <fstream>

using namespace std;
using namespace libvoikko::utils;

namespace libvoikko { namespace hyphenator {

HfstHyphenator::HfstHyphenator(const string & ) throw(setup::DictionaryException) {
}
    
char * HfstHyphenator::hyphenate(const wchar_t * word,
								 size_t wlen) {
	if (wlen > LIBVOIKKO_MAX_WORD_CHARS) {
		return 0;
	}
	char * wordUtf8 = StringUtils::utf8FromUcs4(word, wlen);
	char * result = hyphenate(wordUtf8);
	delete[] wordUtf8;
	return result;
}

char* HfstHyphenator::hyphenate(const char * word) {
	size_t wlen = strlen(word);
	if (wlen > LIBVOIKKO_MAX_WORD_CHARS) {
		return 0;
	}
	char* voikkoHyphenationPattern = new char[strlen(word)+1];
	return voikkoHyphenationPattern;
}

void HfstHyphenator::terminate() {
}

void HfstHyphenator::setUglyHyphenation(bool ugliness) {
	// FIXME: find ugly hyphenation transducer
	(void)ugliness;
}

void HfstHyphenator::setHyphenateUnknown(bool unknown) {
	// FIXME: currently always fall back to unknown hyphenation with algo
	(void)unknown;
}

void HfstHyphenator::setMinHyphenatedWordLength(int wlen) {
	// FIXME: not followed
	(void)wlen;
}

void HfstHyphenator::setIgnoreDot(bool dotness) {
	//FIXME: dotted will be unknown and use such fallback
	(void)dotness;
}

} }
