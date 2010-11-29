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
