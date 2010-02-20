/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2006 - 2009 Harri Pitkänen <hatapitk@iki.fi>
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

#include "morphology/Analysis.hpp"
#include "setup/setup.hpp"
#include "utils/utils.hpp"
#include "utils/StringUtils.hpp"
#include "hyphenator/AnalyzerToFinnishHyphenatorAdapter.hpp"
#include <cstdlib>

namespace libvoikko { namespace hyphenator {

VOIKKOEXPORT char * voikko_hyphenate_ucs4(int /*handle*/, const wchar_t * word) {
	if (word == 0) {
		return 0;
	}
	size_t wlen = wcslen(word);
	
	char * hyphenation = voikko_options.hyphenator->hyphenate(word, wlen);
	if (hyphenation == 0) {
		return 0;
	}
	// Convert to C allocation to maintain compatibility with some
	// broken applications before libvoikko 1.5.
	utils::StringUtils::convertCStringToMalloc(hyphenation);
	return hyphenation;
}

VOIKKOEXPORT char * voikko_hyphenate_cstr(int handle, const char * word) {
	if (word == 0) return 0;
	size_t len = strlen(word);
	if (len > LIBVOIKKO_MAX_WORD_CHARS) return 0;
	wchar_t * word_ucs4 = voikko_cstrtoucs4(word, voikko_options.encoding, len);
	if (word_ucs4 == 0) return 0;
	char * result = voikko_hyphenate_ucs4(handle, word_ucs4);
	delete[] word_ucs4;
	return result;
}

VOIKKOEXPORT void voikko_free_hyphenate(char * hyphenate_result) {
	// C deallocation is used here to maintain compatibility with some
	// broken applications before libvoikko 1.5.
	free(hyphenate_result);
}

} }
