/* Libvoikko: Library of Finnish language tools
 * Copyright (C) 2010 Harri Pitkänen <hatapitk@iki.fi>
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

#include "spellchecker/SpellerCache.hpp"
#include <cwchar>

namespace libvoikko { namespace spellchecker {

/* A small result cache:
 * word_length order elements wchars offset
 * 1           4     16       16     0
 * 2           5     32       64     16
 * 3           6     64       192    80
 * 4           7     128      512    272
 * 5           7     128      640    784
 * 6           7     128      768    1424
 * 7           7     128      896    2192
 * 8           7     128      1024   3088
 * 9           7     128      1152   4112
 * 10          7     128      1280   5264
 * total size      sizeof(wchar_t) * 6544
 */

static const int VOIKKO_HASH_ORDERS[]   =  {0, 3+0,  5+0,  6+0,   7+0,   7+0,    7+0,    7+0,    7+0,    7+0,    7+0};
static const int VOIKKO_CACHE_OFFSETS[] =  {0,   0, 1*16, 1*80, 1*272, 1*784, 1*1424, 1*2192, 1*3088, 1*4112, 1*5264};
static const int VOIKKO_META_OFFSETS[]  =  {0,   0, 1*16, 1*48, 1*112, 1*240,  1*368,  1*496,  1*624,  1*752,  1*880};

/** 
 * Simple string hashing algorithm
 * @param word string to hash
 * @param len length of the word
 * @param order order of the resulting hash value
 * @return integer from range [0, 2^order - 1]
 */
static int voikko_hash(const wchar_t * word, size_t len, int order) {
	int hash = 0;
	for (size_t counter = 0; counter < len; counter++) {
		hash = (hash * 37 + word[counter]) % (1 << order);
	}
	return hash;
}

SpellerCache::SpellerCache(int sizeParam) : sizeParam(sizeParam) {
	words = new wchar_t[6544 << sizeParam];
	memset(words, 0, 6544 * sizeof(wchar_t) << sizeParam);
	spellResults = new char[1008 * sizeof(char) << sizeParam];
	memset(spellResults, 0, 1008 * sizeof(char) << sizeParam);
}

SpellerCache::~SpellerCache() {
	delete[] spellResults;
	delete[] words;
}

int SpellerCache::getSizeParam() const {
	return sizeParam;
}

bool SpellerCache::isInCache(const wchar_t * word, size_t wlen) const {
	if (wlen > 10) {
		return false;
	}
	int hashCode = voikko_hash(word, wlen, VOIKKO_HASH_ORDERS[wlen] + sizeParam);
	int cacheOffset = (VOIKKO_CACHE_OFFSETS[wlen] << sizeParam) + hashCode * static_cast<int>(wlen);
	return wcsncmp(words + cacheOffset, word, wlen) == 0;
}

spellresult SpellerCache::getSpellResult(const wchar_t * word, size_t wlen) const {
	int hashCode = voikko_hash(word, wlen, VOIKKO_HASH_ORDERS[wlen] + sizeParam);
	int resultOffset = (VOIKKO_META_OFFSETS[wlen] << sizeParam) + hashCode;
	if (spellResults[resultOffset] == 'i') {
		return SPELL_CAP_FIRST;
	} else {
		return SPELL_OK;
	}
}

void SpellerCache::setSpellResult(const wchar_t * word, size_t wlen, spellresult result) {
	if (wlen > 10 || (result != SPELL_OK && result != SPELL_CAP_FIRST)) {
		return;
	}
	int hashCode = voikko_hash(word, wlen, VOIKKO_HASH_ORDERS[wlen] + sizeParam);
	int cacheOffset = (VOIKKO_CACHE_OFFSETS[wlen] << sizeParam) + hashCode * static_cast<int>(wlen);
	int resultOffset = (VOIKKO_META_OFFSETS[wlen] << sizeParam) + hashCode;
	wcsncpy(words + cacheOffset, word, wlen);
	spellResults[resultOffset] = (result == SPELL_OK) ? 'p' : 'i';
}

} }
