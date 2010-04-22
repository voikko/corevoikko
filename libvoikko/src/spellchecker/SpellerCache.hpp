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

#ifndef VOIKKO_SPELLCHECKER_SPELLER_CACHE
#define VOIKKO_SPELLCHECKER_SPELLER_CACHE

#include "spellchecker/Speller.hpp"
#include <cstring>

namespace libvoikko { namespace spellchecker {

/**
 * General positive speller cache
 */
class SpellerCache {
	public:
		SpellerCache(int sizeParam);
		~SpellerCache();
		
		int getSizeParam() const;
		
		/**
		 * Check whether given word is in cache.
		 */
		bool isInCache(const wchar_t * word, size_t wlen) const;
		
		/**
		 * Assuming that the word is in cache, return spelling result for it.
		 */
		spellresult getSpellResult(const wchar_t * word, size_t wlen) const;
		
		/**
		 * Add word to cache
		 */
		void setSpellResult(const wchar_t * word, size_t wlen, spellresult result);
	private:
		SpellerCache(const SpellerCache & other);
		SpellerCache & operator = (SpellerCache other);
		
		const int sizeParam;
		wchar_t * words;
		
		/**
		 * i = SPELL_CAP_FIRST
		 * p = SPELL_OK
		 */
		char * spellResults; 
};

} }

#endif
