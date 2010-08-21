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

#include "spellchecker/Speller.hpp"

namespace libvoikko { namespace spellchecker {

class SpellUtils {
	public:
		/** Returns the spelling result of a word when matched against given analysis string
		 *  @param word word  (does not need to be null terminated)
		 *  @param len length of the word
		 *  @param structure word structure from morphological analysis
		 *  @return spelling result
		 */
		static spellresult matchWordAndAnalysis(const wchar_t * word,
		        size_t len, const wchar_t * structure);
};

}}
