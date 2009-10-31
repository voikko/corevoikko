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

#include "spellchecker/spell.hpp"
#include "setup/setup.hpp"

namespace libvoikko { namespace spellchecker {

class SpellWithPriority {
	public:
		/** Checks the spelling of given word. This function does not accept optional hyphens.
		* @param word word to check (does not need to be null terminated)
		* @param len length of the word to check
		* @param prio pointer to an integer that will be set to contain the priority of the result
		*             when words are listed as suggestions for a misspelled word
		* @return spelling result
		*/
		static spellresult spellWithPriority(voikko_options_t * voikkoOptions,
		                   const wchar_t * word, size_t len, int * prio);
};

}}
