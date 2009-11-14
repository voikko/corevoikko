/* Libvoikko: Library of Finnish language tools
 * Copyright (C) 2009 Harri Pitkänen <hatapitk@iki.fi>
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

#ifndef VOIKKO_SPELLCHECKER_SPELLER
#define VOIKKO_SPELLCHECKER_SPELLER

#include <cstring>

namespace libvoikko { namespace spellchecker {

/* SPELL_FAILED:    Word does not exist in the language even if the character cases
 *                  were changed.
 * SPELL_OK:        Word is correct.
 * SPELL_CAP_FIRST: Word would be correct if the first letter was changed to upper case.
 * SPELL_CAP_ERROR: Word would be correct if some of its characters were in different case.
*/
enum spellresult {SPELL_FAILED, SPELL_OK, SPELL_CAP_FIRST, SPELL_CAP_ERROR};
    
/**
 * General interface for spellers.
 */
class Speller {
	public:
		/**
		 * Checks whether given word is correct (or would be correct
		 * with different capitalization).
		 */
		virtual spellresult spell(const wchar_t * word, size_t wlen) = 0;
		virtual void terminate() = 0;
};

} }

#endif
