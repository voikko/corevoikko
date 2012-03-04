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
 * Portions created by the Initial Developer are Copyright (C) 2009 - 2010
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
		virtual ~Speller();
};

} }

#endif
