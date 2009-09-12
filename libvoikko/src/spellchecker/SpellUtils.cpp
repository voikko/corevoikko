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

#include "spellchecker/SpellUtils.hpp"
#include <wctype.h>

namespace libvoikko { namespace spellchecker {

spellresult SpellUtils::matchWordAndAnalysis(const wchar_t * word,
        size_t len, const wchar_t * structure) {
	char captype; /* 'i' = uppercase letter, 'p' = lowercase letter, 'v' = punctuation */
	spellresult result = SPELL_OK;
	size_t j = 0;
	for (size_t i = 0; i < len; i++) {
		while (structure[j] == L'=') {
			j++;
		}
		if (structure[j] == L'\0') {
			break;
		}
		
		if (iswupper(word[i])) {
			captype = 'i';
		}
		else if (iswlower(word[i])) {
			captype = 'p';
		}
		else {
			captype = 'v';
		}
		
		if (captype == 'p' && (structure[j] == L'i' || structure[j] == L'j')) {
			if (i == 0) {
				result = SPELL_CAP_FIRST;
			}
			else {
				result = SPELL_CAP_ERROR;
			}
		}
		if (captype == 'i' && (structure[j] == L'p' || structure[j] == L'q')) {
			result = SPELL_CAP_ERROR;
		}
		if (result == SPELL_CAP_ERROR) {
			break;
		}
		j++;
	}
	return result;
}

}}
