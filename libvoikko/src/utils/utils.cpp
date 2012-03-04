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
 * Portions created by the Initial Developer are Copyright (C) 2006 - 2010
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

#include "utils/utils.hpp"
#include "character/SimpleChar.hpp"
#include <stdlib.h>
#include <cstring>

using namespace libvoikko::character;

namespace libvoikko {

enum casetype voikko_casetype(const wchar_t * word, size_t nchars) {
	bool first_uc = false;
	bool rest_lc = true;
	bool all_uc = true;
	bool no_letters = true;
	if (nchars == 0) return CT_NO_LETTERS;
	if (SimpleChar::isUpper(word[0])) {
		first_uc = true;
		no_letters = false;
	}
	if (SimpleChar::isLower(word[0])) {
		all_uc = false;
		no_letters = false;
	}
	for (size_t i = 1; i < nchars; i++) {
		if (SimpleChar::isUpper(word[i])) {
			no_letters = false;
			rest_lc = false;
		}
		if (SimpleChar::isLower(word[i])) {
			all_uc = false;
			no_letters = false;
		}
	}
	if (no_letters) return CT_NO_LETTERS;
	if (all_uc) return CT_ALL_UPPER;
	if (!rest_lc) return CT_COMPLEX;
	if (first_uc) return CT_FIRST_UPPER;
	else return CT_ALL_LOWER;
}

void voikko_set_case(enum casetype charcase, wchar_t * word, size_t nchars) {
	if (nchars == 0) return;
	switch (charcase) {
		case CT_NO_LETTERS:
		case CT_COMPLEX:
			return; /* Do nothing */
		case CT_ALL_LOWER:
			for (size_t i = 0; i < nchars; i++) {
				word[i] = SimpleChar::lower(word[i]);
			}
			return;
		case CT_ALL_UPPER:
			for (size_t i = 0; i < nchars; i++) {
				word[i] = SimpleChar::upper(word[i]);
			}
			return;
		case CT_FIRST_UPPER:
			word[0] = SimpleChar::upper(word[0]);
			for (size_t i = 1; i < nchars; i++) {
				word[i] = SimpleChar::lower(word[i]);
			}
			return;
	}
}

bool voikko_is_nonword(const wchar_t * word, size_t nchars) {
	// If X is a character (possibly other than '.'), then the following
	// patterns (URLs and email addresses) will be considered non-words:
	//   X*//X*.X+
	//   X*@X+.X+
	//   www.X+.X+
	
	if (nchars < 4) return false;
	
	const wchar_t * i = wmemchr(word, L'/', nchars - 3);
	if (i && i[1] == L'/' && wmemchr(i + 1, L'.', nchars - (i - word) - 2)) {
		return true;
	}
	
	i = wmemchr(word, L'@', nchars - 3);
	if (i && i[1] != L'.' && wmemchr(i + 1, L'.', nchars - (i - word) - 2)) {
		return true;
	}
	
	if (nchars < 7) {
		return false;
	}
	if ((wcsncmp(L"www.", word, 4) == 0) &&
	    word[4] != L'.' &&
	    wmemchr(word + 5, L'.', nchars - 5)) {
		return true;
	}
	
	return false;
}

}
