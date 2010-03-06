/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2006 - 2010 Harri Pitkänen <hatapitk@iki.fi>
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

#include "utils/utils.hpp"
#include "character/SimpleChar.hpp"
#include <stdlib.h>
#include <cstring>

using namespace libvoikko::character;

namespace libvoikko {

int voikko_hash(const wchar_t * word, size_t len, int order) {
	int hash = 0;
	for (size_t counter = 0; counter < len; counter++) {
		hash = (hash * 37 + word[counter]) % (1 << order);
	}
	return hash;
}

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
