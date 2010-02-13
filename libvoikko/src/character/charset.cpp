/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2006 - 2010 Harri Pitkänen <hatapitk@iki.fi>,
 *                           Teemu Likonen <tlikonen@iki.fi>
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

#include "voikko_defs.h"
#include "character/charset.hpp"
#include <cstdlib>
#include <cwctype>
#include <cwchar>

namespace libvoikko {

char_type get_char_type(wchar_t c) {
	if (wcschr(L".,;-!?:'()[]{}"
	           L"\u00AD"  /* SOFT HYPHEN */
	           L"\u2019"  /* RIGHT SINGLE QUOTATION MARK */
	           L"\u2010"  /* HYPHEN */
	           L"\u2011"  /* NON-BREAKING HYPHEN */
	           L"\u201C"  /* LEFT DOUBLE QUOTATION MARK */
	           L"\u2026"  /* HORIZONTAL ELLIPSIS */
	           , c)) return CHAR_PUNCTUATION;
	if (isFinnishQuotationMark(c)) {
		return CHAR_PUNCTUATION;
	}
	if (iswspace(c)) return CHAR_WHITESPACE;
	if (wcschr(L"aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ"
	           L"\u00C0"  /* LATIN CAPITAL LETTER A WITH GRAVE */
	           L"\u00C1"  /* LATIN CAPITAL LETTER A WITH ACUTE */
	           L"\u00C2"  /* LATIN CAPITAL LETTER A WITH CIRCUMFLEX */
	           L"\u00C3"  /* LATIN CAPITAL LETTER A WITH TILDE */
	           L"\u00C4"  /* LATIN CAPITAL LETTER A WITH DIAERESIS */
	           L"\u00C5"  /* LATIN CAPITAL LETTER A WITH RING ABOVE */
	           L"\u00C7"  /* LATIN CAPITAL LETTER C WITH CEDILLA */
	           L"\u00C8"  /* LATIN CAPITAL LETTER E WITH GRAVE */
	           L"\u00C9"  /* LATIN CAPITAL LETTER E WITH ACUTE */
	           L"\u00CA"  /* LATIN CAPITAL LETTER E WITH CIRCUMFLEX */
	           L"\u00CB"  /* LATIN CAPITAL LETTER E WITH DIAERESIS */
	           L"\u00CC"  /* LATIN CAPITAL LETTER I WITH GRAVE */
	           L"\u00CD"  /* LATIN CAPITAL LETTER I WITH ACUTE */
	           L"\u00CE"  /* LATIN CAPITAL LETTER I WITH CIRCUMFLEX */
	           L"\u00CF"  /* LATIN CAPITAL LETTER I WITH DIAERESIS */
	           L"\u00D1"  /* LATIN CAPITAL LETTER N WITH TILDE */
	           L"\u00D2"  /* LATIN CAPITAL LETTER O WITH GRAVE */
	           L"\u00D3"  /* LATIN CAPITAL LETTER O WITH ACUTE */
	           L"\u00D4"  /* LATIN CAPITAL LETTER O WITH CIRCUMFLEX */
	           L"\u00D5"  /* LATIN CAPITAL LETTER O WITH TILDE */
	           L"\u00D6"  /* LATIN CAPITAL LETTER O WITH DIAERESIS */
	           L"\u00D9"  /* LATIN CAPITAL LETTER U WITH GRAVE */
	           L"\u00DA"  /* LATIN CAPITAL LETTER U WITH ACUTE */
	           L"\u00DB"  /* LATIN CAPITAL LETTER U WITH CIRCUMFLEX */
	           L"\u00DC"  /* LATIN CAPITAL LETTER U WITH DIAERESIS */
	           L"\u00DD"  /* LATIN CAPITAL LETTER Y WITH ACUTE */
	           L"\u00E0"  /* LATIN SMALL LETTER A WITH GRAVE */
	           L"\u00E1"  /* LATIN SMALL LETTER A WITH ACUTE */
	           L"\u00E2"  /* LATIN SMALL LETTER A WITH CIRCUMFLEX */
	           L"\u00E3"  /* LATIN SMALL LETTER A WITH TILDE */
	           L"\u00E4"  /* LATIN SMALL LETTER A WITH DIAERESIS */
	           L"\u00E5"  /* LATIN SMALL LETTER A WITH RING ABOVE */
	           L"\u00E7"  /* LATIN SMALL LETTER C WITH CEDILLA */
	           L"\u00E8"  /* LATIN SMALL LETTER E WITH GRAVE */
	           L"\u00E9"  /* LATIN SMALL LETTER E WITH ACUTE */
	           L"\u00EA"  /* LATIN SMALL LETTER E WITH CIRCUMFLEX */
	           L"\u00EB"  /* LATIN SMALL LETTER E WITH DIAERESIS */
	           L"\u00EC"  /* LATIN SMALL LETTER I WITH GRAVE */
	           L"\u00ED"  /* LATIN SMALL LETTER I WITH ACUTE */
	           L"\u00EE"  /* LATIN SMALL LETTER I WITH CIRCUMFLEX */
	           L"\u00EF"  /* LATIN SMALL LETTER I WITH DIAERESIS */
	           L"\u00F1"  /* LATIN SMALL LETTER N WITH TILDE */
	           L"\u00F2"  /* LATIN SMALL LETTER O WITH GRAVE */
	           L"\u00F3"  /* LATIN SMALL LETTER O WITH ACUTE */
	           L"\u00F4"  /* LATIN SMALL LETTER O WITH CIRCUMFLEX */
	           L"\u00F5"  /* LATIN SMALL LETTER O WITH TILDE */
	           L"\u00F6"  /* LATIN SMALL LETTER O WITH DIAERESIS */
	           L"\u00F9"  /* LATIN SMALL LETTER U WITH GRAVE */
	           L"\u00FA"  /* LATIN SMALL LETTER U WITH ACUTE */
	           L"\u00FB"  /* LATIN SMALL LETTER U WITH CIRCUMFLEX */
	           L"\u00FC"  /* LATIN SMALL LETTER U WITH DIAERESIS */
	           L"\u00FD"  /* LATIN SMALL LETTER Y WITH ACUTE */
	           L"\u00FF"  /* LATIN SMALL LETTER Y WITH DIAERESIS */
	           L"\u0160"  /* LATIN CAPITAL LETTER S WITH CARON */
	           L"\u0161"  /* LATIN SMALL LETTER S WITH CARON */
	           L"\u017D"  /* LATIN CAPITAL LETTER Z WITH CARON */
	           L"\u017E"  /* LATIN SMALL LETTER Z WITH CARON */
	           L"\uFB00"  /* LATIN SMALL LIGATURE FF */
	           L"\uFB01"  /* LATIN SMALL LIGATURE FI */
	           L"\uFB02"  /* LATIN SMALL LIGATURE FL */
	           L"\uFB03"  /* LATIN SMALL LIGATURE FFI */
	           L"\uFB04"  /* LATIN SMALL LIGATURE FFL */
	           , c)) return CHAR_LETTER;
	if (wcschr(L"0123456789", c)) return CHAR_DIGIT;
	return CHAR_UNKNOWN;
}

bool isFinnishQuotationMark(wchar_t c) {
	return wcschr(L"\"»"
	              L"\u201d"  /* quotation mark, double comma */
	              , c) != 0;
}

/* Character conversion tables. After normalisation all character sequences on
 * the left are converted to the ones on the right. */

const size_t N_2TO1 = 57; /* Number of entries in this table */
const wchar_t * CONV_2TO1 =
	/* Basic Latin + Combining Diacritical Marks --> Latin-1 Supplement */
	L"A\u0300" L"\u00C0"  /* LATIN CAPITAL LETTER A WITH GRAVE */
	L"A\u0301" L"\u00C1"  /* LATIN CAPITAL LETTER A WITH ACUTE */
	L"A\u0302" L"\u00C2"  /* LATIN CAPITAL LETTER A WITH CIRCUMFLEX */
	L"A\u0303" L"\u00C3"  /* LATIN CAPITAL LETTER A WITH TILDE */
	L"A\u0308" L"\u00C4"  /* LATIN CAPITAL LETTER A WITH DIAERESIS */
	L"A\u030A" L"\u00C5"  /* LATIN CAPITAL LETTER A WITH RING ABOVE */
	L"C\u0327" L"\u00C7"  /* LATIN CAPITAL LETTER C WITH CEDILLA */
	L"E\u0300" L"\u00C8"  /* LATIN CAPITAL LETTER E WITH GRAVE */
	L"E\u0301" L"\u00C9"  /* LATIN CAPITAL LETTER E WITH ACUTE */
	L"E\u0302" L"\u00CA"  /* LATIN CAPITAL LETTER E WITH CIRCUMFLEX */
	L"E\u0308" L"\u00CB"  /* LATIN CAPITAL LETTER E WITH DIAERESIS */
	L"I\u0300" L"\u00CC"  /* LATIN CAPITAL LETTER I WITH GRAVE */
	L"I\u0301" L"\u00CD"  /* LATIN CAPITAL LETTER I WITH ACUTE */
	L"I\u0302" L"\u00CE"  /* LATIN CAPITAL LETTER I WITH CIRCUMFLEX */
	L"I\u0308" L"\u00CF"  /* LATIN CAPITAL LETTER I WITH DIAERESIS */
	L"N\u0303" L"\u00D1"  /* LATIN CAPITAL LETTER N WITH TILDE */
	L"O\u0300" L"\u00D2"  /* LATIN CAPITAL LETTER O WITH GRAVE */
	L"O\u0301" L"\u00D3"  /* LATIN CAPITAL LETTER O WITH ACUTE */
	L"O\u0302" L"\u00D4"  /* LATIN CAPITAL LETTER O WITH CIRCUMFLEX */
	L"O\u0303" L"\u00D5"  /* LATIN CAPITAL LETTER O WITH TILDE */
	L"O\u0308" L"\u00D6"  /* LATIN CAPITAL LETTER O WITH DIAERESIS */
	L"U\u0300" L"\u00D9"  /* LATIN CAPITAL LETTER U WITH GRAVE */
	L"U\u0301" L"\u00DA"  /* LATIN CAPITAL LETTER U WITH ACUTE */
	L"U\u0302" L"\u00DB"  /* LATIN CAPITAL LETTER U WITH CIRCUMFLEX */
	L"U\u0308" L"\u00DC"  /* LATIN CAPITAL LETTER U WITH DIAERESIS */
	L"Y\u0301" L"\u00DD"  /* LATIN CAPITAL LETTER Y WITH ACUTE */
	L"a\u0300" L"\u00E0"  /* LATIN SMALL LETTER A WITH GRAVE */
	L"a\u0301" L"\u00E1"  /* LATIN SMALL LETTER A WITH ACUTE */
	L"a\u0302" L"\u00E2"  /* LATIN SMALL LETTER A WITH CIRCUMFLEX */
	L"a\u0303" L"\u00E3"  /* LATIN SMALL LETTER A WITH TILDE */
	L"a\u0308" L"\u00E4"  /* LATIN SMALL LETTER A WITH DIAERESIS */
	L"a\u030A" L"\u00E5"  /* LATIN SMALL LETTER A WITH RING ABOVE */
	L"c\u0327" L"\u00E7"  /* LATIN SMALL LETTER C WITH CEDILLA */
	L"e\u0300" L"\u00E8"  /* LATIN SMALL LETTER E WITH GRAVE */
	L"e\u0301" L"\u00E9"  /* LATIN SMALL LETTER E WITH ACUTE */
	L"e\u0302" L"\u00EA"  /* LATIN SMALL LETTER E WITH CIRCUMFLEX */
	L"e\u0308" L"\u00EB"  /* LATIN SMALL LETTER E WITH DIAERESIS */
	L"i\u0300" L"\u00EC"  /* LATIN SMALL LETTER I WITH GRAVE */
	L"i\u0301" L"\u00ED"  /* LATIN SMALL LETTER I WITH ACUTE */
	L"i\u0302" L"\u00EE"  /* LATIN SMALL LETTER I WITH CIRCUMFLEX */
	L"i\u0308" L"\u00EF"  /* LATIN SMALL LETTER I WITH DIAERESIS */
	L"n\u0303" L"\u00F1"  /* LATIN SMALL LETTER N WITH TILDE */
	L"o\u0300" L"\u00F2"  /* LATIN SMALL LETTER O WITH GRAVE */
	L"o\u0301" L"\u00F3"  /* LATIN SMALL LETTER O WITH ACUTE */
	L"o\u0302" L"\u00F4"  /* LATIN SMALL LETTER O WITH CIRCUMFLEX */
	L"o\u0303" L"\u00F5"  /* LATIN SMALL LETTER O WITH TILDE */
	L"o\u0308" L"\u00F6"  /* LATIN SMALL LETTER O WITH DIAERESIS */
	L"u\u0300" L"\u00F9"  /* LATIN SMALL LETTER U WITH GRAVE */
	L"u\u0301" L"\u00FA"  /* LATIN SMALL LETTER U WITH ACUTE */
	L"u\u0302" L"\u00FB"  /* LATIN SMALL LETTER U WITH CIRCUMFLEX */
	L"u\u0308" L"\u00FC"  /* LATIN SMALL LETTER U WITH DIAERESIS */
	L"y\u0301" L"\u00FD"  /* LATIN SMALL LETTER Y WITH ACUTE */
	L"y\u0308" L"\u00FF"  /* LATIN SMALL LETTER Y WITH DIAERESIS */
	/* Basic Latin + Combining Diacritical Marks --> Latin Extended-A */
	L"S\u030C" L"\u0160"  /* LATIN CAPITAL LETTER S WITH CARON */
	L"s\u030C" L"\u0161"  /* LATIN SMALL LETTER S WITH CARON */
	L"Z\u030C" L"\u017D"  /* LATIN CAPITAL LETTER Z WITH CARON */
	L"z\u030C" L"\u017E"; /* LATIN SMALL LETTER Z WITH CARON */

const size_t N_1TO1 = 3; /* Number of entries in this table */
const wchar_t * CONV_1TO1 =
	/* General Punctuation --> Basic Latin */
	L"\u2019" L"'"  /* RIGHT SINGLE QUOTATION MARK <--> APOSTROPHE */
	L"\u2010" L"-"  /* HYPHEN <--> HYPHEN-MINUS */
	L"\u2011" L"-"; /* NON-BREAKING HYPHEN <--> HYPHEN-MINUS */

const size_t N_1TO2 = 5; /* Number of entries in this table */
const wchar_t * CONV_1TO2 =
	/* Letterlike Symbols --> Latin-1 Supplement / Basic Latin */
	L"\u2103" L"°C"  /* U+2103 DEGREE CELSIUS <--> U+00B0 DEGREE SIGN + U+0043 LATIN CAPITAL LETTER C */
	L"\u2109" L"°F"  /* U+2109 DEGREE FAHRENHEIT <--> U+00B0 DEGREE SIGN + U+0046 LATIN CAPITAL LETTER F */
	/* Alphabetic Presentation Forms --> Basic Latin */
	L"\uFB00" L"ff"  /* LATIN SMALL LIGATURE FF <--> 2 X LATIN SMALL LETTER F */
	L"\uFB01" L"fi"  /* LATIN SMALL LIGATURE FI <--> LATIN SMALL LETTER F + LATIN SMALL LETTER I */
	L"\uFB02" L"fl"; /* LATIN SMALL LIGATURE FL <--> LATIN SMALL LETTER F + LATIN SMALL LETTER L */

const size_t N_1TO3 = 2; /* Number of entries in this table */
const wchar_t * CONV_1TO3 =
	/* Alphabetic Presentation Forms --> Basic Latin */
	L"\uFB03" L"ffi"  /* LATIN SMALL LIGATURE FFI <--> 2 X LATIN SMALL LETTER F + LATIN SMALL LETTER I */
	L"\uFB04" L"ffl"; /* LATIN SMALL LIGATURE FFL <--> 2 X LATIN SMALL LETTER F + LATIN SMALL LETTER L */


wchar_t * voikko_normalise(const wchar_t * word, size_t len) {
	/* Worst case for space usage is a string with only three character ligatures in it. */
	wchar_t * buffer = new wchar_t[len * 3 + 1];
	wchar_t * ptr = buffer;
	for (size_t i = 0; i < len;) {
		int offset = 0;
		if (i < len - 1) {
			for (size_t j = 0; j < N_2TO1; j++) {
				if (word[i] == CONV_2TO1[3*j] && word[i+1] == CONV_2TO1[3*j+1]) {
					*ptr = CONV_2TO1[3*j+2];
					ptr++;
					offset = 2;
					break;
				}
			}
		}
		if (offset == 0) {
			for (size_t j = 0; j < N_1TO1; j++) {
				if (word[i] == CONV_1TO1[2*j]) {
					*ptr = CONV_1TO1[2*j+1];
					ptr++;
					offset = 1;
					break;
				}
			}
		}
		if (offset == 0) {
			for (size_t j = 0; j < N_1TO2; j++) {
				if (word[i] == CONV_1TO2[3*j]) {
					*ptr = CONV_1TO2[3*j+1];
					*(ptr+1) = CONV_1TO2[3*j+2];
					ptr += 2;
					offset = 1;
					break;
				}
			}
		}
		if (offset == 0) {
			for (size_t j = 0; j < N_1TO3; j++) {
				if (word[i] == CONV_1TO3[4*j]) {
					*ptr = CONV_1TO3[4*j+1];
					*(ptr+1) = CONV_1TO3[4*j+2];
					*(ptr+2) = CONV_1TO3[4*j+3];
					ptr += 3;
					offset = 1;
					break;
				}
			}
		}
		if (offset == 0) {
			*ptr = word[i];
			ptr++;
			offset = 1;
		}
		i += offset;
	}
	*ptr = L'\0';
	return buffer;
}

void voikko_cset_reformat(const wchar_t * orig, size_t orig_len, wchar_t ** modified, size_t modified_len) {
	size_t i, minl;
	if (orig_len < 1 || modified_len < 1) return;
	if (orig_len < modified_len) minl = orig_len;
	else minl = modified_len;

	/* Process the leading part of the string */
	size_t limit = minl;
	for (i = 0; i < limit; i++) {
		if (orig[i] == (*modified)[i]) continue;
		for (size_t j = 0; j < N_1TO1; j++) {
			if (orig[i] == CONV_1TO1[2*j] && (*modified)[i] == CONV_1TO1[2*j+1]) {
				(*modified)[i] = CONV_1TO1[2*j];
				break;
			}
		}
		if (orig[i] != (*modified)[i]) break;
	}

	/* Process the trailing part of the string */
	limit = minl - i;
	for (i = 1; i <= limit; i++) {
		if (orig[orig_len-i] == (*modified)[modified_len-i]) continue;
		for (size_t j = 0; j < N_1TO1; j++) {
			if (orig[orig_len-i] == CONV_1TO1[2*j] &&
 			    (*modified)[modified_len-i] == CONV_1TO1[2*j+1]) {
				(*modified)[modified_len-i] = CONV_1TO1[2*j];
				break;
			}
		}
		if (orig[orig_len-i] != (*modified)[modified_len-i]) break;
	}
}

wchar_t simpleLower(wchar_t input) {
	// Basic Latin
	if (input >= 0x41 && input <= 0x5A) {
		// A-Z
		return input + 0x20;
	}
	// Latin-1 Supplement
	if (input >= 0xC0 && input <= 0xD6) {
		// À-Ö
		return input + 0x20;
	}
	if (input >= 0xD8 && input <= 0xDD) {
		// Ø-Ý
		return input + 0x20;
	}
	// Latin Extended-A
	if (input >= 0x0100 && input <= 0x0136 && input % 2 == 0) {
		// Ā-Ķ
		return input + 1;
	}
	if (input >= 0x0139 && input <= 0x0147 && input % 2 == 1) {
		// Ĺ-Ň
		return input + 1;
	}
	if (input >= 0x014A && input <= 0x0176 && input % 2 == 0) {
		// Ŋ-Ŷ
		return input + 1;
	}
	if (input >= 0x0179 && input <= 0x017D && input % 2 == 1) {
		// Ź-Ž
		return input + 1;
	}
	// TODO: other Unicode character ranges are not yet mapped
	return input;
}

}
