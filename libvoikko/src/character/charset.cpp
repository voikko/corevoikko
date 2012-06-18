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
 * The Initial Developers of the Original Code are Harri Pitkänen <hatapitk@iki.fi>
 * and Teemu Likonen <tlikonen@iki.fi>.
 * Portions created by the Initial Developer are Copyright (C) 2006 - 2012
 * the Initial Developer. All Rights Reserved.
 * 
 * Contributor(s):
 *   Sjur Moshagen
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

#include "character/charset.hpp"
#include "character/SimpleChar.hpp"
#include <cstdlib>
#include <cwchar>

using namespace libvoikko::character;

namespace libvoikko {

char_type get_char_type(wchar_t c) {
	if (wcschr(L".,;-!?:'()[]{}/&"
	           L"\u00AD"  /* SOFT HYPHEN */
	           L"\u2019"  /* RIGHT SINGLE QUOTATION MARK */
	           L"\u2010"  /* HYPHEN */
	           L"\u2011"  /* NON-BREAKING HYPHEN */
	           L"\u2013"  /* EN DASH */
	           L"\u2014"  /* EM DASH */
	           L"\u201C"  /* LEFT DOUBLE QUOTATION MARK */
	           L"\u2026"  /* HORIZONTAL ELLIPSIS */
	           , c)) return CHAR_PUNCTUATION;
	if (isFinnishQuotationMark(c)) {
		return CHAR_PUNCTUATION;
	}
	if (SimpleChar::isWhitespace(c)) {
		return CHAR_WHITESPACE;
	}
	if ((c >= 0x41 && c <= 0x5A) ||
	    (c >= 0x61 && c <= 0x7A) ||
	    (c >= 0xC1 && c <= 0xD6) ||
	    (c >= 0xD8 && c <= 0xF6) ||
	    (c >= 0x00F8 && c <= 0x024F) ||
	    (c >= 0x0400 && c <= 0x0481) ||
	    (c >= 0x048A && c <= 0x0523) ||
	    (c >= 0xFB00 && c <= 0xFB04)) {
		return CHAR_LETTER;
	}
	if (wcschr(L"0123456789", c)) {
		return CHAR_DIGIT;
	}
	return CHAR_UNKNOWN;
}

bool isFinnishQuotationMark(wchar_t c) {
	return wcschr(L"\"»"
	              L"\u201d"  /* quotation mark, double comma */
	              , c) != 0;
}

/* Character conversion tables. After normalisation all character sequences on
 * the left are converted to the ones on the right. */

const size_t N_2TO1 = 67; /* Number of entries in this table */
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
	L"z\u030C" L"\u017E"  /* LATIN SMALL LETTER Z WITH CARON */
	/* Basic Russian alphabet + Combining Diacritical Marks --> Basic Russian alphabet */
	L"\u0418\u0306" L"\u0419" /* CYRILLIC CAPITAL LETTER SHORT I */
	L"\u0438\u0306" L"\u0439" /* CYRILLIC SMALL LETTER SHORT I */
	/* Basic Russian alphabet + Combining Diacritical Marks --> Cyrillic extensions */
	L"\u0415\u0300" L"\u0400" /* CYRILLIC CAPITAL LETTER IE WITH GRAVE */
	L"\u0435\u0300" L"\u0450" /* CYRILLIC SMALL LETTER IE WITH GRAVE */
	L"\u0415\u0308" L"\u0401" /* CYRILLIC CAPITAL LETTER IO */
	L"\u0435\u0308" L"\u0451" /* CYRILLIC SMALL LETTER IO */
	L"\u0413\u0301" L"\u0403" /* CYRILLIC CAPITAL LETTER GJE */
	L"\u0433\u0301" L"\u0453" /* CYRILLIC SMALL LETTER GJE */
	/* Basic Russian alphabet + Combining Diacritical Marks --> Extended Cyrillic */
	L"\u041E\u0308" L"\u04E6" /* CYRILLIC CAPITAL LETTER O WITH DIAERESIS */
	L"\u043E\u0308" L"\u04E7"; /* CYRILLIC SMALL LETTER O WITH DIAERESIS */

const size_t N_1TO1 = 3; /* Number of entries in this table */
const wchar_t * CONV_1TO1 =
	/* General Punctuation --> Basic Latin */
	L"\u2019" L"'"  /* RIGHT SINGLE QUOTATION MARK <--> APOSTROPHE */
	L"\u2010" L"-"  /* HYPHEN <--> HYPHEN-MINUS */
	L"\u2011" L"-"; /* NON-BREAKING HYPHEN <--> HYPHEN-MINUS */

const size_t N_1TO2 = 5; /* Number of entries in this table */
const wchar_t * CONV_1TO2 =
	/* Letterlike Symbols --> Latin-1 Supplement / Basic Latin */
	L"\u2103" L"\u00B0C"  /* U+2103 DEGREE CELSIUS <--> U+00B0 DEGREE SIGN + U+0043 LATIN CAPITAL LETTER C */
	L"\u2109" L"\u00B0F"  /* U+2109 DEGREE FAHRENHEIT <--> U+00B0 DEGREE SIGN + U+0046 LATIN CAPITAL LETTER F */
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

}
