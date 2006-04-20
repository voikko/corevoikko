/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2006 Harri Pitkänen <hatapitk@iki.fi>
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
#include "voikko_utils.h"
#include <iconv.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>

wchar_t * voikko_cstrtoucs4(const char * word, const char * encoding) {
	iconv_t cd;
	int conv_bytes;
	wchar_t * ucs4_buffer = malloc((LIBVOIKKO_MAX_WORD_CHARS + 1) * sizeof(wchar_t));
	size_t res_size;
	char * outptr = (char *) ucs4_buffer;
	ICONV_CONST char * inptr = (ICONV_CONST char *) word;
	size_t inbytesleft = strlen(word);
	size_t outbytesleft = LIBVOIKKO_MAX_WORD_CHARS * sizeof(wchar_t);
	ucs4_buffer[LIBVOIKKO_MAX_WORD_CHARS] = L'\0';
	cd = iconv_open("WCHAR_T", encoding);
	if (cd == (iconv_t) -1) return 0;
	LOG(("inbytesleft = %d\n", (int) inbytesleft));
	LOG(("outbytesleft = %d\n", (int) outbytesleft));
	LOG(("inptr = '%s'\n", inptr));
	iconv(cd, 0, &inbytesleft, &outptr, &outbytesleft);
	conv_bytes = (int) iconv(cd, &inptr, &inbytesleft, &outptr, &outbytesleft);
	LOG(("conv_bytes = %d\n", conv_bytes));
	LOG(("inbytesleft = %d\n", (int) inbytesleft));
	LOG(("outbytesleft = %d\n", (int) outbytesleft));
	LOG(("inptr = '%s'\n", inptr));
	if (conv_bytes == -1) {
		iconv_close(cd);
		return 0;
	}
	iconv_close(cd);
	*((wchar_t *) outptr) = L'\0'; /* teriminate the output buffer */
	LOG(("ucs4_buffer = '%ls'\n", ucs4_buffer));
	res_size = ((size_t) outptr) - ((size_t) ucs4_buffer) + sizeof(wchar_t);
	ucs4_buffer = realloc(ucs4_buffer, res_size);
	return ucs4_buffer;
}

char * voikko_ucs4tocstr(const wchar_t * word, const char * encoding) {
	iconv_t cd;
	int conv_bytes;
	char * utf8_buffer = malloc(LIBVOIKKO_MAX_WORD_CHARS * 6 + 1);
	size_t res_size;
	char * outptr = utf8_buffer;
	ICONV_CONST char * inptr = (ICONV_CONST char *) word;
	size_t inbytesleft = wcslen(word) * sizeof(wchar_t);
	size_t outbytesleft = LIBVOIKKO_MAX_WORD_CHARS;
	utf8_buffer[LIBVOIKKO_MAX_WORD_CHARS] = '\0';
	cd = iconv_open(encoding, "WCHAR_T");
	if (cd == (iconv_t) -1) return 0;
	LOG(("inbytesleft = %d\n", (int) inbytesleft));
	LOG(("outbytesleft = %d\n", (int) outbytesleft));
	LOG(("inptr = '%s'\n", inptr));
	iconv(cd, 0, &inbytesleft, &outptr, &outbytesleft);
	conv_bytes = (int) iconv(cd, &inptr, &inbytesleft, &outptr, &outbytesleft);
	LOG(("conv_bytes = %d\n", conv_bytes));
	LOG(("inbytesleft = %d\n", (int) inbytesleft));
	LOG(("outbytesleft = %d\n", (int) outbytesleft));
	LOG(("inptr = '%s'\n", inptr));
	if (conv_bytes == -1) {
		iconv_close(cd);
		return 0;
	}
	iconv_close(cd);
	*outptr = '\0'; /* terminate the output buffer */
	LOG(("utf8_buffer = '%s'\n", utf8_buffer));
	res_size = ((size_t) outptr) - ((size_t) utf8_buffer) + 1;
	utf8_buffer = realloc(utf8_buffer, res_size);
	return utf8_buffer;
}
