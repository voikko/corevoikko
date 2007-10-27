/* Voikkohyphenate: Testing tool for libvoikko
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

#define _GNU_SOURCE

#include <voikko.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <locale.h>
#include "porting.h"
#ifdef HAVE_NL_LANGINFO
#include <langinfo.h>
#endif // HAVE_NL_LANGINFO
#include <stdio.h>
#include <wchar.h>
#include <string.h>

#ifdef HAVE_MBRLEN
void hyphenate_word(int handle, char * word) {
	size_t len;
	char * result;
	char * hyphenated_word;
	char * wordptr;
	char * hyphenatedptr;
	char * resultptr;
	size_t charlen;
	mbstate_t mbstate;
	result = voikko_hyphenate_cstr(handle, word);
	if (result == 0) {
		printf("E: hyphenation failed\n");
		return;
	}
	len = strlen(word);
	/* We assume that character '-' always has the shortest possible
	   multibyte representation in a given encoding. */
	hyphenated_word = malloc(strlen(word) * 2 + 1);
	if (hyphenated_word == 0) {
		printf("E: out of memory\n");
		return;
	}
	memset(&mbstate, '\0', sizeof(mbstate_t));
	wordptr = word;
	hyphenatedptr = hyphenated_word;
	resultptr = result;
	while (len > 0) {
		charlen = mbrlen(wordptr, len, &mbstate);
		if (*resultptr != ' ') {
			/* FIXME: assumes single byte representation for '-' */
			*hyphenatedptr = '-';
			hyphenatedptr++;
		}
		if (*resultptr != '=') {
			strncpy(hyphenatedptr, wordptr, charlen);
			hyphenatedptr += charlen;
		}
		resultptr++;
		wordptr += charlen;
		len -= charlen;
	}
	*hyphenatedptr = '\0';
	printf("%s\n", hyphenated_word);
	free(hyphenated_word);
	voikko_free_hyphenate(result);
}

void print_tokens(int handle, const char * line) {
	size_t len;
	const char * lineptr;
	size_t charlen;
	mbstate_t mbstate;
	enum voikko_token_type token_type;
	size_t tokenchars;
	len = strlen(line);
	memset(&mbstate, '\0', sizeof(mbstate_t));
	lineptr = line;
	while (len > 0) {
		token_type = voikko_next_token_cstr(lineptr, len, &tokenchars);
		switch (token_type) {
			case TOKEN_WORD:
				printf("W: \"");
				break;
			case TOKEN_PUNCTUATION:
				printf("P: \"");
				break;
			case TOKEN_WHITESPACE:
				printf("S: \"");
				break;
			case TOKEN_NONE:
				printf("E: unknown token\n");
				return;
		}
		while (tokenchars > 0) {
			charlen = mbrlen(lineptr, len, &mbstate);
			while (charlen > 0) {
				putchar(lineptr[0]);
				lineptr++;
				charlen--;
				len--;
			}
			tokenchars--;
		}
		printf("\"\n");
	}
}

int main(int argc, char ** argv) {
	size_t size = LIBVOIKKO_MAX_WORD_CHARS;
	char * line;
	ssize_t chars_read;
	char * encoding;
	char * path = 0;
	int handle;
	int minhwlen;
	int iclevel;
	int i;
	int tokenize = 0;
	
	line = malloc(size);
	if (line == 0) {
		printf("E: Out of memory\n");
		return 1;
	}
	
	for (i = 1; i < argc; i++) {
		if (strcmp(argv[i], "-p") == 0 && i + 1 < argc) path = argv[++i];
	}
	const char * voikko_error = (const char *) voikko_init_with_path(&handle, "fi_FI", 0, path);

	if (voikko_error) {
		printf("E: Initialisation of Voikko failed: %s\n", voikko_error);
		free(line);
		return 1;
	}
	
	setlocale(LC_ALL, "");
	encoding = nl_langinfo(CODESET);
	
	voikko_set_bool_option(handle, VOIKKO_OPT_NO_UGLY_HYPHENATION, 0);
	voikko_set_string_option(handle, VOIKKO_OPT_ENCODING, encoding);
	
	for (i = 1; i < argc; i++) {
		if (strcmp(argv[i], "no_ugly_hyphenation=1") == 0)
			voikko_set_bool_option(handle, VOIKKO_OPT_NO_UGLY_HYPHENATION, 1);
		else if (strcmp(argv[i], "no_ugly_hyphenation=0") == 0)
			voikko_set_bool_option(handle, VOIKKO_OPT_NO_UGLY_HYPHENATION, 0);
		else if (strcmp(argv[i], "ignore_dot=1") == 0)
			voikko_set_bool_option(handle, VOIKKO_OPT_IGNORE_DOT, 1);
		else if (strcmp(argv[i], "ignore_dot=0") == 0)
			voikko_set_bool_option(handle, VOIKKO_OPT_IGNORE_DOT, 0);
		else if (strncmp(argv[i], "min_hyphenated_word_length=", 27) == 0) {
			minhwlen = atoi(argv[i] + 27);
			if (minhwlen < 2) minhwlen = 2;
			voikko_set_int_option(handle, VOIKKO_MIN_HYPHENATED_WORD_LENGTH, minhwlen);
		}
		else if (strncmp(argv[i], "intersect_compound_level=", 25) == 0) {
			iclevel = atoi(argv[i] + 25);
			voikko_set_int_option(handle, VOIKKO_INTERSECT_COMPOUND_LEVEL, iclevel);
		}
		else if (strncmp(argv[i], "--tokenize", 10) == 0) {
			tokenize = 1;
		}
	}
	
	while (1) {
		chars_read = getline(&line, &size, stdin);
		if (chars_read == -1) break;
		if (chars_read > 0 && line[chars_read - 1] == '\n') {
			line[chars_read - 1] = '\0';
		}
		if (tokenize) print_tokens(handle, line);
		else hyphenate_word(handle, line);
	}
	free(line);
	voikko_terminate(handle);
	return 0;
}

#else
int main(int argc, char ** argv) {
	printf("E: This tool is not supported on your operating system.\n");
	return 1;
}
#endif

