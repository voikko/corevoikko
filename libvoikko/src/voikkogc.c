/* Voikkogc: Testing tool for libvoikko
 * Copyright (C) 2008 Harri Pitkänen <hatapitk@iki.fi>
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

#include "voikko.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <wchar.h>
#include <locale.h>
#include "porting.h"
#ifdef HAVE_NL_LANGINFO
#include <langinfo.h>
#endif // HAVE_NL_LANGINFO

#ifdef HAVE_MBRLEN

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
		token_type = voikko_next_token_cstr(handle, lineptr, len, &tokenchars);
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
			case TOKEN_UNKNOWN:
				printf("U: \"");
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


void split_sentences(int handle, const char * line) {
	size_t len;
	const char * lineptr;
	size_t charlen;
	mbstate_t mbstate;
	enum voikko_sentence_type sentence_type;
	size_t sentencechars;
	len = strlen(line);
	memset(&mbstate, '\0', sizeof(mbstate_t));
	lineptr = line;
	while (len > 0) {
		sentence_type = voikko_next_sentence_start_cstr(handle, lineptr, len,
		                &sentencechars);
		switch (sentence_type) {
			case SENTENCE_NONE:
				printf("E: %s\n", lineptr);
				return;
			case SENTENCE_PROBABLE:
				printf("B: ");
				break;
			case SENTENCE_POSSIBLE:
				printf("P: ");
				break;
			case SENTENCE_NO_START:
				// Not returned from this function
				break;
		}
		while (sentencechars > 0) {
			charlen = mbrlen(lineptr, len, &mbstate);
			while (charlen > 0) {
				putchar(lineptr[0]);
				lineptr++;
				charlen--;
				len--;
			}
			sentencechars--;
		}
		printf("\n");
	}
}


int main(int argc, char ** argv) {
	size_t bufsize = 10000;
	char * line;
	char * encoding;
	char * path = 0;
	int handle;
	int tokenize = 0;
	int split_snts = 0;
	
	line = malloc(bufsize);
	if (line == 0) {
		printf("E: Out of memory\n");
		return 1;
	}
	
	for (int i = 1; i < argc; i++) {
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
	
	voikko_set_string_option(handle, VOIKKO_OPT_ENCODING, encoding);
	
	for (int i = 1; i < argc; i++) {
		if (strncmp(argv[i], "--tokenize", 10) == 0) {
			tokenize = 1;
		}
		else if (strncmp(argv[i], "--split-sentences", 17) == 0) {
			split_snts = 1;
		}
	}
	
	while (1) {
		ssize_t chars_read = getline(&line, &bufsize, stdin);
		if (chars_read == -1) break;
		if (chars_read > 0 && line[chars_read - 1] == '\n') {
			line[chars_read - 1] = '\0';
		}
		if (split_snts) split_sentences(handle, line);
		else if (tokenize) print_tokens(handle, line);
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

