/* Voikkospell: Testing tool for libvoikko
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
#include <string.h>

int autotest = 0;
int suggest = 0;
int one_line_output = 0;
char word_separator = ' ';
int space = 0;  /* Set to nonzero if you want to output suggestions that has spaces in them. */

void check_word(int handle, char * word) {
	char ** suggestions;
	int i;
	int result = voikko_spell_cstr(handle, word);
	if (result == VOIKKO_CHARSET_CONVERSION_FAILED) {
		printf("E: charset conversion failed\n");
		return;
	}
	if (result == VOIKKO_INTERNAL_ERROR) {
		printf("E: internal error\n");
		return;
	}
	if (autotest) {
		if (result) printf("C\n");
		else printf("W\n");
		fflush(0);
	}
	else if (one_line_output) {
		printf ("%s", word);
		if (!result) {
			suggestions = voikko_suggest_cstr(handle, word);
			if (suggestions) {
				for (i = 0; suggestions[i] != 0; i++) {
					if (space || (!space && strchr (suggestions[i], ' ') == 0))
						printf ("%c%s", word_separator, suggestions[i]);
				}
				voikko_free_suggest_cstr(suggestions);
			}
		}
		printf ("\n");
	}
	else {
		if (result) printf("C: %s\n", word);
		else printf("W: %s\n", word);
	}
	if (!one_line_output && suggest && !result) {
		suggestions = voikko_suggest_cstr(handle, word);
		if (suggestions) {
			for (i = 0; suggestions[i] != 0; i++) {
				printf("S: %s\n", suggestions[i]);
			}
			voikko_free_suggest_cstr(suggestions);
		}
	}
}



int main(int argc, char ** argv) {
	size_t size = LIBVOIKKO_MAX_WORD_CHARS;
	char * line;
	ssize_t chars_read;
	char * encoding;
	char * path = 0;
	int handle;
	int i;
	int cache_size;

	line = malloc(size);
	if (line == 0) {
		printf("E: Out of memory\n");
		return 1;
	}
	
	cache_size = 0;
	for (i = 1; i < argc; i++) {
		if (strncmp(argv[i], "-c", 2) == 0) {
			cache_size = atoi(argv[i] + 2);
		}
		else if (strcmp(argv[i], "-p") == 0 && i + 1 < argc) path = argv[++i];
	}
	const char * voikko_error = (const char *) voikko_init_with_path(&handle, "fi_FI", cache_size, path);
	if (voikko_error) {
		printf("E: Initialisation of Voikko failed: %s\n", voikko_error);
		free(line);
		return 1;
	}
	
	setlocale(LC_ALL, "");
	encoding = nl_langinfo(CODESET);
	
	voikko_set_string_option(handle, VOIKKO_OPT_ENCODING, encoding);
	
	for (i = 1; i < argc; i++) {
		if (strcmp(argv[i], "-t") == 0) autotest = 1;
		else if (strcmp(argv[i], "ignore_dot=1") == 0)
			voikko_set_bool_option(handle, VOIKKO_OPT_IGNORE_DOT, 1);
		else if (strcmp(argv[i], "ignore_dot=0") == 0)
			voikko_set_bool_option(handle, VOIKKO_OPT_IGNORE_DOT, 0);
		else if (strcmp(argv[i], "ignore_numbers=1") == 0)
			voikko_set_bool_option(handle, VOIKKO_OPT_IGNORE_NUMBERS, 1);
		else if (strcmp(argv[i], "ignore_numbers=0") == 0)
			voikko_set_bool_option(handle, VOIKKO_OPT_IGNORE_NUMBERS, 0);
		else if (strcmp(argv[i], "ignore_nonwords=1") == 0)
			voikko_set_bool_option(handle, VOIKKO_OPT_IGNORE_NONWORDS, 1);
		else if (strcmp(argv[i], "ignore_nonwords=0") == 0)
			voikko_set_bool_option(handle, VOIKKO_OPT_IGNORE_NONWORDS, 0);
		else if (strcmp(argv[i], "accept_first_uppercase=1") == 0)
			voikko_set_bool_option(handle, VOIKKO_OPT_ACCEPT_FIRST_UPPERCASE, 1);
		else if (strcmp(argv[i], "accept_first_uppercase=0") == 0)
			voikko_set_bool_option(handle, VOIKKO_OPT_ACCEPT_FIRST_UPPERCASE, 0);
		else if (strcmp(argv[i], "accept_extra_hyphens=1") == 0)
			voikko_set_bool_option(handle, VOIKKO_OPT_ACCEPT_EXTRA_HYPHENS, 1);
		else if (strcmp(argv[i], "accept_extra_hyphens=0") == 0)
			voikko_set_bool_option(handle, VOIKKO_OPT_ACCEPT_EXTRA_HYPHENS, 0);
		else if (strcmp(argv[i], "ocr_suggestions=1") == 0)
			voikko_set_bool_option(handle, VOIKKO_OPT_OCR_SUGGESTIONS, 1);
		else if (strcmp(argv[i], "ocr_suggestions=0") == 0)
			voikko_set_bool_option(handle, VOIKKO_OPT_OCR_SUGGESTIONS, 0);
		else if (strncmp(argv[i], "-x", 2) == 0) {
			one_line_output = 1;
			if (strlen(argv[i]) == 3) word_separator = argv[i][2];
			space = (word_separator != ' ');
		}
		else if (strcmp(argv[i], "-s") == 0) suggest = 1;
	}
	
	while (1) {
		chars_read = getline(&line, &size, stdin);
		if (chars_read == -1) break;
		if (chars_read > 0 && line[chars_read - 1] == '\n') {
			line[chars_read - 1] = '\0';
		}
		check_word(handle, line);
	}
	free(line);
	voikko_terminate(handle);
	return 0;
}

