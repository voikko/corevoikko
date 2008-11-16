/* Voikkospell: Testing tool for libvoikko
 * Copyright (C) 2006 - 2008 Harri Pitkänen <hatapitk@iki.fi>
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

#include "../voikko.h"
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <wchar.h>
#include <iostream>

using namespace std;

static const int MAX_WORD_LENGTH = 5000;

static int autotest = 0;
static int suggest = 0;
static int one_line_output = 0;
static char word_separator = ' ';
static int space = 0;  /* Set to nonzero if you want to output suggestions that has spaces in them. */

void check_word(int handle, const wchar_t * word, size_t wlen) {
	int result = voikko_spell_ucs4(handle, word);
	if (result == VOIKKO_CHARSET_CONVERSION_FAILED) {
		cerr << "E: charset conversion failed" << endl;
		return;
	}
	if (result == VOIKKO_INTERNAL_ERROR) {
		cerr << "E: internal error" << endl;
		return;
	}
	if (autotest) {
		if (result) {
			cout << "C" << endl;
		}
		else {
			cout << "W" << endl;
		}
		fflush(0);
	}
	else if (one_line_output) {
		cout << word;
		if (!result) {
			wchar_t ** suggestions = voikko_suggest_ucs4(handle, word);
			if (suggestions) {
				for (int i = 0; suggestions[i] != 0; i++) {
					if (space || wcschr(suggestions[i], L' ')) {
						cout << word_separator;
						wcout << suggestions[i];
					}
				}
				voikko_free_suggest_ucs4(suggestions);
			}
		}
		cout << endl;
	}
	else {
		if (result) {
			wcout << L"C: " << word << endl;
		}
		else {
			wcout << L"W: " << word << endl;
		}
	}
	if (!one_line_output && suggest && !result) {
		wchar_t ** suggestions = voikko_suggest_ucs4(handle, word);
		if (suggestions) {
			for (int i = 0; suggestions[i] != 0; i++) {
				wcout << L"S: " << suggestions[i] << endl;
			}
			voikko_free_suggest_ucs4(suggestions);
		}
	}
}



int main(int argc, char ** argv) {
	char * path = 0;
	int handle;
	int cache_size;
	
	cache_size = 0;
	for (int i = 1; i < argc; i++) {
		string args(argv[i]);
		if (args.find("-c") == 0) {
			cache_size = atoi(argv[i] + 2);
		}
		else if (args == "-p" && i + 1 < argc) {
			path = argv[++i];
		}
	}
	const char * voikko_error = (const char *) voikko_init_with_path(&handle, "fi_FI", cache_size, path);
	if (voikko_error) {
		cerr << "E: Initialisation of Voikko failed: " << voikko_error << endl;
		return 1;
	}
	
	for (int i = 1; i < argc; i++) {
		string args(argv[i]);
		if (args == "-t") {
			autotest = 1;
		}
		else if (args == "ignore_dot=1")
			voikko_set_bool_option(handle, VOIKKO_OPT_IGNORE_DOT, 1);
		else if (args == "ignore_dot=0")
			voikko_set_bool_option(handle, VOIKKO_OPT_IGNORE_DOT, 0);
		else if (args == "ignore_numbers=1")
			voikko_set_bool_option(handle, VOIKKO_OPT_IGNORE_NUMBERS, 1);
		else if (args == "ignore_numbers=0")
			voikko_set_bool_option(handle, VOIKKO_OPT_IGNORE_NUMBERS, 0);
		else if (args == "ignore_nonwords=1")
			voikko_set_bool_option(handle, VOIKKO_OPT_IGNORE_NONWORDS, 1);
		else if (args == "ignore_nonwords=0")
			voikko_set_bool_option(handle, VOIKKO_OPT_IGNORE_NONWORDS, 0);
		else if (args == "accept_first_uppercase=1")
			voikko_set_bool_option(handle, VOIKKO_OPT_ACCEPT_FIRST_UPPERCASE, 1);
		else if (args == "accept_first_uppercase=0")
			voikko_set_bool_option(handle, VOIKKO_OPT_ACCEPT_FIRST_UPPERCASE, 0);
		else if (args == "accept_extra_hyphens=1")
			voikko_set_bool_option(handle, VOIKKO_OPT_ACCEPT_EXTRA_HYPHENS, 1);
		else if (args == "accept_extra_hyphens=0")
			voikko_set_bool_option(handle, VOIKKO_OPT_ACCEPT_EXTRA_HYPHENS, 0);
		else if (args == "accept_missing_hyphens=1")
			voikko_set_bool_option(handle, VOIKKO_OPT_ACCEPT_MISSING_HYPHENS, 1);
		else if (args == "accept_missing_hyphens=0")
			voikko_set_bool_option(handle, VOIKKO_OPT_ACCEPT_MISSING_HYPHENS, 0);
		else if (args == "ocr_suggestions=1")
			voikko_set_bool_option(handle, VOIKKO_OPT_OCR_SUGGESTIONS, 1);
		else if (args == "ocr_suggestions=0")
			voikko_set_bool_option(handle, VOIKKO_OPT_OCR_SUGGESTIONS, 0);
		else if (args.find("-x") == 0) {
			one_line_output = 1;
			if (args.size() == 3) {
				word_separator = argv[i][2];
			}
			space = (word_separator != ' ');
		}
		else if (args == "-s") {
			suggest = 1;
		}
	}
	
	wchar_t * line = new wchar_t[MAX_WORD_LENGTH + 1];
	if (!line) {
		cerr << "E: Out of memory" << endl;
	}
	
	setlocale(LC_ALL, "");
	while (fgetws(line, MAX_WORD_LENGTH, stdin)) {
		size_t lineLen = wcslen(line);
		if (lineLen == 0) continue;
		if (line[lineLen - 1] == L'\n') {
			line[lineLen - 1] = L'\0';
			lineLen--;
		}
		check_word(handle, line, lineLen);
	}
	int error = ferror(stdin);
	if (error) {
		cerr << "E: Error while reading from stdin" << endl;
	}
	delete[] line;
	
	voikko_terminate(handle);
	return 0;
}
