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
#include <stdlib.h>
#include <unistd.h>
#include <locale.h>
#include "../porting.h"
#ifdef HAVE_NL_LANGINFO
#include <langinfo.h>
#endif // HAVE_NL_LANGINFO
#include <string>
#include <iostream>

using namespace std;

int autotest = 0;
int suggest = 0;
int one_line_output = 0;
char word_separator = ' ';
int space = 0;  /* Set to nonzero if you want to output suggestions that has spaces in them. */

void check_word(int handle, string &word) {
	char ** suggestions;
	int i;
	int result = voikko_spell_cstr(handle, word.c_str());
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
			suggestions = voikko_suggest_cstr(handle, word.c_str());
			if (suggestions) {
				for (i = 0; suggestions[i] != 0; i++) {
					string suggestion(suggestions[i]);
					if (space || suggestion.find(' ') != string::npos) {
						cout << word_separator << suggestion;
					}
				}
				voikko_free_suggest_cstr(suggestions);
			}
		}
		cout << endl;
	}
	else {
		if (result) {
			cout << "C: " << word << endl;
		}
		else {
			cout << "W: " << word << endl;
		}
	}
	if (!one_line_output && suggest && !result) {
		suggestions = voikko_suggest_cstr(handle, word.c_str());
		if (suggestions) {
			for (i = 0; suggestions[i] != 0; i++) {
				cout << "S: " << suggestions[i] << endl;
			}
			voikko_free_suggest_cstr(suggestions);
		}
	}
}



int main(int argc, char ** argv) {
	char * encoding;
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
	
	setlocale(LC_ALL, "");
	encoding = nl_langinfo(CODESET);
	
	voikko_set_string_option(handle, VOIKKO_OPT_ENCODING, encoding);
	
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
	
	string line;
	while (getline(cin, line)) {
		check_word(handle, line);
	}
	voikko_terminate(handle);
	return 0;
}

