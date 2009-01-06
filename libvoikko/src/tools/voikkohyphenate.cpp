/* Voikkohyphenate: Testing tool for libvoikko
 * Copyright (C) 2006 - 2009 Harri Pitkänen <hatapitk@iki.fi>
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
#include <iostream>
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <wchar.h>

using namespace std;

static const int MAX_WORD_LENGTH = 5000;

void hyphenate_word(int handle, const wchar_t * word, size_t wlen, wchar_t separator) {
	char * result = voikko_hyphenate_ucs4(handle, word);
	if (result == 0) {
		cerr << "E: hyphenation failed" << endl;
		return;
	}
	
	wchar_t * hyphenatedWord = new wchar_t[wlen * 2 + 1];
	if (hyphenatedWord == 0) {
		cerr << "E: out of memory" << endl;
		voikko_free_hyphenate(result);
		return;
	}
	
	const wchar_t * wordPtr = word;
	wchar_t * hyphenatedPtr = hyphenatedWord;
	char * resultPtr = result;
	size_t charsLeft = wlen;
	while (charsLeft > 0) {
		if (*resultPtr != ' ') {
			*hyphenatedPtr = separator;
			hyphenatedPtr++;
		}
		if (*resultPtr != '=' || separator != L'-') {
			*hyphenatedPtr = *wordPtr;
			hyphenatedPtr++;
		}
		resultPtr++;
		wordPtr++;
		charsLeft--;
	}
	*hyphenatedPtr = L'\0';
	wcout << hyphenatedWord << endl;
	delete[] hyphenatedWord;
	voikko_free_hyphenate(result);
}


int main(int argc, char ** argv) {
	char * path = 0;
	wchar_t separator = L'-';
	int handle;
	int minhwlen;
	int iclevel;
	
	for (int i = 1; i < argc; i++) {
		if (strcmp(argv[i], "-p") == 0 && i + 1 < argc) path = argv[++i];
	}
	const char * voikko_error = (const char *) voikko_init_with_path(&handle, "fi_FI", 0, path);
	
	if (voikko_error) {
		cerr << "E: Initialisation of Voikko failed: " << voikko_error << endl;
		return 1;
	}
	
	voikko_set_bool_option(handle, VOIKKO_OPT_NO_UGLY_HYPHENATION, 0);
	
	for (int i = 1; i < argc; i++) {
		if (strcmp(argv[i], "no_ugly_hyphenation=1") == 0)
			voikko_set_bool_option(handle, VOIKKO_OPT_NO_UGLY_HYPHENATION, 1);
		else if (strcmp(argv[i], "no_ugly_hyphenation=0") == 0)
			voikko_set_bool_option(handle, VOIKKO_OPT_NO_UGLY_HYPHENATION, 0);
		else if (strcmp(argv[i], "ignore_dot=1") == 0)
			voikko_set_bool_option(handle, VOIKKO_OPT_IGNORE_DOT, 1);
		else if (strcmp(argv[i], "ignore_dot=0") == 0)
			voikko_set_bool_option(handle, VOIKKO_OPT_IGNORE_DOT, 0);
		else if (strcmp(argv[i], "hyphenate_unknown_words=1") == 0)
			voikko_set_bool_option(handle, VOIKKO_OPT_HYPHENATE_UNKNOWN_WORDS, 1);
		else if (strcmp(argv[i], "hyphenate_unknown_words=0") == 0)
			voikko_set_bool_option(handle, VOIKKO_OPT_HYPHENATE_UNKNOWN_WORDS, 0);
		else if (strncmp(argv[i], "min_hyphenated_word_length=", 27) == 0) {
			minhwlen = atoi(argv[i] + 27);
			if (minhwlen < 2) minhwlen = 2;
			voikko_set_int_option(handle, VOIKKO_MIN_HYPHENATED_WORD_LENGTH, minhwlen);
		}
		else if (strncmp(argv[i], "intersect_compound_level=", 25) == 0) {
			iclevel = atoi(argv[i] + 25);
			voikko_set_int_option(handle, VOIKKO_INTERSECT_COMPOUND_LEVEL, iclevel);
		}
		else if (strncmp(argv[i], "-s", 2) == 0) {
			if (strlen(argv[i]) != 3) {
				cerr << "Invalid separator argument for option -s" << endl;
				return 1;
			}
			mbtowc(&separator, argv[i] + 2, 1);
		}
		else if (strcmp(argv[i], "-p") == 0) {
			i++;
			continue;
		}
		else {
			cerr << "Unknown option " << argv[i] << endl;
			return 1;
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
		hyphenate_word(handle, line, lineLen, separator);
	}
	int error = ferror(stdin);
	if (error) {
		cerr << "E: Error while reading from stdin" << endl;
	}
	delete[] line;
	
	voikko_terminate(handle);
	return 0;
}
