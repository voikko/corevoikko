/* Voikkogc: Testing tool for libvoikko
 * Copyright (C) 2008 - 2009 Harri Pitkänen <hatapitk@iki.fi>
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
#include <cstring>
#include <cstdio>
#include <cwchar>

using namespace std;

static const int MAX_PARAGRAPH_LENGTH = 100000;

void print_tokens(int handle, const wchar_t * line, size_t lineLen) {
	enum voikko_token_type token_type;
	size_t tokenchars;
	const wchar_t * linePtr = line;
	size_t charsLeft = lineLen;
	while (charsLeft > 0) {
		token_type = voikko_next_token_ucs4(handle, linePtr, charsLeft, &tokenchars);
		switch (token_type) {
			case TOKEN_WORD:
				wcout << L"W: \"";
				break;
			case TOKEN_PUNCTUATION:
				wcout << L"P: \"";
				break;
			case TOKEN_WHITESPACE:
				wcout << L"S: \"";
				break;
			case TOKEN_UNKNOWN:
				wcout << L"U: \"";
				break;
			case TOKEN_NONE:
				wcout << L"E: unknown token\n";
				return;
		}
		while (tokenchars > 0) {
			wcout << linePtr[0];
			linePtr++;
			charsLeft--;
			tokenchars--;
		}
		wcout << "\"" << endl;
	}
}


void split_sentences(int handle, const wchar_t * line, size_t lineLen) {
	enum voikko_sentence_type sentence_type;
	size_t sentencechars;
	const wchar_t * linePtr = line;
	size_t charsLeft = lineLen;
	while (charsLeft > 0) {
		sentence_type = voikko_next_sentence_start_ucs4(handle, linePtr, charsLeft,
		                &sentencechars);
		switch (sentence_type) {
			case SENTENCE_NONE:
				wcout << L"E: ";
				wcout << linePtr << endl;
				return;
			case SENTENCE_PROBABLE:
				wcout << L"B: ";
				break;
			case SENTENCE_POSSIBLE:
				wcout << L"P: ";
				break;
			case SENTENCE_NO_START:
				// Not returned from this function
				break;
		}
		while (sentencechars > 0) {
			wcout << linePtr[0];
			linePtr++;
			charsLeft--;
			sentencechars--;
		}
		wcout << endl;
	}
}


void check_grammar(int handle, const wchar_t * line, size_t lineLen,
		const char * explanation_language) {
	voikko_grammar_error grammar_error;
	int skiperrors = 0;
	while (1) {
		grammar_error = voikko_next_grammar_error_ucs4(handle, line, lineLen,
		                0, skiperrors);
		if (grammar_error.error_code == 0) {
			cout << "-" << endl;
			return;
		}
		cout << "[code=" << grammar_error.error_code << ", level=0, ";
		cout << "descr=\"\", stpos=" << grammar_error.startpos << ", ";
		cout << "len=" << grammar_error.errorlen << ", suggs={";
		if (grammar_error.suggestions) {
			char ** sugg = grammar_error.suggestions;
			while (*sugg) {
				// FIXME: convert from UTF-8
				// FIXME: character " is not escaped -> results are not fully parseable
				cout << "\"" << *sugg << "\"";
				sugg++;
				if (*sugg) {
					cout << ",";
				}
			}
			voikko_free_suggest_cstr(grammar_error.suggestions);
		}
		cout << "}]";
		if (explanation_language) {
			cout << " (";
			cout << voikko_error_message_cstr(
			         grammar_error.error_code, explanation_language);
			cout << ")";
		}
		cout << endl;
		skiperrors++;
	}
}

enum operation {TOKENIZE, SPLIT_SENTENCES, CHECK_GRAMMAR};

int main(int argc, char ** argv) {
	const char * path = 0;
	const char * variant = "";
	enum operation op = CHECK_GRAMMAR;
	int handle;
	
	for (int i = 1; i < argc; i++) {
		if (strcmp(argv[i], "-p") == 0 && i + 1 < argc) {
			path = argv[++i];
		}
		else if (strcmp(argv[i], "-d") == 0 && i + 1 < argc) {
			variant = argv[++i];
		}
	}
	const char * voikko_error = (const char *) voikko_init_with_path(&handle, variant, 0, path);

	if (voikko_error) {
		cerr << "E: Initialization of Voikko failed: " << voikko_error << endl;
		return 1;
	}
	
	const char * explanation_language = 0;
	
	for (int i = 1; i < argc; i++) {
		if (strncmp(argv[i], "--tokenize", 10) == 0) {
			op = TOKENIZE;
		}
		else if (strncmp(argv[i], "--split-sentences", 17) == 0) {
			op = SPLIT_SENTENCES;
		}
		else if (strcmp(argv[i], "accept_titles=1") == 0) {
			voikko_set_bool_option(handle, VOIKKO_OPT_ACCEPT_TITLES_IN_GC, 1);
		}
		else if (strcmp(argv[i], "accept_titles=0") == 0) {
			voikko_set_bool_option(handle, VOIKKO_OPT_ACCEPT_TITLES_IN_GC, 0);
		}
		else if (strcmp(argv[i], "accept_unfinished_paragraphs=1") == 0) {
			voikko_set_bool_option(handle, VOIKKO_OPT_ACCEPT_UNFINISHED_PARAGRAPHS_IN_GC, 1);
		}
		else if (strcmp(argv[i], "accept_unfinished_paragraphs=0") == 0) {
			voikko_set_bool_option(handle, VOIKKO_OPT_ACCEPT_UNFINISHED_PARAGRAPHS_IN_GC, 0);
		}
		else if (strcmp(argv[i], "accept_bulleted_lists=1") == 0) {
			voikko_set_bool_option(handle, VOIKKO_OPT_ACCEPT_BULLETED_LISTS_IN_GC, 1);
		}
		else if (strcmp(argv[i], "accept_bulleted_lists=0") == 0) {
			voikko_set_bool_option(handle, VOIKKO_OPT_ACCEPT_BULLETED_LISTS_IN_GC, 0);
		}
		else if (strncmp(argv[i], "explanation_language=fi", 23) == 0) {
			explanation_language = "fi";
		}
		else if (strncmp(argv[i], "explanation_language=", 21) == 0) {
			explanation_language = "en";
		}
		else if (strcmp(argv[i], "-p") == 0 || strcmp(argv[i], "-d") == 0) {
			i++;
			continue;
		}
		else {
			cerr << "Unknown option " << argv[i] << endl;
			return 1;
		}
	}
	
	wchar_t * line = new wchar_t[MAX_PARAGRAPH_LENGTH + 1];
	if (!line) {
		cerr << "E: Out of memory" << endl;
	}
	
	setlocale(LC_ALL, "");
	while (fgetws(line, MAX_PARAGRAPH_LENGTH, stdin)) {
		size_t lineLen = wcslen(line);
		if (lineLen == 0) continue;
		if (line[lineLen - 1] == L'\n') {
			line[lineLen - 1] = L'\0';
			lineLen--;
		}
		switch (op) {
			case TOKENIZE:
				print_tokens(handle, line, lineLen);
				break;
			case SPLIT_SENTENCES:
				split_sentences(handle, line, lineLen);
				break;
			case CHECK_GRAMMAR:
				check_grammar(handle, line, lineLen, explanation_language);
		}
	}
	int error = ferror(stdin);
	if (error) {
		cerr << "E: Error while reading from stdin" << endl;
	}
	delete[] line;
	
	voikko_terminate(handle);
	return 0;
}
