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
 * The Initial Developer of the Original Code is Harri Pitkänen <hatapitk@iki.fi>.
 * Portions created by the Initial Developer are Copyright (C) 2008 - 2010
 * the Initial Developer. All Rights Reserved.
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

#include "../voikko.h"
#include <iostream>
#include <cstdlib>
#include <cstring>
#include <cstdio>
#include <cwchar>

#ifdef HAVE_CONFIG_H
  #include "config.h"
#endif

using namespace std;

static const int MAX_PARAGRAPH_LENGTH = 100000;

bool printLineNumbersInGc = false;
size_t lineNumber = 0;

static void printHelp() {
	cout << "Usage: voikkogc [OPTION]..." << endl;
	cout << "Check grammar of paragraphs read from stdin." << endl;
	cout << endl;
	cout << "For complete descriptions of available options see 'man voikkogc'" << endl;
}

static void printVersion() {
	#ifdef PACKAGE_VERSION
		cout << "voikkogc version " << PACKAGE_VERSION << endl;
	#endif
	cout << "libvoikko version " << voikkoGetVersion() << endl;
}

static void print_tokens(VoikkoHandle * handle, const wchar_t * line, size_t lineLen) {
	enum voikko_token_type token_type;
	size_t tokenchars;
	const wchar_t * linePtr = line;
	size_t charsLeft = lineLen;
	while (charsLeft > 0) {
		token_type = voikkoNextTokenUcs4(handle, linePtr, charsLeft, &tokenchars);
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


static void split_sentences(VoikkoHandle * handle, const wchar_t * line, size_t lineLen) {
	enum voikko_sentence_type sentence_type;
	size_t sentencechars;
	const wchar_t * linePtr = line;
	size_t charsLeft = lineLen;
	while (charsLeft > 0) {
		sentence_type = voikkoNextSentenceStartUcs4(handle, linePtr, charsLeft,
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


static void check_grammar(VoikkoHandle * handle, const wchar_t * line, size_t lineLen,
		const char * explanation_language) {
	int skiperrors = 0;
	while (1) {
		VoikkoGrammarError * grammarError = voikkoNextGrammarErrorUcs4(handle, line, lineLen,
		                0, skiperrors);
		if (printLineNumbersInGc) {
			cout << lineNumber << " ";
		}
		if (!grammarError) {
			cout << "-" << endl;
			return;
		}
		cout << "[code=" << voikkoGetGrammarErrorCode(grammarError) << ", level=0, ";
		cout << "descr=\"\", stpos=" << voikkoGetGrammarErrorStartPos(grammarError) << ", ";
		cout << "len=" << voikkoGetGrammarErrorLength(grammarError) << ", suggs={";
		const char ** suggs = voikkoGetGrammarErrorSuggestions(grammarError);
		if (suggs) {
			const char ** sugg = suggs;
			while (*sugg) {
				// FIXME: convert from UTF-8
				// FIXME: character " is not escaped -> results are not fully parseable
				cout << "\"" << *sugg << "\"";
				sugg++;
				if (*sugg) {
					cout << ",";
				}
			}
		}
		cout << "}]";
		if (explanation_language) {
			cout << " (";
			cout << voikko_error_message_cstr(
			         voikkoGetGrammarErrorCode(grammarError), explanation_language);
			cout << ")";
		}
		voikkoFreeGrammarError(grammarError);
		cout << endl;
		skiperrors++;
	}
}

enum operation {TOKENIZE, SPLIT_SENTENCES, CHECK_GRAMMAR};

int main(int argc, char ** argv) {
	const char * path = 0;
	const char * variant = "fi";
	operation op = CHECK_GRAMMAR;
	
	for (int i = 1; i < argc; i++) {
		if (strcmp(argv[i], "-p") == 0 && i + 1 < argc) {
			path = argv[++i];
		}
		else if (strcmp(argv[i], "-d") == 0 && i + 1 < argc) {
			variant = argv[++i];
		}
		else if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
			printHelp();
			exit(0);
		}
		else if (strcmp(argv[i],  "--version") == 0) {
			printVersion();
			exit(0);
		}
	}
	const char * voikkoError;
	VoikkoHandle * handle = voikkoInit(&voikkoError, variant, path);
	if (!handle) {
		cerr << "E: Initialization of Voikko failed: " << voikkoError << endl;
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
			voikkoSetBooleanOption(handle, VOIKKO_OPT_ACCEPT_TITLES_IN_GC, 1);
		}
		else if (strcmp(argv[i], "accept_titles=0") == 0) {
			voikkoSetBooleanOption(handle, VOIKKO_OPT_ACCEPT_TITLES_IN_GC, 0);
		}
		else if (strcmp(argv[i], "accept_unfinished_paragraphs=1") == 0) {
			voikkoSetBooleanOption(handle, VOIKKO_OPT_ACCEPT_UNFINISHED_PARAGRAPHS_IN_GC, 1);
		}
		else if (strcmp(argv[i], "accept_unfinished_paragraphs=0") == 0) {
			voikkoSetBooleanOption(handle, VOIKKO_OPT_ACCEPT_UNFINISHED_PARAGRAPHS_IN_GC, 0);
		}
		else if (strcmp(argv[i], "accept_bulleted_lists=1") == 0) {
			voikkoSetBooleanOption(handle, VOIKKO_OPT_ACCEPT_BULLETED_LISTS_IN_GC, 1);
		}
		else if (strcmp(argv[i], "accept_bulleted_lists=0") == 0) {
			voikkoSetBooleanOption(handle, VOIKKO_OPT_ACCEPT_BULLETED_LISTS_IN_GC, 0);
		}
		else if (strncmp(argv[i], "explanation_language=fi", 23) == 0) {
			explanation_language = "fi";
		}
		else if (strncmp(argv[i], "explanation_language=", 21) == 0) {
			explanation_language = "en";
		}
		else if (strcmp(argv[i], "-n") == 0) {
			printLineNumbersInGc = true;
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
		lineNumber++;
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
	
	voikkoTerminate(handle);
	return 0;
}
