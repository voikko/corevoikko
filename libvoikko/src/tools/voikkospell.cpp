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
 * Portions created by the Initial Developer are Copyright (C) 2006 - 2010
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
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <cwchar>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <cassert>

#ifdef HAVE_CONFIG_H
  #include "config.h"
#endif
#ifdef HAVE_PTHREAD
  #include <pthread.h>
#endif

#include "Apertium/Stream.hpp"
#include "Apertium/StreamType.hpp"

using namespace std;


enum CheckMode {NORMAL, ONLY_C_W, ONLY_INCORRECT, NONE};

static const int MAX_WORD_LENGTH = 5000;
static const int MAX_THREADS = 200;
static const size_t WORDS_PER_BLOCK = 500;

static CheckMode checkMode = NORMAL;
static bool suggest = false;
static bool morphology = false;
static bool oneLineOutput = false;
static char wordSeparator = ' ';
static bool space = false;  /* Set to true if you want to output suggestions that have spaces in them. */
static int threadCount = 1;
static bool ApertiumStream = false;

struct speller_t {
	VoikkoHandle * handle;
	vector<wstring> * words;
};

static speller_t * spellers;

static void printMorphology(VoikkoHandle * handle, const wchar_t * word, wstringstream & out) {
	voikko_mor_analysis ** analysisList =
	    voikkoAnalyzeWordUcs4(handle, word);
	for (voikko_mor_analysis ** analysis = analysisList;
	     *analysis; analysis++) {
		const char ** keys = voikko_mor_analysis_keys(*analysis);
		for (const char ** key = keys; *key; key++) {
			out << L"A(" << word << L"):";
			out << (analysis - analysisList) + 1 << L":";
			out << *key << L"=";
			out << voikko_mor_analysis_value_ucs4(*analysis, *key);
			out << endl;
		}
	}
	voikko_free_mor_analysis(analysisList);
}

static void check_word(VoikkoHandle * handle, const wchar_t * word, wstringstream & out) {
	int result = (checkMode != NONE ? voikkoSpellUcs4(handle, word) : VOIKKO_SPELL_OK);
	if (result == VOIKKO_CHARSET_CONVERSION_FAILED) {
		cerr << "E: charset conversion failed" << endl;
		return;
	}
	if (result == VOIKKO_INTERNAL_ERROR) {
		cerr << "E: internal error" << endl;
		return;
	}
	
  if (ApertiumStream) {
    out << L'^';

    if (result) {
      out << word << L'/' << word << L'$';
      return;
    }

    out << L'*' << word;
    wchar_t **Suggestions = voikkoSuggestUcs4(handle, word);

    if (Suggestions != NULL) {
      for (std::size_t SuggestionIndex = 0;
           Suggestions[SuggestionIndex] != NULL; ++SuggestionIndex) {
        out << L'/' << Suggestions[SuggestionIndex];
      }

      voikko_free_suggest_ucs4(Suggestions);
    }

    out << L'$';
    return;
  }

  if (oneLineOutput) {
		out << word;
		if (!result) {
			wchar_t ** suggestions = voikkoSuggestUcs4(handle, word);
			if (suggestions) {
				for (int i = 0; suggestions[i] != 0; i++) {
					if (space || wcschr(suggestions[i], L' ') == 0) {
						out << wordSeparator;
						out << suggestions[i];
					}
				}
				voikko_free_suggest_ucs4(suggestions);
			}
		}
		out << endl;
	} else {
		switch (checkMode) {
			case NORMAL:
				if (result) {
					out << L"C: " << word << endl;
				} else {
					out << L"W: " << word << endl;
				}
				break;
			case ONLY_C_W:
				if (result) {
					out << L"C" << endl;
				} else {
					out << L"W" << endl;
				}
				break;
			case ONLY_INCORRECT:
				if (!result) {
					out << word << endl;
				}
				break;
			case NONE:
				break;
		}
	}
	
	if (morphology && result) {
		printMorphology(handle, word, out);
	}
	if (!oneLineOutput && suggest && !result) {
		wchar_t ** suggestions = voikkoSuggestUcs4(handle, word);
		if (suggestions) {
			for (int i = 0; suggestions[i] != 0; i++) {
				out << L"S: " << suggestions[i] << endl;
			}
			voikko_free_suggest_ucs4(suggestions);
		}
	}
}

#ifdef HAVE_PTHREAD

static void * processBlock(void * args) {
	speller_t * speller = static_cast<speller_t *>(args);
	wstringstream out;
	vector<wstring>::const_iterator it = speller->words->begin();
	while (it != speller->words->end()) {
		check_word(speller->handle, it->c_str(), out);
		++it;
	}
	delete speller->words;
	return new wstring(out.str());
}

pthread_t * threads;
int nextThread;
int nThreadsInUse;
vector<wstring> * nextBlock;

static void initNextBlock() {
	nextBlock = new vector<wstring>();
	nextBlock->reserve(WORDS_PER_BLOCK);
}

static void cleanupThread() {
	void * result;
	if (pthread_join(threads[nextThread], &result)) {
		cerr << "E: pthread_join failed" << endl;
		exit(1);
	}
	--nThreadsInUse;
	wstring * resultString = static_cast<wstring *>(result);
	wcout << *resultString;
	delete resultString;
}

static void queueNextBlock() {
	if (nextBlock->empty()) {
		return;
	}
	if (nThreadsInUse == threadCount) {
		cleanupThread();
	}
	spellers[nextThread].words = nextBlock;
	if (pthread_create(threads + nextThread, 0, &processBlock, spellers + nextThread)) {
		cerr << "E: Failed to create thread" << endl;
		exit(1);
	}
	assert(threadCount >= 1);
	nextThread = (nextThread + 1) % threadCount;
	++nThreadsInUse;
	initNextBlock();
}

static void handleWordMultiThread(const wchar_t * word) {
	nextBlock->push_back(wstring(word));
	if (nextBlock->size() == WORDS_PER_BLOCK) {
		queueNextBlock();
	}
}

#endif

static void finishProcessing() {
	#ifdef HAVE_PTHREAD
		queueNextBlock();
		while (nThreadsInUse > 0) {
			cleanupThread();
			nextThread = (nextThread + 1) % threadCount;
		}
	#endif
}

static void initThreads() {
	#ifdef HAVE_PTHREAD
		threads = new pthread_t[threadCount];
		nextThread = 0;
		nThreadsInUse = 0;
		initNextBlock();
	#endif
}

static void handleWordSingleThread(const wchar_t * word) {
	wstringstream out;
	check_word(spellers[0].handle, word, out);
	wcout << out.str();
	fflush(0);
}

static void handleWord(const wchar_t * word) {
	#ifdef HAVE_PTHREAD
		if (threadCount == 1) {
			handleWordSingleThread(word);
		} else {
			handleWordMultiThread(word);
		}
	#else
		handleWordSingleThread(word);	
	#endif
}

static void setBooleanOption(int option, int value) {
	for (int i = 0; i < threadCount; i++) {
		voikkoSetBooleanOption(spellers[i].handle, option, value);
	}
}

/**
 * Print a list of available dictionaries to stdout.
 * @return status code to be returned when the program exits.
 */
static int list_dicts(const char * path) {
	voikko_dict ** dicts = voikko_list_dicts(path);
	if (!dicts) {
		cerr << "E: Failed to list available dictionaries." << endl;
		return 1;
	}
	for (voikko_dict ** i = dicts; *i; i++) {
		cout << voikko_dict_language(*i);
		string script(voikko_dict_script(*i));
		if (script != "") {
			cout << "-" << script;
		}
		cout << "-x-";
		cout << voikko_dict_variant(*i);
		cout << ": ";
		cout << voikko_dict_description(*i);
		cout << endl;
	}
	voikko_free_dicts(dicts);
	return 0;
}

static void printAndDeleteCapabilityList(const char * capability, char ** languageCodes) {
	for (char ** i = languageCodes; *i; i++) {
		cout << capability;
		cout << ":";
		cout << *i;
		cout << endl;
	}
	voikkoFreeCstrArray(languageCodes);
}

/**
 * Print a list of available dictionary capabilities to stdout.
 * @return status code to be returned when the program exits.
 */
static int list_capabilities(const char * path) {
	char ** languageCodes = voikkoListSupportedSpellingLanguages(path);
	if (!languageCodes) {
		cerr << "E: Failed to list dictionaries with spell checking capability" << endl;
		return 1;
	}
	printAndDeleteCapabilityList("spell", languageCodes);
	
	languageCodes = voikkoListSupportedHyphenationLanguages(path);
	if (!languageCodes) {
		cerr << "E: Failed to list dictionaries with hyphenation capability" << endl;
		return 1;
	}
	printAndDeleteCapabilityList("hyphen", languageCodes);
	
	languageCodes = voikkoListSupportedGrammarCheckingLanguages(path);
	if (!languageCodes) {
		cerr << "E: Failed to list dictionaries with grammar checking capability" << endl;
		return 1;
	}
	printAndDeleteCapabilityList("grammar", languageCodes);
	
	return 0;
}


static void printHelp() {
	cout << "Usage: voikkospell [OPTION]..." << endl;
	cout << "Check spelling of words read from stdin." << endl;
	cout << endl;
	cout << "    -s   Print suggestions for misspelled words" << endl;
	cout << "    -m   Print morphological analysis for recongized words" << endl;
	cout << endl;
	cout << "For complete descriptions of available options see 'man voikkospell'" << endl;
}

static void printVersion() {
	#ifdef PACKAGE_VERSION
		cout << "voikkospell version " << PACKAGE_VERSION << endl;
	#endif
	cout << "libvoikko version " << voikkoGetVersion() << endl;
}

int main(int argc, char ** argv) {
	const char * path = 0;
	const char * variant = "fi";
	int cache_size;
	
	cache_size = 0;
	bool list_dicts_requested = false;
	bool list_capabilities_requested = false;
	for (int i = 1; i < argc; i++) {
		string args(argv[i]);
		if (args.find("-c") == 0) {
			cache_size = atoi(argv[i] + 2);
		}
		else if (args == "-p" && i + 1 < argc) {
			path = argv[++i];
		}
		else if (args == "-d" && i + 1 < argc) {
			variant = argv[++i];
		}
		else if (args == "-h" || args == "--help") {
			printHelp();
			exit(0);
		}
		else if (args == "--version") {
			printVersion();
			exit(0);
		}
		else if (args == "-l") {
			list_dicts_requested = true;
		}
		else if (args == "-L") {
			list_capabilities_requested = true;
		}
		else if (args == "-j") {
			#ifdef HAVE_PTHREAD
				if (i + 1 == argc) {
					cerr << "-j must be followed by number of threads" << endl;
					return 1;
				}
				threadCount = atoi(argv[++i]);
				if (threadCount <= 0 || threadCount > MAX_THREADS) {
					cerr << "Number of threads must be between 1 and " << MAX_THREADS << endl;
					return 1;
				}
			#else
				cerr << "Support for threaded operation is not available" << endl;
				return 1;
			#endif
		}
	}
	
	if (list_dicts_requested) {
		return list_dicts(path);
	}
	
	if (list_capabilities_requested) {
		return list_capabilities(path);
	}
	
	spellers = new speller_t[threadCount];
	for (int i = 0; i < threadCount; i++) {
		const char * voikkoError;
		VoikkoHandle * handle = voikkoInit(&voikkoError, variant, path);
		if (!handle) {
			cerr << "E: Initialization of Voikko failed: " << voikkoError << endl;
			return 1;
		}
		voikkoSetIntegerOption(handle, VOIKKO_SPELLER_CACHE_SIZE, cache_size);
		spellers[i].handle = handle;
		spellers[i].words = 0;
	}
	
	for (int i = 1; i < argc; i++) {
		string args(argv[i]);
		if (args == "-t") {
			checkMode = ONLY_C_W;
		} else if (args == "-tt") {
			checkMode = ONLY_INCORRECT;
		} else if (args == "ignore_dot=1")
			setBooleanOption(VOIKKO_OPT_IGNORE_DOT, 1);
		else if (args == "ignore_dot=0")
			setBooleanOption(VOIKKO_OPT_IGNORE_DOT, 0);
		else if (args == "ignore_numbers=1")
			setBooleanOption(VOIKKO_OPT_IGNORE_NUMBERS, 1);
		else if (args == "ignore_numbers=0")
			setBooleanOption(VOIKKO_OPT_IGNORE_NUMBERS, 0);
		else if (args == "ignore_nonwords=1")
			setBooleanOption(VOIKKO_OPT_IGNORE_NONWORDS, 1);
		else if (args == "ignore_nonwords=0")
			setBooleanOption(VOIKKO_OPT_IGNORE_NONWORDS, 0);
		else if (args == "accept_first_uppercase=1")
			setBooleanOption(VOIKKO_OPT_ACCEPT_FIRST_UPPERCASE, 1);
		else if (args == "accept_first_uppercase=0")
			setBooleanOption(VOIKKO_OPT_ACCEPT_FIRST_UPPERCASE, 0);
		else if (args == "accept_all_uppercase=1")
			setBooleanOption(VOIKKO_OPT_ACCEPT_ALL_UPPERCASE, 1);
		else if (args == "accept_all_uppercase=0")
			setBooleanOption(VOIKKO_OPT_ACCEPT_ALL_UPPERCASE, 0);
		else if (args == "accept_extra_hyphens=1")
			setBooleanOption(VOIKKO_OPT_ACCEPT_EXTRA_HYPHENS, 1);
		else if (args == "accept_extra_hyphens=0")
			setBooleanOption(VOIKKO_OPT_ACCEPT_EXTRA_HYPHENS, 0);
		else if (args == "accept_missing_hyphens=1")
			setBooleanOption(VOIKKO_OPT_ACCEPT_MISSING_HYPHENS, 1);
		else if (args == "accept_missing_hyphens=0")
			setBooleanOption(VOIKKO_OPT_ACCEPT_MISSING_HYPHENS, 0);
		else if (args == "ocr_suggestions=1")
			setBooleanOption(VOIKKO_OPT_OCR_SUGGESTIONS, 1);
		else if (args == "ocr_suggestions=0")
			setBooleanOption(VOIKKO_OPT_OCR_SUGGESTIONS, 0);
		else if (args.find("-x") == 0) {
			oneLineOutput = true;
			if (args.size() == 3) {
				wordSeparator = argv[i][2];
			}
			space = (wordSeparator != ' ');
		}
		else if (args == "-s") {
			suggest = true;
		}
		else if (args == "-m") {
			morphology = true;
		}
		else if (args == "-M") {
			morphology = true;
			checkMode = NONE;
		}
		else if (args.find("-c") == 0) {
			continue;
		}
    else if (args == "--apertium-stream") {
      ApertiumStream = true;
    }
		else if (args == "-p" || args == "-d" || args == "-j") {
			i++;
			continue;
		}
		else {
			cerr << "Unknown option " << args << endl;
			return 1;
		}
	}
	
	wchar_t * line = new wchar_t[MAX_WORD_LENGTH + 1];
	
	// Use stdout in wide character mode and stderr in narrow character mode.
	setlocale(LC_ALL, "");
	wcout.imbue(locale(""));
	fwide(stdout, 1);
	fwide(stderr, -1);
	initThreads();

  if (ApertiumStream) {
    Apertium::Stream ApertiumStream_(std::wcin);

    while (true) {
      Apertium::StreamType StreamType_ = ApertiumStream_.getTheNextStreamType();
      std::wcout << StreamType_.TheString;

      if (StreamType_.TheLexicalUnit)
        handleWord(StreamType_.TheLexicalUnit->TheSurfaceForm.c_str());
      else
        break;
    }
  } else {
    while (fgetws(line, MAX_WORD_LENGTH, stdin)) {
      size_t lineLen = wcslen(line);
      if (lineLen == 0) {
        continue;
      }
      if (line[lineLen - 1] == L'\n') {
        line[lineLen - 1] = L'\0';
        lineLen--;
      }
      if (lineLen > LIBVOIKKO_MAX_WORD_CHARS) {
        cerr << "E: Too long word" << endl;
        continue;
      }
      handleWord(line);
    }
  }
	finishProcessing();
	int error = ferror(stdin);
	if (error) {
		cerr << "E: Error while reading from stdin" << endl;
	}
	delete[] line;
	
	for (int i = 0; i < threadCount; i++) {
		voikkoTerminate(spellers[i].handle);
	}
	delete[] spellers;
	#ifdef HAVE_PTHREAD
		delete nextBlock;
		delete[] threads;
	#endif
	return 0;
}
