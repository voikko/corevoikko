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
 * Portions created by the Initial Developer are Copyright (C) 2008 - 2013
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

#include "setup/V5DictionaryLoader.hpp"
#include "setup/LanguageTag.hpp"
#include "porting.h"
#include <string>
#include <fstream>
#include <cstdlib>

#define VOIKKO_DICTIONARY_FILE "index.txt"
#define VOIKKO_DICTIONARY_VERSION "5"
#define VOIKKO_DICTIONARY_VERSION_KEY "Voikko-Dictionary-Format: " VOIKKO_DICTIONARY_VERSION

using namespace std;

namespace libvoikko { namespace setup {

void V5DictionaryLoader::findDictionaries(const string & path) {
	string mainPath(path);
	mainPath.append("/");
	mainPath.append(VOIKKO_DICTIONARY_VERSION);
	list<string> subDirectories = getListOfSubentries(mainPath);
	for (list<string>::iterator i = subDirectories.begin(); i != subDirectories.end(); ++i) {
		string dirName = *i;
		if (dirName.find("mor-") != 0) {
			continue;
		}
		string variantName = dirName.substr(4);
		if (variantName.empty()) {
			continue;
		}
		string fullDirName(mainPath);
		fullDirName.append("/");
		fullDirName.append(dirName);
		Dictionary dict = dictionaryFromPath(fullDirName);
		addDictionary(dict);
	}
}

Dictionary V5DictionaryLoader::dictionaryFromPath(const string & path) {
	string fileName(path);
	fileName.append("/");
	fileName.append(VOIKKO_DICTIONARY_FILE);
	
	string line;
	ifstream file(fileName.c_str(), ifstream::in);
	if (file.good()) {
		getline(file, line);
	}
	if (line.compare(VOIKKO_DICTIONARY_VERSION_KEY) != 0) {
		// Not a valid dictionary for this version of libvoikko
		file.close();
		return Dictionary();
	}
	
	LanguageTag language;
	language.setLanguage("fi");
	string description;
	BackendProperties morBackend("finnishVfst", path, true);
	BackendProperties gramMorBackend;
	BackendProperties grammarBackend("finnishVfst", path, true);
	BackendProperties spellBackend("FinnishSpellerTweaksWrapper(AnalyzerToSpellerAdapter(currentAnalyzer),currentAnalyzer)", true);
	BackendProperties suggestionBackend("FinnishSuggestionStrategy(currentAnalyzer)", true);
	BackendProperties hyphenatorBackend("AnalyzerToFinnishHyphenatorAdapter(currentAnalyzer)", true);
	while (file.good()) {
		getline(file, line);
		if (line.find("Language: ") == 0) {
			language.setBcp47(string(line.substr(10)));
		}
		else if (line.find("Description: ") == 0) {
			description = line.substr(13);
		}
		else if (line.find("Morphology-Backend: ") == 0) {
			morBackend = BackendProperties(line.substr(20), path, true);
		}
		else if (line.find("Speller-Backend: ") == 0) {
			spellBackend = BackendProperties(line.substr(17), true);
		}
		else if (line.find("Suggestion-Backend: ") == 0) {
			suggestionBackend = BackendProperties(line.substr(20), true);
		}
		else if (line.find("Hyphenator-Backend: ") == 0) {
			hyphenatorBackend = BackendProperties(line.substr(20), true);
		}
		else if (line.find("Grammar-Backend: ") == 0) {
			grammarBackend = BackendProperties(line.substr(17), true);
		}
	}
	file.close();
	return Dictionary(morBackend, gramMorBackend, grammarBackend, spellBackend, suggestionBackend,
	                  hyphenatorBackend, language, description);
}


} }
