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

#include "setup/V2DictionaryLoader.hpp"
#include "setup/LanguageTag.hpp"
#include "porting.h"
#include <string>
#include <fstream>
#include <cstdlib>

#define VOIKKO_DICTIONARY_FILE "voikko-fi_FI.pro"
#define MALAGA_DICTIONARY_VERSION "2"
#define MALAGA_DICTIONARY_VERSION_KEY "info: Voikko-Dictionary-Format: " MALAGA_DICTIONARY_VERSION

using namespace std;

namespace libvoikko { namespace setup {

void V2DictionaryLoader::findDictionaries(const string & path) {
	string mainPath(path);
	mainPath.append("/");
	mainPath.append(MALAGA_DICTIONARY_VERSION);
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

Dictionary V2DictionaryLoader::dictionaryFromPath(const string & path) {
	string fileName(path);
	fileName.append("/");
	fileName.append(VOIKKO_DICTIONARY_FILE);
	
	string line;
	ifstream file(fileName.c_str(), ifstream::in);
	if (file.good()) {
		getline(file, line);
	}
	if (line.compare(MALAGA_DICTIONARY_VERSION_KEY) != 0) {
		// Not a valid dictionary for this version of libvoikko
		file.close();
		return Dictionary();
	}
	
	LanguageTag language;
	language.setLanguage("fi");
	string description;
	string morBackend = "malaga";
	string gramMorBackend = "finnish";
	string grammarBackend = "finnish";
	string grammarPath = "";
	string gramMorPath = "";
	string spellBackend = "FinnishSpellerTweaksWrapper(AnalyzerToSpellerAdapter(currentAnalyzer),currentAnalyzer)";
	string suggestionBackend = "FinnishSuggestionStrategy(currentAnalyzer)";
	string hyphenatorBackend = "AnalyzerToFinnishHyphenatorAdapter(currentAnalyzer)";
	while (file.good()) {
		getline(file, line);
		if (line.find("info: Language-Code: ") == 0) {
			language.setLanguage(string(line.substr(21)));
		}
		else if (line.find("info: Language-Variant: ") == 0) {
			string variant = line.substr(24);
			tagToCanonicalForm(variant);
			language.setPrivateUse(variant);
		}
		else if (line.find("info: Description: ") == 0) {
			description = line.substr(19);
		}
		else if (line.find("info: Morphology-Backend: ") == 0) {
			morBackend = line.substr(26);
		}
		else if (line.find("info: Speller-Backend: ") == 0) {
			spellBackend = line.substr(23);
		}
		else if (line.find("info: Suggestion-Backend: ") == 0) {
			suggestionBackend = line.substr(26);
		}
		else if (line.find("info: Hyphenator-Backend: ") == 0) {
			hyphenatorBackend = line.substr(26);
		}
	}
	file.close();
	return Dictionary(path, gramMorPath, grammarPath, morBackend, gramMorBackend, grammarBackend, spellBackend, suggestionBackend,
	                  hyphenatorBackend, language, description);
}


} }
