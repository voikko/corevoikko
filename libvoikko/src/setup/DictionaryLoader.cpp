/* Libvoikko: Library of Finnish language tools
 * Copyright (C) 2008 - 2010 Harri Pitkänen <hatapitk@iki.fi>
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

#include "setup/DictionaryLoader.hpp"
#include "setup/LanguageTag.hpp"
#include "porting.h"
#include <string>
#include <fstream>
#include <cstdlib>
#ifdef WIN32
# include <windows.h>
#else
# include <pwd.h>
# include <dirent.h>
#endif

#define VOIKKO_DICTIONARY_FILE "voikko-fi_FI.pro"
#define VOIKKO_DICTIONARY_VERSION "2"
#define VOIKKO_DICTIONARY_VERSION_KEY "info: Voikko-Dictionary-Format: " VOIKKO_DICTIONARY_VERSION
#ifdef WIN32
# define VOIKKO_KEY                   "SOFTWARE\\Voikko"
# define VOIKKO_VALUE_DICTIONARY_PATH "DictionaryPath"
# define BUFFER_LENGTH 200
#endif

using namespace std;

namespace libvoikko { namespace setup {

static void tagToCanonicalForm(string & languageTag) {
	for (size_t i = 0; i < languageTag.size(); ++i) {
		char current = languageTag.at(i);
		if (current >= 65 && current <= 90) {
			languageTag[i] = current + 32;
		}
	}
}

static LanguageTag parseFromBCP47(const string & language) {
	// TODO: this parsing algorithm is incomplete
	LanguageTag tag;
	if (language.size() < 2) {
		return tag;
	}
	
	string canonicalLanguage = language;
	tagToCanonicalForm(canonicalLanguage);
	
	size_t languageEnd = canonicalLanguage.find("-");
	if (languageEnd == string::npos) {
		tag.setLanguage(canonicalLanguage);
	} else {
		if (languageEnd < 2) {
			// Invalid tag "-..." or "f-..."
			return tag;
		}
		tag.setLanguage(canonicalLanguage.substr(0, languageEnd));
	}
	
	size_t privateUseStart = canonicalLanguage.find("-x-");
	if (privateUseStart != string::npos) {
		string privateUse = canonicalLanguage.substr(privateUseStart + 3);
		for (size_t hyphenPos = privateUse.find("-"); hyphenPos != string::npos;
		            hyphenPos = privateUse.find("-")) {
			privateUse.erase(hyphenPos, 1);
		}
		tag.setPrivateUse(privateUse);
	}
	
	return tag;
}

/**
 * Returns true if the given variant map contains a default dictionary for given language.
 */
static bool hasDefaultForLanguage(map<string, Dictionary> & variants, const string & language) {
	for (map<string, Dictionary>::iterator i = variants.begin(); i != variants.end(); ++i) {
		if (i->second.getLanguage().getLanguage() == language && i->second.isDefault()) {
			return true;
		}
	}
	return false;
}

list<Dictionary> DictionaryLoader::findAllAvailable() {
	return findAllAvailable(string());
}


list<Dictionary> DictionaryLoader::findAllAvailable(const std::string & path) {
	list<string> locations = getDefaultLocations();
	if (!path.empty()) {
		locations.push_front(path);
	}
	
	map<string, Dictionary> dictMap;
	for (list<string>::iterator i = locations.begin(); i != locations.end(); ++i) {
		addVariantsFromPath(*i, dictMap);
	}
	
	list<Dictionary> dicts;
	for (map< string, Dictionary >::iterator i = dictMap.begin(); i != dictMap.end(); ++i) {
		if (i->second.isDefault()) {
			dicts.push_front(i->second);
		}
		else if (i->first.rfind("-x-standard") == i->first.size() - 11 &&
		         !hasDefaultForLanguage(dictMap, i->second.getLanguage().getLanguage())) {
			dicts.push_front(i->second);
		}
		else {
			dicts.push_back(i->second);
		}
	}
	return dicts;
}

Dictionary DictionaryLoader::load(const string & language) throw(DictionaryException) {
	return load(language, string());
}

static bool isMatchingLanguage(const LanguageTag & requested, const LanguageTag & available) {
	if (requested.getLanguage() != available.getLanguage()) {
		return false;
	}
	if (!requested.getPrivateUse().empty() && requested.getPrivateUse() != available.getPrivateUse()) {
		return false;
	}
	return true;
}

Dictionary DictionaryLoader::load(const string & language, const string & path)
		throw(DictionaryException) {
	LanguageTag requestedTag = parseFromBCP47(language);
	
	list<Dictionary> dicts = findAllAvailable(path);
	if (dicts.empty()) {
		throw DictionaryException("No valid dictionaries were found");
	}
	
	const string privateUse = requestedTag.getPrivateUse();
	if (privateUse.empty() || privateUse == "default" || privateUse == "fi_FI") {
		// Use dictionary specified by environment variable VOIKKO_DICTIONARY_PATH
		// XXX: Not actually thread safe but will most probably work
		char * dict_from_env = getenv("VOIKKO_DICTIONARY");
		if (dict_from_env) {
			requestedTag.setPrivateUse(string(dict_from_env));
		}
	}
	
	for (list<Dictionary>::iterator i = dicts.begin(); i != dicts.end(); ++i) {
		LanguageTag availableTag = (*i).getLanguage();
		if (isMatchingLanguage(requestedTag, availableTag)) {
			return *i;
		}
	}
	throw DictionaryException("Specified dictionary variant was not found");
}

void DictionaryLoader::addVariantsFromPath(const string & path, map<string, Dictionary> & variants) {
	string mainPath(path);
	mainPath.append("/");
	mainPath.append(VOIKKO_DICTIONARY_VERSION);
#ifdef WIN32
	string searchPattern(mainPath);
	searchPattern.append("\\*");
	WIN32_FIND_DATA dirData;
	HANDLE handle = FindFirstFile(searchPattern.c_str(), &dirData);
	if (handle == INVALID_HANDLE_VALUE) {
		return;
	}
	do {
		string dirName(dirData.cFileName);
#else
	DIR * dp = opendir(mainPath.c_str());
	if (!dp) {
		return;
	}
	while (dirent * dirp = readdir(dp)) {
		string dirName(dirp->d_name);
#endif
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
		if (variantName == "default" && !hasDefaultForLanguage(variants, dict.getLanguage().getLanguage())) {
			dict.setDefault(true);
		}
		if (dict.isValid()) {
			if (variants.find(dict.getLanguage().toBcp47()) == variants.end()) {
				variants[dict.getLanguage().toBcp47()] = dict;
			}
			else if (dict.isDefault()) {
				variants[dict.getLanguage().toBcp47()].setDefault(true);
			}
		}
#ifdef WIN32
	} while (FindNextFile(handle, &dirData) != 0);
	FindClose(handle);
#else
	}
	closedir(dp);
#endif
}

Dictionary DictionaryLoader::dictionaryFromPath(const string & path) {
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
	string morBackend = "malaga";
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
	return Dictionary(path, morBackend, spellBackend, suggestionBackend,
	                  hyphenatorBackend, language, description);
}

list<string> DictionaryLoader::getDefaultLocations() {
	list<string> locations;
	
	#ifndef DISABLE_EXTDICTS
	/* Path specified by environment variable VOIKKO_DICTIONARY_PATH */
	// XXX: Not actually thread safe but will most probably work
	char * path_from_env = getenv("VOIKKO_DICTIONARY_PATH");
	if (path_from_env) {
		locations.push_back(string(path_from_env));
	}

	#ifdef HAVE_GETPWUID_R
	/* $HOME/.voikko/VOIKKO_DICTIONARY_FILE */
	passwd * pwdResult;
	char * pwdBuf = new char[10000];
	if (pwdBuf) {
		passwd pwd;
		getpwuid_r(getuid(), &pwd, pwdBuf, 10000, &pwdResult);
		if (pwdResult && pwd.pw_dir) {
			string pwdPath(pwd.pw_dir);
			pwdPath.append("/.voikko");
			locations.push_back(pwdPath);
		}
		delete[] pwdBuf;
	}
	
	/* /etc on the same systems where getpwuid_r is available */
	locations.push_back("/etc/voikko");
	#endif
	
	#ifdef WIN32
	/* User default dictionary from Windows registry */
	HKEY hKey;
	LONG lRet = RegOpenKeyEx(HKEY_CURRENT_USER, VOIKKO_KEY,
						0, KEY_QUERY_VALUE, &hKey);
	char buffer[BUFFER_LENGTH];
	DWORD dwBufLen = BUFFER_LENGTH;
	if (ERROR_SUCCESS == lRet) {
		lRet = RegQueryValueEx(hKey, VOIKKO_VALUE_DICTIONARY_PATH, NULL, NULL,
		                       (LPBYTE)buffer, &dwBufLen);
		RegCloseKey(hKey);
		if ((ERROR_SUCCESS == lRet)) {
			string dirName(buffer);
			locations.push_back(dirName);
		}
	}
	
	/* System default dictionary from Windows registry */
	lRet = RegOpenKeyEx(HKEY_LOCAL_MACHINE, VOIKKO_KEY,
		                    0, KEY_QUERY_VALUE, &hKey);
	dwBufLen = BUFFER_LENGTH;
	if (ERROR_SUCCESS == lRet) {
		lRet = RegQueryValueEx(hKey, VOIKKO_VALUE_DICTIONARY_PATH, NULL, NULL,
		                       (LPBYTE)buffer, &dwBufLen);
		RegCloseKey(hKey);
		if ((ERROR_SUCCESS == lRet)) {
			string dirName(buffer);
			locations.push_back(dirName);
		}
	}
	#endif // WIN32
	
	/* Directory specified on compile time */
	locations.push_back(DICTIONARY_PATH);
	#endif
	
	return locations;
}

} }
