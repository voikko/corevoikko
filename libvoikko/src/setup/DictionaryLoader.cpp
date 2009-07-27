/* Libvoikko: Library of Finnish language tools
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

#include "setup/DictionaryLoader.hpp"
#include "config.h"
#include <string>
#include <fstream>
#include <cstdlib>
#ifdef WIN32
# include <windows.h>
#else
# include <pwd.h>
# include <dirent.h>
#endif
#include <malaga.h>

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

list<Dictionary> DictionaryLoader::findAllAvailable() {
	return findAllAvailable(string());
}


list<Dictionary> DictionaryLoader::findAllAvailable(const std::string & path) {
	list<string> locations = getDefaultLocations();
	if (!path.empty()) {
		locations.push_front(path);
	}
	
	map<string, Dictionary> dictMap;
	for (list<string>::iterator i = locations.begin(); i != locations.end(); i++) {
		addVariantsFromPath(*i, dictMap);
	}
	
	list<Dictionary> dicts;
	for (map< string, Dictionary >::iterator i = dictMap.begin(); i != dictMap.end(); i++) {
		if (i->second.isDefault()) {
			dicts.push_front(i->second);
		}
		else if (i->first == "standard" && !hasDefault(dictMap)) {
			dicts.push_front(i->second);
		}
		else {
			dicts.push_back(i->second);
		}
	}
	return dicts;
}

Dictionary DictionaryLoader::load(const string & variant) throw(DictionaryException) {
	return load(variant, string());
}

Dictionary DictionaryLoader::load(const string & variant, const string & path)
		throw(DictionaryException) {
	list<Dictionary> dicts = findAllAvailable(path);
	if (dicts.size() == 0) {
		throw DictionaryException("No valid dictionaries were found");
	}
	
	string finalVariant(variant);
	if (variant.empty() || variant == "default" || variant == "fi_FI") {
		// Use dictionary specified by environment variable VOIKKO_DICTIONARY_PATH
		// FIXME: thread safety
		char * dict_from_env = getenv("VOIKKO_DICTIONARY");
		if (dict_from_env) {
			finalVariant = string(dict_from_env);
		}
		else {
			// No dictionary specified anywhere, so take the one with highest priority.
			loadDictionary(dicts.front());
			return dicts.front();
		}
	}
	for (list<Dictionary>::iterator i = dicts.begin(); i != dicts.end(); i++) {
		if ((*i).getVariant() == finalVariant) {
			loadDictionary(*i);
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
		if (variantName == "default" && !hasDefault(variants)) {
			dict.setDefault(true);
		}
		if (dict.isValid()) {
			if (variants.find(dict.getVariant()) == variants.end()) {
				variants[dict.getVariant()] = dict;
			}
			else if (dict.isDefault()) {
				variants[dict.getVariant()].setDefault(true);
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
	
	string variant;
	string description;
	while (file.good()) {
		getline(file, line);
		if (line.find("info: Language-Variant: ") == 0) {
			variant = line.substr(24);
		}
		else if (line.find("info: Description: ") == 0) {
			description = line.substr(19);
		}
	}
	file.close();
	return Dictionary(path, variant, description);
}

list<string> DictionaryLoader::getDefaultLocations() {
	list<string> locations;
	
	/* Path specified by environment variable VOIKKO_DICTIONARY_PATH */
	// FIXME: thread safety
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
	
	return locations;
}

bool DictionaryLoader::hasDefault(map<string, Dictionary> & variants) {
	for (map<string, Dictionary>::iterator i = variants.begin(); i != variants.end(); i++) {
		if (i->second.isDefault()) {
			return true;
		}
	}
	return false;
}

void DictionaryLoader::loadDictionary(const Dictionary & dictionary)
		throw(DictionaryException) {
	string projectFile(dictionary.getMorPath());
	projectFile.append("/");
	projectFile.append(VOIKKO_DICTIONARY_FILE);
	init_libmalaga(projectFile.c_str());
	if (malaga_error) {
		throw DictionaryException(malaga_error);
	}
}

} }
