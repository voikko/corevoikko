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
#include <pwd.h>
#include <dirent.h>

#define VOIKKO_DICTIONARY_FILE "voikko-fi_FI.pro"
#define VOIKKO_DICTIONARY_VERSION_KEY "info: Voikko-Dictionary-Format: 1"
#ifdef WIN32
# define VOIKKO_KEY                   "SOFTWARE\\Voikko"
# define VOIKKO_VALUE_DICTIONARY_PATH "DictionaryPath"
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
	
	map<string, string> dictMap;
	for (list<string>::iterator i = locations.begin(); i != locations.end(); i++) {
		addVariantsFromPath(*i, dictMap);
	}
	
	list<Dictionary> dicts;
	for (map< string, string >::iterator i = dictMap.begin(); i != dictMap.end(); i++) {
		dicts.push_back(Dictionary(i->second, i->first));
	}
	return dicts;
}

Dictionary DictionaryLoader::load(const string & variant) throw(DictionaryException) {
	return load(variant, string());
}

Dictionary DictionaryLoader::load(const string & variant, const string & path)
		throw(DictionaryException) {
	//TODO: unimplemented
	return Dictionary(string(), string());
}

void DictionaryLoader::addVariantsFromPath(const string & path, map<string, string> & variants) {
	DIR * dp = opendir(path.c_str());
	if (!dp) {
		return;
	}
	while (dirent * dirp = readdir(dp)) {
		string dirName(dirp->d_name);
		if (dirName.find("mor_") != 0) {
			continue;
		}
		string variantName = dirName.substr(4);
		if (variantName.empty() || variants.find(variantName) != variants.end()) {
			continue;
		}
		string fullDirName(path);
		fullDirName.append("/");
		fullDirName.append(dirName);
		if (isValid(fullDirName)) {
			variants[variantName] = fullDirName;
		}
	}
}

bool DictionaryLoader::isValid(const string & path) {
	string fileName(path);
	fileName.append("/");
	fileName.append(VOIKKO_DICTIONARY_FILE);
	
	string line;
	ifstream file(fileName.c_str(), ifstream::in);
	if (file.good()) {
		getline(file, line);
	}
	file.close();
	
	return line.compare(VOIKKO_DICTIONARY_VERSION_KEY) == 0;
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
	passwd pwd;
	passwd * pwdResult;
	char * pwdBuf = new char[10000];
	if (pwdBuf) {
		getpwuid_r(getuid(), &pwd, pwdBuf, 10000, &pwdResult);
		if (pwdResult && pwd.pw_dir) {
			string pwdPath(pwd.pw_dir);
			pwdPath.append("/.voikko");
			locations.push_back(pwdPath);
		}
		delete[] pwdBuf;
	}
	
	/* /etc on the same systems where getpwuid_r is available */
	locations.push_back("/etc");
	#endif
	
	#ifdef WIN32
	/* User default dictionary from Windows registry */
	// FIXME: not yet converted from old code
	lRet = RegOpenKeyEx(HKEY_CURRENT_USER, VOIKKO_KEY,
	                    0, KEY_QUERY_VALUE, &hKey);
	dwBufLen = buflen - 18;
	if (ERROR_SUCCESS == lRet) {
		lRet = RegQueryValueEx(hKey, VOIKKO_VALUE_DICTIONARY_PATH, NULL, NULL,
		                       (LPBYTE)buffer, &dwBufLen);
		RegCloseKey(hKey);
		if ((ERROR_SUCCESS == lRet)) {
			strcpy(buffer + dwBufLen - 1, "/" VOIKKO_DICTIONARY_FILE);
			if (voikko_check_file(buffer)) {
				delete[] tmp_buf;
				return 1;
			}
		}
	}
	
	/* System default dictionary from Windows registry */
	lRet = RegOpenKeyEx(HKEY_LOCAL_MACHINE, VOIKKO_KEY,
		                    0, KEY_QUERY_VALUE, &hKey);
	dwBufLen = buflen - 18;
	if (ERROR_SUCCESS == lRet) {
		lRet = RegQueryValueEx(hKey, VOIKKO_VALUE_DICTIONARY_PATH, NULL, NULL,
		                       (LPBYTE)buffer, &dwBufLen);
		RegCloseKey(hKey);
		if ((ERROR_SUCCESS == lRet)) {
			strcpy(buffer + dwBufLen - 1, "/" VOIKKO_DICTIONARY_FILE);
			if (voikko_check_file(buffer)) {
				delete[] tmp_buf;
				return 1;
			}
		}
	}
	#endif // WIN32
	
	/* Directory specified on compile time */
	locations.push_back(DICTIONARY_PATH);
	
	return locations;
}

} }
