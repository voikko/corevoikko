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

#include "setup/DictionaryFactory.hpp"
#include "setup/LanguageTag.hpp"
#include "porting.h"
#include <string>
#include <fstream>
#include <cstdlib>

#ifdef WIN32
# include <windows.h>
# define VOIKKO_KEY                   "SOFTWARE\\Voikko"
# define VOIKKO_VALUE_DICTIONARY_PATH "DictionaryPath"
# define BUFFER_LENGTH 200
#else
# include <pwd.h>
# include <dirent.h>
# include <unistd.h>
#endif

#include "setup/V2DictionaryLoader.hpp"
#include "setup/V3DictionaryLoader.hpp"

using namespace std;

namespace libvoikko { namespace setup {

static bool isMatchingLanguage(const LanguageTag & requested, const LanguageTag & available) {
	if (requested.getLanguage() != available.getLanguage()) {
		return false;
	}
	if (!requested.getPrivateUse().empty() && requested.getPrivateUse() != available.getPrivateUse()) {
		return false;
	}
	return true;
}

list<Dictionary> DictionaryFactory::findAllAvailable() {
	return findAllAvailable(string());
}


list<Dictionary> DictionaryFactory::findAllAvailable(const std::string & path) {
	list<string> locations = getDefaultLocations();
	if (!path.empty()) {
		locations.push_front(path);
	}
	
	map<string, Dictionary> dictMap;
	for (list<string>::iterator i = locations.begin(); i != locations.end(); ++i) {
		addAllVersionVariantsFromPath(*i, dictMap);
	}
	
	list<Dictionary> dicts;
	for (map< string, Dictionary >::iterator i = dictMap.begin(); i != dictMap.end(); ++i) {
		if (i->second.isDefault()) {
			dicts.push_front(i->second);
		}
		else if (i->first.rfind("-x-standard") == i->first.size() - 11 &&
		         !DictionaryLoader::hasDefaultForLanguage(dictMap, i->second.getLanguage().getLanguage())) {
			dicts.push_front(i->second);
		}
		else {
			dicts.push_back(i->second);
		}
	}
	return dicts;
}

Dictionary DictionaryFactory::load(const string & language) throw(DictionaryException) {
	return load(language, string());
}

Dictionary DictionaryFactory::load(const string & language, const string & path)
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

LanguageTag DictionaryFactory::parseFromBCP47(const string & language) {
	// TODO: this parsing algorithm is incomplete
	LanguageTag tag;
	if (language.size() < 2) {
		return tag;
	}
	
	string canonicalLanguage = language;
	DictionaryLoader::tagToCanonicalForm(canonicalLanguage);
	
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

void DictionaryFactory::addAllVersionVariantsFromPath(const string & path, map<string, Dictionary> & variants) {
	list<DictionaryLoader*> loaders;
	#ifdef HAVE_HFST
		loaders.push_back(new V3DictionaryLoader());
	#endif
	loaders.push_back(new V2DictionaryLoader());
	
	for (list<DictionaryLoader*>::iterator i = loaders.begin(); i != loaders.end(); ++i) {
		(*i)->addVariantsFromPath(path, variants);
		delete *i;
	}
}

list<string> DictionaryFactory::getDefaultLocations() {
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
	
	#ifdef DICTIONARY_PATH
	/* Directory specified on compile time */
	locations.push_back(DICTIONARY_PATH);
	#endif
	
	#endif
	
	return locations;
}

} }
