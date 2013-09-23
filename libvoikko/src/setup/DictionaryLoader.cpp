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

#include "setup/DictionaryLoader.hpp"
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

using namespace std;

namespace libvoikko { namespace setup {

DictionaryLoader::~DictionaryLoader() {
}

void DictionaryLoader::tagToCanonicalForm(string & languageTag) {
	for (size_t i = 0; i < languageTag.size(); ++i) {
		char current = languageTag.at(i);
		if (current >= 65 && current <= 90) {
			languageTag[i] = current + 32;
		}
	}
}

/**
 * Returns true if the given variant map contains a default dictionary for given language.
 */
bool DictionaryLoader::hasDefaultForLanguage(map<string, Dictionary> & variants, const string & language) {
	for (map<string, Dictionary>::iterator i = variants.begin(); i != variants.end(); ++i) {
		if (i->second.getLanguage().getLanguage() == language && i->second.isDefault()) {
			return true;
		}
	}
	return false;
}

list<string> DictionaryLoader::getListOfSubentries(const string & mainPath) {
	list<string> results;
#ifdef WIN32
	string searchPattern(mainPath);
	searchPattern.append("\\*");
	WIN32_FIND_DATA dirData;
	HANDLE handle = FindFirstFile(searchPattern.c_str(), &dirData);
	if (handle == INVALID_HANDLE_VALUE) {
		return results;
	}
	do {
		string dirName(dirData.cFileName);
#else
	DIR * dp = opendir(mainPath.c_str());
	if (!dp) {
		return results;
	}
	while (dirent * dirp = readdir(dp)) {
		string dirName(dirp->d_name);
#endif
		results.push_back(dirName);
#ifdef WIN32
	} while (FindNextFile(handle, &dirData) != 0);
	FindClose(handle);
#else
	}
	closedir(dp);
#endif
	return results;
}

} }
