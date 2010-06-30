/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2009 Harri Pitkänen <hatapitk@iki.fi>
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

#include <cstring>
#include <set>
#include "setup/Dictionary.hpp"
#include "setup/DictionaryLoader.hpp"
#include "porting.h"

using namespace std;

namespace libvoikko { namespace setup {

typedef Dictionary voikko_dict;

VOIKKOEXPORT voikko_dict ** voikko_list_dicts(const char * path) {
	list<Dictionary> dictList = path ?
	                            DictionaryLoader::findAllAvailable(path) :
	                            DictionaryLoader::findAllAvailable();
	
	voikko_dict ** dicts = new voikko_dict*[dictList.size() + 1];
	size_t n = 0;
	for (list<Dictionary>::iterator i = dictList.begin(); i != dictList.end(); ++i) {
		dicts[n++] = new Dictionary(*i);
	}
	dicts[n] = 0;
	return dicts;
}

VOIKKOEXPORT void voikko_free_dicts(voikko_dict ** dicts) {
	for (voikko_dict ** i = dicts; *i; i++) {
		delete *i;
	}
	delete[] dicts;
}

VOIKKOEXPORT const char * voikko_dict_language(const voikko_dict * dict) {
	return dict->getLanguage().getLanguage().c_str();
}

VOIKKOEXPORT const char * voikko_dict_variant(const voikko_dict * dict) {
	return dict->getLanguage().getPrivateUse().c_str();
}

VOIKKOEXPORT const char * voikko_dict_description(const voikko_dict * dict) {
	return dict->getDescription().c_str();
}

VOIKKOEXPORT char ** voikkoListSupportedSpellingLanguages(const char * path) {
	list<Dictionary> dictList = path ?
	                            DictionaryLoader::findAllAvailable(path) :
	                            DictionaryLoader::findAllAvailable();
	set<string> allLanguages;
	for (list<Dictionary>::iterator i = dictList.begin(); i != dictList.end(); ++i) {
		allLanguages.insert((*i).getLanguage().getLanguage());
	}
	
	char ** languages = new char*[allLanguages.size() + 1];
	size_t n = 0;
	for (set<string>::iterator i = allLanguages.begin(); i != allLanguages.end(); ++i) {
		string lang = *i;
		languages[n] = new char[lang.size() + 1];
		strcpy(languages[n], lang.c_str());
		n++;
	}
	languages[n] = 0;
	return languages;
}

VOIKKOEXPORT const char * voikkoGetVersion() {
	#ifdef PACKAGE_VERSION
		return PACKAGE_VERSION;
	#else
		return "(unknown)";
	#endif
}

} }
