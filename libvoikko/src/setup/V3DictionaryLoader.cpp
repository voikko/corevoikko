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

#include "setup/V3DictionaryLoader.hpp"
#include "setup/LanguageTag.hpp"
#include "porting.h"
#include <string>
#include <fstream>
#include <cstdlib>

#include <ZHfstOspeller.h>
#include <ospell.h>
#include <ol-exceptions.h>

#define HFST_DICTIONARY_VERSION "3"

using namespace std;

namespace libvoikko { namespace setup {

void V3DictionaryLoader::findDictionaries(const string & path) {
	string mainPath(path);
	mainPath.append("/");
	mainPath.append(HFST_DICTIONARY_VERSION);
	list<string> subDirectories = getListOfSubentries(mainPath);
	for (list<string>::iterator i = subDirectories.begin(); i != subDirectories.end(); ++i) {
		string dirName = *i;
		if (dirName.find(".zhfst") + 6 == dirName.length()) {
			string fullPath = mainPath + "/" + dirName;
			string morBackend = "null";
			string spellBackend = "hfst";
			string suggestionBackend = "hfst";
			// TODO implement null hyphenator
			string hyphenatorBackend = "AnalyzerToFinnishHyphenatorAdapter(currentAnalyzer)";
			
			hfst_ol::ZHfstOspeller * speller = new hfst_ol::ZHfstOspeller();
			try {
				speller->read_zhfst(fullPath.c_str());
			}
			catch (hfst_ol::ZHfstZipReadingError& zhzre) {
				delete speller;
				continue; // broken dictionary
			}
			const hfst_ol::ZHfstOspellerXmlMetadata spellerMetadata = speller->get_metadata();
			
			LanguageTag language;
			language.setBcp47(spellerMetadata.info_.locale_);
			map<string, string> languageVersions = spellerMetadata.info_.title_;
			string description = languageVersions[spellerMetadata.info_.locale_];
			delete speller;
			Dictionary dict = Dictionary(fullPath, morBackend, spellBackend, suggestionBackend,
			                        hyphenatorBackend, language, description);
			addDictionary(dict);
		}
	}
}


} }
