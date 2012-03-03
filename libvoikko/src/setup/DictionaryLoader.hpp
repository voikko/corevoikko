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
 * Portions created by the Initial Developer are Copyright (C) 2008 - 2010
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

#ifndef VOIKKO_SETUP_DICTIONARYLOADER
#define VOIKKO_SETUP_DICTIONARYLOADER

#include "setup/Dictionary.hpp"
#include "setup/DictionaryException.hpp"
#include <map>
#include <list>

namespace libvoikko { namespace setup {

class DictionaryLoader {

	public:
	/**
	 * Find available dictionaries from default locations.
	 * @return a set of uninitialized dictionaries
	 */
	static std::list<Dictionary> findAllAvailable();
	
	/**
	 * Find available dictionaries from given path and default locations.
	 * @return a set of uninitialized dictionaries
	 */
	static std::list<Dictionary> findAllAvailable(const std::string & path);
	
	/**
	 * Load dictionary from default locations. The dictionary must match given language
	 * tag.
	 * @return an initialized dictionary
	 */
	static Dictionary load(const std::string & language) throw(DictionaryException);
	
	/**
	 * Load dictionary from given path and default locations. The dictionary must match
	 * given language tag.
	 * @return an initialized dictionary
	 */
	static Dictionary load(const std::string & language, const std::string & path)
	       throw(DictionaryException);

	private:
	/**
	 * Add dictionary variants from a directory path to a map
	 * "variant name" -> "dictionary".
	 * If a dictionary already exists in the map, it will not be replaced.
	 */
	static void addVariantsFromPath(const std::string & path,
	       std::map<std::string, Dictionary> & variants);
	
	/**
	 * Create a dictionary object from a path to a morphology location. If the
	 * location does not contain a valid dictionary, the method retuns an invalid
	 * dictionary.
	 */
	static Dictionary dictionaryFromPath(const std::string & path);
	
	/**
	 * Get a list of default dictionary locations. The entries are listed in
	 * decreasing priority order.
	 */
	static std::list<std::string> getDefaultLocations();
};

} }

#endif
