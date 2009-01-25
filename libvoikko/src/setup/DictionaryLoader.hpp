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
	 * @return a set of unitialized dictionaries
	 */
	static std::list<Dictionary> findAllAvailable();
	
	/**
	 * Find available dictionaries from given path and default locations.
	 * @return a set of unitialized dictionaries
	 */
	static std::list<Dictionary> findAllAvailable(const std::string & path);
	
	/**
	 * Load dictionary from default locations.
	 * @return an initialized dictionary
	 */
	static Dictionary load(const std::string & variant) throw(DictionaryException);
	
	/**
	 * Load dictionary from given path and default locations.
	 * @return an initialized dictionary
	 */
	static Dictionary load(const std::string & variant, const std::string & path)
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
	
	/**
	 * Returns true if the given variant map contains a default dictionary.
	 */
	static bool hasDefault(std::map<std::string, Dictionary> & variants);
};

} }

#endif
