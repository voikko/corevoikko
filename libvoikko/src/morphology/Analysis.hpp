/* Libvoikko: Library of Finnish language tools
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

#ifndef VOIKKO_MORPHOLOGY_ANALYSIS
#define VOIKKO_MORPHOLOGY_ANALYSIS

#include <string>
#include <map>

namespace libvoikko { namespace morphology {

/**
 * Results from morphological analysis.
 */
class Analysis {
	public:
		Analysis();
		~Analysis();
		
		/**
		 * Adds an attribute to analysis. Ownership of value
		 * is transferred to this object.
		 */
		void addAttribute(const char * key, wchar_t * value);
		
		/**
		 * Returns a null terminated array of strings containing
		 * the attribute names in this analysis.
		 */
		const char ** getKeys() const;

		/**
		 * Returns the value of given attribute. If no such
		 * attribute exists, returns null.
		 */
		const wchar_t * getValue(const char * key) const;
	private:
		Analysis(Analysis const & other);
		Analysis & operator = (const Analysis & other);
		
		void deleteKeys();
		void recreateKeys();
		const char ** keys;
		std::map<std::string, wchar_t *> attributes;
};

} }

#endif
