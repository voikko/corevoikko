/* Libvoikko: Library of Finnish language tools
 * Copyright (C) 2010 Harri Pitkänen <hatapitk@iki.fi>
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

#ifndef VOIKKO_SETUP_LANGUAGE_TAG
#define VOIKKO_SETUP_LANGUAGE_TAG

#include <string>

namespace libvoikko { namespace setup {

/**
 * Class for representing and processing BCP 47 language tags.
 */
class LanguageTag {
	
	friend bool operator<(const LanguageTag & l1, const LanguageTag & l2);
	
	public:
		LanguageTag();
		LanguageTag(const LanguageTag & languageTag);
		
		const std::string & getLanguage() const;
		void setLanguage(const std::string & language);
		
		const std::string & getPrivateUse() const;
		void setPrivateUse(const std::string & privateUse);
		
		std::string toBcp47() const;
	private:
		std::string language;
		std::string privateUse;
};

} }

#endif
