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

#ifndef VOIKKO_HYPHENATOR_HYPHENATOR_FACTORY_HPP
#define VOIKKO_HYPHENATOR_HYPHENATOR_FACTORY_HPP

#include "hyphenator/Hyphenator.hpp"
#include "setup/Dictionary.hpp"
#include "setup/DictionaryException.hpp"
#include "setup/setup.hpp"

namespace libvoikko { namespace hyphenator {

/**
 * Factory for creating new Hyphenator objects.
 */
class HyphenatorFactory {

	public:
		/**
		 * Creates and initializes a new Hyphenator that matches given dictionary.
		 * The hyphenator must be terminated and deleted after use.
		 * @throws DictionaryException if the hyphenator cannot be initialized.
		 */
		static Hyphenator * getHyphenator(const voikko_options_t * options,
		                                  const setup::Dictionary & dictionary)
		                                 throw(setup::DictionaryException);
	
};

} }

#endif
