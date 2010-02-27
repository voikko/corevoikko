/* Libvoikko: Finnish spellchecker and hyphenator library
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

#ifndef VOIKKO_SPELLCHECKER_SUGGESTION_SUGGESTION_GENERATOR_FACTORY_H
#define VOIKKO_SPELLCHECKER_SUGGESTION_SUGGESTION_GENERATOR_FACTORY_H

#include "spellchecker/suggestion/SuggestionType.hpp"
#include "spellchecker/suggestion/SuggestionGenerator.hpp"
#include "setup/Dictionary.hpp"
#include "setup/DictionaryException.hpp"

namespace libvoikko { namespace spellchecker { namespace suggestion {

/**
 * Factory for obtaining suitable suggestion generator.
 */
class SuggestionGeneratorFactory {
	public:
		/**
		 * Creates and initializes a new SuggestionGenerator that matches given
		 * dictionary and settings.
		 * The suggestion generator must be terminated and deleted after use.
		 * @throws DictionaryException if the suggestion generator cannot be initialized.
		 */
		static SuggestionGenerator * getSuggestionGenerator(voikko_options_t * voikkoOptions,
		                            SuggestionType suggestionType)
		                              throw(setup::DictionaryException);
};

}}}

#endif
