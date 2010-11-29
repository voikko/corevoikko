/* Libvoikko: Library of Finnish language tools
 * Copyright (C) 2010 Flammie Pirinen <flammie@iki.fi>
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

#ifndef VOIKKO_SPELLCHECKER_HFST_SUGGESTION
#define VOIKKO_SPELLCHECKER_HFST_SUGGESTION

#include <string>
#include <ospell.h>
#include "setup/DictionaryException.hpp"
#include "spellchecker/suggestion/SuggestionStatus.hpp"
#include "spellchecker/suggestion/SuggestionGenerator.hpp"

namespace libvoikko { namespace spellchecker { namespace suggestion {

/**
 * HFST based speller.
 */
class HfstSuggestion : public SuggestionGenerator {
	public:
		HfstSuggestion(const std::string & directoryName) throw(setup::DictionaryException);
		void generate(SuggestionStatus* s) const;
		void terminate();
	private:
        hfst_ol::Speller* speller_;
};

} } }

#endif
