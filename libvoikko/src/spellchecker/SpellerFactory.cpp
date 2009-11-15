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

#include "spellchecker/SpellerFactory.hpp"
#include "spellchecker/AnalyzerToSpellerAdapter.hpp"

using namespace std;

namespace libvoikko { namespace spellchecker {

Speller * SpellerFactory::getSpeller(voikko_options_t * voikkoOptions,
	                             const setup::Dictionary & dictionary)
	                              throw(setup::DictionaryException) {
	// FIXME: add proper parsing and possibility to combine components freely
	if (dictionary.getSpellBackend() == "AnalyzerToSpellerAdapter(currentAnalyzer)") {
		return new AnalyzerToSpellerAdapter(voikkoOptions->morAnalyzer);
	}
	throw setup::DictionaryException("Failed to create speller because backend configuration could not be parsed");
}

} }
