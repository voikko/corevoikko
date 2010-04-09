/* Libvoikko: Library of Finnish language tools
 * Copyright (C) 2009 - 2010 Harri Pitkänen <hatapitk@iki.fi>
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

#include "porting.h"
#include "hyphenator/HyphenatorFactory.hpp"
#include "hyphenator/AnalyzerToFinnishHyphenatorAdapter.hpp"
#ifdef HAVE_HFST
#include "hyphenator/HfstHyphenator.hpp"
#endif

using namespace libvoikko::setup;
using namespace std;

namespace libvoikko { namespace hyphenator {

Hyphenator * HyphenatorFactory::getHyphenator(const voikko_options_t * options,
                                const Dictionary & dictionary)
                                throw(DictionaryException) {
	string backend = dictionary.getHyphenatorBackend();
	if (backend == "AnalyzerToFinnishHyphenatorAdapter(currentAnalyzer)") {
		return new AnalyzerToFinnishHyphenatorAdapter(options->morAnalyzer);
	}
	#ifdef HAVE_HFST
	if (backend == "hfst") {
		return new HfstHyphenator(dictionary.getMorPath());
	}
	#endif
	throw DictionaryException("Could not create hyphenator due to unknown hyphenator backend");
}

} }
