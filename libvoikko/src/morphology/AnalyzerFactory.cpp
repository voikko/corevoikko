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

#include "morphology/AnalyzerFactory.hpp"
#include "morphology/NullAnalyzer.hpp"
#include "porting.h"

#ifdef HAVE_MALAGA
#include "morphology/MalagaAnalyzer.hpp"
#endif

#ifdef HAVE_HFST
#include "morphology/HfstAnalyzer.hpp"
#endif

#ifdef HAVE_LTTOOLBOX
#include "morphology/LttoolboxAnalyzer.hpp"
#endif

using namespace std;

namespace libvoikko { namespace morphology {

Analyzer * AnalyzerFactory::getAnalyzer(const setup::Dictionary & dictionary)
	                              throw(setup::DictionaryException) {
	if (dictionary.getMorBackend() == "null") {
		return new NullAnalyzer();
	}
	#ifdef HAVE_MALAGA
	if (dictionary.getMorBackend() == "malaga") {
		return new MalagaAnalyzer(dictionary.getMorPath());
	}
	#endif
	#ifdef HAVE_HFST
	if (dictionary.getMorBackend() == "hfst") {
		return new HfstAnalyzer(dictionary.getMorPath());
	}
	#endif
	#ifdef HAVE_LTTOOLBOX
	if (dictionary.getMorBackend() == "lttoolbox") {
		return new LttoolboxAnalyzer(dictionary.getMorPath());
	}
	#endif
	throw setup::DictionaryException("Failed to create analyzer because of unknown morphology backend");
}

} }
