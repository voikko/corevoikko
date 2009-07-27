/* Libvoikko: Finnish spellchecker and hyphenator library
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

#include "voikko_defs.h"
#include "morphology/Analysis.hpp"
#include "morphology/Analyzer.hpp"
#include "morphology/AnalyzerFactory.hpp"

using namespace std;

namespace libvoikko { namespace morphology {

typedef Analysis voikko_mor_analysis;

VOIKKOEXPORT voikko_mor_analysis ** voikko_analyze_word_ucs4(
                                    int /*handle*/, const wchar_t * word) {
	const Analyzer * analyzer = AnalyzerFactory::getAnalyzer();
	list<Analysis *> * analyses = analyzer->analyze(word);
	voikko_mor_analysis ** result
	    = new voikko_mor_analysis*[analyses->size() + 1];
	list<Analysis *>::const_iterator it = analyses->begin();
	size_t i = 0;
	while (it != analyses->end()) {
		result[i++] = *it++;
	}
	result[i] = 0;
	delete analyses;
	return result;
}

VOIKKOEXPORT void voikko_free_mor_analysis(voikko_mor_analysis ** analysis) {
	if (!analysis) {
		return;
	}
	for (voikko_mor_analysis ** i = analysis; *i; i++) {
		delete *i;
	}
	delete[] analysis;
}

VOIKKOEXPORT const char ** voikko_mor_analysis_keys(
                           const voikko_mor_analysis * analysis) {
	return analysis->getKeys();
}

VOIKKOEXPORT const wchar_t * voikko_mor_analysis_value_ucs4(
                             const voikko_mor_analysis * analysis,
                             const char * key) {
	return analysis->getValue(key);
}

} }
