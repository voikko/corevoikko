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

#include "porting.h"
#include "morphology/Analysis.hpp"
#include "morphology/Analyzer.hpp"
#include "setup/setup.hpp"
#include "utils/StringUtils.hpp"
#include <cstring>

using namespace std;

namespace libvoikko { namespace morphology {

typedef Analysis voikko_mor_analysis;

VOIKKOEXPORT voikko_mor_analysis ** voikko_analyze_word_ucs4(
                                    int /*handle*/, const wchar_t * word) {
	Analyzer * analyzer = voikko_options.morAnalyzer;
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

VOIKKOEXPORT voikko_mor_analysis ** voikko_analyze_word_cstr(
                                    int handle, const char * word) {
	if (word == 0 || word[0] == '\0') {
		return 0;
	}
	size_t len = strlen(word);
	if (len > LIBVOIKKO_MAX_WORD_CHARS) {
		return 0;
	}
	wchar_t * word_ucs4 = utils::StringUtils::ucs4FromUtf8(word, len);
	if (word_ucs4 == 0) {
		return 0;
	}
	voikko_mor_analysis ** result = voikko_analyze_word_ucs4(handle, word_ucs4);
	delete[] word_ucs4;
	return result;
}

VOIKKOEXPORT char * voikko_mor_analysis_value_cstr(
                const voikko_mor_analysis * analysis,
                const char * key) {
	const wchar_t * value_ucs4 = voikko_mor_analysis_value_ucs4(analysis, key);
	if (value_ucs4) {
		return utils::StringUtils::utf8FromUcs4(value_ucs4);
	}
	else {
		return 0;
	}
}

VOIKKOEXPORT void voikko_free_mor_analysis_value_cstr(char * analysis_value) {
	delete[] analysis_value;
}

} }
