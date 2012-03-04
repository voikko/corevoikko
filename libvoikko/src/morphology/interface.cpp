/* The contents of this file are subject to the Mozilla Public License Version 
 * 1.1 (the "License"); you may not use this file except in compliance with 
 * the License. You may obtain a copy of the License at 
 * http://www.mozilla.org/MPL/
 * 
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 * 
 * The Original Code is Libvoikko: Library of natural language processing tools.
 * The Initial Developer of the Original Code is Harri Pitkänen <hatapitk@iki.fi>.
 * Portions created by the Initial Developer are Copyright (C) 2009
 * the Initial Developer. All Rights Reserved.
 * 
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
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

VOIKKOEXPORT voikko_mor_analysis ** voikkoAnalyzeWordUcs4(
                                    voikko_options_t * options, const wchar_t * word) {
	Analyzer * analyzer = options->morAnalyzer;
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

VOIKKOEXPORT voikko_mor_analysis ** voikkoAnalyzeWordCstr(
                                    voikko_options_t * options, const char * word) {
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
	voikko_mor_analysis ** result = voikkoAnalyzeWordUcs4(options, word_ucs4);
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
