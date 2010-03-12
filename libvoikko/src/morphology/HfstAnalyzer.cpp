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

#include "morphology/HfstAnalyzer.hpp"
#include "setup/DictionaryException.hpp"
#include "utils/StringUtils.hpp"
#include "voikko_defs.h"
#include <fstream>
#include <hfst2/string.h>

using namespace std;
using namespace libvoikko::utils;

namespace libvoikko { namespace morphology {

HfstAnalyzer::HfstAnalyzer(const string & directoryName) throw(setup::DictionaryException) {
	keyTable = HWFST::create_key_table();
	string morFile = directoryName + "/mor.hwfst";
	ifstream morStream(morFile.c_str());
	if (morStream.good()) {
		morphology = HWFST::read_transducer(morStream, keyTable);
		for (HWFST::Key k = 0; k < keyTable->get_unused_key(); ++k) {
			flagTable.define_diacritic(k, HWFST::get_symbol_name(HWFST::get_key_symbol(k, keyTable)));
			if (flagTable.is_diacritic(k)) {
				flags.insert(k);
			}
		}
	}
	else {
		throw setup::DictionaryException("Failed to open mor.hwfst");
	}
}
    
list<Analysis *> * HfstAnalyzer::analyze(const wchar_t * word) {
	return analyze(word, wcslen(word));
}

list<Analysis *> * HfstAnalyzer::analyze(const wchar_t * word,
                                         size_t wlen) {
	if (wlen > LIBVOIKKO_MAX_WORD_CHARS) {
		return new list<Analysis *>();
	}
	char * wordUtf8 = StringUtils::utf8FromUcs4(word, wlen);
	list<Analysis *> * result = analyze(wordUtf8);
	delete[] wordUtf8;
	return result;
}

list<Analysis *> * HfstAnalyzer::analyze(const char * word) {
	size_t wlen = strlen(word);
	if (wlen > LIBVOIKKO_MAX_WORD_CHARS) {
		return new list<Analysis *>();
	}
	list<Analysis *> * analysisList = new list<Analysis *>();
	HWFST::KeyVector * wordPath = HWFST::stringUtf8ToKeyVector(word, keyTable);
	HWFST::KeyVectorVector * analysisVector = HWFST::lookup_all(morphology, wordPath, &flags);
	int currentAnalysisCount = 0;
	for (HWFST::KeyVectorVector::iterator analysisIt = analysisVector->begin();
	     analysisIt != analysisVector->end() && currentAnalysisCount < LIBVOIKKO_MAX_ANALYSIS_COUNT;
	     ++analysisIt) {
		HWFST::KeyVector * analysis = *analysisIt;
		KeyVector* filtlkv = flagTable.filter_diacritics(analysis);
		if (filtlkv) {
			addAnalysis(filtlkv, analysisList, wlen);
			++currentAnalysisCount;
			delete filtlkv;
		}
	}
	delete analysisVector;
	delete wordPath;
	return analysisList;
}

void HfstAnalyzer::addAnalysis(HWFST::KeyVector * hfstAnalysis, list<Analysis *> * analysisList, size_t charCount) const {
	Analysis * analysis = new Analysis();
	string * analysisString = HWFST::keyVectorToString(hfstAnalysis, keyTable);
	// TODO: do something with the analysis
	delete analysisString;
	wchar_t * structure = new wchar_t[charCount + 2];
	structure[0] = L'=';
	for (size_t i = 1; i < charCount + 1; i++) {
		structure[i] = L'p';
	}
	structure[charCount + 1] = L'\0';
	analysis->addAttribute("STRUCTURE", structure);
	analysis->addAttribute("CLASS", utils::StringUtils::copy(L"none"));
	analysis->addAttribute("SIJAMUOTO", utils::StringUtils::copy(L"none"));
	analysisList->push_back(analysis);
}

void HfstAnalyzer::terminate() {
	delete keyTable;
	keyTable = 0;
	delete morphology;
	morphology = 0;
}

} }
