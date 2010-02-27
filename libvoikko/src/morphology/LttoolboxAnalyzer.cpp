/* Libvoikko: Library of Finnish language tools
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

#include "morphology/LttoolboxAnalyzer.hpp"
#include "setup/DictionaryException.hpp"
#include "utils/StringUtils.hpp"
#include "voikko_defs.h"

using namespace std;
using namespace libvoikko::utils;

namespace libvoikko { namespace morphology {

LttoolboxAnalyzer::LttoolboxAnalyzer(const string & directoryName) throw(setup::DictionaryException) {
	string fileName = directoryName + "/mor.bin";
	FILE * transducerFile = fopen(fileName.c_str(), "r");
	if (!transducerFile) {
		throw setup::DictionaryException("Failed to open mor.bin");
	}
	processor.load(transducerFile);
	fclose(transducerFile);
	processor.initBiltrans();
}
    
list<Analysis *> * LttoolboxAnalyzer::analyze(const wchar_t * word) {
	return analyze(word, wcslen(word));
}

list<Analysis *> * LttoolboxAnalyzer::analyze(const wchar_t * word,
                                         size_t wlen) {
	if (wlen > LIBVOIKKO_MAX_WORD_CHARS) {
		return new list<Analysis *>();
	}
	wstring inputString = L"^";
	inputString.append(word);
	inputString.append(L"$");
	wstring analysisString = processor.biltrans(inputString);
	list<Analysis *> * result = new list<Analysis *>();
	addAnalysis(analysisString, result, wlen);
	return result;
}

list<Analysis *> * LttoolboxAnalyzer::analyze(const char * word) {
	size_t wlen = strlen(word);
	if (wlen > LIBVOIKKO_MAX_WORD_CHARS) {
		return new list<Analysis *>();
	}
	wchar_t * wordUcs4 = StringUtils::ucs4FromUtf8(word);
	list<Analysis *> * analysisList = analyze(wordUcs4);
	delete[] wordUcs4;
	return analysisList;
}

void LttoolboxAnalyzer::addAnalysis(wstring analysisString, list<Analysis *> * analysisList, size_t charCount) const {
	if (analysisString.find(L"^@") == 0) {
		return;
	}
	Analysis * analysis = new Analysis();
	// TODO: do something with the analysis
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

void LttoolboxAnalyzer::terminate() {
}

} }
