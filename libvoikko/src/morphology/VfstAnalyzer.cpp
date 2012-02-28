/* Libvoikko: Library of Finnish language tools
 * Copyright (C) 2012 Harri Pitkänen <hatapitk@iki.fi>
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

#include "morphology/VfstAnalyzer.hpp"
#include "setup/DictionaryException.hpp"
#include "utils/StringUtils.hpp"
#include "voikko_defines.h"

using namespace std;
using namespace libvoikko::utils;
using namespace libvoikko::fst;

namespace libvoikko { namespace morphology {

static const int BUFFER_SIZE = 2000;

VfstAnalyzer::VfstAnalyzer(const string & directoryName) throw(setup::DictionaryException) {
	string morFile = directoryName + "/mor.vfst";
	transducer = new Transducer(morFile.c_str());
	configuration = new Configuration(transducer->getFlagDiacriticFeatureCount(), BUFFER_SIZE);
	outputBuffer = new char[BUFFER_SIZE];
}

list<Analysis *> * VfstAnalyzer::analyze(const wchar_t * word) {
	return analyze(word, wcslen(word));
}

list<Analysis *> * VfstAnalyzer::analyze(const wchar_t * word, size_t wlen) {
	if (wlen > LIBVOIKKO_MAX_WORD_CHARS) {
		return new list<Analysis *>();
	}
	char * wordUtf8 = StringUtils::utf8FromUcs4(word, wlen);
	list<Analysis *> * result = analyze(wordUtf8);
	delete[] wordUtf8;
	return result;
}

list<Analysis *> * VfstAnalyzer::analyze(const char * word) {
	size_t wlen = strlen(word);
	if (wlen > LIBVOIKKO_MAX_WORD_CHARS) {
		return new list<Analysis *>();
	}
	list<Analysis *> * analysisList = new list<Analysis *>();
	if (transducer->prepare(configuration, word, wlen)) {
		wchar_t * wordUcs4 = StringUtils::ucs4FromUtf8(word, wlen);
		size_t ucsLen = wcslen(wordUcs4);
		while (transducer->next(configuration, outputBuffer, BUFFER_SIZE)) {
			Analysis * analysis = new Analysis();
			wchar_t * structure = new wchar_t[ucsLen + 2];
			structure[0] = L'=';
			for (size_t i = 1; i < ucsLen + 1; i++) {
				structure[i] = L'p';
			}
			structure[ucsLen + 1] = L'\0';
			analysis->addAttribute("STRUCTURE", structure);
			analysis->addAttribute("CLASS", utils::StringUtils::copy(L"none"));
			analysis->addAttribute("SIJAMUOTO", utils::StringUtils::copy(L"none"));
			analysis->addAttribute("FSTOUTPUT", StringUtils::ucs4FromUtf8(outputBuffer));
			analysisList->push_back(analysis);
		}
		delete[] wordUcs4;
	}
	return analysisList;
}

void VfstAnalyzer::terminate() {
	delete[] outputBuffer;
	delete configuration;
	transducer->terminate();
	delete transducer;
}

} }
