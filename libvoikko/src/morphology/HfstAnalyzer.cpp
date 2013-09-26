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
#include "voikko_defines.h"
#include <fstream>

using namespace std;
using namespace libvoikko::utils;

namespace libvoikko { namespace morphology {

HfstAnalyzer::HfstAnalyzer(const string& s) throw(setup::DictionaryException) {
	cerr << "HfstAnalyzer::HfstAnalyzer: " << s << endl;
	if(s.find(".zhfst") != std::string::npos) {
		return;
	}
	FILE *fd = fopen(s.c_str(), "rb");
	t = new hfst_ol::Transducer(fd);

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
	cerr << "HfstAnalyzer::analyze (" << string(word) << ")" << endl;
	size_t wlen = strlen(word);
	if (wlen > LIBVOIKKO_MAX_WORD_CHARS) {
		return new list<Analysis *>();
	}
	list<Analysis *> * analysisList = new list<Analysis *>();
	vector<string> res = t->lookup(word);
	for(vector<string>::iterator it = res.begin(); it != res.end(); it++) {
		cerr << "  analysis: " << *it << endl; 
	}

	return analysisList;
}


void HfstAnalyzer::terminate() {
}

} }
