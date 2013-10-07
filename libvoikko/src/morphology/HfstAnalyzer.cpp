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
#include <ospell.h>

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
	//cerr << "HfstAnalyzer::analyze (" << string(word) << ")" << endl;
	size_t wlen = strlen(word);
	if (wlen > LIBVOIKKO_MAX_WORD_CHARS) {
		return new list<Analysis *>();
	}
	list<Analysis *> * analysisList = new list<Analysis *>();

	/* I know this is the wrong thing, but going to do it anyway */
	std::string str(word);
	char * writable = new char[str.size() + 1];
	std::copy(str.begin(), str.end(), writable);
	writable[str.size()] = '\0';

	hfst_ol::AnalysisQueue q = t->lookup(writable);

	while(q.size() > 0) {
		hfst_ol::StringWeightPair pair = q.top();
		string analysis = pair.first;
		string lemma = "";
		string tags = "";
		lemma = analysis.substr(0,analysis.find("+"));
		tags = analysis.substr(analysis.find("+"),analysis.length()-1);
		//cerr << "  analysis  " << lemma << "/" << tags << endl; 
		Analysis * a = new Analysis();
		a->addAttribute("lemma",  StringUtils::ucs4FromUtf8(lemma.c_str()));
		a->addAttribute("tags",  StringUtils::ucs4FromUtf8(tags.c_str()));
		analysisList->push_back(a);
		q.pop();
	}

	return analysisList;
}


void HfstAnalyzer::terminate() {
}

} }
