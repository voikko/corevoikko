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

#include "morphology/HfstAnalyzer.hpp"
#include "setup/DictionaryException.hpp"
#include "utils/StringUtils.hpp"
#include "voikko_defines.h"
#include <fstream>
#include <ospell.h>

using namespace libvoikko::utils;

using std::string;
using std::list;

namespace libvoikko { namespace morphology {

HfstAnalyzer::HfstAnalyzer(const string& s) throw(setup::DictionaryException) {
	if(s.find(".zhfst") != std::string::npos) {
		return;
	}
	FILE *fd = fopen(s.c_str(), "rb");
	t = new hfst_ol::Transducer(fd);

}

list<Analysis *> * HfstAnalyzer::analyze(const wchar_t * word, size_t wlen, bool fullMorphology) {
	if (wlen > LIBVOIKKO_MAX_WORD_CHARS) {
		return new list<Analysis *>();
	}
	char * wordUtf8 = StringUtils::utf8FromUcs4(word, wlen);
	list<Analysis *> * result = analyze(wordUtf8, fullMorphology);
	delete[] wordUtf8;
	return result;
}

list<Analysis *> * HfstAnalyzer::analyze(const char * word, bool fullMorphology) {
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
		string tags = analysis.substr(analysis.find("+"),analysis.length()-1);
		Analysis * a = new Analysis();
		if (fullMorphology) {
			string lemma = analysis.substr(0,analysis.find("+"));
			a->addAttribute("lemma",  StringUtils::ucs4FromUtf8(lemma.c_str()));
		}
		a->addAttribute("tags",  StringUtils::ucs4FromUtf8(tags.c_str()));
		analysisList->push_back(a);
		q.pop();
	}

	return analysisList;
}


void HfstAnalyzer::terminate() {
}

} }
