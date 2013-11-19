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

#include <iostream>

#include "setup/setup.hpp"
#include "grammar/CgGrammarChecker.hpp"
#include "grammar/CgRuleEngine.hpp"
#include "grammar/HfstAnalysis.hpp"
#include "morphology/HfstAnalyzer.hpp"

#include "grammar/tinyxml2.hpp"

using namespace std;

namespace libvoikko { namespace grammar {

CgGrammarChecker::CgGrammarChecker()  {

	
}

CgGrammarChecker::CgGrammarChecker(const string & f_analyser, const string & rules, voikko_options_t * voikkoOptions) {

	cerr << "CgGrammarChecker::CgGrammarChecker: " << analyser << " " << rules << endl;

	// FIXME: This is not platform independent.
	size_t found = rules.rfind("/");
	string errorlistpath = "errors.xml";
	if(found != string::npos) {
		errorlistpath = rules.substr(0, found) + "/errors.xml";
	}


	// Should probably separate this into a separate method.
	cerr << "  -- errorlistpath: " << errorlistpath << endl;
	tinyxml2::XMLDocument doc;
	doc.LoadFile(errorlistpath.c_str());
	tinyxml2::XMLNode *docp;
	docp = doc.FirstChildElement("errors")->FirstChildElement("error");
//	pair<code, lang>, pair<title, description>
	map< pair<string , string >, string > titles;
	map< pair<string , string >, string > descriptions;
	while(docp != NULL) {
		const char *id = docp->ToElement()->Attribute("id");
		const char *title = NULL;
		const char *desc = NULL;
		const char *lang = NULL;
		cerr << "   -- &" << id << endl;
		tinyxml2::XMLNode *docp2;
		docp2 = docp->FirstChildElement("header")->FirstChildElement("title");
		while(docp2 != NULL) {
			lang = docp2->ToElement()->Attribute("xml:lang");			
			title = docp2->ToElement()->GetText();	
			cerr << "   " << lang << ": " << title << endl;
			titles[make_pair("&" +string(id), string(lang))] = string(title);
			docp2 = docp2->NextSibling();
		}
		docp2 = docp->FirstChildElement("body")->FirstChildElement("description");
		while(docp2 != NULL) {
			lang = docp2->ToElement()->Attribute("xml:lang");			
			desc = docp2->ToElement()->GetText();	
			cerr << "   " << lang << ": " << desc << endl;
			descriptions[make_pair("&" + string(id), string(lang))] = string(desc);
			docp2 = docp2->NextSibling();
		}

		docp = docp->NextSibling();	
	}
	map< pair<string, string>, string >::iterator it;
	for(it = titles.begin(); it != titles.end(); it++) {
		errorlist[it->first] = make_pair(titles[it->first], descriptions[it->first]);
	}

	CgRuleEngine * cgRuleEngine = new CgRuleEngine(voikkoOptions);
	ruleEngine = cgRuleEngine;
	cgRuleEngine->load(rules);
	cgRuleEngine->setErrorList(&errorlist);

	analyser = new morphology::HfstAnalyzer(f_analyser);

	paragraphAnalyser = new HfstAnalysis(analyser, voikkoOptions);

//	tokeniser = new tokenizer::Tokenizer();

}


CgGrammarChecker::~CgGrammarChecker() {
	delete paragraphAnalyser;
	delete ruleEngine;
}

void CgGrammarChecker::init() { 

	return;
}


} }
