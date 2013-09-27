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

#include "setup/setup.hpp"
#include "grammar/CgRuleEngine.hpp"

namespace libvoikko { namespace grammar {

CgRuleEngine::CgRuleEngine()  {
	grammar = 0;
	applicator = 0;
	
	fprintf(stderr, "CgRuleEngine::CgRuleEngine\n");
	int res = cg3_init(stdin, stdout, stderr);
        if (!res) {
                fprintf(stderr, "Error: Failed cg3_init()! %d\n", res);
        } else {
		fprintf(stderr, "CgRuleEngine::cg3_init()! %d\n", res);
	}
}

CgRuleEngine::~CgRuleEngine() {

}

int CgRuleEngine::load(const std::string path) {
	fprintf(stderr, "CgRuleEngine::load\n");
	grammar = cg3_grammar_load(path.c_str());
	if (!grammar) {
		fprintf(stderr, "Error: Failed cg3_grammar_load( %s )!\n", path.c_str());
		return -1;
	}

	applicator = cg3_applicator_create(grammar);
	return 1;
}

void CgRuleEngine::check(GcCache & cache, const Paragraph * paragraph) { 
	fprintf(stderr, "CgRuleEngine::check\n");

	for (size_t i = 0; i < paragraph->sentenceCount; i++) {

		for(size_t j = 0; j < paragraph->sentences[i]->tokenCount; j++) {
			Token t =  paragraph->sentences[i]->tokens[j];
			fprintf(stderr, "CgRuleEngine::check %ls (%d) %ld\n", paragraph->sentences[i]->tokens[j].str, t.isValidWord, t.analyses->size());
			list<morphology::Analysis *>::iterator it = t.analyses->begin();
			unsigned int num_analyses = 0;
			while (it != t.analyses->end()) {
				num_analyses++;
				++it;
			}
			fprintf(stderr, "CgRuleEngine::check %d analyses\n", num_analyses);
			
		}
	}
	
	return;
}


} }
