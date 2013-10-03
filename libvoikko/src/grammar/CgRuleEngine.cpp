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

#include "grammar/error.hpp"
#include "setup/setup.hpp"
#include "utils/StringUtils.hpp"
#include "grammar/CgRuleEngine.hpp"
#include "grammar/cache.hpp"

namespace libvoikko { namespace grammar {

CgRuleEngine::CgRuleEngine(voikko_options_t * voikkoOptions)  {
	grammar = 0;
	applicator = 0;

	options = voikkoOptions ; 
	
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
        cg3_applicator_setflags(applicator, CG3F_TRACE);
	return 1;
}

void CgRuleEngine::check(const Paragraph * paragraph) { 
	fprintf(stderr, "CgRuleEngine::check\n");

	for (size_t i = 0; i < paragraph->sentenceCount; i++) {
		cg3_sentence *sentence = 0;
		sentence = cg3_sentence_new(applicator);
		for(size_t j = 0; j < paragraph->sentences[i]->tokenCount; j++) {
			Token t =  paragraph->sentences[i]->tokens[j];
			if(wcscmp(t.str, L" ") == 0) {
				continue;
			}
			cg3_cohort *cohort = 0;
			cohort = cg3_cohort_create(sentence);
			//fprintf(stderr, "CgRuleEngine::check %ls (%d) %ld\n", paragraph->sentences[i]->tokens[j].str, t.isValidWord, t.analyses->size());
			cg3_tag *tag = 0;	
			string wordform = string("\"<") + utils::StringUtils::StringUtils::utf8FromUcs4(paragraph->sentences[i]->tokens[j].str) + string(">\""); 
			//fprintf(stderr, "wordform is: %s\n", wordform.c_str());
			tag = cg3_tag_create_u8(applicator, wordform.c_str());
			cg3_cohort_setwordform(cohort, tag);

			list<morphology::Analysis *>::iterator it = t.analyses->begin();
			unsigned int num_analyses = 0;
			while (it != t.analyses->end()) {
				morphology::Analysis *a = *it;
				cg3_reading *reading = 0;
				reading = cg3_reading_create(cohort);
				string baseform = string("\"") + utils::StringUtils::StringUtils::utf8FromUcs4((a->getValue("lemma"))) + string("\"");
				tag = cg3_tag_create_u8(applicator, baseform.c_str());
				cg3_reading_addtag(reading, tag);
				string taglist = string(utils::StringUtils::StringUtils::utf8FromUcs4((a->getValue("tags"))));
				string buf = "";
				for(string::iterator it2 = taglist.begin(); it2 != taglist.end(); it2++) {
					// This assumes + separated tags.
					if(*it2 == '+' && buf.length() > 0) {
						tag = cg3_tag_create_u8(applicator, buf.c_str());
						cg3_reading_addtag(reading, tag);
						buf = "";
						continue;
					} else if(*it2 == '+' && buf.length() == 0) {
						continue;
					}
					buf = buf + *it2;
				}
				num_analyses++;
				cg3_cohort_addreading(cohort, reading);
				++it;
			}
			cg3_sentence_addcohort(sentence, cohort);
			//fprintf(stderr, "CgRuleEngine::check %d analyses\n", num_analyses);
			
		}
		int num_cohorts = cg3_sentence_numcohorts(sentence);
		fprintf(stderr, "CgRuleEngine::num_cohorts %d \n", num_cohorts);
		cg3_sentence_runrules(applicator, sentence);

		// We've run the grammar on the sentence, now we need to go through and look
		// for error tags which by convention begin with &, e.g.
		//
		//   "<beassát>"
		//	"beassi" G3 N Sg Acc PxSg2 @OBJ> &real-beassat #8->8 ADD:3178:beassat 

		cg3_tag *tag = 0;	
		cg3_cohort *cohort = 0;
		cg3_reading *reading = 0;
		size_t ci = 0, ce = 0, ri = 0, re = 0, ti = 0, te = 0;
		const char *tmp;

		for (ci = 0, ce = cg3_sentence_numcohorts(sentence) ; ci != ce ; ++ci) {
			cohort = cg3_sentence_getcohort(sentence, ci);
			tag = cg3_cohort_getwordform(cohort);
			tmp = cg3_tag_gettext_u8(tag);
			fprintf(stderr, "CG: %s\n", tmp);
	
			for (ri = 0, re = cg3_cohort_numreadings(cohort) ; ri != re ; ++ri) {
				reading = cg3_cohort_getreading(cohort, ri);
				fprintf(stderr, "CG: \t");
				for (ti = 0, te = cg3_reading_numtags(reading) ; ti != te ; ++ti) {
					tag = cg3_reading_gettag(reading, ti);
					tmp = cg3_tag_gettext_u8(tag);
					fprintf(stderr, "%s ", tmp);
					if(tmp[0] == '&') { 
						// We've found an error tag, mark the current cohort
						fprintf(stderr, "\nnew CacheEntry (%ld, %d, %d)\n", ci, GCERR_NEGATIVE_VERB_MISMATCH, 1);
						CacheEntry * e = new CacheEntry(0);
						e->error.error_code = GCERR_NEGATIVE_VERB_MISMATCH;
						e->error.startpos = ci;
						e->error.errorlen = 1;
						gc_cache_append_error(options->grammarChecker->gc_cache, e);
					}
				}
				for (ti = 0, te = cg3_reading_numtraces(reading) ; ti != te ; ++ti) {
					uint32_t rule_line = cg3_reading_gettrace(reading, ti);
					fprintf(stderr, "TRACE:%u ", rule_line);
				}
				fprintf(stderr, "\n");
			}
		}



	}
	
	return;
}


} }
