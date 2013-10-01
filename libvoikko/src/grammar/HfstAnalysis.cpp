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
 * Portions created by the Initial Developer are Copyright (C) 2008 - 2011
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

#include "grammar/HfstAnalysis.hpp"
#include "tokenizer/Tokenizer.hpp"
#include "sentence/Sentence.hpp"
#include "utils/StringUtils.hpp"
#include "utils/utils.hpp"
#include <cstdlib>
#include <cstring>
#include <stdio.h>

//#include <hfst/HfstTransducer.h>
//#include <hfst-ol.h>

#include <iostream>  
#include <fstream>
#include <set>
#include <vector>

using namespace libvoikko::grammar;
using namespace std;
using namespace hfst_ol;
//using namespace hfst;

namespace libvoikko {

HfstAnalysis::HfstAnalysis()  {

	
}

HfstAnalysis::HfstAnalysis(morphology::Analyzer * a)  {

	analyser = a;
	
}


HfstAnalysis::~HfstAnalysis() {

}


/** Analyse given text token. Token type, length and text must have already
 *  been set. */
void HfstAnalysis::analyse_token(voikko_options_t * voikkoOptions, Token * token) {
	token->isValidWord = false;
	
	wchar_t * wordBuffer =
	    utils::StringUtils::stripSpecialCharsForMalaga(token->str,
	                                                   token->tokenlen);
	const std::string swordBuffer(utils::StringUtils::utf8FromUcs4(wordBuffer));
	fprintf(stderr, "HfstAnalysis::analyse_token (%ls)\n", wordBuffer);

	token->analyses = analyser->analyze(wordBuffer);

	//int num_analyses = 0;
	//num_analyses = token->analyses->size();	
	//fprintf(stderr, "HfstAnalysis::analyse_token (num: %d)\n", num_analyses);
}

/** Analyse sentence text. Sentence type must be set by the caller. */
Sentence * HfstAnalysis::analyse_sentence(voikko_options_t * voikkoOptions,
	          const wchar_t * text, size_t textlen, size_t sentencepos) {
	Sentence * s = new Sentence;
	s->pos = sentencepos;
	size_t tokenlen;
	const wchar_t * pos = text;
	size_t remaining = textlen;
	bool next_word_is_possible_sentence_start = false;
	for (int i = 0; i < Sentence::MAX_TOKENS_IN_SENTENCE; i++) {
		enum voikko_token_type tt;
		int ignore_dot_saved = voikkoOptions->ignore_dot;
		voikkoOptions->ignore_dot = 0;
		tt = tokenizer::Tokenizer::nextToken(voikkoOptions, pos, remaining, &tokenlen);
		voikkoOptions->ignore_dot = ignore_dot_saved;
		if (tt == TOKEN_NONE) return s;

		s->tokens[i].type = tt;
		s->tokens[i].tokenlen = tokenlen;
		wchar_t * tstr = new wchar_t[tokenlen + 1];
		if (!tstr) break;
		memcpy(tstr, pos, tokenlen * sizeof(wchar_t));
		tstr[tokenlen] = L'\0';
		s->tokens[i].str = tstr;
		s->tokens[i].pos = sentencepos + (pos - text);
		analyse_token(voikkoOptions, s->tokens + i);
		
		if (next_word_is_possible_sentence_start && tt == TOKEN_WORD) {
			s->tokens[i].possibleSentenceStart = true;
			next_word_is_possible_sentence_start = false;
		}
		else if (tt == TOKEN_PUNCTUATION &&
		         ((tokenlen == 1 && wcschr(L".:\u2026\u2013\u2014", tstr[0]) != 0)
		          || tokenlen == 3)) { // . : ... may separate sentences
			next_word_is_possible_sentence_start = true;
		}
		
		s->tokenCount++;
		pos += tokenlen;
		remaining -= tokenlen;
		if (!remaining) return s;
	}
	// Too long sentence or error
	delete s;
	return 0;
}


Paragraph * HfstAnalysis::analyse_paragraph(voikko_options_t * voikkoOptions, const wchar_t * text, size_t textlen) {
	Paragraph * p = new Paragraph;
	const wchar_t * pos = text;
	size_t remaining = textlen;
	enum voikko_sentence_type st;
	do {
		const wchar_t * pos2 = pos;
		size_t sentencelen = 0;
		do {
			size_t sentencelen2;
			st = sentence::Sentence::next(voikkoOptions, pos2, remaining,
			                                     &sentencelen2);
			pos2 += sentencelen2;
			sentencelen += sentencelen2;
			remaining -= sentencelen2;
		} while (st == SENTENCE_POSSIBLE);
		
		Sentence * s = analyse_sentence(voikkoOptions, pos, sentencelen,
		                                   pos - text);
		if (!s) {
			delete p;
			return 0;
		}
		s->type = st;
		p->sentences[p->sentenceCount++] = s;
		pos += sentencelen;
	} while (st != SENTENCE_NONE && st != SENTENCE_NO_START &&
	         p->sentenceCount < Paragraph::MAX_SENTENCES_IN_PARAGRAPH);
	return p;
}

}
