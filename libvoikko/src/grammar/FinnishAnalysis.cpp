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

#include "grammar/FinnishAnalysis.hpp"
#include "tokenizer/Tokenizer.hpp"
#include "sentence/Sentence.hpp"
#include "utils/StringUtils.hpp"
#include "utils/utils.hpp"
#include <cstdlib>
#include <cstring>

using namespace libvoikko::grammar;
using namespace std;

namespace libvoikko {

FinnishAnalysis::FinnishAnalysis()  {

	
}

FinnishAnalysis::~FinnishAnalysis() {

}


/** Analyse given text token. Token type, length and text must have already
 *  been set. */
void FinnishAnalysis::analyse_token(voikko_options_t * voikkoOptions, Token * token) {
	token->isValidWord = false;
	token->possibleSentenceStart = false;
	token->isGeographicalNameInGenitive = false;
	token->possibleGeographicalName = false;
	token->possibleMainVerb = false;
	token->possibleConjunction = false;
	token->isMainVerb = true;
	token->isVerbNegative = true;
	token->isPositiveVerb = true;
	token->isConjunction = true;
	token->requireFollowingVerb = FOLLOWING_VERB_NONE;
	token->verbFollowerType = FOLLOWING_VERB_NONE;
	if (token->type != TOKEN_WORD) {
		token->firstLetterLcase = false;
		token->isConjunction = false;
		token->isVerbNegative = false;
		return;
	}
	
	wchar_t * wordBuffer =
	    utils::StringUtils::stripSpecialCharsForMalaga(token->str,
	                                                   token->tokenlen);
	morphology::Analyzer * analyzer = voikkoOptions->morAnalyzer;
	list<morphology::Analysis *> * analyses = analyzer->analyze(wordBuffer);
	token->analyses = analyses;
	delete[] wordBuffer;
	
	list<morphology::Analysis *>::const_iterator it = analyses->begin();
	token->firstLetterLcase = true;
	bool verbFollowerTypeSet = false;
	while (it != analyses->end()) {
		token->isValidWord = true;
		const wchar_t * structure = (*it)->getValue("STRUCTURE");
		const wchar_t * wclass = (*it)->getValue("CLASS");
		const wchar_t * mood = (*it)->getValue("MOOD");
		const wchar_t * person = (*it)->getValue("PERSON");
		const wchar_t * negative = (*it)->getValue("NEGATIVE");
		const wchar_t * possibleGeographicalName = (*it)->getValue("POSSIBLE_GEOGRAPHICAL_NAME");
		const wchar_t * requireFollowingVerb = (*it)->getValue("REQUIRE_FOLLOWING_VERB");
		if (wcslen(structure) < 2 || (structure[1] != L'p' &&
		    structure[1] != L'q')) {
			// Word may start with a capital letter anywhere
			token->firstLetterLcase = false;
			const wchar_t * wcase = (*it)->getValue("SIJAMUOTO");
			if (wclass && wcscmp(L"paikannimi", wclass) == 0 &&
			    wcase && wcscmp(L"omanto", wcase) == 0) {
				token->isGeographicalNameInGenitive = true;
			}
		}
		
		if (wclass && wcscmp(L"sidesana", wclass) == 0) {
			token->possibleConjunction = true;
		}
		else {
			token->isConjunction = false;
		}
		
		if (!wclass) {
			token->isPositiveVerb = false;
			token->possibleMainVerb = true;
			token->isMainVerb = false;
			token->isVerbNegative = false;
		}
		else if (wcscmp(L"kieltosana", wclass) == 0) {
			token->isPositiveVerb = false;
			token->isMainVerb = false;
		}
		else if (wcscmp(L"teonsana", wclass) == 0) {
			if (!negative || wcscmp(L"false", negative) != 0 ||
			   ((!mood || wcscmp(L"conditional", mood) == 0) && (!person || wcscmp(L"3", person) == 0))) { // "en _lukisi_"
				token->isPositiveVerb = false;
			}
			if ((!mood || (wcscmp(L"A-infinitive", mood) != 0 && wcscmp(L"E-infinitive", mood) != 0)) &&
			    (!negative || wcscmp(L"true", negative) != 0)) {
				token->possibleMainVerb = true;
			}
			if (!mood || wcscmp(L"indicative", mood) != 0) {
				token->isMainVerb = false;
			}
			token->isVerbNegative = false;
		}
		else {
			token->isPositiveVerb = false;
			token->isMainVerb = false;
			token->isVerbNegative = false;
		}
		
		if (possibleGeographicalName && wcscmp(L"true", possibleGeographicalName) == 0) {
			token->possibleGeographicalName = true;
		}
		{
			FollowingVerbType requiredType = FOLLOWING_VERB_NONE;
			if (requireFollowingVerb) {
				if (wcscmp(L"A-infinitive", requireFollowingVerb) == 0) {
					requiredType = FOLLOWING_VERB_A_INFINITIVE;
				} else if (wcscmp(L"MA-infinitive", requireFollowingVerb) == 0) {
					requiredType = FOLLOWING_VERB_MA_INFINITIVE;
				}
			}
			if (requiredType == FOLLOWING_VERB_NONE ||
			    it == analyses->begin()) {
				token->requireFollowingVerb = requiredType;
			} else if (token->requireFollowingVerb != requiredType) {
				token->requireFollowingVerb = FOLLOWING_VERB_NONE;
			}
		}
		{
			FollowingVerbType followerType = FOLLOWING_VERB_NONE;
			if (mood) {
				if (wcscmp(L"A-infinitive", mood) == 0) {
					followerType = FOLLOWING_VERB_A_INFINITIVE;
				} else if (wcscmp(L"MA-infinitive", mood) == 0) {
					followerType = FOLLOWING_VERB_MA_INFINITIVE;
				}
			}
			if (followerType != FOLLOWING_VERB_NONE) {
				if (!verbFollowerTypeSet) {
					token->verbFollowerType = followerType;
					verbFollowerTypeSet = true;
				} else if (token->verbFollowerType != followerType) {
					token->verbFollowerType = FOLLOWING_VERB_NONE;
				}
			}
		}
		++it;
	}
	morphology::Analyzer::deleteAnalyses(analyses);
	if (!token->isValidWord) {
		token->isPositiveVerb = false;
		token->isConjunction = false;
		token->isMainVerb = false;
		token->isVerbNegative = false;
	}
}

/** Analyse sentence text. Sentence type must be set by the caller. */
Sentence * FinnishAnalysis::analyse_sentence(voikko_options_t * voikkoOptions,
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


Paragraph * FinnishAnalysis::analyse_paragraph(voikko_options_t * voikkoOptions, const wchar_t * text, size_t textlen) {
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
