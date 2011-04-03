/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2008 - 2010 Harri Pitkänen <hatapitk@iki.fi>
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

#include "grammar/analysis.hpp"
#include "tokenizer/Tokenizer.hpp"
#include "sentence/Sentence.hpp"
#include "utils/StringUtils.hpp"
#include "utils/utils.hpp"
#include <cstdlib>
#include <cstring>

using namespace libvoikko::grammar;
using namespace std;

namespace libvoikko {

/** Analyze given text token. Token type, length and text must have already
 *  been set. */
static void gc_analyze_token(voikko_options_t * voikkoOptions, Token * token) {
	token->isValidWord = false;
	token->possibleSentenceStart = false;
	token->isGeographicalNameInGenitive = false;
	token->possibleGeographicalName = false;
	token->isVerbNegative = false;
	token->isPositiveVerb = true;
	token->requireFollowingVerb = FOLLOWING_VERB_NONE;
	token->verbFollowerType = FOLLOWING_VERB_NONE;
	if (token->type != TOKEN_WORD) {
		token->firstLetterLcase = false;
		return;
	}
	
	wchar_t * wordBuffer =
	    utils::StringUtils::stripSpecialCharsForMalaga(token->str,
	                                                   token->tokenlen);
	morphology::Analyzer * analyzer = voikkoOptions->morAnalyzer;
	list<morphology::Analysis *> * analyses = analyzer->analyze(wordBuffer);
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
		if (wclass && wcscmp(L"kieltosana", wclass) == 0) {
			token->isVerbNegative = true;
			token->isPositiveVerb = false;
		} else if (!wclass || wcscmp(L"teonsana", wclass) != 0 ||
			   !negative || wcscmp(L"false", negative) != 0 ||
			   ((!mood || wcscmp(L"conditional", mood) == 0) && (!person || wcscmp(L"3", person) == 0))) { // "en _lukisi_"
			token->isPositiveVerb = false;
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
	}
}

/** Analyze sentence text. Sentence type must be set by the caller. */
static Sentence * gc_analyze_sentence(voikko_options_t * voikkoOptions,
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
		gc_analyze_token(voikkoOptions, s->tokens + i);
		
		if (next_word_is_possible_sentence_start && tt == TOKEN_WORD) {
			s->tokens[i].possibleSentenceStart = true;
			next_word_is_possible_sentence_start = false;
		}
		else if (tt == TOKEN_PUNCTUATION &&
		         ((tokenlen == 1 &&
		           (tstr[0] == L'.' || tstr[0] == L':' || tstr[0] == L'\u2026'))
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


Paragraph * gc_analyze_paragraph(voikko_options_t * voikkoOptions, const wchar_t * text, size_t textlen) {
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
		
		Sentence * s = gc_analyze_sentence(voikkoOptions, pos, sentencelen,
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
