/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2008 Harri Pitkänen <hatapitk@iki.fi>
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
	token->firstLetterLcase = false;
	token->possibleSentenceStart = false;
	if (token->type != TOKEN_WORD) return;
	
	wchar_t * wordBuffer =
	    utils::StringUtils::stripSpecialCharsForMalaga(token->str,
	                                                   token->tokenlen);
	morphology::Analyzer * analyzer = voikkoOptions->morAnalyzer;
	list<morphology::Analysis *> * analyses = analyzer->analyze(wordBuffer);
	delete[] wordBuffer;
	
	// Check if first letter should be lower case letter
	list<morphology::Analysis *>::const_iterator it = analyses->begin();
	while (it != analyses->end()) {
		token->isValidWord = true;
		const wchar_t * structure = (*it)->getValue("STRUCTURE");
		if (wcslen(structure) < 2 || (structure[1] != L'p' &&
		    structure[1] != L'q')) {
			morphology::Analyzer::deleteAnalyses(analyses);
			return;
		}
		it++;
	}
	morphology::Analyzer::deleteAnalyses(analyses);
	token->firstLetterLcase = true;
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
		int ignore_dot_saved = voikko_options.ignore_dot;
		voikko_options.ignore_dot = 0;
		tt = tokenizer::Tokenizer::nextToken(voikkoOptions, pos, remaining, &tokenlen);
		voikko_options.ignore_dot = ignore_dot_saved;
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
			st = voikko_next_sentence_start_ucs4(1, pos2, remaining,
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
