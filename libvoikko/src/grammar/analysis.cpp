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
#include "utils/utils.hpp"
#include "setup/setup.hpp"
#include <malaga.h>
#include <cstdlib>
#include <cstring>

namespace libvoikko {

/** Free the memory allocated for sentence analysis */
void free_gc_sentence(gc_sentence * sentence) {
	if (!sentence) return;
	for (size_t i = 0; i < sentence->token_count; i++) {
		delete[] sentence->tokens[i].str;
	}
	delete sentence;
}

void free_gc_paragraph(gc_paragraph * para) {
	if (!para) return;
	if (para->sentences) {
		for (int i = 0; i < para->sentence_count; i++) {
			free_gc_sentence(para->sentences[i]);
		}
		delete[] para->sentences;
	}
	delete para;
}

/** Analyze given text token. Token type, length and text must have already
 *  been set. */
void gc_analyze_token(int handle, gc_token * token) {
	token->is_valid_word = 0;
	token->first_letter_lcase = 0;
	token->possible_sentence_start = 0;
	if (token->type != TOKEN_WORD) return;
	
	char * malaga_buffer = voikko_ucs4tocstr(token->str, "UTF-8", token->tokenlen);
	if (malaga_buffer == 0) return;
	analyse_item(malaga_buffer, MORPHOLOGY);
	delete[] malaga_buffer;
	
	// Check if first letter should be lower case letter
	value_t analysis = first_analysis_result();
	while (analysis) {
		token->is_valid_word = 1;
		char * analysis_str = get_value_string(analysis);
		if (strlen(analysis_str) < 2 || (analysis_str[1] != 'p' &&
		    analysis_str[1] != 'q')) {
			free(analysis_str);
			return;
		}
		free(analysis_str);
		analysis = next_analysis_result();
	}
	token->first_letter_lcase = 1;
}

/** Analyze sentence text. Sentence type must be set by the caller. */
gc_sentence * gc_analyze_sentence(int handle, const wchar_t * text,
                                   size_t textlen, size_t sentencepos) {
	gc_sentence * s = new gc_sentence;
	if (!s) return 0;
	s->token_count = 0;
	s->pos = sentencepos;
	size_t tokenlen;
	const wchar_t * pos = text;
	size_t remaining = textlen;
	int next_word_is_possible_sentence_start = 0;
	for (int i = 0; i < GCANALYSIS_MAX_TOKENS; i++) {
		enum voikko_token_type tt;
		int ignore_dot_saved = voikko_options.ignore_dot;
		voikko_options.ignore_dot = 0;
		tt = voikko_next_token_ucs4(handle, pos, remaining, &tokenlen);
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
		gc_analyze_token(handle, s->tokens + i);
		
		if (next_word_is_possible_sentence_start && tt == TOKEN_WORD) {
			s->tokens[i].possible_sentence_start = 1;
			next_word_is_possible_sentence_start = 0;
		}
		else if (tt == TOKEN_PUNCTUATION &&
		         ((tokenlen == 1 && (tstr[0] == L'.' || tstr[0] == L':'))
		          || tokenlen == 3)) { // . : ... may separate sentences
			next_word_is_possible_sentence_start = 1;
		}
		
		s->token_count++;
		pos += tokenlen;
		remaining -= tokenlen;
		if (!remaining) return s;
	}
	// Too long sentence or error
	free_gc_sentence(s);
	return 0;
}


gc_paragraph * gc_analyze_paragraph(int handle, const wchar_t * text, size_t textlen) {
	gc_paragraph * p = new gc_paragraph;
	if (!p) return 0;
	p->sentences = new gc_sentence*[GCANALYSIS_MAX_SENTENCES];
	if (!p->sentences) {
		delete p;
		return 0;
	}
	p->sentence_count = 0;
	size_t sentencelen;
	const wchar_t * pos = text;
	size_t remaining = textlen;
	enum voikko_sentence_type st;
	do {
		const wchar_t * pos2 = pos;
		size_t sentencelen2;
		sentencelen = 0;
		do {
			st = voikko_next_sentence_start_ucs4(handle, pos2, remaining,
			                                     &sentencelen2);
			pos2 += sentencelen2;
			sentencelen += sentencelen2;
			remaining -= sentencelen2;
		} while (st == SENTENCE_POSSIBLE);
		
		gc_sentence * s = gc_analyze_sentence(handle, pos, sentencelen, pos - text);
		if (!s) {
			free_gc_paragraph(p);
			return 0;
		}
		s->type = st;
		p->sentences[p->sentence_count++] = s;
		pos += sentencelen;
	} while (st != SENTENCE_NONE && st != SENTENCE_NO_START &&
	         p->sentence_count < GCANALYSIS_MAX_SENTENCES);
	return p;
}

}
