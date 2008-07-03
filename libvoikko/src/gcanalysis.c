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

#include "gcanalysis.h"
#include <stdlib.h>
#include <string.h>

/** Free the memory allocated for sentence analysis */
void free_gc_sentence(gc_sentence * sentence) {
	if (!sentence) return;
	for (int i = 0; i < sentence->token_count; i++) {
		free(sentence->tokens[i].str);
	}
	free(sentence);
}

void free_gc_paragraph(gc_paragraph * para) {
	if (!para) return;
	if (para->sentences) {
		for (int i = 0; i < para->sentence_count; i++) {
			free_gc_sentence(para->sentences[i]);
		}
		free(para->sentences);
	}
	free(para);
}

/** Analyze sentence text. Sentence type must be set by the caller. */
gc_sentence * gc_analyze_sentence(int handle, const wchar_t * text,
                                   size_t textlen, size_t sentencepos) {
	gc_sentence * s = malloc(sizeof(gc_sentence));
	if (!s) return 0;
	s->token_count = 0;
	s->pos = sentencepos;
	size_t tokenlen;
	const wchar_t * pos = text;
	size_t remaining = textlen;
	for (int i = 0; i < GCANALYSIS_MAX_TOKENS; i++) {
		enum voikko_token_type tt;
		tt = voikko_next_token_ucs4(handle, pos, remaining, &tokenlen);
		if (tt == TOKEN_NONE) return s;
		s->tokens[i].type = tt;
		s->tokens[i].tokenlen = tokenlen;
		wchar_t * tstr = malloc((tokenlen + 1) * sizeof(wchar_t));
		if (!tstr) break;
		memcpy(tstr, pos, tokenlen * sizeof(wchar_t));
		tstr[tokenlen] = L'\0';
		s->tokens[i].str = tstr;
		s->tokens[i].pos = sentencepos + (pos - text);
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
	gc_paragraph * p = malloc(sizeof(gc_paragraph));
	if (!p) return 0;
	p->sentences = malloc(GCANALYSIS_MAX_SENTENCES * sizeof(gc_sentence *));
	if (!p->sentences) {
		free(p);
		return 0;
	}
	p->sentence_count = 0;
	size_t sentencelen;
	const wchar_t * pos = text;
	size_t remaining = textlen;
	enum voikko_sentence_type st;
	do {
		st = voikko_next_sentence_start_ucs4(handle, pos, remaining,
		                                     &sentencelen);
		gc_sentence * s = gc_analyze_sentence(handle, pos, sentencelen, pos - text);
		if (!s) {
			free_gc_paragraph(p);
			return 0;
		}
		s->type = st;
		p->sentences[p->sentence_count++] = s;
		pos += sentencelen;
		remaining -= sentencelen;
		
	} while (st != SENTENCE_NONE && st != SENTENCE_NO_START &&
	         p->sentence_count < GCANALYSIS_MAX_SENTENCES);
	return p;
}
