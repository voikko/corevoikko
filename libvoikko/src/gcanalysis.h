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

#ifndef VOIKKO_GCANALYSIS_H
#define VOIKKO_GCANALYSIS_H

#include "voikko_defs.h"

/* Maximum number of sentences in a paragraph */
#define GCANALYSIS_MAX_SENTENCES 200

/* Maximum number of tokens in a sentence */
#define GCANALYSIS_MAX_TOKENS 500

/**
 * Grammar checker sentence token.
 */
typedef struct {
	/** Type of this token */
	enum voikko_token_type type;
	/** True if this word token was recognized as a valid word */
	int is_valid_word;
	/** True if this is a word token that should start with
	 *  lower case letter. */
	int first_letter_lcase;
	/** Null terminated string containing the token text */
	wchar_t * str;
	/** Length of the token */
	size_t tokenlen;
	/** Position of this token within paragraph */
	size_t pos;
} gc_token;

/**
 * Analyzed sentence for grammar checker.
 */
typedef struct {
	/** Type of this sentence (start type of next sentence) */
	enum voikko_sentence_type type;
	/** Array of gc tokens */
	gc_token tokens[GCANALYSIS_MAX_TOKENS];
	/** Number of tokens in the sentence */
	int token_count;
	/** Position of this sentence within paragraph */
	size_t pos;
} gc_sentence;

/**
 * Analyzed paragraph for grammar checker.
 */
typedef struct {
	/** Pointers to analyzed sentences */
	gc_sentence ** sentences;
	/** Number of sentences in the paragraph */
	int sentence_count;
} gc_paragraph;

/** Analyze paragraph text */
gc_paragraph * gc_analyze_paragraph(int handle, const wchar_t * text, size_t textlen);

/** Free the memory allocated for paragraph analysis */
void free_gc_paragraph(gc_paragraph * para);

#endif
