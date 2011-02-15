/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2006 - 2010 Harri Pitkänen <hatapitk@iki.fi>
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

#ifndef VOIKKO_VOIKKO_ENUMS_H
#define VOIKKO_VOIKKO_ENUMS_H

/**
 * Token types for string tokenization
 * TOKEN_NONE:        End of text or error
 * TOKEN_WORD:        Word
 * TOKEN_PUNCTUATION: Punctuation
 * TOKEN_WHITESPACE:  Whitespace
 * TOKEN_UNKNOWN:     Character not used in any of the supported natural languages
 */
enum voikko_token_type {TOKEN_NONE, TOKEN_WORD, TOKEN_PUNCTUATION, TOKEN_WHITESPACE, TOKEN_UNKNOWN};

/**
 * Sentence start types
 * SENTENCE_NONE: End of text reached or error.
 * SENTENCE_NO_START: This is not a start of a new sentence.
 * SENTENCE_PROBABLE: This is a probable start of a new sentence.
 * SENTENCE_POSSIBLE: This may be a start of a new sentence.
 */
enum voikko_sentence_type {SENTENCE_NONE, SENTENCE_NO_START, SENTENCE_PROBABLE, SENTENCE_POSSIBLE};

#endif // VOIKKO_ENUMS_H
