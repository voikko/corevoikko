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

#ifndef VOIKKO_GRAMMAR_ERROR_H
#define VOIKKO_GRAMMAR_ERROR_H

#include "voikko_structs.h"

#define GCERR_INVALID_SPELLING 1
#define GCERR_EXTRA_WHITESPACE 2
#define GCERR_SPACE_BEFORE_PUNCTUATION 3
#define GCERR_EXTRA_COMMA 4
#define GCERR_INVALID_SENTENCE_STARTER 5
#define GCERR_WRITE_FIRST_LOWERCASE 6
#define GCERR_WRITE_FIRST_UPPERCASE 7
#define GCERR_REPEATING_WORD 8
#define GCERR_TERMINATING_PUNCTUATION_MISSING 9
#define GCERR_INVALID_PUNCTUATION_AT_END_OF_QUOTATION 10
#define GCERR_FOREIGN_QUOTATION_MARK 11
#define GCERR_MISPLACED_CLOSING_PARENTHESIS 12
#define GCERR_NEGATIVE_VERB_MISMATCH 13
#define GCERR_A_INFINITIVE_REQUIRED 14
#define GCERR_MA_INFINITIVE_REQUIRED 15
#define GCERR_MISPLACED_SIDESANA 16
#define GCERR_MISSING_MAIN_VERB 17

namespace libvoikko {

/**
 * Initialize given grammar error to "no error"
 */
void init_grammar_error(voikko_grammar_error * error);

}

#endif
