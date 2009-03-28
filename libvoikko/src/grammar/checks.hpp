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

#ifndef VOIKKO_GRAMMAR_CHECKS_H
#define VOIKKO_GRAMMAR_CHECKS_H

#include "grammar/analysis.hpp"

namespace libvoikko {

/**
 * GC errors due to wrong context independent use of punctuation or whitespace
 * within a sentence.
 */
void gc_local_punctuation(int handle, const grammar::Sentence * sentence);

/**
 * GC errors due to incorrect character case
 */
void gc_character_case(int handle, const grammar::Sentence * sentence, bool isFirstInParagraph);

/**
 * GC errors due to word repetition
 */
void gc_repeating_words(int handle, const grammar::Sentence * sentence);

/**
 * GC errors due to missing punctuation at the end of paragraph
 */
void gc_end_punctuation(int handle, const grammar::Paragraph * paragraph);

}

#endif
