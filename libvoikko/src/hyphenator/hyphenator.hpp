/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2006 - 2009 Harri Pitkänen <hatapitk@iki.fi>
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

#ifndef VOIKKO_HYPHENATOR_HYPHENATOR_H
#define VOIKKO_HYPHENATOR_HYPHENATOR_H

#include <cstddef>
#include "morphology/Analysis.hpp"

namespace libvoikko {

/**
 * Checks if the proposed hyphenation point is valid.
 * @param word word to hyphenate
 * @param hyphenation_points hyphenation buffer containing the existing hyphenation points
 * @param new_hyphen_pos position of the proposed new hyphenation point
 * @param nchars number of characters in the word
 * @return true if the proposed hyphenation point is valid (will not result in
 * syllables without any vowels), false if it is invalid.
 */
bool is_good_hyphen_position(const wchar_t * word, const char * hyphenation_points,
                             size_t new_hyphen_pos, size_t nchars);

/**
 * Checks if given word can be safely hyphenated using standard hyphenation rules
 * @param word word to check
 * @param nchars number of characters in the word
 * @return true if the word should be hyphenated with rule based hyphenator, otherwise false.
 */
bool allow_rule_hyphenation(const wchar_t * word, size_t nchars);

/**
 * Performs rule-based hyphenation.
 * @param word word to hyphenate
 * @param hyphenation_points hyphenation buffer where the results will be stored
 * @param nchars number of characters in the word
 */
void rule_hyphenation(const wchar_t * word, char * hyphenation_points, size_t nchars);

/**
 * Sets the known hyphenation points (compound word borders) according to given
 * morphological analysis.
 * @param analysis morphological analysis of the word
 * @param buffer hyphenation buffer to store the results to
 * @param len length of the buffer
 */
void interpret_analysis(const morphology::Analysis * analysis,
                        char * buffer, size_t len);

/**
 * Calculates the intersection of hyphenation points.
 * @param hyphenations array of hyphenation buffers
 * @return hyphenation buffer that contains the intersection of given hyphenations
 */
char * intersect_hyphenations(char ** hyphenations);

/**
 * Hyphenates a compound word.
 * @param word word to hyphenate
 * @param hyphenation buffer to write the results to. It is assumed that
 * compound word borders have already been marked on the buffer.
 * @param len length of the word to hyphenate
 */
void compound_hyphenation(const wchar_t * word, char * hyphenation, size_t len);

/**
 * Creates an array of hyphenation buffers for given word.
 * @param word word to analyse
 * @param len length of the word
 * @param dot_removed pointer to an integer that will be set to 1 if trailing
 * dot is ignored. Otherwise it will be set to 0.
 * @return array of hyphenation buffers that correspond to different ways how
 * word could be split
 */
char ** split_compounds(const wchar_t * word, size_t len, int * dot_removed);

/**
 * Removes hyphenation buffers that are considered unnecessary to analyse.
 * @param hyphenations list of hyphenation buffers. It is assumed that compound
 * word borders have already been marked on the buffer.
 * @param len length of the word
 * @param intersect_compound_level value of option intersect_compound_level
 */
void remove_extra_hyphenations(char ** hyphenations, size_t len,
                               int intersect_compound_level);

}

#endif
