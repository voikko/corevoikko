/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2006 Harri Pitkänen <hatapitk@iki.fi>
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

#ifndef VOIKKO_HYPHENATE_H
#define VOIKKO_HYPHENATE_H

#include <stddef.h>
#include <malaga.h>

/**
 * Performs rule-based hyphenation.
 * @param word word to hyphenate
 * @param hyphenation_points hyphenation buffer where the results will be stored
 * @param nchars number of characters in the word
 */
void voikko_simple_hyphenation(const wchar_t * word, char * hyphenation_points, size_t nchars);

/**
 * Sets the known hyphenation points (compound word borders) according to given Malaga analysis.
 * @param analysis Malaga analysis of the word
 * @param buffer hyphenation buffer to store the results to
 * @param len length of the buffer
 */
void voikko_interpret_analysis(value_t analysis, char * buffer, size_t len);

/**
 * Calculates the intersection of hyphenation points.
 * @param hyphenations array of hyphenation buffers
 * @return hyphenation buffer that contains the intersection of given hyphenations
 */
char * voikko_intersect_hyphenations(char ** hyphenations);

/**
 * Hyphenates a compound word.
 * @param word word to hyphenate
 * @param hyphenation buffer to write the results to. It is assumed that compound word borders
 *        have already been marked on the buffer.
 */
void voikko_compound_hyphenation(const wchar_t * word, char * hyphenation);

/**
 * Creates an array of hyphenation buffers for given word.
 * @param word word to analyse
 * @return array of hyphenation buffers that correspond to different ways how word could
 *         be split
 */
char ** voikko_split_compounds(const wchar_t * word);

/**
 * Removes hyphenation buffers that are considered unnecessary to analyse.
 * @param hyphenations list of hyphenation buffers. It is assumed that compound word borders
 *        have already been marked on the buffer.
 * @param len length of the word
 * @param intersect_compound_level value of option intersect_compound_level
 */
void voikko_remove_extra_hyphenations(char ** hyphenations, size_t len, int intersect_compound_level);

#endif
