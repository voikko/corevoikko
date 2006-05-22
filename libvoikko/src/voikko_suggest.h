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

#ifndef VOIKKO_SUGGEST_H
#define VOIKKO_SUGGEST_H

#include <stddef.h>

/** Suggests corrections to character case
 * @param handle handle
 * @param suggestions pointer to a pointer to the first free slot in a suggestion array.
 *        The pointer to the first free slot will be incremented if suggestions are added.
 * @param max_suggestions number of empty suggestion slots left. This will be changed to match
 *        the correct value after suggestions are added.
 * @param word string to find suggestions for
 * @param wlen length of word
 * @param cost total computational cost of the suggestion algorithm. This will be incremented as needed.
 */
void voikko_suggest_correct_case(int handle, wchar_t *** suggestions, int * max_suggestions,
                                 const wchar_t * word, size_t wlen, int * cost);

#endif
