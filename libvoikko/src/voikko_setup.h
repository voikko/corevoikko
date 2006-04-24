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

#ifndef VOIKKO_SETUP_H
#define VOIKKO_SETUP_H

typedef struct {
	int ignore_dot;
	int ignore_numbers;
	int ignore_uppercase;
	int no_ugly_hyphenation;
	int intersect_compound_level;
	const char * encoding;
	} voikko_options_t;

voikko_options_t voikko_options;

int voikko_handle_count;

/**
 * Finds the malaga project file for given language
 * @param buffer Buffer where path to the project file will be written
 * @param buflen Length of buffer
 * @param langcode Language code
 * @return true if finding project file succeeded, otherwise false. It is not guaranteed that
 *         the file actually exists.
 */
int voikko_find_malaga_project(char * buffer, size_t buflen, const char * langcode);

#endif
