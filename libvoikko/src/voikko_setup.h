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
	const char * encoding;
	} voikko_options_t;

voikko_options_t voikko_options;

int voikko_set_bool_option(int option, int value);

int voikko_set_string_option(int option, const char * value);


#endif
