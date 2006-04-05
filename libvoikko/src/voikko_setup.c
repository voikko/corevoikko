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

#include "voikko_defs.h"
#include "voikko_setup.h"
#include <malaga.h>

int voikko_set_bool_option(int option, int value) {
	switch (option) {
		case VOIKKO_OPT_IGNORE_DOT:
			if (value) voikko_options.ignore_dot = 1;
			else voikko_options.ignore_dot = 0;
			return 1;
		case VOIKKO_OPT_IGNORE_NUMBERS:
			if (value) voikko_options.ignore_numbers = 1;
			else voikko_options.ignore_numbers = 0;
			return 1;
		case VOIKKO_OPT_IGNORE_UPPERCASE:
			if (value) voikko_options.ignore_uppercase = 1;
			else voikko_options.ignore_uppercase = 0;
			return 1;
		case VOIKKO_OPT_NO_UGLY_HYPHENATION:
			if (value) voikko_options.no_ugly_hyphenation = 1;
			else voikko_options.no_ugly_hyphenation = 0;
			return 1;
	}
	return 0;
}

int voikko_set_string_option(int option, const char * value) {
	switch (option) {
		case VOIKKO_OPT_ENCODING:
			if (!value) return 0;
			voikko_options.encoding = value;
			return 1;
	}
	return 0;
}

char * voikko_init() {
	char * project;
	voikko_options.ignore_dot = 0;
	voikko_options.ignore_numbers = 0;
	voikko_options.ignore_uppercase = 0;
	voikko_options.no_ugly_hyphenation = 0;
	voikko_options.encoding = "UTF-8";
	project = DICTIONARY_PATH "/suomi.pro";
	init_libmalaga(project);
	return malaga_error;
}
