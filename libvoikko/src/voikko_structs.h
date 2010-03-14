/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2010 Harri Pitkänen <hatapitk@iki.fi>
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

#ifndef VOIKKO_VOIKKO_STRUCTS_H
#define VOIKKO_VOIKKO_STRUCTS_H

/* This header file contains public structures of the library. Using structures
 * this way is deprecated. */

#include "voikko_defines.h"
#include <stddef.h>

BEGIN_C_DECLS

/**
 * Grammar error description.
 */
typedef struct {
	/** Error code. 0 = no error was found */
	int error_code;
	/** Unused (indicates the probability of a false positive) */
	int error_level;
	/** Unused (detailed error description) */
	char * error_description;
	/** Start position of the error in the text */
	size_t startpos;
	/** Length of the error in the text */
	size_t errorlen;
	/** Possible corrections. These should be freed after
	 *  use by calling voikko_free_suggest_cstr */
	char ** suggestions;
} voikko_grammar_error;

END_C_DECLS

#endif
