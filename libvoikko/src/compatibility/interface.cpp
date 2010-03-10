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

/**
 * Compatibility implementations for deprecated public API.
 */

#include "voikko_defs.h"
#include "setup/setup.hpp"
#include <cstring>

typedef libvoikko::voikko_options_t VoikkoHandle;

/* FIXME: fix the voikko_deprecated.h mess so that we can directly include the public headers */
extern "C" {
VoikkoHandle * voikkoInit(const char ** error, const char * langcode,
                          int cache_size, const char * path);
}

VOIKKOEXPORT const char * voikko_init_with_path(int * handle, const char * langcode,
                                   int cache_size, const char * path) {
	if (libvoikko::voikko_handle_count++ > 0) {
		return "Maximum handle count exceeded";
	}
	const char * error;
	VoikkoHandle * theHandle = voikkoInit(&error, langcode, cache_size, path);
	if (theHandle) {
		*handle = 1;
		libvoikko::voikko_options = *theHandle;
		delete theHandle;
		return 0;
	} else {
		libvoikko::voikko_handle_count--;
		*handle = 0;
		return error;
	}
}

VOIKKOEXPORT const char * voikko_init(int * handle, const char * langcode, int cache_size) {
	return voikko_init_with_path(handle, langcode, cache_size, 0);
}

VOIKKOEXPORT int voikko_set_string_option(int /*handle*/, int option, const char * value) {
	// If deprecated VOIKKO_OPT_ENCODING is used and value is "UTF-8" return success.
	// Otherwise return failure.
	if (!value || option != 2) {
		return 0;
	}
	if (strcmp(value, "UTF-8") == 0) {
		return 1;
	}
	return 0;
}
