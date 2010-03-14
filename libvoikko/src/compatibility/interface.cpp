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

#include "porting.h"
#include "setup/setup.hpp"
#include "voikko.h"
#include <cstring>

namespace libvoikko { namespace compatibility {

VOIKKOEXPORT const char * voikko_init_with_path(int * handle, const char * langcode,
                                   int cache_size, const char * path) {
	if (voikko_handle_count++ > 0) {
		return "Maximum handle count exceeded";
	}
	const char * error;
	voikko_options_t * theHandle =
			reinterpret_cast<voikko_options_t *>(voikkoInit(&error, langcode, cache_size, path));
	if (theHandle) {
		*handle = 1;
		voikko_options = *theHandle;
		delete theHandle;
		return 0;
	} else {
		voikko_handle_count--;
		*handle = 0;
		return error;
	}
}

VOIKKOEXPORT const char * voikko_init(int * handle, const char * langcode, int cache_size) {
	return voikko_init_with_path(handle, langcode, cache_size, 0);
}

VOIKKOEXPORT int voikko_terminate(int handle) {
	if (handle == 1 && voikko_handle_count > 0) {
		voikko_handle_count--;
		voikkoTerminate(reinterpret_cast<VoikkoHandle *>(&voikko_options));
		return 1;
	} else {
		return 0;
	}
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

} }
