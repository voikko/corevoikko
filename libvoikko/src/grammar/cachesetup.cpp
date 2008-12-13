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

#include "grammar/cachesetup.hpp"
#include "setup/setup.hpp"
#include <cstring>
#include <cstdlib>

namespace libvoikko {

void init_gc_cache(voikko_gc_cache * gc_cache) {
	memset(gc_cache, 0, sizeof(voikko_gc_cache));
}

void gc_clear_cache(int handle) {
	if (voikko_options.gc_cache.paragraph)
		free(voikko_options.gc_cache.paragraph);
	voikko_gc_cache_entry * entry = voikko_options.gc_cache.first_error;
	while (entry) {
		voikko_gc_cache_entry * next = entry->next_error;
		voikko_free_suggest_cstr(entry->error.suggestions);
		free(entry);
		entry = next;
	}
	init_gc_cache(&voikko_options.gc_cache);
}

}
