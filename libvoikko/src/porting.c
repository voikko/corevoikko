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
 
/* GNU extensions of C library that need to be separately implemented
 * on systems that do not have them. */

#include <config.h>
#ifndef HAVE_GETLINE

#include "porting.h"

/* Clone of getline() from the GNU C library. FIXME: this implementation is not complete
 * or correct. */
size_t getline(char ** lineptr, size_t * n, FILE * stream) {
	size_t chars_read = 0;
	char * charptr = *lineptr;
	int i;
	if (n <= 0) return 0;
	for (i = 0; i < *n - 1; i++) {
		if (fgets(charptr, 2, stream) == 0) {
			if (i == 0) return -1;
			else break;
		}
		if (*charptr == '\n') break;
		charptr++;
	}
	*charptr = '\0';
	return i;
}
#endif

/* Empty source file is forbidden, so put something here */
int voikko_unused;
