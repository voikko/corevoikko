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

#ifndef HAVE_NL_LANGINFO
#ifdef WIN32
#include <windows.h>
#endif // WIN32
#include "porting.h"

char * nl_langinfo(nl_item codeset) {
	char * retval = NULL;
#ifdef WIN32
	CPINFOEX codePageInfo;
	memset(&codePageInfo, 0x00, sizeof(codePageInfo));
	if (GetCPInfoEx (CP_OEMCP, 0, &codePageInfo)) {
		static char buf[2 + 10 + 1]; // "CPXXXXX\0"
		sprintf(buf, "CP%u", codePageInfo.CodePage);
		retval = buf;
	}
#endif // WIN32
	return retval;
}
#endif

/* Empty source file is forbidden, so put something here */
int voikko_unused;
