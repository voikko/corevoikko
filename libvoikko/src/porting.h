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
 
/* Prototypes for GNU extensions of C library that need to be separately implemented
 * on systems that do not have them. */

#ifndef VOIKKO_PORTING_H
#define VOIKKO_PORTING_H

#include <config.h>

#if defined(WIN32) || defined(_WIN32) || defined(__WIN32__)
# define WIN32
# define INTERNAL_CHARSET "UCS-2-INTERNAL"
# include <locale.h>
# define ENTER_V char*enter_v_origl=setlocale(LC_CTYPE,0);setlocale(LC_CTYPE,"fin");
# define EXIT_V setlocale(LC_CTYPE,enter_v_origl);
#else
# define ENTER_V
# define EXIT_V
# if defined(__FreeBSD__) || defined(__APPLE__)
#  define INTERNAL_CHARSET "UCS-4-INTERNAL"
# else
#  define INTERNAL_CHARSET "WCHAR_T"
# endif
#endif

#endif
