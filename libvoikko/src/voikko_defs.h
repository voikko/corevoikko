/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2006 - 2008 Harri Pitkänen <hatapitk@iki.fi>
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

/* Definitions for private use in libvoikko */

#ifndef VOIKKO_DEFS_H
#define VOIKKO_DEFS_H
#include "voikko.h"
#include <config.h>
#include <cstddef>
#include <assert.h>
#include "porting.h"

#define VOIKKOLINKAGE extern "C"

/* Shared library support */
#ifdef WIN32
  #define VOIKKOEXPORT VOIKKOLINKAGE __declspec(dllexport)
#else
  #ifdef GCC_VISIBILITY
    #define VOIKKOEXPORT VOIKKOLINKAGE __attribute__ ((visibility("default")))
  #else
    #define VOIKKOEXPORT VOIKKOLINKAGE
  #endif
#endif

#endif
