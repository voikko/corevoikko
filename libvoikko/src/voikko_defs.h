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

/* Definitions for private use in libvoikko */

#ifndef VOIKKO_DEFS_H
#define VOIKKO_DEFS_H
#include "voikko.h"
#include <config.h>
#include <stddef.h>
#include <assert.h>
#include "porting.h"

/* Shared library support */
#ifdef WIN32
  #define VOIKKOEXPORT __declspec(dllexport)
#else
  #ifdef GCC_VISIBILITY
    #define VOIKKOEXPORT __attribute__ ((visibility("default")))
  #else
    #define VOIKKOEXPORT
  #endif
#endif


VOIKKOEXPORT const char * voikko_init(int * handle, const char * langcode, int cache_size);

VOIKKOEXPORT const char * voikko_init_with_path(int * handle, const char * langcode,
                                                int cache_size, const char * path);

VOIKKOEXPORT int voikko_terminate(int handle);

VOIKKOEXPORT int voikko_set_bool_option(int handle, int option, int value);

VOIKKOEXPORT int voikko_set_int_option(int handle, int option, int value);

VOIKKOEXPORT int voikko_set_string_option(int handle, int option, const char * value);

VOIKKOEXPORT int voikko_spell_cstr(int handle, const char * word);

VOIKKOEXPORT int voikko_spell_ucs4(int handle, const wchar_t * word);

VOIKKOEXPORT char ** voikko_suggest_cstr(int handle, const char * word);

VOIKKOEXPORT wchar_t ** voikko_suggest_ucs4(int handle, const wchar_t * word);

VOIKKOEXPORT char * voikko_hyphenate_cstr(int handle, const char * word);

VOIKKOEXPORT char * voikko_hyphenate_ucs4(int handle, const wchar_t * word);

VOIKKOEXPORT void voikko_free_suggest_ucs4(wchar_t ** suggest_result);

VOIKKOEXPORT void voikko_free_suggest_cstr(char ** suggest_result);

VOIKKOEXPORT void voikko_free_hyphenate(char * hyphenate_result);

#endif
