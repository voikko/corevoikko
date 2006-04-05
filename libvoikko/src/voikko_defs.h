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
#include <config.h>
#include <stddef.h>

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


#define LIBVOIKKO_MAX_WORD_CHARS 255
#define LIBVOIKKO_MAX_ANALYSIS_COUNT 31

#define VOIKKO_SPELL_FAILED 0
#define VOIKKO_SPELL_OK 1
#define VOIKKO_INTERNAL_ERROR 2
#define VOIKKO_CHARSET_CONVERSION_FAILED 3

#define VOIKKO_OPT_IGNORE_DOT 0
#define VOIKKO_OPT_IGNORE_NUMBERS 1
#define VOIKKO_OPT_ENCODING 2
#define VOIKKO_OPT_IGNORE_UPPERCASE 3
#define VOIKKO_OPT_NO_UGLY_HYPHENATION 4


VOIKKOEXPORT char * voikko_init();

VOIKKOEXPORT int voikko_set_bool_option(int option, int value);

VOIKKOEXPORT int voikko_set_string_option(int option, const char * value);

VOIKKOEXPORT int voikko_spell_cstr(const char * word);

VOIKKOEXPORT int voikko_spell_ucs4(const wchar_t * word);

VOIKKOEXPORT char ** voikko_suggest_cstr(const char * word);

VOIKKOEXPORT wchar_t ** voikko_suggest_ucs4(const wchar_t * word);

VOIKKOEXPORT char * voikko_hyphenate_cstr(const char * word);

VOIKKOEXPORT char * voikko_hyphenate_ucs4(const wchar_t * word);


#endif