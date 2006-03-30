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


#ifndef VOIKKO_VOIKKO_H
#define VOIKKO_VOIKKO_H
#include <stddef.h>


#undef BEGIN_C_DECLS
#undef END_C_DECLS
#ifdef __cplusplus
# define BEGIN_C_DECLS extern "C" {
# define END_C_DECLS }
#else
# define BEGIN_C_DECLS /* empty */
# define END_C_DECLS /* empty */
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

BEGIN_C_DECLS

char * voikko_init();

int voikko_set_bool_option(int option, int value);

int voikko_set_string_option(int option, const char * value);

int voikko_spell_cstr(const char * word);

int voikko_spell_ucs4(const wchar_t * word);

char ** voikko_suggest_cstr(const char * word);

wchar_t ** voikko_suggest_ucs4(const wchar_t * word);

char * voikko_hyphenate_cstr(const char * word);

char * voikko_hyphenate_ucs4(const wchar_t * word);

END_C_DECLS

#endif
