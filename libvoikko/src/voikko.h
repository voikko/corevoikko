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

/* This library should be used in the following manner:
 * 
 * int handle;
 * char * voikko_error = voikko_init(&handle, "fi_FI");
 * // check for errors
 * // set options
 * // use spell/suggest/hyphenate
 * voikko_terminate(handle);
 *
 * This library is currently not MT safe (limitation in Malaga). If you need to use
 * it in a threaded program you should use single mutex to protect all calls to this
 * library. It may be necessary to set options and call spell/suggest/hyphenate in the
 * same critical section.
 *
 * Currently Finnish is the only supported language. Acquiring more than one simultaneous
 * handle is not possible in this version.
 *
 */

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


/* Fixed limits */
#define LIBVOIKKO_MAX_WORD_CHARS 255
#define LIBVOIKKO_MAX_ANALYSIS_COUNT 31

/* Spellchecker return codes */
#define VOIKKO_SPELL_FAILED 0
#define VOIKKO_SPELL_OK 1
#define VOIKKO_INTERNAL_ERROR 2
#define VOIKKO_CHARSET_CONVERSION_FAILED 3

/* Boolean options */
#define VOIKKO_OPT_IGNORE_DOT 0
#define VOIKKO_OPT_IGNORE_NUMBERS 1
#define VOIKKO_OPT_IGNORE_UPPERCASE 3
#define VOIKKO_OPT_NO_UGLY_HYPHENATION 4

/* Integer options */
#define VOIKKO_INTERSECT_COMPOUND_LEVEL 5

/* String options */
#define VOIKKO_OPT_ENCODING 2


#ifndef VOIKKO_DEFS_H
BEGIN_C_DECLS

/**
 * Initialises the library for use in the specified language.
 * @param handle after succesful initialisation handle will contain a handle that
 *        refers to this particular instance of voikko
 * @param langcode the language code, for example "fi_FI"
 * @return null, if initialisation completed without error, otherwise a pointer
 *         to a string describing the error
 */
const char * voikko_init(int * handle, const char * langcode);

/**
 * Terminates an instance of voikko.
 * @param handle voikko instance
 * @return true, if termination succeeded, otherwise false
 */
int voikko_terminate(int handle);

/**
 * Sets a boolean option.
 * @param handle voikko instance
 * @param option option name
 * @param value option value
 * @return true if option was succesfully set, otherwise false
 */
int voikko_set_bool_option(int handle, int option, int value);

/**
 * Sets an integer option.
 * @param handle voikko instance
 * @param option option name
 * @param value option value
 * @return true if option was succesfully set, otherwise false
 */
int voikko_set_int_option(int handle, int option, int value);

/**
 * Sets a string option.
 * @param handle voikko instance
 * @param option option name
 * @param value option value
 * @return true if option was succesfully set, otherwise false
 */
int voikko_set_string_option(int handle, int option, const char * value);

/**
 * Checks the spelling of a character string in current multibyte encoding.
 * @param handle voikko instance
 * @param word word to check
 * @return one of the spellchecker return codes
 */
int voikko_spell_cstr(int handle, const char * word);

/**
 * Checks the spelling of a wide character Unicode string
 * @param handle voikko instance
 * @param word word to check
 * @return one of the spellchecker return codes
 */
int voikko_spell_ucs4(int handle, const wchar_t * word);

/**
 * Finds suggested correct spellings for given word in current multibyte encoding.
 * @param handle voikko instance
 * @param word word to find suggestions for
 * @return null, if no suggestions could be generated. Otherwise returns a pointer to a
 *         null-terminated array of 0 or more strings containing the suggestions in the current
 *         multibyte encoding. Caller is assumed to free the strings and the array after use.
 */
char ** voikko_suggest_cstr(int handle, const char * word);

/**
 * Finds suggested correct spellings for given word in wide character Unicode string.
 * @param handle voikko instance
 * @param word word to find suggestions for
 * @return null, if no suggestions could be generated. Otherwise returns a pointer to a
 *         null-terminated array of 0 or more strings containing the suggestions in wide character
 *         Unicode strings. Caller is assumed to free the strings and the array after use.
 */
wchar_t ** voikko_suggest_ucs4(int handle, const wchar_t * word);

/**
 * Hyphenates the given word in current multibyte encoding.
 * @param handle voikko instance
 * @param word word to hyphenate
 * @return null-terminated character string containing the hyphenation using the following notation:
 *         ' ' = no hyphenation at this character, '-' = hyphenation point (character at this position
 *         is preserved in the hyphenated form), '=' = hyphentation point (character at this position
 *         is replaced by the hyphen.)
 */
char * voikko_hyphenate_cstr(int handle, const char * word);

/**
 * Hyphenates the given word in wide character Unicode string.
 * @param handle voikko instance
 * @param word word to hyphenate
 * @return null-terminated character string containing the hyphenation using the following notation:
 *         ' ' = no hyphenation at this character, '-' = hyphenation point (character at this position
 *         is preserved in the hyphenated form), '=' = hyphentation point (character at this position
 *         is replaced by the hyphen.)
 */
char * voikko_hyphenate_ucs4(int handle, const wchar_t * word);

END_C_DECLS
#endif

#endif
