/* The contents of this file are subject to the Mozilla Public License Version 
 * 1.1 (the "License"); you may not use this file except in compliance with 
 * the License. You may obtain a copy of the License at 
 * http://www.mozilla.org/MPL/
 * 
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 * 
 * The Original Code is Libvoikko: Library of natural language processing tools.
 * The Initial Developer of the Original Code is Harri Pitkänen <hatapitk@iki.fi>.
 * Portions created by the Initial Developer are Copyright (C) 2010
 * the Initial Developer. All Rights Reserved.
 * 
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *********************************************************************************/

#ifndef VOIKKO_VOIKKO_DEFINES_H
#define VOIKKO_VOIKKO_DEFINES_H

#undef BEGIN_C_DECLS
#undef END_C_DECLS
#ifdef __cplusplus
# define BEGIN_C_DECLS extern "C" {
# define END_C_DECLS }
#else
# define BEGIN_C_DECLS /* empty */
# define END_C_DECLS /* empty */
#endif

/**
 * Maximum number of characters in a word. Will be deprecated. These days
 * backends may have different limits and they will behave sensibly (for
 * example return SPELL_FAILED) if a too long word is given.
 */
#define LIBVOIKKO_MAX_WORD_CHARS 255

/* Spell checker return codes */
#define VOIKKO_SPELL_FAILED 0
#define VOIKKO_SPELL_OK 1
#define VOIKKO_INTERNAL_ERROR 2
#define VOIKKO_CHARSET_CONVERSION_FAILED 3

/* Boolean options */

/* Ignore dot at the end of the word (needed for use in some word processors).
 * If this option is set and input word ends with a dot, spell checking and
 * hyphenation functions try to analyze the word without the dot if no results
 * can be obtained for the original form. Also with this option, string tokenizer
 * will consider trailing dot of a word to be a part of that word.
 * Default: false */
#define VOIKKO_OPT_IGNORE_DOT 0

/* (Spell checking only) Ignore words containing numbers
 * Default: false */
#define VOIKKO_OPT_IGNORE_NUMBERS 1

/* Accept words that are written completely in uppercase letters without checking
 * them at all.
 * Default: false */
#define VOIKKO_OPT_IGNORE_UPPERCASE 3

/* Accept words even when the first letter is in uppercase (start of sentence etc.)
 * Default: true */
#define VOIKKO_OPT_ACCEPT_FIRST_UPPERCASE 6

/* Accept words even when all of the letters are in uppercase. Note that this is
 * not the same as VOIKKO_OPT_IGNORE_UPPERCASE: with this option the word is still
 * checked, only case differences are ignored.
 * Default: true */
#define VOIKKO_OPT_ACCEPT_ALL_UPPERCASE 7

/* Do not insert hyphenation positions that are considered to be ugly but correct
 * Default: false */
#define VOIKKO_OPT_NO_UGLY_HYPHENATION 4

/* Use suggestions optimized for optical character recognition software.
 * By default suggestions are optimized for typing errors.
 * Default: false */
#define VOIKKO_OPT_OCR_SUGGESTIONS 8

/* (Spell checking only): Ignore non-words such as URLs and email addresses.
 * Default: true */
#define VOIKKO_OPT_IGNORE_NONWORDS 10

/* (Spell checking only): Allow some extra hyphens in words. This option relaxes
 * hyphen checking rules to work around some unresolved issues in the underlying
 * morphology, but it may cause some incorrect words to be accepted. The exact
 * behavior (if any) of this option is not specified.
 * Default: false */
#define VOIKKO_OPT_ACCEPT_EXTRA_HYPHENS 11

/* (Spell checking only): Accept missing hyphens at the start and end of the word.
 * Some application programs do not consider hyphens to be word characters. This
 * is reasonable assumption for many languages but not for Finnish. If the
 * application cannot be fixed to use proper tokenisation algorithm for Finnish,
 * this option may be used to tell libvoikko to work around this defect.
 * Default: false */
#define VOIKKO_OPT_ACCEPT_MISSING_HYPHENS 12

/* (Grammar checking only): Accept incomplete sentences that could occur in
 * titles or headings. Set this option to true if your application is not able
 * to differentiate titles from normal text paragraphs, or if you know that
 * you are checking title text.
 * Default: false */
#define VOIKKO_OPT_ACCEPT_TITLES_IN_GC 13

/* (Grammar checking only): Accept incomplete sentences at the end of the
 * paragraph. These may exist when text is still being written.
 * Default: false */
#define VOIKKO_OPT_ACCEPT_UNFINISHED_PARAGRAPHS_IN_GC 14

/* (Hyphenation only): Hyphenate unknown words.
 * Default: true */
#define VOIKKO_OPT_HYPHENATE_UNKNOWN_WORDS 15

/* (Grammar checking only): Accept paragraphs if they would be valid within
 * bulleted lists.
 * Default: false */
#define VOIKKO_OPT_ACCEPT_BULLETED_LISTS_IN_GC 16

/* Integer options */

/* The minimum length for words that may be hyphenated. This limit is also enforced on
 * individual parts of compound words.
 * Default: 2 */
#define VOIKKO_MIN_HYPHENATED_WORD_LENGTH 9

/* Size of the spell checker cache. This can be -1 (no cache) or
 * >= 0 ( size in bytes = 2^cache_size * (6544*sizeof(wchar_t) + 1008) ).
 * Default: 0*/
#define VOIKKO_SPELLER_CACHE_SIZE 17

#endif
