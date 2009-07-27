/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2006 - 2009 Harri Pitkänen <hatapitk@iki.fi>
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
 * const char * voikko_error = voikko_init(&handle, "fi_FI", 0);
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

/* Ignore dot at the end of the word (needed for use in some word processors).
 * If this option is set and input word ends with a dot, spell checking and
 * hyphenation functions try to analyse the word without the dot if no results
 * can be obtained for the original form. Also with this option, string tokenizer
 * will consider trailing dot of a word to be a part of that word.
 * Default: false */
#define VOIKKO_OPT_IGNORE_DOT 0

/* Ignore words containing numbers
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

/* Use suggestions optimised for optical character recognition software.
 * By default suggestions are optimised for typing errors.
 * Default: false */
#define VOIKKO_OPT_OCR_SUGGESTIONS 8

/* (Spell checking only): Ignore non-words such as URLs and email addresses.
 * Default: true */
#define VOIKKO_OPT_IGNORE_NONWORDS 10

/* (Spell checking only): Allow some extra hyphens in words. This option relaxes
 * hyphen checking rules to work around some unresolved issues in the underlying
 * morphology, but it may cause some incorrect words to be accepted. The exact
 * behaviour (if any) of this option is not specified.
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

/* There are two possible rules that can be applied when hyphenating compound words
 * that can be split in more than one different way. We either take the intersection
 * of (1) all possible hyphenations or (2) all hyphenations where the compound word
 * has the minimal amount of parts (:= m) in it. The rule (1) is applied if and only
 * if m > voikko_intersect_compound_level.
 * Default: 1 */
#define VOIKKO_INTERSECT_COMPOUND_LEVEL 5

/* The minumum length for words that may be hyphenated. This limit is also enforced on
 * individual parts of compound words.
 * Default: 2 */
#define VOIKKO_MIN_HYPHENATED_WORD_LENGTH 9

/* String options */
/* The encoding in which multibyte character strings are interpreteted and returned
 * as results.
 * This option is deprecated. Future releases will support UTF-8 encoding only.
 * Default: UTF-8 */
#define VOIKKO_OPT_ENCODING 2

/**
 * Grammar error description.
 */
typedef struct {
	/** Error code. 0 = no error was found */
	int error_code;
	/** Unused (indicates the probability of a false positive) */
	int error_level;
	/** Unused (detailed error description) */
	char * error_description;
	/** Start position of the error in the text */
	size_t startpos;
	/** Length of the error in the text */
	size_t errorlen;
	/** Possible corrections. These should be freed after
	 *  use by calling voikko_free_suggest_cstr */
	char ** suggestions;
} voikko_grammar_error;

#ifndef VOIKKO_DEFS_H
BEGIN_C_DECLS

/**
 * Same as voikko_init_with_path(handle, langcode, cache_size, NULL)
 */
const char * voikko_init(int * handle, const char * langcode, int cache_size);

/**
 * Initialises the library for use in the specified language, adding an extra directory
 * to the standard dictionary search path.
 * @param handle after succesful initialisation handle will contain a handle that
 *        refers to this particular instance of voikko
 * @param langcode the language code. The following values can be used:
 *        - "", "default" or "fi_FI": Use the default dictionary. The default
 *          dictionary can be assumed to be present in a complete installation of
 *          libvoikko.
 *        - any other string: Use the specified dictionary variant. Usually there
 *          is at least the "standard" variant, but this is not guaranteed. If the
 *          specified dictionary does not exist, an error message is returned.
 *        - NULL: Reserved for future use. Currently leads to undefined behavior.
 * @param cache_size size of the spellchecker cache. This can be -1 (no cache) or
 *        >= 0 ( size in bytes = 2^cache_size * (6544*sizeof(wchar_t) + 1008) ).
 * @param path path to a directory from which dictionary files should be searched
 *        first before looking into the standard dictionary locations. If NULL, no
 *        additional search path will be used.
 * @return null, if initialisation completed without error, otherwise a pointer
 *         to a string describing the error
 */
const char * voikko_init_with_path(int * handle, const char * langcode,
                                   int cache_size, const char * path);

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
 * @return null-terminated character string containing the hyphenation using
 * the following notation:
 *     ' ' = no hyphenation at this character,
 *     '-' = hyphenation point (character at this position
 *           is preserved in the hyphenated form),
 *     '=' = hyphentation point (character at this position
 *           is replaced by the hyphen.)
 * Returns 0 on error.
 */
char * voikko_hyphenate_cstr(int handle, const char * word);

/**
 * Hyphenates the given word in wide character Unicode string.
 * @param handle voikko instance
 * @param word word to hyphenate
 * @return null-terminated character string containing the hyphenation using
 * the following notation:
 *     ' ' = no hyphenation at this character,
 *     '-' = hyphenation point (character at this position
 *           is preserved in the hyphenated form),
 *     '=' = hyphentation point (character at this position
 *           is replaced by the hyphen.)
 * Returns 0 on error.
 */
char * voikko_hyphenate_ucs4(int handle, const wchar_t * word);

/**
 * Frees the memory allocated for spelling suggestions.
 * @param suggest_result spelling suggestions
 */
void voikko_free_suggest_ucs4(wchar_t ** suggest_result);

/**
 * Frees the memory allocated for spelling suggestions.
 * @param suggest_result spelling suggestions
 */
void voikko_free_suggest_cstr(char ** suggest_result);

/**
 * Frees the memory allocated for hyphenation results.
 * @param hyphenate_result hyphenation result
 */
void voikko_free_hyphenate(char * hyphenate_result);

/**
 * Token types for string tokenization
 * TOKEN_NONE:        End of text or error
 * TOKEN_WORD:        Word
 * TOKEN_PUNCTUATION: Punctuation
 * TOKEN_WHITESPACE:  Whitespace
 * TOKEN_UNKNOWN:     Character not used in Finnish
 */
enum voikko_token_type {TOKEN_NONE, TOKEN_WORD, TOKEN_PUNCTUATION, TOKEN_WHITESPACE, TOKEN_UNKNOWN};

/**
 * Find the next token in text stream.
 * @param handle voikko instance
 * @param text Pointer to the start of a text buffer
 * @param textlen Number of characters left in the buffer
 * @param tokenlen (out) Number of characters in the identified token
 * @return Type of the identified token.
 */
enum voikko_token_type voikko_next_token_ucs4(int handle, const wchar_t * text,
                       size_t textlen, size_t * tokenlen);

/**
 * Find the next token in text stream.
 * @param handle voikko instance
 * @param text Pointer to the start of a text buffer
 * @param textlen Number of bytes left in the buffer
 * @param tokenlen (out) Number of characters in the identified token
 * @return Type of the identified token.
 */
enum voikko_token_type voikko_next_token_cstr(int handle, const char * text,
                       size_t textlen, size_t * tokenlen);

/**
 * Sentence start types
 * SENTENCE_NONE: End of text reached or error.
 * SENTENCE_NO_START: This is not a start of a new sentence.
 * SENTENCE_POSSIBLE: This may be a start of a new sentence.
 * SENTENCE_PROBABLE: This is a probable start of a new sentence.
 */
enum voikko_sentence_type {SENTENCE_NONE, SENTENCE_NO_START, SENTENCE_PROBABLE, SENTENCE_POSSIBLE};

/**
 * Find the next sentence in text stream.
 * @param handle voikko instance
 * @param text Pointer to the start of a text buffer
 * @param textlen Number of characters left in the buffer
 * @param sentencelen (out) Offset of the character that starts the next sentence.
 * @return Type of the next found sentence, if any.
 */
enum voikko_sentence_type voikko_next_sentence_start_ucs4(int handle,
                          const wchar_t * text, size_t textlen, size_t * sentencelen);

/**
 * Find the next sentence in text stream.
 * @param handle voikko instance
 * @param text Pointer to the start of a text buffer
 * @param textlen Number of bytes left in the buffer
 * @param sentencelen (out) Offset of the character (in charactes, not bytes) that
 *        starts the next sentence.
 * @return Type of the next found sentence, if any.
 */
enum voikko_sentence_type voikko_next_sentence_start_cstr(int handle,
                          const char * text, size_t textlen, size_t * sentencelen);

/**
 * Find next grammar error.
 * @param handle Voikko instance
 * @param text Pointer to the start of a text buffer. This should usually
 *        be at the start of a paragraph or a sentence.
 * @param textlen Number of characters in the buffer. The end of the buffer should
 *        be the end of a paragraph or a sentence.
 * @param startpos Do not consider errors that start before this character
 *        offset from the start of the text.
 * @param skiperrors Skip this number of errors from the start of the text.
 * @return Grammar error description.
 */
voikko_grammar_error voikko_next_grammar_error_ucs4(int handle, const wchar_t * text,
                     size_t textlen, size_t startpos, int skiperrors);

/**
 * Find next grammar error.
 * @param handle Voikko instance
 * @param text Pointer to the start of a text buffer. This should usually
 *        be at the start of a paragraph or a sentence.
 * @param textlen Number of bytes in the buffer. The end of the buffer should
 *        be the end of a paragraph or a sentence.
 * @param startpos Do not consider errors that start before this character
 *        offset from the start of the text.
 * @param skiperrors Skip this number of errors from the start of the text.
 * @return Grammar error description.
 */
voikko_grammar_error voikko_next_grammar_error_cstr(int handle, const char * text,
                     size_t textlen, size_t startpos, int skiperrors);

/**
 * Localized error message.
 * @param error_code Error code (from voikko_grammar_error)
 * @param language ISO language code or null, if the language from current locale
 *        should be used.
 * @return The UTF-8 encoded localized error message for the error code.
 */
const char * voikko_error_message_cstr(int error_code, const char * language);

/**
 * A type representing a dictionary
 */
struct voikko_dict;

/**
 * Get a list of available dictionaries.
 * @param path path to a directory from which dictionary files should be searched
 *        first before looking into the standard dictionary locations.
 * @return A pointer to a null terminated array of dictionary entries.
 */
struct voikko_dict ** voikko_list_dicts(const char * path);

/**
 * Free the memory allocated for dictionary list.
 * @param dicts A list of available dictionaries obtained with voikko_list_dicts
 */
void voikko_free_dicts(struct voikko_dict ** dicts);

/**
 * Get the variant identifier for a dictionary.
 * @return The variant identifier for given dictionary.
 */
const char * voikko_dict_variant(const struct voikko_dict * dict);

/**
 * Get the human readable description for a dictionary.
 * @return The description for given dictionary.
 */
const char * voikko_dict_description(const struct voikko_dict * dict);

/**
 * A type representing result from morphological analysis.
 */
struct voikko_mor_analysis;

/**
 * Analyzes the morphology of given word.
 * @param handle voikko instance
 * @param word word to be analyzed.
 * @return A pointer to a null terminated array of analysis results.
 */
struct voikko_mor_analysis ** voikko_analyze_word_ucs4(
                              int handle, const wchar_t * word);

/**
 * Free the memory allocated for morphology analysis results.
 * @param analysis A list of analysis results obtained with voikko_mor_analysis.
 */
void voikko_free_mor_analysis(struct voikko_mor_analysis ** analysis);

/**
 * Lists the keys available within given morphology analysis result.
 * @param analysis Analysis to be examined.
 * @return List of known keys within the result. This list will be freed
 * upon call to voikko_free_mor_analysis.
 */
const char ** voikko_mor_analysis_keys(const struct voikko_mor_analysis * analysis);

/**
 * Get a value from morphology analysis result.
 * @param analysis Analysis to be examined.
 * @param key Key whose value should be returned.
 * @return Value of the given key or null, if analysis does not contain
 * any value for the key. Value will be freed upon call to voikko_free_mor_analysis.
 */
const wchar_t * voikko_mor_analysis_value_ucs4(
                const struct voikko_mor_analysis * analysis,
                const char * key);

END_C_DECLS
#endif

#endif
