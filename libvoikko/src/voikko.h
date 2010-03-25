/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2006 - 2010 Harri Pitkänen <hatapitk@iki.fi>
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

#include "voikko_defines.h"
#include "voikko_enums.h"
#include "voikko_structs.h"

BEGIN_C_DECLS

/** Handle to a specific Voikko instance */
struct VoikkoHandle;

/**
 * Initialises the library for use in the specified language, adding an extra directory
 * to the standard dictionary search path.
 * @param error Will be set to null if initialisation completed without error.
 *        Otherwise will be set to a pointer to a string describing the error.
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
 * @return A handle to the initialized Voikko instance or null, if initialization
 *         failed.
 */
VoikkoHandle * voikkoInit(const char ** error, const char * langcode,
                          int cache_size, const char * path);

/**
 * Terminates an instance of voikko.
 */
void voikkoTerminate(VoikkoHandle * handle);

/**
 * Sets a boolean option.
 * @param handle voikko instance
 * @param option option name
 * @param value option value
 * @return true if option was succesfully set, otherwise false
 */
int voikkoSetBooleanOption(VoikkoHandle * handle, int option, int value);

/**
 * Sets an integer option.
 * @param handle voikko instance
 * @param option option name
 * @param value option value
 * @return true if option was succesfully set, otherwise false
 */
int voikkoSetIntegerOption(VoikkoHandle * handle, int option, int value);

/**
 * Checks the spelling of an UTF-8 character string.
 * @param handle voikko instance
 * @param word word to check
 * @return one of the spellchecker return codes
 */
int voikkoSpellCstr(VoikkoHandle * handle, const char * word);

/**
 * Checks the spelling of a wide character Unicode string
 * @param handle voikko instance
 * @param word word to check
 * @return one of the spellchecker return codes
 */
int voikkoSpellUcs4(VoikkoHandle * handle, const wchar_t * word);

/**
 * Finds suggested correct spellings for given UTF-8 encoded word.
 * @param handle voikko instance
 * @param word word to find suggestions for
 * @return null, if no suggestions could be generated. Otherwise returns a pointer to a
 *         null-terminated array of 0 or more strings containing the suggestions in
 *         UTF-8 encoding. Use voikkoFreeCstrArray to free the array and strings after use.
 */
char ** voikkoSuggestCstr(VoikkoHandle * handle, const char * word);

/**
 * Finds suggested correct spellings for given word in wide character Unicode string.
 * @param handle voikko instance
 * @param word word to find suggestions for
 * @return null, if no suggestions could be generated. Otherwise returns a pointer to a
 *         null-terminated array of 0 or more strings containing the suggestions in wide character
 *         Unicode strings. Use voikko_free_suggest_ucs4 to free the array and strings after use.
 */
wchar_t ** voikkoSuggestUcs4(VoikkoHandle * handle, const wchar_t * word);

/**
 * Hyphenates the given word in UTF-8 encoding.
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
char * voikkoHyphenateCstr(VoikkoHandle * handle, const char * word);

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
char * voikkoHyphenateUcs4(VoikkoHandle * handle, const wchar_t * word);

/**
 * Frees the memory allocated for spelling suggestions.
 * @param suggest_result spelling suggestions
 */
void voikko_free_suggest_ucs4(wchar_t ** suggest_result);

/**
 * Frees the memory allocated for a char string array.
 * @param cstrArray char string array
 */
void voikkoFreeCstrArray(char ** cstrArray);

/**
 * Frees the memory allocated for a char string.
 * @param cstr char string allocated by libvoikko
 */
void voikkoFreeCstr(char * cstr);

/**
 * Find the next token in text stream.
 * @param handle voikko instance
 * @param text Pointer to the start of a text buffer
 * @param textlen Number of characters left in the buffer
 * @param tokenlen (out) Number of characters in the identified token
 * @return Type of the identified token.
 */
enum voikko_token_type voikkoNextTokenUcs4(VoikkoHandle * handle, const wchar_t * text,
                       size_t textlen, size_t * tokenlen);

/**
 * Find the next token in text stream.
 * @param handle voikko instance
 * @param text Pointer to the start of a text buffer
 * @param textlen Number of bytes left in the buffer
 * @param tokenlen (out) Number of characters in the identified token
 * @return Type of the identified token.
 */
enum voikko_token_type voikkoNextTokenCstr(VoikkoHandle * handle, const char * text,
                       size_t textlen, size_t * tokenlen);

/**
 * Find the next sentence in text stream.
 * @param handle voikko instance
 * @param text Pointer to the start of a text buffer
 * @param textlen Number of characters left in the buffer
 * @param sentencelen (out) Offset of the character that starts the next sentence.
 * @return Type of the next found sentence, if any.
 */
enum voikko_sentence_type voikkoNextSentenceStartUcs4(VoikkoHandle * handle,
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
enum voikko_sentence_type voikkoNextSentenceStartCstr(VoikkoHandle * handle,
                          const char * text, size_t textlen, size_t * sentencelen);

/**
 * A type representing a new style grammar error.
 */
struct VoikkoGrammarError;

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
VoikkoGrammarError * voikkoNextGrammarErrorUcs4(VoikkoHandle * handle, const wchar_t * text,
                     size_t textlen, size_t startpos, int skiperrors);

/**
 * Get the error code associated with given grammar error
 * @param error The grammar error
 * @return The error code
 */
int voikkoGetGrammarErrorCode(const VoikkoGrammarError * error);

/**
 * Get the starting position of the error in checked paragraph.
 * @param error The grammar error
 * @return The starting position of the error in the checked paragraph (in characters).
 */
size_t voikkoGetGrammarErrorStartPos(const VoikkoGrammarError * error);

/**
 * Free the memory reserved for a grammar error, including suggestions for corrections.
 * @param error The grammar error
 */
void voikkoFreeGrammarError(VoikkoGrammarError * error);

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
 * Analyzes the morphology of given word.
 * @param handle voikko instance
 * @param word word to be analyzed.
 * @return A pointer to a null terminated array of analysis results.
 */
struct voikko_mor_analysis ** voikko_analyze_word_cstr(
                              int handle, const char * word);

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

/**
 * Get a value from morphology analysis result.
 * @param analysis Analysis to be examined.
 * @param key Key whose value should be returned.
 * @return Value of the given key or null, if analysis does not contain
 * any value for the key.
 * Value must be freed after use using voikko_free_mor_analysis_value_cstr.
 */
char * voikko_mor_analysis_value_cstr(
                const struct voikko_mor_analysis * analysis,
                const char * key);


/**
 * Frees the memory allocated for morphologival analysis value.
 * @param analysis_value analysis value.
 */
void voikko_free_mor_analysis_value_cstr(char * analysis_value);

#ifndef VOIKKO_NO_DEPRECATED_API
#include "voikko_deprecated.h"
#endif

END_C_DECLS

#endif
