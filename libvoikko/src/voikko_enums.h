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
 * Portions created by the Initial Developer are Copyright (C) 2006 - 2010
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

#ifndef VOIKKO_VOIKKO_ENUMS_H
#define VOIKKO_VOIKKO_ENUMS_H

/**
 * Token types for string tokenization
 * TOKEN_NONE:        End of text or error
 * TOKEN_WORD:        Word
 * TOKEN_PUNCTUATION: Punctuation
 * TOKEN_WHITESPACE:  Whitespace
 * TOKEN_UNKNOWN:     Character not used in any of the supported natural languages
 */
enum voikko_token_type {TOKEN_NONE, TOKEN_WORD, TOKEN_PUNCTUATION, TOKEN_WHITESPACE, TOKEN_UNKNOWN};

/**
 * Sentence start types
 * SENTENCE_NONE: End of text reached or error.
 * SENTENCE_NO_START: This is not a start of a new sentence.
 * SENTENCE_PROBABLE: This is a probable start of a new sentence.
 * SENTENCE_POSSIBLE: This may be a start of a new sentence.
 */
enum voikko_sentence_type {SENTENCE_NONE, SENTENCE_NO_START, SENTENCE_PROBABLE, SENTENCE_POSSIBLE};

#endif // VOIKKO_ENUMS_H
