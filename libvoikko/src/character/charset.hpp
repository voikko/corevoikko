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
 * Portions created by the Initial Developer are Copyright (C) 2006 - 2009
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

#ifndef VOIKKO_CHARACTER_CHARSET_H
#define VOIKKO_CHARACTER_CHARSET_H

#include <cstddef>

namespace libvoikko {

enum char_type {CHAR_UNKNOWN, CHAR_LETTER, CHAR_DIGIT, CHAR_WHITESPACE, CHAR_PUNCTUATION};

/** Returns character type for given character
 * @param c character to check
 * @return the character type
 */
char_type get_char_type(wchar_t c);

/** 
 * Checks if a character is a Finnish quotation mark.
 * @param c character to check
 * @return true if the character is a Finnish quotation mark, false otherwise.
 */
bool isFinnishQuotationMark(wchar_t c);

/** Normalises an unicode string according to our conventions
 * @param word string to normalise
 * @param len length of the string
 * @return the normalised string or null if normalisation failed
 */
wchar_t * voikko_normalise(const wchar_t * word, size_t len);

/** Formats modified string to more closely match the original
 * @param orig original string
 * @param orig_len length of the original string
 * @param modified pointer to the modified string. The string may be
 *        relocated if it needs to be expanded, otherwise it is modified
 *        in place.
 * @param modified_len length of the modified string
 */
void voikko_cset_reformat(const wchar_t * orig, size_t orig_len, wchar_t ** modified, size_t modified_len);

}

#endif
