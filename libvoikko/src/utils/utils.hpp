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

#ifndef VOIKKO_UTILS_UTILS_H
#define VOIKKO_UTILS_UTILS_H

#include <cstddef>

#ifdef DEBUG
#define LOG(x) printf x
#else
#define LOG(X)
#endif

#define VOIKKO_CONSONANTS L"bcdfghjklmnpqrstvwxz\u0161\u017e"
#define VOIKKO_VOWELS L"aeiouy\u00e4\u00f6"

namespace libvoikko {

enum casetype {CT_NO_LETTERS, CT_ALL_LOWER, CT_FIRST_UPPER, CT_COMPLEX, CT_ALL_UPPER};

/** Returns the type of given string wrt the use of character cases.
 * @param word string to check
 * @param nchars length of the string
 * @return character case type of the string
 */
enum casetype voikko_casetype(const wchar_t * word, size_t nchars);

/** Changes a string to use defined character case
 * @param charcase character case
 * @param word string to change
 * @param nchars length of the string
 */
void voikko_set_case(enum casetype charcase, wchar_t * word, size_t nchars);

/** Detect special non-word strings such as URLs and email addresses
 * @param word string to check
 * @param nchars length of the string
 * @return true if string is a non-word, otherwise false
 */
bool voikko_is_nonword(const wchar_t * word, size_t nchars);

}

#endif
