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
 * Portions created by the Initial Developer are Copyright (C) 2009 - 2010
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

#ifndef VOIKKO_HYPHENATOR_HYPHENATOR
#define VOIKKO_HYPHENATOR_HYPHENATOR

#include <cstring>

namespace libvoikko { namespace hyphenator {
/**
 * General interface for hyphenators.
 */
class Hyphenator {
	public:
		/**
		 * Hyphenate given word.
		 * @param word word to hyphenate
		 * @param wlen length of the word in wchar_t units
		 * @return null-terminated character string containing the hyphenation
		 * using the following notation:
		 *    ' ' = no hyphenation point before or at this character
		 *    '-' = hyphenation point before this character
		 *          (character at this position
		 *          is preserved in the hyphenated form)
		 *    '=' = hyphentation point (character at this position
		 *          is replaced with hyphen.)
		 * Returns null on error.
		 */
		virtual char * hyphenate(const wchar_t * word, size_t wlen) = 0;

		/**
		 * Terminate this component.
		 */
		virtual void terminate() = 0;

		/**
		 * Insert hyphenation positions that are considered to be ugly
		 * but correct. Typically this option is not set in text processors that
		 * use hyphenation for splitting words at the end of line. It is
		 * used in applications that need to split words into syllables.
		 * Default: true
		 */
		virtual void setUglyHyphenation(bool uglyHyphenation) = 0;

		/**
		 * Hyphenate unknown words. Default: true
		 */
		virtual void setHyphenateUnknown(bool hyphenateUnknown) = 0;

		/**
		 * The minumum length for words that may be hyphenated. This limit is
		 * also enforced on individual parts of compound words. Default: 2
		 */
		virtual void setMinHyphenatedWordLength(int length) = 0;

		/**
		 * Ignore extra dot at the end of word. This option is set when
		 * the provider of words to be hyphenated cannot know whether a dot
		 * at the end of a word is a part of that word. Default: false.
		 */
		virtual void setIgnoreDot(bool ignoreDot) = 0;
		
		virtual ~Hyphenator();
};

} }

#endif
