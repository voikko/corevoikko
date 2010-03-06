/* Libvoikko: Library of Finnish language tools
 * Copyright (C) 2009 - 2010 Harri Pitkänen <hatapitk@iki.fi>
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
