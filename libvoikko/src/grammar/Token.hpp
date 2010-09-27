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

#ifndef VOIKKO_GRAMMAR_TOKEN
#define VOIKKO_GRAMMAR_TOKEN

#include <stddef.h>
#include "voikko_enums.h"

namespace libvoikko { namespace grammar {

/**
 * Grammar checker sentence token.
 */
class Token {
	public:
		voikko_token_type type;
		
		/** True if this word token was recognized as a valid word */
		bool isValidWord;
		
		/** True if this is a word token that should start with
		 *  lower case letter. */
		bool firstLetterLcase;
		
		/** True if this word may be (but is not necessarily) the first
		 *  word in a sentence. */
		bool possibleSentenceStart;
		
		/** True if this word may be a geographical name in genitive case. */
		bool isGeographicalNameInGenitive;
		
		/** True if this is a proper noun that might be a geographical name ("Sämpyläjoki") */
		bool possibleGeographicalName;
		
		/** True if this word is a verb negative ("en", "älä" jne.) */
		bool isVerbNegative;
		
		/** True if this word cannot be anything else than a positive verb */
		bool isPositiveVerb;
		
		/** Null terminated string containing the token text */
		wchar_t * str;
		
		/** Length of the token */
		size_t tokenlen;
		
		/** Position of this token within paragraph */
		size_t pos;
};

} }

#endif
