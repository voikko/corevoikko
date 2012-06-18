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

#ifndef VOIKKO_GRAMMAR_TOKEN
#define VOIKKO_GRAMMAR_TOKEN

#include <stddef.h>
#include "voikko_enums.h"

namespace libvoikko { namespace grammar {

/** Possible types for trailing parts of compound verbs */
enum FollowingVerbType {
	FOLLOWING_VERB_NONE,
	FOLLOWING_VERB_A_INFINITIVE,
	FOLLOWING_VERB_MA_INFINITIVE
};

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
		
		/** True if this word may be the main verb */
		bool possibleMainVerb;
		
		/** True if this word is definitely the main verb */
		bool isMainVerb;
		
		/** True if this word is a verb negative ("en", "älä" jne.) */
		bool isVerbNegative;
		
		/** True if this word cannot be anything else than a positive verb */
		bool isPositiveVerb;
		
		/** True if this word is a conjunction */
		bool isConjunction;
		
		/** True if this word may be a conjunction */
		bool possibleConjunction;
		
		/**
		 * What kind of verb must follow this verb in compound verb check. NONE if
		 * this word is not (or may not be) a verb.
		 */
		FollowingVerbType requireFollowingVerb;
		
		/**
		 * What kind of verb this word is if it is used as a trailing part in compound
		 * verb constructs. NONE if this word is not a verb.
		 */
		FollowingVerbType verbFollowerType;
		
		/** Null terminated string containing the token text */
		wchar_t * str;
		
		/** Length of the token */
		size_t tokenlen;
		
		/** Position of this token within paragraph */
		size_t pos;
};

} }

#endif
