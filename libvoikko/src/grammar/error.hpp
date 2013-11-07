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
 * Portions created by the Initial Developer are Copyright (C) 2008 - 2012
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

#ifndef VOIKKO_GRAMMAR_ERROR_H
#define VOIKKO_GRAMMAR_ERROR_H

#include "voikko_structs.h"
#include "grammar/VoikkoGrammarError.hpp"

#define GCERR_INVALID_SPELLING 1
#define GCERR_EXTRA_WHITESPACE 2
#define GCERR_SPACE_BEFORE_PUNCTUATION 3
#define GCERR_EXTRA_COMMA 4
#define GCERR_INVALID_SENTENCE_STARTER 5
#define GCERR_WRITE_FIRST_LOWERCASE 6
#define GCERR_WRITE_FIRST_UPPERCASE 7
#define GCERR_REPEATING_WORD 8
#define GCERR_TERMINATING_PUNCTUATION_MISSING 9
#define GCERR_INVALID_PUNCTUATION_AT_END_OF_QUOTATION 10
#define GCERR_FOREIGN_QUOTATION_MARK 11
#define GCERR_MISPLACED_CLOSING_PARENTHESIS 12
#define GCERR_NEGATIVE_VERB_MISMATCH 13
#define GCERR_A_INFINITIVE_REQUIRED 14
#define GCERR_MA_INFINITIVE_REQUIRED 15
#define GCERR_MISPLACED_SIDESANA 16
#define GCERR_MISSING_MAIN_VERB 17
#define GCERR_EXTRA_MAIN_VERB 18

#endif
