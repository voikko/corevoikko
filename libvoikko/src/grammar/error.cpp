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

#include "grammar/VoikkoGrammarError.hpp"
#include "porting.h"
#include "utils/StringUtils.hpp"
#include <cstring>


namespace libvoikko {

VOIKKOEXPORT const char * voikko_error_message_cstr(int error_code, const char * language) {
	if (strncmp(language, "fi", 2) == 0) {
		// ä=\xc3\xa4, ö=\xc3\xb6, Ä=\xc3\x84, Ö=\xc3\x96
		switch (error_code) {
			case GCERR_INVALID_SPELLING:
				return "Virheellinen kirjoitusasu";
			case GCERR_EXTRA_WHITESPACE:
				return "Poista ylim\xc3\xa4\xc3\xa4r\xc3\xa4inen v\xc3\xa4li.";
			case GCERR_SPACE_BEFORE_PUNCTUATION:
				return "Ylim\xc3\xa4\xc3\xa4r\xc3\xa4inen v\xc3\xa4li v\xc3\xa4limerkin edess\xc3\xa4";
			case GCERR_EXTRA_COMMA:
				return "Poista ylim\xc3\xa4\xc3\xa4r\xc3\xa4inen pilkku.";
			case GCERR_INVALID_SENTENCE_STARTER:
				return "Virheellinen virkkeen aloittava merkki";
			case GCERR_WRITE_FIRST_LOWERCASE:
				return "Harkitse sanan kirjoittamista pienell\xc3\xa4 alkukirjaimella.";
			case GCERR_WRITE_FIRST_UPPERCASE:
				return "Sana on kirjoitettava isolla alkukirjaimella.";
			case GCERR_REPEATING_WORD:
				return "Sana on kirjoitettu kahteen kertaan.";
			case GCERR_TERMINATING_PUNCTUATION_MISSING:
				return "V\xc3\xa4limerkki puuttuu virkkeen lopusta.";
			case GCERR_INVALID_PUNCTUATION_AT_END_OF_QUOTATION:
				return "Virheelliset v\xc3\xa4limerkit lainauksen lopussa";
			case GCERR_FOREIGN_QUOTATION_MARK:
				return "Suomenkieliseen tekstiin sopimaton lainausmerkki";
			case GCERR_MISPLACED_CLOSING_PARENTHESIS:
				return "V\xc3\xa4\xc3\xa4rin sijoitettu sulkumerkki";
			case GCERR_NEGATIVE_VERB_MISMATCH:
				return "Kieltoverbi ja p\xc3\xa4\xc3\xa4verbi eiv\xc3\xa4t sovi yhteen.";
			case GCERR_A_INFINITIVE_REQUIRED:
				return "J\xc3\xa4lkimm\xc3\xa4isen verbin tulisi olla a/\xc3\xa4-p\xc3\xa4\xc3\xa4tteisess\xc3\xa4 infinitiiviss\xc3\xa4.";
			case GCERR_MA_INFINITIVE_REQUIRED:
				return "J\xc3\xa4lkimm\xc3\xa4isen verbin tulisi olla maan/m\xc3\xa4\xc3\xa4n-p\xc3\xa4\xc3\xa4tteisess\xc3\xa4 infinitiiviss\xc3\xa4.";
			case GCERR_MISPLACED_SIDESANA:
				return "Sidesana (ja, tai, mutta, ...) ei voi olla virkkeen viimeinen sana.";
			case GCERR_MISSING_MAIN_VERB:
				return "Tarkista, puuttuuko virkkeest\xc3\xa4 p\xc3\xa4\xc3\xa4verbi tai -verbej\xc3\xa4.";
			case GCERR_EXTRA_MAIN_VERB:
				return "Virkkeest\xc3\xa4 saattaa puuttua pilkku, tai siin\xc3\xa4 voi olla ylim\xc3\xa4\xc3\xa4r\xc3\xa4inen verbi.";
		}
		return "Tuntematon virhe";
	}
	else {
		switch (error_code) {
			case GCERR_INVALID_SPELLING:
				return "Incorrect spelling of word(s)";
			case GCERR_EXTRA_WHITESPACE:
				return "Remove extra space.";
			case GCERR_SPACE_BEFORE_PUNCTUATION:
				return "Remove space before punctuation.";
			case GCERR_EXTRA_COMMA:
				return "Remove extra comma.";
			case GCERR_INVALID_SENTENCE_STARTER:
				return "Invalid character at the start of a sentence";
			case GCERR_WRITE_FIRST_LOWERCASE:
				return "Consider changing first letter to lower case.";
			case GCERR_WRITE_FIRST_UPPERCASE:
				return "Change first letter to upper case.";
			case GCERR_REPEATING_WORD:
				return "Remove duplicate word.";
			case GCERR_TERMINATING_PUNCTUATION_MISSING:
				return "Terminating punctuation is missing.";
			case GCERR_INVALID_PUNCTUATION_AT_END_OF_QUOTATION:
				return "Invalid punctuation at the end of quotation";
			case GCERR_FOREIGN_QUOTATION_MARK:
				return "Foreign quotation mark";
			case GCERR_MISPLACED_CLOSING_PARENTHESIS:
				return "Misplaced closing parenthesis";
			case GCERR_NEGATIVE_VERB_MISMATCH:
				return "Mismatched negative and verb";
			case GCERR_A_INFINITIVE_REQUIRED:
				return "You should use infinitive ending with a/\xc3\xa4 as the second verb.";
			case GCERR_MA_INFINITIVE_REQUIRED:
				return "You should use infinitive ending with maan/m\xc3\xa4\xc3\xa4n as the second verb.";
			case GCERR_MISPLACED_SIDESANA:
				return "Sentence should not end with a conjunction.";
			case GCERR_MISSING_MAIN_VERB:
				return "Sentence should contain one or more main verbs.";
			case GCERR_EXTRA_MAIN_VERB:
				return "Check if sentence contains unnecessary verbs or if a comma is missing.";
		}
		return "Unknown error";
	}
}

VOIKKOEXPORT const char * voikkoGetGrammarErrorShortDescription(const grammar::VoikkoGrammarError * error, const char * language) {
	return utils::StringUtils::copy(voikko_error_message_cstr(error->getErrorCode(), language));
}


}
