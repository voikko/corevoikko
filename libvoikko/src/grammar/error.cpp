/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2008 - 2010 Harri Pitkänen <hatapitk@iki.fi>
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

#include "grammar/error.hpp"
#include "porting.h"
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
				return "Väärin sijoitettu sulkumerkki";
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
		}
		return "Unknown error";
	}
}

void init_grammar_error(voikko_grammar_error * error) {
	error->error_code = 0;
	error->error_level = 0;
	error->error_description = 0;
	error->startpos = 0;
	error->errorlen = 0;
	error->suggestions = 0;
}

}
