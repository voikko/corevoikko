/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2006 - 2009 Harri Pitkänen <hatapitk@iki.fi>
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

#include "voikko_defs.h"
#include "setup/setup.hpp"
#include "utils/utils.hpp"
#include "utils/StringUtils.hpp"
#include "character/charset.hpp"
#include "spellchecker/spell.hpp"
#include "spellchecker/suggestions.hpp"
#include <cstdlib>
#include <cstring>
#include <wchar.h>
#include <wctype.h>
#include <malaga.h>

#define MAX_SUGGESTIONS 5
#define COST_LIMIT 350

namespace libvoikko {

void suggest_correct_case(SuggestionStatus * s, const wchar_t * buffer, size_t buflen) {
	wchar_t * newsugg;
	size_t wlen;
	int prio;
	const wchar_t * word;
	if (s->shouldAbort()) {
		return;
	}
	if (buffer == 0) {
		word = s->getWord();
		wlen = s->getWordLength();
	}
	else {
		word = buffer;
		wlen = buflen;
	}
	spellresult sres = voikko_spell_with_priority(word, wlen, &prio);
	s->charge();
	switch (sres) {
		case SPELL_FAILED:
			return;
		case SPELL_OK:
			newsugg = new wchar_t[wlen + 1];
			wcscpy(newsugg, word);
			s->addSuggestion(newsugg, prio);
			return;
		case SPELL_CAP_FIRST:
			newsugg = new wchar_t[wlen + 1];
			newsugg[0] = towupper(word[0]);
			wcsncpy(newsugg + 1, word + 1, wlen - 1);
			newsugg[wlen] = L'\0';
			s->addSuggestion(newsugg, prio);
			return;
		case SPELL_CAP_ERROR:
			char * malaga_buffer = voikko_ucs4tocstr(word, "UTF-8", wlen);
			if (malaga_buffer == 0) return;
			analyse_item(malaga_buffer, MORPHOLOGY);
			delete[] malaga_buffer;
			s->charge();
			value_t analysis = first_analysis_result();
			if (!analysis) return;
			const char * analysis_str = get_value_string(analysis);
			newsugg = new wchar_t[wlen + 1];
			wcscpy(newsugg, word);
			size_t j = 0;
			for (size_t i = 0; i < wlen; i++) {
				while (analysis_str[j] == '=') j++;
				if (analysis_str[j] == '\0') break;
				if (analysis_str[j] == 'i' || analysis_str[j] == 'j')
					newsugg[i] = towupper(newsugg[i]);
				else if (analysis_str[j] == 'p' || analysis_str[j] == 'q')
					newsugg[i] = towlower(newsugg[i]);
				j++;
			}
			free((char *) analysis_str);
			s->addSuggestion(newsugg, prio);
			return;
	}
}

const wchar_t * BACK_VOWELS =  L"aouAOU";
const wchar_t * FRONT_VOWELS = L"\u00e4\u00f6y\u00c4\u00d6Y";

void suggest_vowel_change(SuggestionStatus * s) {
	int mask = 0;
	size_t vcount = 0;
	int pat = 1;
	for (size_t i = 0; i < s->getWordLength(); i++)
		for (int j = 0; j < 6; j++)
			if (s->getWord()[i] == BACK_VOWELS[j] ||
			    s->getWord()[i] == FRONT_VOWELS[j]) {
				vcount++;
				mask <<= 1;
				mask++;
				break;
			}
	if (vcount == 0 || vcount > 7) return;
	wchar_t * buffer = new wchar_t[s->getWordLength() + 1];
	while ((pat & mask) != 0) {
		size_t i = 0;
		wcscpy(buffer, s->getWord());
		for (size_t j = 0; j < vcount; j++) {
			while (!wcschr(BACK_VOWELS,  buffer[i]) &&
			       !wcschr(FRONT_VOWELS, buffer[i])) i++;
			if (pat & (1 << j)) {
				for (int k = 0; k < 6; k++) {
					if (buffer[i] == BACK_VOWELS[k]) {
						buffer[i] = FRONT_VOWELS[k];
						break;
					}
					if (buffer[i] == FRONT_VOWELS[k]) {
						buffer[i] = BACK_VOWELS[k];
						break;
					}
				}
			}
			i++;
		}
		if (s->shouldAbort()) {
			delete[] buffer;
			return;
		}
		suggest_correct_case(s, buffer, s->getWordLength());
		pat++;
	}
	delete[] buffer;
}

void suggest_word_split(SuggestionStatus * s) {
	int prio_part;
	int prio_total;
	wchar_t * part1 = new wchar_t[s->getWordLength() + 1];
	wcsncpy(part1, s->getWord(), s->getWordLength());
	part1[s->getWordLength()] = L'\0';

	for (size_t splitind = s->getWordLength() - 2; splitind >= 2; splitind--) {
		/* Do not split a word if there is a hyphen before the last character of part1
		   or after the first character of part2. Do not suggest splitting immediately
		   before or after a hyphen either. */
		if (s->getWord()[splitind-2] == L'-' || s->getWord()[splitind-1] == L'-' ||
		    s->getWord()[splitind]   == L'-' || s->getWord()[splitind+1] == L'-') continue;
		part1[splitind] = L'\0';
		spellresult part1_res = voikko_spell_with_priority(part1, splitind, &prio_total);
		s->charge();
		if (part1_res == SPELL_OK || part1_res == SPELL_CAP_FIRST) {
			spellresult part2_res = voikko_spell_with_priority(s->getWord() + splitind, s->getWordLength() - splitind, &prio_part);
			prio_total += prio_part;
			s->charge();
			if (part2_res == SPELL_OK || part2_res == SPELL_CAP_FIRST) {
				wchar_t * suggestion = new wchar_t[s->getWordLength() + 2];
				wcsncpy(suggestion, s->getWord(), splitind);
				if (part1_res == SPELL_CAP_FIRST)
					suggestion[0] = towupper(suggestion[0]);
				suggestion[splitind] = L' ';
				wcsncpy(suggestion + (splitind + 1), s->getWord() + splitind,
				        s->getWordLength() - splitind + 1);
				if (part2_res == SPELL_CAP_FIRST)
					suggestion[splitind+1] = towupper(suggestion[splitind+1]);
				s->addSuggestion(suggestion, prio_total);
			}
		}
		if (s->shouldAbort()) break;
	}

	delete[] part1;
}

/* ä=\u00e4, ö=\u00f6, å=\u00e5, š=\u0161, ž=\u017e, é=\u00e9, â=\u00e2 */


const wchar_t * STD_REPL_ORIG =
	L".aiittes"  L"snulkko\u00e4mrrvppyhjjddd\u00f6gggffbbccwwxz"  L"zq\u00e5\u00e5\u00e5\u00e5aitesnul"
	L"ko\u00e4mrvpyhjd\u00f6gfbcwxzq\u00e5a"  L"e"  L"a"  L"ks";
const wchar_t * STD_REPL_REPL = 
	L",suordr\u0161amiklgi\u00f6netbbotjhktsf\u00e4fhkgdpnvsevc\u017exao"  L"p"  L"\u00e4\u00f6ekysdhj\u00f6"
	L"jpp"  L"kdglhuiel"  L"tvvkasaka"  L"\u00e5\u00e9\u00e2cc";

const wchar_t * OCR_REPL_ORIG =
	L"0liuoa"  L"\u00e4o"  L"\u00f6s"  L"\u0161z"  L"\u017ee"  L"\u00e9a"  L"\u00e2pbefqonmuvocbh"
	L"___________________________"  L"_";
const wchar_t * OCR_REPL_REPL =
	L"oilou\u00e4a"  L"\u00f6o"  L"\u0161s"  L"\u017ez"  L"\u00e9e"  L"\u00e2a"  L"bpfeoqmnvucohb"
	L"abcdefghijklmnopqrstuvwxyz\u00e4\u00f6";

void suggest_replacement(SuggestionStatus * s, const wchar_t * from, const wchar_t * to, int count) {
	wchar_t * buffer = new wchar_t[s->getWordLength() + 1];
	wcsncpy(buffer, s->getWord(), s->getWordLength());
	buffer[s->getWordLength()] = L'\0';
	for (int i = 0; i < count; i++) {
		for (wchar_t * pos = wcschr(buffer, from[i]); pos != 0; pos = wcschr(pos+1, from[i])) {
			*pos = to[i];
			suggest_correct_case(s, buffer, s->getWordLength());
			if (s->shouldAbort()) break;
			*pos = from[i];
		}
		if (s->shouldAbort()) break;
		
		/* Only search for upper case letter if it differs from lower case version */
		wchar_t upper_from = towupper(from[i]);
		if (upper_from == from[i]) continue;
		for (wchar_t * pos = wcschr(buffer, upper_from); pos != 0;
		     pos = wcschr(pos + 1, upper_from)) {
			*pos = towupper(to[i]);
			suggest_correct_case(s, buffer, s->getWordLength());
			if (s->shouldAbort()) break;
			*pos = upper_from;
		}
		if (s->shouldAbort()) break;
	}
	delete[] buffer;
}

void suggest_deletion(SuggestionStatus * s) {
	wchar_t * buffer = new wchar_t[s->getWordLength()];
	for (size_t i = 0; i < s->getWordLength() && !s->shouldAbort(); i++) {
		if (i == 0 || towlower(s->getWord()[i]) != towlower(s->getWord()[i-1])) {
			wcsncpy(buffer, s->getWord(), i);
			wcsncpy(buffer + i, s->getWord() + (i + 1), s->getWordLength() - i);
			suggest_correct_case(s, buffer, s->getWordLength() - 1);
		}
	}
	delete[] buffer;
}

void suggest_insert_special(SuggestionStatus * s) {
	wchar_t * buffer = new wchar_t[s->getWordLength() + 2];
	wcsncpy(buffer + 1, s->getWord(), s->getWordLength());
	buffer[s->getWordLength()+1] = L'\0';
	
	/* suggest adding '-' */
	for (size_t j = 2; j <= s->getWordLength() - 2 && !s->shouldAbort(); j++) {
		/* Do not add hyphen if there already is another nearby */
		if (s->getWord()[j-2] == L'-' || s->getWord()[j-1] == L'-' ||
		    s->getWord()[j] ==   L'-' || s->getWord()[j+1] == L'-')
			continue;
		wcsncpy(buffer, s->getWord(), j);
		buffer[j] = L'-';
		suggest_correct_case(s, buffer, s->getWordLength() + 1);
	}
	/* suggest character duplication */
	wcsncpy(buffer + 1, s->getWord(), s->getWordLength() + 1);
	for (size_t j = 0; j < s->getWordLength() && !s->shouldAbort(); j++) {
		buffer[j] = s->getWord()[j];
		/* Do not duplicate if there already are two same letters */
		if (j < s->getWordLength() - 1 && s->getWord()[j] == s->getWord()[j+1]) {
			j++;
			continue;
		}
		/* These should not be duplicated */
		if (s->getWord()[j] == L'-' || s->getWord()[j] == L'\'') continue;
		suggest_correct_case(s, buffer, s->getWordLength() + 1);
	}
	delete[] buffer;
}

const wchar_t * INS_CHARS = L"aitesnulko\u00e4mrvpyhjd\u00f6gfbcw:xzq\u00e5";

void suggest_insertion(SuggestionStatus * s, int start, int end) {
	wchar_t * buffer = new wchar_t[s->getWordLength() + 2];
	for (int i = start; i <= end; i++) {
		buffer[0] = s->getWord()[0];
		wcsncpy(buffer + 1, s->getWord(), s->getWordLength());
		buffer[s->getWordLength()+1] = L'\0';
		for (size_t j = 0; j < s->getWordLength() && !s->shouldAbort(); j++) {
			if (j != 0) buffer[j-1] = s->getWord()[j-1];
			if ((wint_t) INS_CHARS[i] == towlower((s->getWord()[j]))) continue; /* avoid duplicates */
			if (j > 0 && (wint_t) INS_CHARS[i] == towlower((s->getWord()[j-1]))) continue; /* avoid duplicates */
			buffer[j] = INS_CHARS[i];
			suggest_correct_case(s, buffer, s->getWordLength() + 1);
		}
		if (s->shouldAbort()) break;
		if (INS_CHARS[i] == s->getWord()[s->getWordLength()-1]) continue;
		buffer[s->getWordLength()-1] = s->getWord()[s->getWordLength()-1];
		buffer[s->getWordLength()] = INS_CHARS[i];
		suggest_correct_case(s, buffer, s->getWordLength() + 1);
	}
	delete[] buffer;
}

void suggest_swap(SuggestionStatus * s) {
	size_t max_distance;
	if (s->getWordLength() <= 8) max_distance = 10;
	else max_distance = 50 / s->getWordLength();
	if (max_distance == 0) return;
	wchar_t * buffer = new wchar_t[s->getWordLength() + 1];
	wcsncpy(buffer, s->getWord(), s->getWordLength());
	buffer[s->getWordLength()] = L'\0';
	for (size_t i = 0; i < s->getWordLength() && !s->shouldAbort(); i++) {
		for (size_t j = i + 1; j < s->getWordLength() && !s->shouldAbort(); j++) {
			if (j - i > max_distance) break;
			/* do not suggest the same word */
			if (towlower(buffer[i]) == towlower(buffer[j])) continue;
			/* do not suggest swapping front and back vowels that have already been
			   tested earlier */
			int k;
			for (k = 0; k < 3; k++) {
				if ((towlower(buffer[i]) == (wint_t) BACK_VOWELS[k] &&
				     towlower(buffer[j]) == (wint_t) FRONT_VOWELS[k]) ||
				    (towlower(buffer[i]) == (wint_t) FRONT_VOWELS[k] &&
				     towlower(buffer[j]) == (wint_t) BACK_VOWELS[k])) break;
			}
			if (k < 3) continue;
			buffer[i] = s->getWord()[j];
			buffer[j] = s->getWord()[i];
			suggest_correct_case(s, buffer, s->getWordLength());
			buffer[i] = s->getWord()[i];
			buffer[j] = s->getWord()[j];
		}
	}
	delete[] buffer;
}

wchar_t ** getSuggestions(SuggestionStatus & status, bool addDot) {
	const Suggestion * const originalSuggestions = status.getSuggestions();
	size_t returnedSuggestionCount = MAX_SUGGESTIONS < status.getSuggestionCount() ?
	                                 MAX_SUGGESTIONS : status.getSuggestionCount();
	wchar_t ** suggestions = new wchar_t*[returnedSuggestionCount + 1];
	for (size_t i = 0; i < returnedSuggestionCount; i++) {
		size_t sugglen = wcslen(originalSuggestions[i].word);
		wchar_t * buffer = new wchar_t[sugglen + 1 + (addDot ? 1 : 0)];
		wcsncpy(buffer, originalSuggestions[i].word, sugglen);
		if (addDot) {
			buffer[sugglen] = L'.';
			buffer[sugglen+1] = L'\0';
		}
		else {
			buffer[sugglen] = L'\0';
		}
		suggestions[i] = buffer;
	}
	suggestions[returnedSuggestionCount] = 0;
	return suggestions;
}

VOIKKOEXPORT wchar_t ** voikko_suggest_ucs4(int handle, const wchar_t * word) {
	bool add_dots = false;
	if (word == 0) return 0;
	size_t wlen = wcslen(word);
	if (wlen <= 1 || wlen > LIBVOIKKO_MAX_WORD_CHARS) return 0;
	
	ENTER_V
	wchar_t * nword = voikko_normalise(word, wlen);
	if (nword == 0) {
		EXIT_V
		return 0;
	}
	wlen = wcslen(nword);
	
	if (voikko_options.ignore_dot) {
		if (wlen == 2) {
			delete[] nword;
			EXIT_V
			return 0;
		}
		if (nword[wlen-1] == L'.') {
			nword[--wlen] = L'\0';
			add_dots = true;
		}
	}
	
	size_t maxCost = COST_LIMIT;
	if (voikko_options.suggestion_type == ST_OCR) {
		maxCost = maxCost * 3;
	}
	
	SuggestionStatus status(handle, nword, wlen, MAX_SUGGESTIONS * 3, maxCost);
	
	suggest_correct_case(&status, 0, 0);
	if (status.getSuggestionCount() > 0) {
		delete[] nword;
		EXIT_V
		return getSuggestions(status, add_dots);
	}
	
	if (voikko_options.suggestion_type == ST_OCR) {
		suggest_replacement(&status, OCR_REPL_ORIG, OCR_REPL_REPL, 59);
	}
	else {
		suggest_vowel_change(&status);
		if (!status.shouldAbort()) suggest_replacement(&status, STD_REPL_ORIG, STD_REPL_REPL, 52);
		if (!status.shouldAbort()) suggest_deletion(&status);
		if (!status.shouldAbort()) suggest_insert_special(&status);
		if (!status.shouldAbort()) suggest_word_split(&status);
		if (!status.shouldAbort()) suggest_insertion(&status, 0, 5);
		if (!status.shouldAbort()) suggest_swap(&status);
		if (!status.shouldAbort()) suggest_replacement(&status, STD_REPL_ORIG + 52, STD_REPL_REPL + 52, 30);
		if (!status.shouldAbort()) suggest_insertion(&status, 6, 29);
	}

	if (status.getSuggestionCount() == 0) {
		delete[] nword;
		EXIT_V
		return 0;
	}
	
	status.sortSuggestions();

	wchar_t ** suggestions = getSuggestions(status, add_dots);

	/* Change the character case to match the original word */
	enum casetype origcase = voikko_casetype(nword, wlen);
	size_t suglen;
	if (origcase == CT_FIRST_UPPER) {
		size_t i = 0;
		while (suggestions[i] != 0) {
			suglen = wcslen(suggestions[i]);
			if (voikko_casetype(suggestions[i], suglen) == CT_ALL_LOWER)
				voikko_set_case(CT_FIRST_UPPER, suggestions[i], suglen);
			i++;
		}
	}
	if (origcase == CT_ALL_UPPER) {
		size_t i = 0;
		while (suggestions[i] != 0) {
			suglen = wcslen(suggestions[i]);
			voikko_set_case(CT_ALL_UPPER, suggestions[i], suglen);
			i++;
		}
	}

	/* Undo character set normalisation */
	for (size_t i = 0; suggestions[i] != 0;) {
		suglen = wcslen(suggestions[i]);
		voikko_cset_reformat(word, wlen, &(suggestions[i]), suglen);
		i++;
	}

	delete[] nword;
	EXIT_V
	return suggestions;
}

VOIKKOEXPORT char ** voikko_suggest_cstr(int handle, const char * word) {
	if (word == 0 || word[0] == '\0') return 0;
	size_t len = strlen(word);
	if (len > LIBVOIKKO_MAX_WORD_CHARS) return 0;
	wchar_t * word_ucs4 = voikko_cstrtoucs4(word, voikko_options.encoding, len);
	if (word_ucs4 == 0) return 0;
	wchar_t ** suggestions_ucs4 = voikko_suggest_ucs4(handle, word_ucs4);
	delete[] word_ucs4;
	if (suggestions_ucs4 == 0) return 0;
	int scount = 0;
	while (suggestions_ucs4[scount] != 0) scount++;
	
	char ** suggestions = new char*[scount + 1];
	if (suggestions == 0) {
		voikko_free_suggest_ucs4(suggestions_ucs4);
		return 0;
	}
	
	int j = 0;
	for (int i = 0; i < scount; i++) {
		char * suggestion = voikko_ucs4tocstr(suggestions_ucs4[i], voikko_options.encoding, 0);
		if (suggestion == 0) continue; /* suggestion cannot be encoded */
		suggestions[j++] = suggestion;
	}
	voikko_free_suggest_ucs4(suggestions_ucs4);
	if (j == 0) {
		delete[] suggestions;
		return 0;
	}
	for (; j <= scount; j++) {
		suggestions[j] = 0;
	}
	
	// Convert to C allocation to maintain compatibility with some
	// broken applications before libvoikko 1.5.
	utils::StringUtils::convertCStringArrayToMalloc(suggestions);
	return suggestions;
}

VOIKKOEXPORT void voikko_free_suggest_ucs4(wchar_t ** suggest_result) {
	if (suggest_result) {
		for (wchar_t ** p = suggest_result; *p; p++) {
			delete[] *p;
		}
		delete[] suggest_result;
	}
}

VOIKKOEXPORT void voikko_free_suggest_cstr(char ** suggest_result) {
	// C deallocation is used here to maintain compatibility with some
	// broken applications before libvoikko 1.5.
	if (suggest_result) {
		for (char ** p = suggest_result; *p; p++) {
			free(*p);
		}
		free(suggest_result);
	}
}

}
