/* Libvoikko: Library of Finnish language tools
 * Copyright (C) 2010 Harri Pitkänen <hatapitk@iki.fi>
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

#include "spellchecker/FinnishSpellerTweaksWrapper.hpp"
#include "spellchecker/SpellUtils.hpp"
#include "character/SimpleChar.hpp"
#include "utils/utils.hpp"

using namespace std;
using namespace libvoikko::morphology;
using namespace libvoikko::character;

namespace libvoikko { namespace spellchecker {

// TODO: stop passing voikkoOptions here
FinnishSpellerTweaksWrapper::FinnishSpellerTweaksWrapper(Speller * speller, Analyzer * analyzer,
	 voikko_options_t * voikkoOptions) :
	speller(speller), analyzer(analyzer), voikkoOptions(voikkoOptions) { }

spellresult FinnishSpellerTweaksWrapper::spell(const wchar_t * word, size_t wlen) {
	spellresult result_with_border = SPELL_FAILED;
	spellresult result_without_border = SPELL_FAILED;
	
	spellresult result = speller->spell(word, wlen);
	const wchar_t * hyphen_pos;
	if (result != SPELL_OK && wlen > 3) {
		hyphen_pos = wmemchr(word + 1, L'-', wlen - 2);
	}
	else {
		hyphen_pos = 0;
	}
	
	if (hyphen_pos) { /* Check optional hyphens */
		size_t leading_len = hyphen_pos - word;
		wchar_t * buffer = new wchar_t[wlen];
		wcsncpy(buffer, word, leading_len);
		wcsncpy(buffer + leading_len, hyphen_pos + 1, wlen - leading_len - 1);
		buffer[wlen - 1] = L'\0';
		
		if (voikkoOptions->accept_extra_hyphens && leading_len > 1 &&
		    buffer[leading_len] != L'-') {
			/* All hyphens are optional */
			/* FIXME: deep recursion */
			spellresult spres = spell(buffer, wlen - 1);
			if (spres == SPELL_OK) {
				delete[] buffer;
				return spres;
			}
		}
		
		/* Leading part ends with the same VC pair as the trailing part starts ('pop-opisto') */
		if (leading_len >= 2 && wlen - leading_len >= 3) {
			wchar_t vctest1 = SimpleChar::lower(word[leading_len - 2]);
			wchar_t vctest2 = SimpleChar::lower(word[leading_len - 1]);
			if (wcschr(VOIKKO_VOWELS, vctest1) &&
			    wcschr(VOIKKO_CONSONANTS, vctest2) &&
			    SimpleChar::lower(word[leading_len + 1]) == vctest1 &&
			    SimpleChar::lower(word[leading_len + 2]) == vctest2) {
				spellresult spres = speller->spell(buffer, wlen - 1);
				if (spres != SPELL_FAILED && (result == SPELL_FAILED || result > spres)) {
					delete[] buffer;
					return spres;
				}
			}
		}
		
		/* Ambiguous compound ('syy-silta', 'syys-ilta') */
		list<Analysis *> * analyses = analyzer->analyze(buffer);
		
		if (analyses->empty()) {
			Analyzer::deleteAnalyses(analyses);
			delete[] buffer;
			return result;
		}
		
		list<Analysis *>::const_iterator it = analyses->begin();
		while (it != analyses->end()) {
			const wchar_t * structure = (*it)->getValue("STRUCTURE");
			size_t j = 0;
			size_t i;
			for (i = 0; i < leading_len; i++) {
				while (structure[j] == L'=') {
					j++;
				}
				if (structure[j] == L'\0') {
					break;
				}
				j++;
			}
			if (i == leading_len) {
				spellresult spres = SpellUtils::matchWordAndAnalysis(buffer, wlen - 1, structure);
				if (structure[j] == L'=' && (result_with_border == SPELL_FAILED ||
				    result_with_border > spres)) {
					result_with_border = spres;
				}
				if (structure[j] != L'=' && (result_without_border == SPELL_FAILED ||
				    result_without_border > spres)) {
					result_without_border = spres;
				}
			}
			it++;
		}
		
		Analyzer::deleteAnalyses(analyses);
		delete[] buffer;
		if (result_with_border != SPELL_FAILED && result_without_border != SPELL_FAILED &&
		    (result == SPELL_FAILED || result > result_with_border)) {
			return result_with_border;
		}
	}
	
	return result;
}

void FinnishSpellerTweaksWrapper::terminate() {
	delete speller;
}

} }
