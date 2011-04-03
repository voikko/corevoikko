/* Libvoikko: Library of Finnish language tools
 * Copyright (C) 2010 - 2011 Harri Pitkänen <hatapitk@iki.fi>
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
#include "hyphenator/AnalyzerToFinnishHyphenatorAdapter.hpp"
#include "character/SimpleChar.hpp"
#include "utils/utils.hpp"

using namespace std;
using namespace libvoikko::morphology;
using namespace libvoikko::character;

namespace libvoikko { namespace spellchecker {

// TODO: stop passing voikkoOptions here
FinnishSpellerTweaksWrapper::FinnishSpellerTweaksWrapper(Speller * speller, Analyzer * analyzer,
	 voikko_options_t * voikkoOptions) :
	speller(speller), analyzer(analyzer),
	hyphenator(new hyphenator::AnalyzerToFinnishHyphenatorAdapter(analyzer)),
	voikkoOptions(voikkoOptions) {
	hyphenator->setUglyHyphenation(true);
	hyphenator->setHyphenateUnknown(true);
	hyphenator->setMinHyphenatedWordLength(3);
	hyphenator->setIgnoreDot(true);
}

spellresult FinnishSpellerTweaksWrapper::spellWithoutSoftHyphen(const wchar_t * word, size_t wlen) {
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
			spellresult spres = spellWithoutSoftHyphen(buffer, wlen - 1);
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
		
		// "ja-sana" and such are valid if "ja" is any valid word and "sana" has MALAGA_VAPAA_JALKIOSA=true
		for (size_t i = wlen - 2; i > 0; --i) {
			if (word[i] == L'-') {
				spellresult leadingResult = spell(word, i);
				if (leadingResult != SPELL_FAILED) {
					list<Analysis *> * trailingAnalyses = analyzer->analyze(word + i + 1);
					list<Analysis *>::const_iterator it = trailingAnalyses->begin();
					bool isTrailingAcceptable = false;
					while (it != trailingAnalyses->end()) {
						const wchar_t * trailingAttr = (*it)->getValue("MALAGA_VAPAA_JALKIOSA");
						if (trailingAttr != 0 && wcscmp(trailingAttr, L"true") == 0) {
							isTrailingAcceptable = true;
							break;
						}
						++it;
					}
					Analyzer::deleteAnalyses(trailingAnalyses);
					if (isTrailingAcceptable) {
						delete[] buffer;
						// TODO: not entirely accurate for character case checks.
						// We did not check the case of trailing part at all.
						return leadingResult;
					}
				}
				break;
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
			++it;
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

spellresult FinnishSpellerTweaksWrapper::spell(const wchar_t * word, size_t wlen) {
	const wchar_t * softHyphen = wmemchr(word, L'\u00AD', wlen);
	if (softHyphen) {
		wchar_t * buffer = new wchar_t[wlen];
		list<size_t> shyPositions;
		size_t j = 0;
		for (size_t i = 0; i < wlen; ++i) {
			if (word[i] != L'\u00AD') {
				buffer[j++] = word[i];
			} else {
				if (j == 0 || i + 1 == wlen || (!shyPositions.empty() && shyPositions.back() == j)) {
					delete [] buffer;
					return SPELL_FAILED;
				}
				shyPositions.push_back(j);
			}
		}
		buffer[j] = L'\0';
		spellresult resultWoShy = spellWithoutSoftHyphen(buffer, j);
		if (resultWoShy != SPELL_FAILED) {
			// check if positions of all soft hyphens are acceptable
			char * hyphenPositions = hyphenator->allPossibleHyphenPositions(buffer, j);
			delete[] buffer;
			if (!hyphenPositions) {
				return SPELL_FAILED;
			}
			for (list<size_t>::iterator it = shyPositions.begin(); it != shyPositions.end(); ++it) {
				if (hyphenPositions[*it] != '-') {
					delete[] hyphenPositions;
					return SPELL_FAILED;
				}
			}
			delete[] hyphenPositions;
		} else {
			delete[] buffer;
		}
		return resultWoShy;
	} else {
		return spellWithoutSoftHyphen(word, wlen);
	}
}

void FinnishSpellerTweaksWrapper::terminate() {
	delete hyphenator;
	delete speller;
}

} }
