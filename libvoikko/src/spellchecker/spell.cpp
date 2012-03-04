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
 * Portions created by the Initial Developer are Copyright (C) 2006 - 2010
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

#include "utils/utils.hpp"
#include "utils/StringUtils.hpp"
#include "character/charset.hpp"
#include "character/SimpleChar.hpp"
#include "spellchecker/Speller.hpp"
#include "setup/setup.hpp"
#include "porting.h"
#include <cstdlib>
#include <cstring>
#include <cwchar>

using namespace libvoikko::spellchecker;
using namespace libvoikko::character;
using namespace std;

namespace libvoikko {

/**
 * Checks the spelling of given word. Missing hyphens at the start or end of the
 * word are ignored if requested in voikkoOptions.
 * @param word word to check (does not need to be null terminated)
 * @param len length of the word to check
 * @return spelling result
 */
static spellresult hyphenAwareSpell(voikko_options_t * voikkoOptions,
	                                   const wchar_t * word, size_t len) {
	spellresult spres = voikkoOptions->speller->spell(word, len);
	if (spres != SPELL_FAILED || !voikkoOptions->accept_missing_hyphens) {
		return spres;
	}
	
	// Hyphens were already present, so we cannot do anything more
	if (len < 2 || (word[0] == L'-' && word[len - 1] == L'-')) return SPELL_FAILED;
	
	wchar_t * buffer = new wchar_t[len + 2];
	size_t newlen = len + 1;
	if (word[0] == L'-') {
		wcsncpy(buffer, word, len);
		buffer[len] = L'-';
	}
	else {
		buffer[0] = L'-';
		wcsncpy(buffer + 1, word, len);
		if (word[len - 1] != L'-') {
			buffer[len + 1] = L'-';
			newlen++;
		}
	}
	spres = voikkoOptions->speller->spell(buffer, newlen);
	delete[] buffer;
	return spres;
}


/** Checks the spelling of given word and uses cache if possible
 * @param word word to check. Word does not need to be null terminated and it must
 *             not contain upper case letters
 * @param len length of the word to check
 * @return spelling result
 */
static spellresult voikko_cached_spell(voikko_options_t * voikkoOptions, const wchar_t * buffer, size_t len) {
	SpellerCache * cache = voikkoOptions->spellerCache;
	if (cache) {
		if (cache->isInCache(buffer, len)) {
			/* is in cache */
			return cache->getSpellResult(buffer, len);
		}
		/* not in cache */
		spellresult result = hyphenAwareSpell(voikkoOptions, buffer, len);
		cache->setSpellResult(buffer, len, result);
		return result;
	}
	/* no cache available */
	return hyphenAwareSpell(voikkoOptions, buffer, len);
}


VOIKKOEXPORT int voikkoSpellUcs4(voikko_options_t * voikkoOptions, const wchar_t * word) {
	size_t nchars = wcslen(word);
	int result;
	spellresult sres;
	if (nchars == 0) {
		return VOIKKO_SPELL_OK;
	}
	if (nchars > LIBVOIKKO_MAX_WORD_CHARS) {
		return VOIKKO_SPELL_FAILED;
	}
	
	wchar_t * nword = voikko_normalise(word, nchars);
	if (nword == 0) {
		return VOIKKO_INTERNAL_ERROR;
	}
	nchars = wcslen(nword);
	
	if (voikkoOptions->ignore_numbers) {
		for (size_t i = 0; i < nchars; i++) {
			if (SimpleChar::isDigit(nword[i])) {
				delete[] nword;
				return VOIKKO_SPELL_OK;
			}
		}
	}
	casetype caps = voikko_casetype(nword, nchars);
	if ((voikkoOptions->ignore_uppercase && caps == CT_ALL_UPPER) ||
	    (voikkoOptions->ignore_nonwords && voikko_is_nonword(nword, nchars))) {
		delete[] nword;
		return VOIKKO_SPELL_OK;
	}
	if (caps == CT_ALL_UPPER && !voikkoOptions->accept_all_uppercase) {
		// all upper case is nothing special
		caps = CT_COMPLEX;
	}
	
	wchar_t * buffer = new wchar_t[nchars + 1];

	for (size_t i = 0; i < nchars; i++) {
		buffer[i] = SimpleChar::lower(nword[i]);
	}
	buffer[nchars] = L'\0';
	
	int dot_index;
	size_t realChars;
	if (voikkoOptions->ignore_dot && buffer[nchars - 1] == L'.') {
		dot_index = static_cast<int>(nchars - 1);
		buffer[dot_index] = L'\0';
		realChars = nchars - 1;
	}
	else {
		dot_index = -1;
		realChars = nchars;
	}
	
	/* Check words that require exact captialisation */
	if (caps == CT_COMPLEX || caps == CT_NO_LETTERS) {
		wcsncpy(buffer, nword, nchars);
		buffer[0] = SimpleChar::lower(buffer[0]);
		sres = hyphenAwareSpell(voikkoOptions, buffer, nchars);
		if (sres == SPELL_OK ||
		    (sres == SPELL_CAP_FIRST && voikkoOptions->accept_first_uppercase && SimpleChar::isUpper(nword[0]))) {
			result = VOIKKO_SPELL_OK;
		}
		else {
			result = VOIKKO_SPELL_FAILED;
		}
		if (result == VOIKKO_SPELL_FAILED && dot_index != -1) { /* remove dot */
			buffer[dot_index] = L'\0';
			sres = hyphenAwareSpell(voikkoOptions, buffer, realChars);
			if (sres == SPELL_OK ||
			    (sres == SPELL_CAP_FIRST && voikkoOptions->accept_first_uppercase && SimpleChar::isUpper(nword[0]))) {
				result = VOIKKO_SPELL_OK;
			}
		}
		delete[] nword;
		delete[] buffer;
		return result;
	}
	
	
	/* Check without trailing dot */
	sres = voikko_cached_spell(voikkoOptions, buffer, realChars);
	switch (caps) {
		case CT_ALL_LOWER:
			result = (sres == SPELL_OK) ? VOIKKO_SPELL_OK : VOIKKO_SPELL_FAILED;
			break;
		case CT_FIRST_UPPER:
			if ((sres == SPELL_OK && voikkoOptions->accept_first_uppercase) || sres == SPELL_CAP_FIRST)
				result = VOIKKO_SPELL_OK;
			else result = VOIKKO_SPELL_FAILED;
			break;
		case CT_ALL_UPPER:
			result = (sres == SPELL_FAILED) ? VOIKKO_SPELL_FAILED : VOIKKO_SPELL_OK;
			break;
		default: /* should not happen */
			result = VOIKKO_INTERNAL_ERROR;
	}
	if (result == VOIKKO_SPELL_OK) {
		delete[] nword;
		delete[] buffer;
		return VOIKKO_SPELL_OK;
	}
	
	/* Check with trailing dot */
	if (dot_index != -1) {
		buffer[dot_index] = L'.';
		switch (caps) {
			case CT_ALL_LOWER:
				sres = voikko_cached_spell(voikkoOptions, buffer, nchars);
				result = (sres == SPELL_OK) ? VOIKKO_SPELL_OK : VOIKKO_SPELL_FAILED;
				break;
			case CT_FIRST_UPPER:
				sres = voikko_cached_spell(voikkoOptions, buffer, nchars);
				if ((sres == SPELL_OK && voikkoOptions->accept_first_uppercase) || sres == SPELL_CAP_FIRST)
					result = VOIKKO_SPELL_OK;
				else result = VOIKKO_SPELL_FAILED;
				break;
			case CT_ALL_UPPER:
				sres = voikko_cached_spell(voikkoOptions, buffer, nchars);
				result = (sres == SPELL_FAILED) ? VOIKKO_SPELL_FAILED : VOIKKO_SPELL_OK;
				break;
			default: /* should not happen */
				result = VOIKKO_INTERNAL_ERROR;
		}
	}
	delete[] nword;
	delete[] buffer;
	return result;
}

VOIKKOEXPORT int voikkoSpellCstr(voikko_options_t * handle, const char * word) {
	if (word == 0 || word[0] == '\0') {
		return VOIKKO_SPELL_OK;
	}
	size_t len = strlen(word);
	if (len > LIBVOIKKO_MAX_WORD_CHARS) {
		return VOIKKO_SPELL_FAILED;
	}
	wchar_t * word_ucs4 = utils::StringUtils::ucs4FromUtf8(word, len);
	if (word_ucs4 == 0) {
		return VOIKKO_CHARSET_CONVERSION_FAILED;
	}
	int result = voikkoSpellUcs4(handle, word_ucs4);
	delete[] word_ucs4;
	return result;
}

}
