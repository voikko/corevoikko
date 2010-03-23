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

#include "setup/setup.hpp"
#include "utils/StringUtils.hpp"
#include "sentence/Sentence.hpp"
#include "porting.h"

namespace libvoikko { namespace sentence {

VOIKKOEXPORT enum voikko_sentence_type voikkoNextSentenceStartUcs4(voikko_options_t * options,
                          const wchar_t * text, size_t textlen, size_t * sentencelen) {
	return Sentence::next(options, text, textlen, sentencelen);
}

VOIKKOEXPORT enum voikko_sentence_type voikkoNextSentenceStartCstr(voikko_options_t * options, const char * text,
                          size_t textlen, size_t * sentencelen) {
	enum voikko_sentence_type result;
	if (text == 0) {
		return SENTENCE_NONE;
	}
	wchar_t * textUcs4 = utils::StringUtils::ucs4FromUtf8(text, textlen);
	if (textUcs4 == 0) {
		return SENTENCE_NONE;
	}
	result = voikkoNextSentenceStartUcs4(options, textUcs4, wcslen(textUcs4),
	                                         sentencelen);
	delete[] textUcs4;
	return result;
}

} }
