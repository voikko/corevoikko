/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2007 - 2010 Harri Pitkänen <hatapitk@iki.fi>
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

#include "porting.h"
#include "setup/setup.hpp"
#include "utils/StringUtils.hpp"
#include "tokenizer/Tokenizer.hpp"
#include <cwchar>

namespace libvoikko { namespace tokenizer {

VOIKKOEXPORT enum voikko_token_type voikkoNextTokenUcs4(voikko_options_t * options,
		const wchar_t * text, size_t textlen, size_t * tokenlen) {
	return Tokenizer::nextToken(options, text, textlen, tokenlen);
}

VOIKKOEXPORT enum voikko_token_type voikko_next_token_cstr(int /*handle*/, const char * text, size_t textlen,
                                                           size_t * tokenlen) {
	if (text == 0) return TOKEN_NONE;
	wchar_t * text_ucs4 = utils::StringUtils::ucs4FromUtf8(text, textlen);
	if (text_ucs4 == 0) return TOKEN_NONE;
	voikko_token_type result =
		voikkoNextTokenUcs4(&voikko_options, text_ucs4, wcslen(text_ucs4), tokenlen);
	delete[] text_ucs4;
	return result;
}

} }
