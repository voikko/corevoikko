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
 * Portions created by the Initial Developer are Copyright (C) 2007 - 2010
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

VOIKKOEXPORT enum voikko_token_type voikkoNextTokenCstr(voikko_options_t * options, const char * text, size_t textlen,
                                                           size_t * tokenlen) {
	if (text == 0) return TOKEN_NONE;
	wchar_t * text_ucs4 = utils::StringUtils::ucs4FromUtf8(text, textlen);
	if (text_ucs4 == 0) return TOKEN_NONE;
	voikko_token_type result =
		voikkoNextTokenUcs4(options, text_ucs4, wcslen(text_ucs4), tokenlen);
	delete[] text_ucs4;
	return result;
}

} }
