/* Libvoikko: Library of Finnish language tools
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

#ifndef VOIKKO_TOKENIZER_TOKENIZER_HPP
#define VOIKKO_TOKENIZER_TOKENIZER_HPP

#include <cstring>
#include "voikko_enums.h"
#include "setup/setup.hpp"

namespace libvoikko { namespace tokenizer {

class Tokenizer {
	public:
		static voikko_token_type nextToken(voikko_options_t * options, const wchar_t * text,
                                           size_t textlen, size_t * tokenlen);
};

} }

#endif
