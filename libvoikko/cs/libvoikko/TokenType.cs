/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2011 Harri Pitkänen <hatapitk@iki.fi>
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

using System;
namespace libvoikko
{
	public enum TokenType
	{
		/// <summary>
		/// End of text or error
		/// </summary>
		NONE = 0,
		/// <summary>
		/// Word
		/// </summary>
		WORD = 1,
		/// <summary>
		/// Punctuation
		/// </summary>
		PUNCTUATION = 2,
		/// <summary>
		/// Whitespace
		/// </summary>
		WHITESPACE = 3,
		/// <summary>
		/// Character not used in any of the supported natural languages
		/// </summary>
		UNKNOWN = 4
	}
}
