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
using System.Collections.Generic;
namespace libvoikko
{
	public class GrammarError
	{
		private readonly int errorCode;
		private readonly int startPos;
		private readonly int errorLen;
		private readonly List<string> suggestions;

		public GrammarError(int errorCode, int startPos, int errorLen, List<string> suggestions)
		{
			this.errorCode = errorCode;
			this.startPos = startPos;
			this.errorLen = errorLen;
			this.suggestions = suggestions;
		}
		
		public int ErrorCode
		{
			get
			{
				return errorCode;
			}
		}
		
		public int StartPos
		{
			get
			{
				return startPos;
			}
		}
		
		public int ErrorLen
		{
			get
			{
				return errorLen;
			}
		}
		
		public List<string> Suggestions
		{
			get
			{
				return suggestions;
			}
		}
	}
}

