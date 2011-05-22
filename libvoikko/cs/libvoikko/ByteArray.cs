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
using System.Text;
using System.Collections.Generic;
namespace libvoikko
{
	public static class ByteArray
	{
		public static string n2s(IntPtr ptr)
		{
			if (ptr == IntPtr.Zero)
			{
				return null;
			}
			List<byte> bytes = new List<byte>();
			unsafe
			{
				for (byte * p = (byte *) ptr; *p != 0; p++)
				{
					bytes.Add(*p);
				}
			}
			return n2s(bytes.ToArray());
		}
		
		public static string n2s(byte[] bytes)
		{
			if (bytes == null)
			{
				return null;
			}
			return Encoding.UTF8.GetString(bytes);
		}
		
		public static byte[] s2n(string str)
		{
			if (str == null)
			{
				return null;
			}
			return Encoding.UTF8.GetBytes(str + '\0');
		}
	}
}

