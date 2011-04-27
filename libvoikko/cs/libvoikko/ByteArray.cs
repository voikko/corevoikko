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

