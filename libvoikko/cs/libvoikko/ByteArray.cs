using System;
using System.Text;
namespace libvoikko
{
	public static class ByteArray
	{
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

