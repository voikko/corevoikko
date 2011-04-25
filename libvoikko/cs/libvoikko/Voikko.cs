using System;
using System.Runtime.InteropServices;
namespace libvoikko
{

	public static class Libvoikko
	{
		private const string DLL_LIB = "voikko";

		[DllImport(DLL_LIB)]
		public static extern IntPtr voikkoInit(ref IntPtr error, byte[] langCode, byte[] path);
	}

	public class Voikko : IDisposable
	{

		IntPtr handle;

		public Voikko(String language, String path)
		{
			IntPtr error = new IntPtr();
			handle = Libvoikko.voikkoInit(ref error, ByteArray.s2n(language), ByteArray.s2n(path));
			if (handle == IntPtr.Zero && error != IntPtr.Zero)
			{
				throw new VoikkoException("TODO error message");
			}
		}

		/// <summary>
		/// Creates a new Voikko instance using only the default dictionary search path
		/// </summary>
		/// <param name="language">
		/// BCP 47 language tag to be used
		/// </param>
		public Voikko(String language) : this(language, null)
		{
		}

		public void Dispose()
		{
			// TODO
		}
	}
	
	
}
