using System;
using System.Runtime.InteropServices;
namespace libvoikko
{

	public static class Libvoikko
	{
		private const string DLL_LIB = "voikko";
		public const int VOIKKO_SPELL_FAILED = 0;
		public const int VOIKKO_SPELL_OK = 1;
		public const int VOIKKO_INTERNAL_ERROR = 2;
		public const int VOIKKO_CHARSET_CONVERSION_FAILED = 3;

		[DllImport(DLL_LIB)]
		public static extern IntPtr voikkoInit(ref IntPtr error, byte[] langCode, byte[] path);
		
		[DllImport(DLL_LIB)]
		public static extern void voikkoTerminate(IntPtr handle);
		
		[DllImport(DLL_LIB)]
		public static extern int voikkoSpellCstr(IntPtr handle, byte[] word);
	}

	public class Voikko : IDisposable
	{

		IntPtr handle;
		private readonly Object lockObj = new Object();

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
			if (handle != IntPtr.Zero)
			{
				Libvoikko.voikkoTerminate(handle);
				handle = IntPtr.Zero;
			}
		}
		
		public bool Spell(string word)
		{
			lock (lockObj)
			{
				requireValidHandle();
				int spellResult = Libvoikko.voikkoSpellCstr(handle, ByteArray.s2n(word));
				return (spellResult == Libvoikko.VOIKKO_SPELL_OK);
			}
		}
		
		private void requireValidHandle()
		{
			if (handle == IntPtr.Zero)
			{
				throw new VoikkoException("Attempt to use disposed Voikko instance");
			}
		}
}
	
	
}
