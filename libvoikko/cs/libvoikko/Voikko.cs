using System;
using System.Runtime.InteropServices;
using System.Collections.Generic;
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

		[DllImport(DLL_LIB)]
		public static extern IntPtr voikko_list_dicts(byte[] path);

		[DllImport(DLL_LIB)]
		public static extern void voikko_free_dicts(IntPtr dict);

		[DllImport(DLL_LIB)]
		public static extern IntPtr voikko_dict_language(IntPtr dict);

		[DllImport(DLL_LIB)]
		public static extern IntPtr voikko_dict_variant(IntPtr dict);

		[DllImport(DLL_LIB)]
		public static extern IntPtr voikko_dict_description(IntPtr dict);
		
		[DllImport(DLL_LIB)]
		public static extern IntPtr voikkoSuggestCstr(IntPtr handle, byte[] word);
		
		[DllImport(DLL_LIB)]
		public static extern void voikkoFreeCstrArray(IntPtr array);
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
				throw new VoikkoException(ByteArray.n2s(error));
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

		public static List<Dictionary> listDicts()
		{
			return listDicts(null);
		}

		public static List<Dictionary> listDicts(string path)
		{
			List<Dictionary> dicts = new List<Dictionary>();
			IntPtr cDicts = Libvoikko.voikko_list_dicts(ByteArray.s2n(path));
			unsafe
			{
				for (void** cDict = (void**)cDicts; *cDict != (void*)0; cDict++)
				{
					dicts.Add(new Dictionary(ByteArray.n2s(Libvoikko.voikko_dict_language(new IntPtr(*cDict))), ByteArray.n2s(Libvoikko.voikko_dict_variant(new IntPtr(*cDict))), ByteArray.n2s(Libvoikko.voikko_dict_description(new IntPtr(*cDict)))));
				}
			}
			Libvoikko.voikko_free_dicts(cDicts);
			return dicts;
		}

		public List<string> Suggest(string word)
		{
			lock (lockObj)
			{
				requireValidHandle();
				IntPtr voikkoSuggestCstr = Libvoikko.voikkoSuggestCstr(handle, ByteArray.s2n(word));
				if (voikkoSuggestCstr == IntPtr.Zero)
				{
					return new List<string>();
				}
				List<string> suggestions = new List<string>();
				unsafe
				{
					for (byte** cStr = (byte**) voikkoSuggestCstr; *cStr != (byte*)0; cStr++)
					{
						suggestions.Add(ByteArray.n2s(new IntPtr(*cStr)));
					}
				}
				Libvoikko.voikkoFreeCstrArray(voikkoSuggestCstr);
				return suggestions;
			}
		}
		
	}
	
	
}
