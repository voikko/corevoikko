using System;
using System.Runtime.InteropServices;
using System.Collections.Generic;
using System.Text.RegularExpressions;
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

		[DllImport(DLL_LIB)]
		public static extern IntPtr voikkoNextGrammarErrorCstr(IntPtr handle, byte[] text, IntPtr textLen, IntPtr startPos, int skipErrors);

		[DllImport(DLL_LIB)]
		public static extern void voikkoFreeGrammarError(IntPtr error);

		[DllImport(DLL_LIB)]
		public static extern int voikkoGetGrammarErrorCode(IntPtr error);

		[DllImport(DLL_LIB)]
		public static extern IntPtr voikkoGetGrammarErrorStartPos(IntPtr error);

		[DllImport(DLL_LIB)]
		public static extern IntPtr voikkoGetGrammarErrorLength(IntPtr error);

		[DllImport(DLL_LIB)]
		public static extern IntPtr voikkoGetGrammarErrorSuggestions(IntPtr error);

		[DllImport(DLL_LIB)]
		public static extern IntPtr voikko_error_message_cstr(int errorCode, byte[] language);

		[DllImport(DLL_LIB)]
		public static extern IntPtr voikkoAnalyzeWordCstr(IntPtr handle, byte[] word);

		[DllImport(DLL_LIB)]
		public static extern void voikko_free_mor_analysis(IntPtr analysis);

		[DllImport(DLL_LIB)]
		public static extern IntPtr voikko_mor_analysis_keys(IntPtr analysis);

		[DllImport(DLL_LIB)]
		public static extern IntPtr voikko_mor_analysis_value_cstr(IntPtr analysis, byte[] key);

		[DllImport(DLL_LIB)]
		public static extern void voikko_free_mor_analysis_value_cstr(IntPtr analysisValue);

		[DllImport(DLL_LIB)]
		public static extern int voikkoNextTokenCstr(IntPtr handle, byte[] text, IntPtr textLen, ref IntPtr tokenLen);

		[DllImport(DLL_LIB)]
		public static extern int voikkoNextSentenceStartCstr(IntPtr handle, byte[] text, IntPtr textLen, ref IntPtr sentenceLen);

		[DllImport(DLL_LIB)]
		public static extern IntPtr voikkoHyphenateCstr(IntPtr handle, byte[] word);

		[DllImport(DLL_LIB)]
		public static extern void voikkoFreeCstr(IntPtr cstr);
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

		public static List<VoikkoDictionary> listDicts()
		{
			return listDicts(null);
		}

		public static List<VoikkoDictionary> listDicts(string path)
		{
			List<VoikkoDictionary> dicts = new List<VoikkoDictionary>();
			IntPtr cDicts = Libvoikko.voikko_list_dicts(ByteArray.s2n(path));
			unsafe
			{
				for (void** cDict = (void**)cDicts; *cDict != (void*)0; cDict++)
				{
					dicts.Add(new VoikkoDictionary(ByteArray.n2s(Libvoikko.voikko_dict_language(new IntPtr(*cDict))), ByteArray.n2s(Libvoikko.voikko_dict_variant(new IntPtr(*cDict))), ByteArray.n2s(Libvoikko.voikko_dict_description(new IntPtr(*cDict)))));
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
					for (byte** cStr = (byte**)voikkoSuggestCstr; *cStr != (byte*)0; cStr++)
					{
						suggestions.Add(ByteArray.n2s(new IntPtr(*cStr)));
					}
				}
				Libvoikko.voikkoFreeCstrArray(voikkoSuggestCstr);
				return suggestions;
			}
		}

		public List<GrammarError> GrammarErrors(string text)
		{
			lock (lockObj)
			{
				requireValidHandle();
				List<GrammarError> errorList = new List<GrammarError>();
				int offset = 0;
				foreach (String paragraph in Regex.Split(text, "\\r?\\n"))
				{
					appendErrorsFromParagraph(errorList, paragraph, offset);
					offset += paragraph.Length + 1;
				}
				return errorList;
			}
		}

		private void appendErrorsFromParagraph(List<GrammarError> errorList, string paragraph, int offset)
		{
			int paragraphLen = ByteArray.s2n(paragraph).Length - 1;
			int skipErrors = 0;
			while (true)
			{
				IntPtr cError = Libvoikko.voikkoNextGrammarErrorCstr(handle, ByteArray.s2n(paragraph), new IntPtr(paragraphLen), IntPtr.Zero, skipErrors);
				if (cError == IntPtr.Zero)
				{
					return;
				}
				errorList.Add(getGrammarError(cError, offset));
				Libvoikko.voikkoFreeGrammarError(cError);
				skipErrors++;
			}
		}

		private GrammarError getGrammarError(IntPtr cError, int offset)
		{
			int errorCode = Libvoikko.voikkoGetGrammarErrorCode(cError);
			IntPtr startPos = Libvoikko.voikkoGetGrammarErrorStartPos(cError);
			IntPtr errorLength = Libvoikko.voikkoGetGrammarErrorLength(cError);
			IntPtr cSuggestions = Libvoikko.voikkoGetGrammarErrorSuggestions(cError);
			List<string> suggestions = new List<string>();
			if (cSuggestions != IntPtr.Zero)
			{
				unsafe
				{
					for (byte** cStr = (byte**)cSuggestions; *cStr != (byte*)0; cStr++)
					{
						suggestions.Add(ByteArray.n2s(new IntPtr(*cStr)));
					}
				}
			}
			return new GrammarError(errorCode, offset + (int)startPos, (int)errorLength, suggestions);
		}

		public string GrammarErrorExplanation(int errorCode, string language)
		{
			return ByteArray.n2s(Libvoikko.voikko_error_message_cstr(errorCode, ByteArray.s2n(language)));
		}

		public List<Analysis> Analyze(string word)
		{
			lock (lockObj)
			{
				requireValidHandle();
				IntPtr cAnalysisList = Libvoikko.voikkoAnalyzeWordCstr(handle, ByteArray.s2n(word));
				
				List<Analysis> analysisList = new List<Analysis>();
				
				if (cAnalysisList == IntPtr.Zero)
				{
					return analysisList;
				}
				
				unsafe
				{
					for (void** cAnalysis = (void**)cAnalysisList; *cAnalysis != (void*)0; cAnalysis++)
					{
						IntPtr cKeys = Libvoikko.voikko_mor_analysis_keys(new IntPtr(*cAnalysis));
						Analysis analysis = new Analysis();
						for (byte** cKey = (byte**)cKeys; *cKey != (byte*)0; cKey++)
						{
							string key = ByteArray.n2s(new IntPtr(*cKey));
							IntPtr val = Libvoikko.voikko_mor_analysis_value_cstr(new IntPtr(*cAnalysis), ByteArray.s2n(key));
							analysis[key] = ByteArray.n2s(val);
							Libvoikko.voikko_free_mor_analysis_value_cstr(val);
						}
						analysisList.Add(analysis);
					}
				}
				Libvoikko.voikko_free_mor_analysis(cAnalysisList);
				
				return analysisList;
			}
		}

		public List<Token> Tokens(string text)
		{
			List<Token> allTokens = new List<Token>();
			int lastStart = 0;
			for (int i = text.IndexOf('\0'); i != -1; i = text.IndexOf('\0', i + 1))
			{
				allTokens.AddRange(tokensNonNull(text.Substring(lastStart, i)));
				allTokens.Add(new Token(TokenType.UNKNOWN, "\0"));
				lastStart = i + 1;
			}
			allTokens.AddRange(tokensNonNull(text.Substring(lastStart)));
			return allTokens;
		}

		private List<Token> tokensNonNull(String text)
		{
			lock (lockObj)
			{
				List<Token> result = new List<Token>();
				byte[] textBytes = ByteArray.s2n(text);
				int textLen = textBytes.Length - 1;
				IntPtr tokenLenByRef = new IntPtr();
				while (textLen > 0)
				{
					int tokenTypeInt = Libvoikko.voikkoNextTokenCstr(handle, textBytes, new IntPtr(textLen), ref tokenLenByRef);
					int tokenLen = tokenLenByRef.ToInt32();
					TokenType tokenType = (TokenType)Enum.ToObject(typeof(TokenType), tokenTypeInt);
					String tokenText = text.Substring(0, tokenLen);
					result.Add(new Token(tokenType, tokenText));
					text = text.Substring(tokenLen);
					textBytes = ByteArray.s2n(text);
					textLen = textBytes.Length - 1;
				}
				return result;
			}
		}

		public List<Sentence> Sentences(string text)
		{
			lock (lockObj)
			{
				requireValidHandle();
				List<Sentence> result = new List<Sentence>();
				if (!isValidInput(text))
				{
					result.Add(new Sentence(text, SentenceStartType.NONE));
					return result;
				}
				byte[] textBytes = ByteArray.s2n(text);
				int textLen = textBytes.Length - 1;
				
				IntPtr sentenceLenByRef = new IntPtr();
				while (textLen > 0)
				{
					int sentenceTypeInt = Libvoikko.voikkoNextSentenceStartCstr(handle, textBytes, new IntPtr(textLen), ref sentenceLenByRef);
					int sentenceLen = sentenceLenByRef.ToInt32();
					SentenceStartType sentenceType = (SentenceStartType)Enum.ToObject(typeof(SentenceStartType), sentenceTypeInt);
					string tokenText = text.Substring(0, sentenceLen);
					result.Add(new Sentence(tokenText, sentenceType));
					text = text.Substring(sentenceLen);
					textBytes = ByteArray.s2n(text);
					textLen = textBytes.Length - 1;
				}
				
				return result;
			}
		}

		public string GetHyphenationPattern(string word)
		{
			lock (lockObj)
			{
				requireValidHandle();
				if (!isValidInput(word))
				{
					// return string of spaces
					return String.Format("%1$#" + word.Length + "s", "");
				}
				IntPtr cPattern = Libvoikko.voikkoHyphenateCstr(handle, ByteArray.s2n(word));
				string pattern = ByteArray.n2s(cPattern);
				Libvoikko.voikkoFreeCstr(cPattern);
				return pattern;
			}
		}

		private bool isValidInput(string text)
		{
			return text.IndexOf('\0') == -1;
		}
	}
	
	
}
