/* The contents of this file are subject to the Mozilla Public License Version 
 * 1.1 (the "License"); you may not use this file except in compliance with 
 * the License. You may obtain a copy of the License at 
 * http://www.mozilla.org/MPL/
 * 
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 * 
 * The Original Code is Libvoikko: Library of natural language processing tools.
 * The Initial Developer of the Original Code is Harri Pitkänen <hatapitk@iki.fi>.
 * Portions created by the Initial Developer are Copyright (C) 2011
 * the Initial Developer. All Rights Reserved.
 * 
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *********************************************************************************/

using System;
using System.Runtime.InteropServices;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Text;
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
		public static extern IntPtr voikkoGetGrammarErrorShortDescription(IntPtr error, byte[] language);

		[DllImport(DLL_LIB)]
		public static extern IntPtr voikkoAnalyzeWordCstr(IntPtr handle, byte[] word);

		[DllImport(DLL_LIB)]
		public static extern void voikkoFreeErrorMessageCstr(IntPtr message);

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

		[DllImport(DLL_LIB)]
		public static extern int voikkoSetBooleanOption(IntPtr handle, int option, int val);

		[DllImport(DLL_LIB)]
		public static extern int voikkoSetIntegerOption(IntPtr handle, int option, int val);
	}

	public class Voikko : IDisposable
	{

		IntPtr handle;
		private readonly Object lockObj = new Object();

		public Voikko(String language, String path)
		{
			IntPtr error = new IntPtr();
			handle = Libvoikko.voikkoInit(ref error, ByteArray.s2n(language), ByteArray.s2ansi(path));
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
				if (!isValidInput(word))
				{
					return false;
				}
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
			IntPtr cDicts = Libvoikko.voikko_list_dicts(ByteArray.s2ansi(path));
			unsafe
			{
				for (void** cDict = (void**)cDicts; *cDict != (void*)0; cDict++)
				{
					dicts.Add(new VoikkoDictionary(ByteArray.n2s(Libvoikko.voikko_dict_language(new IntPtr(*cDict))),
					                               ByteArray.n2s(Libvoikko.voikko_dict_variant(new IntPtr(*cDict))),
					                               ByteArray.n2s(Libvoikko.voikko_dict_description(new IntPtr(*cDict)))));
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
				List<string> suggestions = new List<string>();
				if (!isValidInput(word))
				{
					return suggestions;
				}
				IntPtr voikkoSuggestCstr = Libvoikko.voikkoSuggestCstr(handle, ByteArray.s2n(word));
				if (voikkoSuggestCstr == IntPtr.Zero)
				{
					return suggestions;
				}
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

		public List<GrammarError> GrammarErrors(string text, string language)
		{
			lock (lockObj)
			{
				requireValidHandle();
				List<GrammarError> errorList = new List<GrammarError>();
				if (!isValidInput(text))
				{
					return errorList;
				}
				int offset = 0;
				foreach (String paragraph in Regex.Split(text, "\\r?\\n"))
				{
					appendErrorsFromParagraph(errorList, paragraph, offset, language);
					offset += paragraph.Length + 1;
				}
				return errorList;
			}
		}

		private void appendErrorsFromParagraph(List<GrammarError> errorList, string paragraph, int offset, string language)
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
				errorList.Add(getGrammarError(cError, offset, language));
				Libvoikko.voikkoFreeGrammarError(cError);
				skipErrors++;
			}
		}

		private GrammarError getGrammarError(IntPtr cError, int offset, string language)
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
			IntPtr cShortDescription = Libvoikko.voikkoGetGrammarErrorShortDescription(cError, ByteArray.s2n(language));
			string shortDescription = ByteArray.n2s(cShortDescription);
			Libvoikko.voikkoFreeErrorMessageCstr(cShortDescription);
			return new GrammarError(errorCode, offset + (int)startPos, (int)errorLength, suggestions, shortDescription);
		}

		public List<Analysis> Analyze(string word)
		{
			lock (lockObj)
			{
				requireValidHandle();
				List<Analysis> analysisList = new List<Analysis>();
				if (!isValidInput(word))
				{
					return analysisList;
				}
				
				IntPtr cAnalysisList = Libvoikko.voikkoAnalyzeWordCstr(handle, ByteArray.s2n(word));
				
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
				allTokens.AddRange(tokensNonNull(text.Substring(lastStart, i - lastStart)));
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
					return new string(' ', word.Length);
				}
				IntPtr cPattern = Libvoikko.voikkoHyphenateCstr(handle, ByteArray.s2n(word));
				string pattern = ByteArray.n2s(cPattern);
				Libvoikko.voikkoFreeCstr(cPattern);
				return pattern;
			}
		}

		public string Hyphenate(string word)
		{
			string pattern = GetHyphenationPattern(word);
			StringBuilder hyphenated = new StringBuilder();
			for (int i = 0; i < pattern.Length; i++)
			{
				char patternC = pattern[i];
				if (patternC == ' ')
				{
					hyphenated.Append(word[i]);
				} else if (patternC == '-')
				{
					hyphenated.Append('-');
					hyphenated.Append(word[i]);
				} else if (patternC == '=')
				{
					hyphenated.Append('-');
				}
			}
			return hyphenated.ToString();
		}

		private bool isValidInput(string text)
		{
			return text.IndexOf('\0') == -1;
		}

		private static int boolToInt(bool val)
		{
			return val ? 1 : 0;
		}

		private void setBoolOption(int option, bool val)
		{
			lock (lockObj)
			{
				requireValidHandle();
				int result = Libvoikko.voikkoSetBooleanOption(handle, option, boolToInt(val));
				if (result == 0)
				{
					throw new VoikkoException("Could not set boolean option " + option + " to value " + val + ".");
				}
			}
		}

		public bool IgnoreDot {
			set { setBoolOption(0, value); }
		}

		public bool IgnoreNumbers {
			set { setBoolOption(1, value); }
		}

		public bool IgnoreUppercase {
			set { setBoolOption(3, value); }
		}

		public bool AcceptFirstUppercase {
			set { setBoolOption(6, value); }
		}

		public bool AcceptAllUppercase {
			set { setBoolOption(7, value); }
		}

		public bool IgnoreNonwords {
			set { setBoolOption(10, value); }
		}

		public bool AcceptExtraHyphens {
			set { setBoolOption(11, value); }
		}

		public bool AcceptMissingHyphens {
			set { setBoolOption(12, value); }
		}

		public bool AcceptTitlesInGc {
			set { setBoolOption(13, value); }
		}

		public bool AcceptUnfinishedParagraphsInGc {
			set { setBoolOption(14, value); }
		}

		public bool AcceptBulletedListsInGc {
			set { setBoolOption(16, value); }
		}

		public bool NoUglyHyphenation {
			set { setBoolOption(4, value); }
		}

		public bool HyphenateUnknownWords {
			set { setBoolOption(15, value); }
		}

		private void setIntegerOption(int option, int val)
		{
			lock (lockObj)
			{
				requireValidHandle();
				int result = Libvoikko.voikkoSetIntegerOption(handle, option, val);
				if (result == 0)
				{
					throw new VoikkoException("Could not set integer option " + option + " to value " + val + ".");
				}
			}
		}

		public int MinHyphenatedWordLength {
			set { setIntegerOption(9, value); }
		}

		public int SpellerCacheSize {
			set { setIntegerOption(17, value); }
		}

		public SuggestionStrategy SuggestionStrategy {
			set {
				switch (value)
				{
				case SuggestionStrategy.OCR:
					setBoolOption(8, true);
					break;
				case SuggestionStrategy.TYPO:
					setBoolOption(8, false);
					break;
				}
			}
		}
	}
	
	
}
