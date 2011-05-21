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
