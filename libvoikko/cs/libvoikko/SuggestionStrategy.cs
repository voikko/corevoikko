using System;
namespace libvoikko
{
	public enum SuggestionStrategy
	{
		/// <summary>
		/// Suggestion strategy for correcting human typing errors.
		/// </summary>
		TYPO = 0,
		/// <summary>
		/// Suggestion strategy for correcting errors in text produced by
		/// optical character recognition software.
		/// </summary>
		OCR = 1
	}
}

