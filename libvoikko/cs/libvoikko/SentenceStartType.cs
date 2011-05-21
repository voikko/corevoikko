using System;
namespace libvoikko
{
	public enum SentenceStartType
	{
		/// <summary>
		/// End of text reached or error.
		/// </summary>
		NONE,
		/// <summary>
		/// This is not a start of a new sentence.
		/// </summary>
		NO_START,
		/// <summary>
		/// This is a probable start of a new sentence.
		/// </summary>
		PROBABLE,
		/// <summary>
		/// This may be a start of a new sentence.
		/// </summary>
		POSSIBLE
	}
}
