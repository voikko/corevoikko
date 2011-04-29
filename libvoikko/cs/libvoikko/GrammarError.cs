using System;
using System.Collections.Generic;
namespace libvoikko
{
	public class GrammarError
	{
		private readonly int errorCode;
		private readonly int startPos;
		private readonly int errorLen;
		private readonly List<string> suggestions;

		public GrammarError(int errorCode, int startPos, int errorLen, List<string> suggestions)
		{
			this.errorCode = errorCode;
			this.startPos = startPos;
			this.errorLen = errorLen;
			this.suggestions = suggestions;
		}
		
		public int ErrorCode
		{
			get
			{
				return errorCode;
			}
		}
		
		public int StartPos
		{
			get
			{
				return startPos;
			}
		}
		
		public int ErrorLen
		{
			get
			{
				return errorLen;
			}
		}
		
		public List<string> Suggestions
		{
			get
			{
				return suggestions;
			}
		}
	}
}

