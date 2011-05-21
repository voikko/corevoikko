using System;
namespace libvoikko
{
	public class Token
	{
		private readonly TokenType type;
		public TokenType Type
		{
			get
			{
				return type;
			}
		}
		
		private readonly String text;
		public string Text
		{
			get
			{
				return text;
			}
		}
		
		public Token(TokenType type, String text)
		{
			this.type = type;
			this.text = text;
		}
		
	}
}

