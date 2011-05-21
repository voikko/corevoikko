using System;
namespace libvoikko
{
	public class Sentence
	{
		private readonly string text;
		public string Text
		{
			get
			{
				return text;
			}
		}
		
		private readonly SentenceStartType nextStartType;
		public SentenceStartType NextStartType
		{
			get
			{
				return nextStartType;
			}
		}
		
		public Sentence(string text, SentenceStartType nextStartType)
		{
			this.text = text;
			this.nextStartType = nextStartType;
		}
	}
}

