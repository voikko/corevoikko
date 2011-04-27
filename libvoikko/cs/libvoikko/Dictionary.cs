using System;
namespace libvoikko
{
	public class Dictionary : IComparable<Dictionary>
	{

		private readonly string language;
		public string Language {
			get { return language; }
		}

		private readonly string variant;
		public string Variant {
			get { return variant; }
		}

		private readonly string description;
		public string Description {
			get { return description; }
		}

		public Dictionary(String language, String variant, String description)
		{
			this.language = language;
			this.variant = variant;
			this.description = description;
		}

		public int CompareTo(Dictionary other)
		{
			int cmp = language.CompareTo(other.Language);
			if (cmp != 0) {
				return cmp;
			}
			cmp = variant.CompareTo(other.Variant);
			if (cmp != 0) {
				return cmp;
			}
			return description.CompareTo(other.Description);
		}

		public override int GetHashCode()
		{
			const int prime = 31;
			int result = 1;
			result = prime * result + ((description == null) ? 0 : description.GetHashCode());
			result = prime * result + ((language == null) ? 0 : language.GetHashCode());
			result = prime * result + ((variant == null) ? 0 : variant.GetHashCode());
			return result;
		}

		public override bool Equals(Object obj)
		{
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (!(obj is Dictionary))
				return false;
			Dictionary other = (Dictionary)obj;
			if (description == null) {
				if (other.description != null)
					return false;
			} else if (!description.Equals(other.Description))
				return false;
			if (language == null) {
				if (other.language != null)
					return false;
			} else if (!language.Equals(other.Language))
				return false;
			if (variant == null) {
				if (other.variant != null)
					return false;
			} else if (!variant.Equals(other.Variant))
				return false;
			return true;
		}
		
		public override string ToString()
		{
			 return "<" + language + "," + variant + "," + description + ">";
		}
		
	}
}
