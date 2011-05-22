/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2011 Harri Pitkänen <hatapitk@iki.fi>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *********************************************************************************/

using System;
namespace libvoikko
{
	public class VoikkoDictionary : IComparable<VoikkoDictionary>
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

		public VoikkoDictionary(String language, String variant, String description)
		{
			this.language = language;
			this.variant = variant;
			this.description = description;
		}

		public int CompareTo(VoikkoDictionary other)
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
			if (!(obj is VoikkoDictionary))
				return false;
			VoikkoDictionary other = (VoikkoDictionary)obj;
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
