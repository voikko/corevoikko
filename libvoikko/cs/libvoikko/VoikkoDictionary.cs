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
