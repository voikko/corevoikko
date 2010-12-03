/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2010 Harri Pitkänen <hatapitk@iki.fi>
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

package org.puimula.libvoikko;

/**
 * Represents a morphological dictionary.
 */
public class Dictionary implements Comparable<Dictionary> {

    private final String language;
    private final String variant;
    private final String description;

    public Dictionary(String language, String variant, String description) {
        this.language = language;
        this.variant = variant;
        this.description = description;
    }

    /**
     * @return the language
     */
    public String getLanguage() {
        return language;
    }

    /**
     * @return the variant
     */
    public String getVariant() {
        return variant;
    }

    /**
     * @return the description
     */
    public String getDescription() {
        return description;
    }

    public int compareTo(Dictionary arg0) {
        int cmp = language.compareTo(arg0.getLanguage());
        if (cmp != 0) {
            return cmp;
        }
        cmp = variant.compareTo(arg0.getVariant());
        if (cmp != 0) {
            return cmp;
        }
        return description.compareTo(arg0.getDescription());
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((description == null) ? 0 : description.hashCode());
        result = prime * result
                + ((language == null) ? 0 : language.hashCode());
        result = prime * result + ((variant == null) ? 0 : variant.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        Dictionary other = (Dictionary) obj;
        if (description == null) {
            if (other.description != null)
                return false;
        } else if (!description.equals(other.description))
            return false;
        if (language == null) {
            if (other.language != null)
                return false;
        } else if (!language.equals(other.language))
            return false;
        if (variant == null) {
            if (other.variant != null)
                return false;
        } else if (!variant.equals(other.variant))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return "<" + language + "," + variant + "," + description + ">";
    }
    
}
