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
