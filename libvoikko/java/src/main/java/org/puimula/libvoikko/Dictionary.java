package org.puimula.libvoikko;

/**
 * Represents a morphological dictionary.
 */
public class Dictionary {

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
  
}
