package org.puimula.libvoikko;

public enum SuggestionStrategy {

  TYPO(0),
  OCR(1);
  
  private final int id;

  private SuggestionStrategy(int id) {
    this.id = id;
  }

  /**
   * @return the id
   */
  public int getId() {
    return id;
  }
  
}
