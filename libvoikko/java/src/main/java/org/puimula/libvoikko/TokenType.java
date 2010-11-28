package org.puimula.libvoikko;

public enum TokenType {
    NONE(0),
    WORD(1),
    PUNCTUATION(2),
    WHITESPACE(3),
    UNKNOWN(4);

    private final int id;

    private TokenType(int id) {
        this.id = id;
    }

    /**
     * @return the id
     */
    public int getId() {
        return id;
    }
}
