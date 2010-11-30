package org.puimula.libvoikko;

/** Sentence start types */
public enum SentenceStartType {
    /** End of text reached or error. */
    NONE,
    /** This is not a start of a new sentence. */
    NO_START,
    /** This is a probable start of a new sentence. */
    PROBABLE,
    /** This may be a start of a new sentence. */
    POSSIBLE;
}
