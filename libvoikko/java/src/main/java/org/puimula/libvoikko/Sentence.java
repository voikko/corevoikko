package org.puimula.libvoikko;

public class Sentence {

    private final String text;
    private final SentenceStartType nextStartType;
    
    public Sentence(String text, SentenceStartType nextStartType) {
        this.text = text;
        this.nextStartType = nextStartType;
    }

    public String getText() {
        return text;
    }

    public SentenceStartType getNextStartType() {
        return nextStartType;
    }
}
