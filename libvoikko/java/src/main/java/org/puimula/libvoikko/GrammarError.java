package org.puimula.libvoikko;

import java.util.List;

public class GrammarError {

    private final int errorCode;
    private final int startPos;
    private final int errorLen;
    private final List<String> suggestions;

    public GrammarError(int errorCode, int startPos, int errorLen, List<String> suggestions) {
        this.errorCode = errorCode;
        this.startPos = startPos;
        this.errorLen = errorLen;
        this.suggestions = suggestions;
        
    }

    public int getErrorCode() {
        return errorCode;
    }

    public int getStartPos() {
        return startPos;
    }

    public int getErrorLen() {
        return errorLen;
    }

    public List<String> getSuggestions() {
        return suggestions;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("[code=");
        sb.append(errorCode);
        sb.append(", level=0, descr=\"\", stpos=");
        sb.append(startPos);
        sb.append(", len=");
        sb.append(errorLen);
        sb.append(", suggs={");
        boolean first = true;
        for (String suggestion : suggestions) {
            if (!first) {
                sb.append(",");
            }
            first = false;
            sb.append("\"");
            sb.append(suggestion);
            sb.append("\"");
        }
        sb.append("}]");
        return sb.toString();
    }
    
}
