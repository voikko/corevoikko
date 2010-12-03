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
