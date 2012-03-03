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
 * Portions created by the Initial Developer are Copyright (C) 2010
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

package org.puimula.libvoikko;

import java.util.List;

/**
 * A grammar error is associated with a segment of text in a paragraph. It contains an error code
 * which can be used to provide human readable error explanation and optionally replacement suggestions
 * for the error segment that would fix the error. GUI software could visually display the error
 * by for example underlining the error segment starting from {@link #getStartPos()} and consisting of
 * {@link #getErrorLen()} characters. 
 */
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

    /**
     * Error code describing the type of error. Use {@link Voikko#grammarErrorExplanation(int, String)}
     * to get a human readable explanation for the error.
     * @return the error code
     */
    public int getErrorCode() {
        return errorCode;
    }

    /**
     * @return start of the error segment within the paragraph
     */
    public int getStartPos() {
        return startPos;
    }

    /**
     * @return length of the error segment
     */
    public int getErrorLen() {
        return errorLen;
    }

    /**
     * @return list of suggested replacements for the marked error
     */
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
