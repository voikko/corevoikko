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

import static org.puimula.libvoikko.ByteArray.n2s;
import static org.puimula.libvoikko.ByteArray.s2n;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.puimula.libvoikko.Libvoikko.VoikkoGrammarError;
import org.puimula.libvoikko.Libvoikko.VoikkoHandle;

import com.sun.jna.Native;
import com.sun.jna.NativeLong;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.NativeLongByReference;
import com.sun.jna.ptr.PointerByReference;

public class Voikko {

    private static Libvoikko library = null;

    private synchronized static Libvoikko getLib() {
        if (library == null) {
            library = (Libvoikko) Native.loadLibrary("voikko", Libvoikko.class); // TODO: voikko-1 on Windows
        }
        return library;
    }

    private VoikkoHandle handle;

    public Voikko(String language) {
        this(language, null);
    }

    public Voikko(String language, String path) {
        PointerByReference error = new PointerByReference();
        handle = getLib().voikkoInit(error, s2n(language), s2n(path));
        if (error.getPointer() != Pointer.NULL && error.getPointer().getString(0).length() > 0) {
            handle = null;
            throw new VoikkoException(error.getPointer().getString(0));
        }
    }

    public synchronized void terminate() {
        if (handle != null) {
            getLib().voikkoTerminate(handle);
            handle = null;
        }
    }

    /* (non-Javadoc)
     * @see java.lang.Object#finalize()
     */
    @Override
    protected void finalize() throws Throwable {
        terminate();
    }

    public synchronized boolean spell(String word) {
        requireValidHandle();
        int spellResult = getLib().voikkoSpellCstr(handle, s2n(word));
        switch (spellResult) {
        case Libvoikko.VOIKKO_SPELL_OK:
            return true;
        case Libvoikko.VOIKKO_SPELL_FAILED:
            return false;
        default:
            throw new VoikkoException("Internal error returned from libvoikko");
        }
    }

    private void requireValidHandle() {
        if (handle == null) {
            throw new VoikkoException("Attempt to use Voikko instance after terminate() was called");
        }
    }

    public static List<Dictionary> listDicts() {
        return listDicts(null);
    }
        
    public static List<Dictionary> listDicts(String path) {
        Libvoikko lib = getLib();
        Pointer cDicts = lib.voikko_list_dicts(s2n(path));
        Pointer[] pointerArray = cDicts.getPointerArray(0);
        List<Dictionary> dicts = new ArrayList<Dictionary>(pointerArray.length);
        for (Pointer cDict : pointerArray) {
            dicts.add(new Dictionary(lib.voikko_dict_language(cDict).toString(),
                    lib.voikko_dict_variant(cDict).toString(), lib.voikko_dict_description(cDict).toString()));
        }
        lib.voikko_free_dicts(cDicts);
        return dicts;
    }

    public synchronized List<String> suggest(String word) {
        requireValidHandle();
        Pointer voikkoSuggestCstr = getLib().voikkoSuggestCstr(handle, s2n(word));
        Pointer[] pointerArray = voikkoSuggestCstr.getPointerArray(0);
        List<String> suggestions = new ArrayList<String>(pointerArray.length);
        for (Pointer cStr : pointerArray) {
            suggestions.add(stringFromPointer(cStr));
        }
        getLib().voikkoFreeCstrArray(voikkoSuggestCstr);
        return suggestions;
    }

    private String stringFromPointer(Pointer cStr) {
        return n2s(cStr.getByteArray(0L, (int) cStr.indexOf(0L, (byte) 0)));
    }

    public synchronized List<GrammarError> grammarErrors(String text) {
        requireValidHandle();
        List<GrammarError> errorList = new ArrayList<GrammarError>();
        int offset = 0;
        for (String paragraph : text.split("\\r?\\n")) {
            appendErrorsFromParagraph(errorList, paragraph, offset);
            offset += paragraph.length() + 1;
        }
        return errorList;
    }

    private void appendErrorsFromParagraph(List<GrammarError> errorList, String paragraph, int offset) {
        final int paragraphLen = s2n(paragraph).length - 1;
        final Libvoikko lib = getLib();
        int skipErrors = 0;
        while (true) {
            VoikkoGrammarError cError = lib.voikkoNextGrammarErrorCstr(handle,
                    s2n(paragraph), new NativeLong(paragraphLen), new NativeLong(0), skipErrors);
            if (cError == null) {
                return;
            }
            errorList.add(getGrammarError(cError, offset));
            lib.voikkoFreeGrammarError(cError);
            skipErrors++;
        }
    }

    private GrammarError getGrammarError(VoikkoGrammarError cError, int offset) {
        final Libvoikko lib = getLib();
        int errorCode = lib.voikkoGetGrammarErrorCode(cError);
        int startPos = lib.voikkoGetGrammarErrorStartPos(cError).intValue();
        int errorLength = lib.voikkoGetGrammarErrorLength(cError).intValue();
        Pointer cSuggestions = lib.voikkoGetGrammarErrorSuggestions(cError);
        List<String> suggestions;
        if (cSuggestions == null) {
            suggestions = Collections.emptyList();
        } else {
            Pointer[] pointerArray = cSuggestions.getPointerArray(0);
            suggestions = new ArrayList<String>(pointerArray.length);
            for (Pointer cStr : pointerArray) {
                suggestions.add(stringFromPointer(cStr));
            }
        }
        return new GrammarError(errorCode, offset + startPos, errorLength, suggestions);
    }

    public String grammarErrorExplanation(int errorCode, String language) {
        return getLib().voikko_error_message_cstr(errorCode, s2n(language)).toString();
    }

    public synchronized List<Analysis> analyze(String word) {
        requireValidHandle();
        Libvoikko lib = getLib();
        Pointer cAnalysisList = lib.voikkoAnalyzeWordCstr(handle, s2n(word));
        
        List<Analysis> analysisList = new ArrayList<Analysis>();
        
        if (cAnalysisList == null) {
            return analysisList;
        }

        for (Pointer cAnalysis : cAnalysisList.getPointerArray(0)) {
            Pointer cKeys = lib.voikko_mor_analysis_keys(cAnalysis);
            Analysis analysis = new Analysis();
            for (Pointer cKey : cKeys.getPointerArray(0)) {
                String key = stringFromPointer(cKey);
                ByteArray value = lib.voikko_mor_analysis_value_cstr(cAnalysis, s2n(key));
                analysis.put(key, value.toString());
                lib.voikko_free_mor_analysis_value_cstr(value);
            }
            analysisList.add(analysis);
        }
        lib.voikko_free_mor_analysis(cAnalysisList);
        
        return analysisList;
    }

    public synchronized List<Token> tokens(String text) {
        requireValidHandle();
        Libvoikko lib = getLib();
        List<Token> result = new ArrayList<Token>();
        byte[] textBytes = s2n(text);
        int textLen = textBytes.length - 1;
        NativeLongByReference tokenLenByRef = new NativeLongByReference();
        while (textLen > 0) {
            int tokenTypeInt = lib.voikkoNextTokenCstr(handle, textBytes, new NativeLong(textLen), tokenLenByRef);
            int tokenLen = tokenLenByRef.getValue().intValue();
            TokenType tokenType = TokenType.values()[tokenTypeInt];
            String tokenText = text.substring(0, tokenLen);
            result.add(new Token(tokenType, tokenText));
            text = text.substring(tokenLen);
            textBytes = s2n(text);
            textLen = textBytes.length - 1;
        }
        return result;
    }

    public synchronized List<Sentence> sentences(String text) {
        requireValidHandle();
        Libvoikko lib = getLib();
        List<Sentence> result = new ArrayList<Sentence>();
        byte[] textBytes = s2n(text);
        int textLen = textBytes.length - 1;
        NativeLongByReference sentenceLenByRef = new NativeLongByReference();
        while (textLen > 0) {
            int sentenceTypeInt = lib.voikkoNextSentenceStartCstr(handle, textBytes, new NativeLong(textLen), sentenceLenByRef);
            int sentenceLen = sentenceLenByRef.getValue().intValue();
            SentenceStartType sentenceType = SentenceStartType.values()[sentenceTypeInt];
            String tokenText = text.substring(0, sentenceLen);
            result.add(new Sentence(tokenText, sentenceType));
            text = text.substring(sentenceLen);
            textBytes = s2n(text);
            textLen = textBytes.length - 1;
        }
        return result;
    }

    public synchronized String getHyphenationPattern(String word) {
        requireValidHandle();
        ByteArray cPattern = getLib().voikkoHyphenateCstr(handle, s2n(word));
        String pattern = cPattern.toString();
        getLib().voikkoFreeCstr(cPattern);
        return pattern;
    }

    public String hyphenate(String word) {
        String pattern = getHyphenationPattern(word);
        StringBuilder hyphenated = new StringBuilder();
        for (int i = 0; i < pattern.length(); i++) {
            char patternC = pattern.charAt(i);
            if (patternC == ' ') {
                hyphenated.append(word.charAt(i));
            } else if (patternC == '-') {
                hyphenated.append('-');
                hyphenated.append(word.charAt(i));
            } else if (patternC == '=') {
                hyphenated.append('-');
            }
        }
        return hyphenated.toString();
    }

    private static int boolToInt(boolean value) {
        return value ? 1 : 0;
    }
    
    private synchronized void setBoolOption(int option, boolean value) {
        requireValidHandle();
        int result = getLib().voikkoSetBooleanOption(handle, option, boolToInt(value));
        if (result == 0) {
            throw new VoikkoException("Could not set boolean option " + option + " to value " + value + ".");
        }
    }
    
    public void setIgnoreDot(boolean value) {
        setBoolOption(0, value);
    }

    public void setIgnoreNumbers(boolean value) {
        setBoolOption(1, value);
    }

    public void setIgnoreUppercase(boolean value) {
        setBoolOption(3, value);
    }

    public void setAcceptFirstUppercase(boolean value) {
        setBoolOption(6, value);
    }

    public void setAcceptAllUppercase(boolean value) {
        setBoolOption(7, value);
    }

    public void setIgnoreNonwords(boolean value) {
        setBoolOption(10, value);
    }

    public void setAcceptExtraHyphens(boolean value) {
        setBoolOption(11, value);
    }

    public void setAcceptMissingHyphens(boolean value) {
        setBoolOption(12, value);
    }

    public void setAcceptTitlesInGc(boolean value) {
        setBoolOption(13, value);
    }

    public void setAcceptUnfinishedParagraphsInGc(boolean value) {
        setBoolOption(14, value);
    }

    public void setAcceptBulletedListsInGc(boolean value) {
        setBoolOption(16, value);
    }

    public void setNoUglyHyphenation(boolean value) {
        setBoolOption(4, value);
    }

    public void setHyphenateUnknownWords(boolean value) {
        setBoolOption(15, value);
    }
}
