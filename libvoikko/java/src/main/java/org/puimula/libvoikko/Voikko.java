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
 * Portions created by the Initial Developer are Copyright (C) 2010 - 2012
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

import static org.puimula.libvoikko.ByteArray.n2s;
import static org.puimula.libvoikko.ByteArray.s2n;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.puimula.libvoikko.Libvoikko.VoikkoGrammarError;
import org.puimula.libvoikko.Libvoikko.VoikkoHandle;

import com.sun.jna.Native;
import com.sun.jna.NativeLibrary;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.PointerByReference;

/**
 * Represents an instance of Voikko. The instance has state, such as
 * settings related to spell checking and hyphenation, and methods for performing
 * various natural language analysis operations.
 * 
 * Methods in this class are synchronized so that, unlike the underlying C library,
 * these objects can be safely used from multiple threads. Heavily multithreaded
 * applications should still create separate instances for each thread for better
 * performance.
 */
public class Voikko {

    private static Libvoikko library = null;

    private synchronized static Libvoikko getLib() {
        if (library == null) {
            NativeLibrary nativeLibrary;
            try {
                nativeLibrary = NativeLibrary.getInstance("voikko");
            } catch (UnsatisfiedLinkError e) {
                // On Windows major version of library API is part of the library name
                nativeLibrary = NativeLibrary.getInstance("voikko-1");  
            }
            library = (Libvoikko) Native.loadLibrary(nativeLibrary.getFile().getAbsolutePath(), Libvoikko.class);
        }
        return library;
    }

    private VoikkoHandle handle;

    /**
     * Creates a new Voikko instance using only the default dictionary search path
     * @param language BCP 47 language tag to be used
     */
    public Voikko(String language) {
        this(language, null);
    }

    /**
     * Creates a new Voikko instance
     * @param language BCP 47 language tag to be used
     * @param path Extra path that will be checked first when looking for linguistic resources
     */
    public Voikko(String language, String path) {
        PointerByReference error = new PointerByReference();
        handle = getLib().voikkoInit(error, s2n(language), s2n(path));
        if (handle == null && error.getPointer() != Pointer.NULL) {
            handle = null;
            throw new VoikkoException(stringFromPointer(error.getPointer().getPointer(0)));
        }
    }

    /**
     * Releases the resources allocated by libvoikko for this instance. The instance cannot be used anymore
     * after this method has been called.
     * 
     * The resources are released automatically when the object is finalized. This method may be used
     * to make sure that the resources are immediately released since they may take significant amount
     * of memory.
     */
    public synchronized void terminate() {
        if (handle != null) {
            getLib().voikkoTerminate(handle);
            handle = null;
        }
    }

    @Override
    protected void finalize() throws Throwable {
        terminate();
        super.finalize();
    }

    /**
     * Check the spelling of given word.
     * @param word
     * @return true if the word is correct, false if it is incorrect.
     */
    public synchronized boolean spell(String word) {
        requireValidHandle();
        if (!isValidInput(word)) {
            return false;
        }
        int spellResult = getLib().voikkoSpellCstr(handle, s2n(word));
        return (spellResult == Libvoikko.VOIKKO_SPELL_OK);
    }

    private boolean isValidInput(String word) {
        return word.indexOf('\0') == -1;
    }

    private void requireValidHandle() {
        if (handle == null) {
            throw new VoikkoException("Attempt to use Voikko instance after terminate() was called");
        }
    }

    /**
     * @return a list of Dictionary objects representing the available
     * dictionary variants within standard search path.
     */
    public static List<Dictionary> listDicts() {
        return listDicts(null);
    }
        
    /**
     * @param path
     * @return a list of Dictionary objects representing the available
     * dictionary variants within given directory and standard search path.
     */
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

    /**
     * @param word
     * @return a list of suggested spellings for given (misspelled) word.
     * If the given word is correct, the list contains only the word itself
     */
    public synchronized List<String> suggest(String word) {
        requireValidHandle();
        if (!isValidInput(word)) {
            return Collections.emptyList();
        }
        Pointer voikkoSuggestCstr = getLib().voikkoSuggestCstr(handle, s2n(word));
        if (voikkoSuggestCstr == null) {
            return Collections.emptyList();
        }
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

    /**
     * Check the given text for grammar errors and return a
     * list of GrammarError objects representing the errors that were found.
     * Unlike the C based API this method accepts multiple paragraphs
     * separated by newline characters.
     * @param text
     * @return list of grammar errors
     */
    public synchronized List<GrammarError> grammarErrors(String text) {
        requireValidHandle();
        List<GrammarError> errorList = new ArrayList<GrammarError>();
        if (!isValidInput(text)) {
            return errorList;
        }
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
                    s2n(paragraph), new SizeT(paragraphLen), new SizeT(0), skipErrors);
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

    /**
     * @param errorCode grammar error code
     * @param language language in which the explanation should be returned
     * @return a human readable explanation for grammar error
     */
    public String grammarErrorExplanation(int errorCode, String language) {
        return getLib().voikko_error_message_cstr(errorCode, s2n(language)).toString();
    }

    /**
     * Analyze the morphology of given word and return the list of
     * analysis results.
     * @param word
     * @return analysis results
     */
    public synchronized List<Analysis> analyze(String word) {
        requireValidHandle();
        List<Analysis> analysisList = new ArrayList<Analysis>();
        if (!isValidInput(word)) {
            return analysisList;
        }
            
        Libvoikko lib = getLib();
        Pointer cAnalysisList = lib.voikkoAnalyzeWordCstr(handle, s2n(word));
        
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

    /**
     * Split the given natural language text into a list of Token objects.
     * @param text
     * @return list of tokens
     */
    public synchronized List<Token> tokens(String text) {
        requireValidHandle();
        List<Token> allTokens = new ArrayList<Token>();
        int lastStart = 0;
        for (int i = text.indexOf('\0'); i != -1; i = text.indexOf('\0', i + 1)) {
            allTokens.addAll(tokensNonNull(text.substring(lastStart, i)));
            allTokens.add(new Token(TokenType.UNKNOWN, "\0"));
            lastStart = i + 1;
        }
        allTokens.addAll(tokensNonNull(text.substring(lastStart)));
        return allTokens;
    }

    private List<Token> tokensNonNull(String text) {
        Libvoikko lib = getLib();
        List<Token> result = new ArrayList<Token>();
        byte[] textBytes = s2n(text);
        int textLen = textBytes.length - 1;
        SizeTByReference tokenLenByRef = new SizeTByReference();
        while (textLen > 0) {
            int tokenTypeInt = lib.voikkoNextTokenCstr(handle, textBytes, new SizeT(textLen), tokenLenByRef);
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

    /**
     * Split the given natural language text into a list of Sentence objects.
     * @param text
     * @return list of sentences
     */
    public synchronized List<Sentence> sentences(String text) {
        requireValidHandle();
        Libvoikko lib = getLib();
        List<Sentence> result = new ArrayList<Sentence>();
        if (!isValidInput(text)) {
            result.add(new Sentence(text, SentenceStartType.NONE));
            return result;
        }
        byte[] textBytes = s2n(text);
        int textLen = textBytes.length - 1;
        SizeTByReference sentenceLenByRef = new SizeTByReference();
        while (textLen > 0) {
            int sentenceTypeInt = lib.voikkoNextSentenceStartCstr(handle, textBytes, new SizeT(textLen), sentenceLenByRef);
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

    /**
     * Return a character pattern that describes the hyphenation of given word.
     *   ' ' = no hyphenation at this character,
     *   '-' = hyphenation point (character at this position
     *         is preserved in the hyphenated form),
     *   '=' = hyphentation point (character at this position
     *         is replaced by the hyphen.)
     * @param word
     * @return hyphenation pattern
     */
    public synchronized String getHyphenationPattern(String word) {
        requireValidHandle();
        if (!isValidInput(word)) {
            // return string of spaces
            return String.format("%1$#" + word.length() + "s", "");
        }
        ByteArray cPattern = getLib().voikkoHyphenateCstr(handle, s2n(word));
        String pattern = cPattern.toString();
        getLib().voikkoFreeCstr(cPattern);
        return pattern;
    }

    /**
     * @param word
     * @return the given word in fully hyphenated form.
     */
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
    
    /**
     * Ignore dot at the end of the word (needed for use in some word processors).
     * If this option is set and input word ends with a dot, spell checking and
     * hyphenation functions try to analyze the word without the dot if no results
     * can be obtained for the original form. Also with this option, string tokenizer
     * will consider trailing dot of a word to be a part of that word.
     * Default: false
     * @param value
     */
    public void setIgnoreDot(boolean value) {
        setBoolOption(0, value);
    }

    /**
     * Ignore words containing numbers.
     * Default: false
     * @param value
     */
    public void setIgnoreNumbers(boolean value) {
        setBoolOption(1, value);
    }

    /**
     * Accept words that are written completely in uppercase letters without checking
     * them at all.
     * Default: false
     * @param value
     */
    public void setIgnoreUppercase(boolean value) {
        setBoolOption(3, value);
    }

    /**
     * Accept words even when the first letter is in uppercase (start of sentence etc.)
     * Default: true
     * @param value
     */
    public void setAcceptFirstUppercase(boolean value) {
        setBoolOption(6, value);
    }

    /**
     * Accept words even when all of the letters are in uppercase. Note that this is
     * not the same as setIgnoreUppercase: with this option the word is still
     * checked, only case differences are ignored.
     * Default: true
     * @param value
     */
    public void setAcceptAllUppercase(boolean value) {
        setBoolOption(7, value);
    }

    /**
     * (Spell checking only): Ignore non-words such as URLs and email addresses.
     * Default: true
     * @param value
     */
    public void setIgnoreNonwords(boolean value) {
        setBoolOption(10, value);
    }

    /**
     * (Spell checking only): Allow some extra hyphens in words. This option relaxes
     * hyphen checking rules to work around some unresolved issues in the underlying
     * morphology, but it may cause some incorrect words to be accepted. The exact
     * behaviour (if any) of this option is not specified.
     * Default: false
     * @param value
     */
    public void setAcceptExtraHyphens(boolean value) {
        setBoolOption(11, value);
    }

    /**
     * (Spell checking only): Accept missing hyphens at the start and end of the word.
     * Some application programs do not consider hyphens to be word characters. This
     * is reasonable assumption for many languages but not for Finnish. If the
     * application cannot be fixed to use proper tokenization algorithm for Finnish,
     * this option may be used to tell libvoikko to work around this defect.
     * Default: false
     * @param value
     */
    public void setAcceptMissingHyphens(boolean value) {
        setBoolOption(12, value);
    }

    /**
     * (Grammar checking only): Accept incomplete sentences that could occur in
     * titles or headings. Set this option to true if your application is not able
     * to differentiate titles from normal text paragraphs, or if you know that
     * you are checking title text.
     * Default: false
     * @param value
     */
    public void setAcceptTitlesInGc(boolean value) {
        setBoolOption(13, value);
    }

    /**
     * (Grammar checking only): Accept incomplete sentences at the end of the
     * paragraph. These may exist when text is still being written.
     * Default: false
     * @param value
     */
    public void setAcceptUnfinishedParagraphsInGc(boolean value) {
        setBoolOption(14, value);
    }

    /**
     * (Grammar checking only): Accept paragraphs if they would be valid within
     * bulleted lists.
     * Default: false
     * @param value
     */
    public void setAcceptBulletedListsInGc(boolean value) {
        setBoolOption(16, value);
    }

    /**
     * Do not insert hyphenation positions that are considered to be ugly but correct
     * Default: false
     * @param value
     */
    public void setNoUglyHyphenation(boolean value) {
        setBoolOption(4, value);
    }

    /**
     * (Hyphenation only): Hyphenate unknown words.
     * Default: true
     * @param value
     */
    public void setHyphenateUnknownWords(boolean value) {
        setBoolOption(15, value);
    }

    private synchronized void setIntegerOption(int option, int value) {
        requireValidHandle();
        int result = getLib().voikkoSetIntegerOption(handle, option, value);
        if (result == 0) {
            throw new VoikkoException("Could not set integer option " + option + " to value " + value + ".");
        }
    }
    
    /**
     * The minimum length for words that may be hyphenated. This limit is also enforced on
     * individual parts of compound words.
     * Default: 2
     * @param length
     */
    public void setMinHyphenatedWordLength(int length) {
        setIntegerOption(9, length);
    }

    /**
     * Controls the size of in memory cache for spell checking results. 0 is the default size,
     * 1 is twice as large as 0 etc. -1 disables the spell checking cache entirely.
     * @param sizeParam
     */
    public void setSpellerCacheSize(int sizeParam) {
        setIntegerOption(17, sizeParam);
    }

    /**
     * Set the suggestion strategy to be used when generating spelling suggestions.
      Default: {@link SuggestionStrategy#TYPO}
     * @param suggestionStrategy
     */
    public void setSuggestionStrategy(SuggestionStrategy suggestionStrategy) {
        switch (suggestionStrategy) {
        case OCR:
            setBoolOption(8, true);
            break;
        case TYPO:
            setBoolOption(8, false);
            break;
        }
    }

    /**
     * Set the explicit path to the folder containing shared library files.
     * @param libraryPath
     */
    public static void addLibraryPath(String libraryPath) {
        NativeLibrary.addSearchPath("voikko", libraryPath);
        NativeLibrary.addSearchPath("voikko-1", libraryPath);
    }

}
