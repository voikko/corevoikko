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
        Pointer[] cDicts = lib.voikko_list_dicts(s2n(path));
        List<Dictionary> dicts = new ArrayList<Dictionary>(cDicts.length);
        for (Pointer cDict : cDicts) {
            dicts.add(new Dictionary(lib.voikko_dict_language(cDict).toString(),
                    lib.voikko_dict_variant(cDict).toString(), lib.voikko_dict_description(cDict).toString()));
        }
        lib.voikko_free_dicts(cDicts);
        return dicts;
    }

    public synchronized List<String> suggest(String word) {
        requireValidHandle();
        Pointer[] voikkoSuggestCstr = getLib().voikkoSuggestCstr(handle, s2n(word));
        List<String> suggestions = new ArrayList<String>(voikkoSuggestCstr.length);
        for (Pointer cStr : voikkoSuggestCstr) {
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
        Pointer[] cSuggestions = lib.voikkoGetGrammarErrorSuggestions(cError);
        List<String> suggestions;
        if (cSuggestions == null) {
            suggestions = Collections.emptyList();
        } else {
            suggestions = new ArrayList<String>(cSuggestions.length);
            for (Pointer cStr : cSuggestions) {
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
        Pointer[] cAnalysisList = lib.voikkoAnalyzeWordCstr(handle, s2n(word));
        
        List<Analysis> analysisList = new ArrayList<Analysis>();
        
        if (cAnalysisList == null) {
            return analysisList;
        }

        for (Pointer cAnalysis : cAnalysisList) {
            Pointer[] cKeys = lib.voikko_mor_analysis_keys(cAnalysis);
            Analysis analysis = new Analysis();
            for (Pointer cKey : cKeys) {
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
    
}
