package org.puimula.libvoikko;

import com.sun.jna.Library;
import com.sun.jna.NativeLong;
import com.sun.jna.Pointer;
import com.sun.jna.PointerType;
import com.sun.jna.ptr.PointerByReference;

// XXX: NativeLong is used to represent size_t. This may not work everywhere (64 bit Windows?)
interface Libvoikko extends Library {

    public static final int VOIKKO_SPELL_FAILED = 0;
    public static final int VOIKKO_SPELL_OK = 1;
    public static final int VOIKKO_INTERNAL_ERROR = 2;
    public static final int VOIKKO_CHARSET_CONVERSION_FAILED = 3;

    static class VoikkoHandle extends PointerType {

    };
    
    static class VoikkoGrammarError extends PointerType {
        
    };
    
    public abstract VoikkoHandle voikkoInit(PointerByReference error, byte[] langCode, byte[] path);

    public abstract void voikkoTerminate(VoikkoHandle handle);

    public abstract int voikkoSpellCstr(VoikkoHandle handle, byte[] word);

    public abstract Pointer[] voikko_list_dicts(byte[] path);
    
    public abstract void voikko_free_dicts(Pointer[] dicts);
    
    public abstract ByteArray voikko_dict_language(Pointer dict);
    
    public abstract ByteArray voikko_dict_variant(Pointer dict);
    
    public abstract ByteArray voikko_dict_description(Pointer dict);

    public abstract Pointer[] voikkoSuggestCstr(VoikkoHandle handle, byte[] word);
    
    public abstract void voikkoFreeCstrArray(Pointer[] array);
    
    public abstract VoikkoGrammarError voikkoNextGrammarErrorCstr(VoikkoHandle handle, byte[] text,
            NativeLong textLen, NativeLong startPos, int skipErrors);

    public abstract void voikkoFreeGrammarError(VoikkoGrammarError error);
    
    public abstract int voikkoGetGrammarErrorCode(VoikkoGrammarError error);
    
    public abstract NativeLong voikkoGetGrammarErrorStartPos(VoikkoGrammarError error);
    
    public abstract NativeLong voikkoGetGrammarErrorLength(VoikkoGrammarError error);
    
    public abstract Pointer[] voikkoGetGrammarErrorSuggestions(VoikkoGrammarError error);
    
    public abstract ByteArray voikko_error_message_cstr(int errorCode, byte[] language);
    
    public abstract Pointer[] voikkoAnalyzeWordCstr(VoikkoHandle handle, byte[] word);
    
    public abstract void voikko_free_mor_analysis(Pointer[] analysis);
    
    public abstract Pointer[] voikko_mor_analysis_keys(Pointer analysis);
    
    public abstract ByteArray voikko_mor_analysis_value_cstr(Pointer analysis, byte[] key);
    
    public abstract void voikko_free_mor_analysis_value_cstr(ByteArray analysisValue);
}
