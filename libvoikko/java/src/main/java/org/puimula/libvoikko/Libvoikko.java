package org.puimula.libvoikko;

import com.sun.jna.Library;
import com.sun.jna.Pointer;
import com.sun.jna.PointerType;
import com.sun.jna.ptr.PointerByReference;

interface Libvoikko extends Library {

    public static final int VOIKKO_SPELL_FAILED = 0;
    public static final int VOIKKO_SPELL_OK = 1;
    public static final int VOIKKO_INTERNAL_ERROR = 2;
    public static final int VOIKKO_CHARSET_CONVERSION_FAILED = 3;

    static class VoikkoHandle extends PointerType {

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
}
