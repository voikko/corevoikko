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
    
    static class VoikkoGrammarError extends PointerType {
        
    };
    
    public abstract VoikkoHandle voikkoInit(PointerByReference error, byte[] langCode, byte[] path);

    public abstract void voikkoTerminate(VoikkoHandle handle);

    public abstract int voikkoSpellCstr(VoikkoHandle handle, byte[] word);

    public abstract Pointer voikko_list_dicts(byte[] path);
    
    public abstract void voikko_free_dicts(Pointer dicts);
    
    public abstract ByteArray voikko_dict_language(Pointer dict);
    
    public abstract ByteArray voikko_dict_variant(Pointer dict);
    
    public abstract ByteArray voikko_dict_description(Pointer dict);

    public abstract Pointer voikkoSuggestCstr(VoikkoHandle handle, byte[] word);
    
    public abstract void voikkoFreeCstrArray(Pointer array);
    
    public abstract VoikkoGrammarError voikkoNextGrammarErrorCstr(VoikkoHandle handle, byte[] text,
            SizeT textLen, SizeT startPos, int skipErrors);

    public abstract void voikkoFreeGrammarError(VoikkoGrammarError error);
    
    public abstract int voikkoGetGrammarErrorCode(VoikkoGrammarError error);
    
    public abstract SizeT voikkoGetGrammarErrorStartPos(VoikkoGrammarError error);
    
    public abstract SizeT voikkoGetGrammarErrorLength(VoikkoGrammarError error);
    
    public abstract Pointer voikkoGetGrammarErrorSuggestions(VoikkoGrammarError error);
    
    public abstract ByteArray voikko_error_message_cstr(int errorCode, byte[] language);
    
    public abstract Pointer voikkoAnalyzeWordCstr(VoikkoHandle handle, byte[] word);
    
    public abstract void voikko_free_mor_analysis(Pointer analysis);
    
    public abstract Pointer voikko_mor_analysis_keys(Pointer analysis);
    
    public abstract ByteArray voikko_mor_analysis_value_cstr(Pointer analysis, byte[] key);
    
    public abstract void voikko_free_mor_analysis_value_cstr(ByteArray analysisValue);
    
    public abstract int voikkoNextTokenCstr(VoikkoHandle handle, byte[] text, SizeT textLen,
            SizeTByReference tokenLen);
    
    public abstract int voikkoNextSentenceStartCstr(VoikkoHandle handle, byte[] text, SizeT textLen,
            SizeTByReference sentenceLen);

    public abstract ByteArray voikkoHyphenateCstr(VoikkoHandle handle, byte[] word);
    
    public abstract void voikkoFreeCstr(ByteArray cstr);

    public abstract int voikkoSetBooleanOption(VoikkoHandle handle, int option, int value);
    
    public abstract int voikkoSetIntegerOption(VoikkoHandle handle, int option, int value);
}
