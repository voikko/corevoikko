package org.puimula.libvoikko;

import org.puimula.libvoikko.Libvoikko.VoikkoHandle;

import com.sun.jna.Native;
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
        handle = getLib().voikkoInit(error, language, path);
        // TODO: error checking
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
        // TODO: synchronization
        int spellResult = getLib().voikkoSpellCstr(handle, word);
        switch (spellResult) {
        case Libvoikko.VOIKKO_SPELL_OK:
            return true;
        case Libvoikko.VOIKKO_SPELL_FAILED:
            return false;
        default:
            throw new VoikkoException("Internal error returned from libvoikko");
        }
    }
}
