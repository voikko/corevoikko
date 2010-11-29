package org.puimula.libvoikko;

import java.nio.charset.Charset;

import com.sun.jna.Pointer;
import com.sun.jna.PointerType;

public class ByteArray extends PointerType {
    
    private static final Charset UTF8 = Charset.forName("UTF-8");
    
    /**
     * Native-to-Java string mapping
     * @param bytes
     * @return
     */
    public static String n2s(byte[] bytes) {
        if (bytes == null) {
            return null;
        }
        return new String(bytes, UTF8);
    }
    
    /**
     * Java-to-native string mapping
     * @param bytes
     * @return
     */
    public static byte[] s2n(String string) {
        if (string == null) {
            return null;
        }
        byte[] stringBytes = string.getBytes(UTF8);
        byte[] allBytes = new byte[stringBytes.length + 1];
        System.arraycopy(stringBytes, 0, allBytes, 0, stringBytes.length);
        return allBytes;
    }

    @Override
    public String toString() {
        if (getPointer() == Pointer.NULL) {
            return null;
        }
        return n2s(getPointer().getByteArray(0L, (int) getPointer().indexOf(0L, (byte) 0)));
    }
}
