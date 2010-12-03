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
