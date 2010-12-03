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

/**
 * Test the same stuff as VoikkoTest but with jna.encoding set to something else than UTF-8 
 */
public class VoikkoJnaLatinTest extends VoikkoTest {

    private static final String JNA_ENCODING = "jna.encoding";
    private String originalJnaEncoding;
    
    @Override
    public void setUp() {
        originalJnaEncoding = System.getProperty(JNA_ENCODING);
        System.setProperty(JNA_ENCODING, "iso-8859-1");
        super.setUp();
    }

    @Override
    public void tearDown() {
        super.tearDown();
        if (originalJnaEncoding == null) {
            System.clearProperty(JNA_ENCODING);
        } else {
            System.setProperty(JNA_ENCODING, originalJnaEncoding);
        }
    }

    
}
