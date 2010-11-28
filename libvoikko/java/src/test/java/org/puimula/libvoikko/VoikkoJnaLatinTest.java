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
