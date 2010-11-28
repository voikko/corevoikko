package org.puimula.libvoikko;

import static junit.framework.Assert.assertFalse;
import static junit.framework.Assert.assertTrue;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class VoikkoTest {

    private Voikko voikko;

    @Before
    public void setUp() {
        voikko = new Voikko("fi");
    }

    @After
    public void tearDown() {
        voikko.terminate();
    }

    @Test
    public void initAndTerminate() {
        // do nothing, just check that setUp and tearDown complete successfully
    }

    @Test
    public void terminateCanBeCalledMultipleTimes() {
        voikko.terminate();
        voikko.terminate();
    }

    @Test
    public void anotherObjectCanBeCreatedUsedAndDeletedInParallel() {
        Voikko medicalVoikko = new Voikko("fi-x-medicine");
        assertTrue(medicalVoikko.spell("amifostiini"));
        assertFalse(voikko.spell("amifostiini"));
        medicalVoikko.terminate();
        assertFalse(voikko.spell("amifostiini"));
    }
    
    @Test
    public void dictionaryComparisonWorks() {
        Dictionary d1 = new Dictionary("fi", "a", "b");
        Dictionary d2 = new Dictionary("fi", "a", "c");
        Dictionary d3 = new Dictionary("fi", "c", "b");
        Dictionary d4 = new Dictionary("fi", "a", "b");
        Dictionary d5 = new Dictionary("sv", "a", "b");
        assertFalse(d1.equals("kissa"));
        assertFalse("kissa".equals(d1));
        assertFalse(d1.equals(d2));
        assertFalse(d1.equals(d3));
        assertFalse(d4.equals(d5));
        assertTrue(d1.equals(d4));
        assertTrue(d1.compareTo(d2) < 0);
        assertTrue(d2.compareTo(d3) < 0);
        assertTrue(d4.compareTo(d5) < 0);
    }
    
    @Test
    public void testDictionaryHashCodeWorks() {
        Dictionary d1 = new Dictionary("fi", "a", "b");
        Dictionary d2 = new Dictionary("fi", "a", "c");
        Dictionary d3 = new Dictionary("fi", "c", "b");
        Dictionary d4 = new Dictionary("fi", "a", "b");
        Dictionary d5 = new Dictionary("sv", "a", "b");
        assertTrue(d1.hashCode() != d2.hashCode());
        assertTrue(d1.hashCode() != d3.hashCode());
        assertTrue(d4.hashCode() != d5.hashCode());
        assertTrue(d1.hashCode() == d4.hashCode());
    }

    @Test
    public void spell() {
        assertTrue(voikko.spell("määrä"));
        assertFalse(voikko.spell("määä"));
    }
}
