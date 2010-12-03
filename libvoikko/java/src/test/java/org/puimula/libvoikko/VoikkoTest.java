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

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertFalse;
import static junit.framework.Assert.assertTrue;
import static junit.framework.Assert.fail;

import java.util.Arrays;
import java.util.List;

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
    public void dictionaryHashCodeWorks() {
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
    public void listDictsWithoutPath() {
        List<Dictionary> dicts = Voikko.listDicts();
        assertTrue(dicts.size() > 0);
        Dictionary standard = dicts.get(0);
        assertEquals("Standard dictionary must be the default in test environment.", "standard", standard.getVariant());
    }
    
    //@Test TODO: should work, write test
    public void listDictsWithPathAndAttributes() {

    }

    @Test
    public void initWithCorrectDictWorks() {
        voikko.terminate();
        voikko = new Voikko("fi-x-standard");
        assertFalse(voikko.spell("amifostiini"));
        voikko.terminate();
        voikko = new Voikko("fi-x-medicine");
        assertTrue(voikko.spell("amifostiini"));
    }
    
    @Test
    public void initWithNonExistentDictThrowsException() {
        voikko.terminate();
        try {
            voikko = new Voikko("fi-x-non-existent-variant");
        } catch (VoikkoException e) {
            return;
        }
        fail("Expected exception not thrown");
    }
    
    @Test
    public void initWithPathWorks() {
        // TODO: better test
        voikko.terminate();
        voikko = new Voikko("fi", "/path/to/nowhere");
        assertTrue(voikko.spell("kissa"));
    }
    
    @Test
    public void spellAfterTerminateThrowsException() {
        voikko.terminate();
        try {
            voikko.spell("kissa");
        } catch (VoikkoException e) {
            return;
        }
        fail("Expected exception not thrown");
    }
    
    @Test
    public void spell() {
        assertTrue(voikko.spell("määrä"));
        assertFalse(voikko.spell("määä"));
    }
    
    @Test
    public void suggest() {
        assertTrue(voikko.suggest("koirra").contains("koira"));
        assertTrue(voikko.suggest("määärä").contains("määrä"));
    }
    
    @Test
    public void suggestReturnsArgumentIfWordIsCorrect() {
        List<String> suggestions = voikko.suggest("koira");
        assertEquals(1, suggestions.size());
        assertEquals("koira", suggestions.get(0));
    }
    
    @Test
    public void grammarErrorsAndExplanation() {
        List<GrammarError> errors = voikko.grammarErrors("Minä olen joten kuten kaunis.");
        assertEquals(1, errors.size());
        GrammarError error = errors.get(0);
        assertEquals(10, error.getStartPos());
        assertEquals(11, error.getErrorLen());
        assertEquals(Arrays.asList("jotenkuten"), error.getSuggestions());
        int code = error.getErrorCode();
        assertEquals("Virheellinen kirjoitusasu", voikko.grammarErrorExplanation(code, "fi"));
        assertEquals("Incorrect spelling of word(s)", voikko.grammarErrorExplanation(code, "en"));
    }
    
    @Test
    public void noGrammarErrorsInEmptyParagraph() {
        List<GrammarError> errors = voikko.grammarErrors("Olen täi.\n\nOlen täi.");
        assertTrue(errors.isEmpty());
    }
    
    @Test
    public void grammarErrorOffsetsInMultipleParagraphs() {
        List<GrammarError> errors = voikko.grammarErrors("Olen täi.\n\nOlen joten kuten.");
        assertEquals(1, errors.size());
        assertEquals(16, errors.get(0).getStartPos());
        assertEquals(11, errors.get(0).getErrorLen());
    }
    
    @Test
    public void analyze() {
        List<Analysis> analysisList = voikko.analyze("kansaneläkehakemus");
        assertEquals(1, analysisList.size());
        assertEquals("=pppppp=ppppp=ppppppp", analysisList.get(0).get("STRUCTURE"));
    }
    
    @Test
    public void tokens() {
        List<Token> tokens = voikko.tokens("kissa ja koira sekä härkä");
        assertEquals(9, tokens.size());
        assertEquals(TokenType.WORD, tokens.get(2).getType());
        assertEquals("ja", tokens.get(2).getText());
        assertEquals(TokenType.WHITESPACE, tokens.get(7).getType());
        assertEquals(" ", tokens.get(7).getText());
        assertEquals(TokenType.WORD, tokens.get(8).getType());
        assertEquals("härkä", tokens.get(8).getText());
    }
    
    @Test
    public void sentences() {
        List<Sentence> sentences = voikko.sentences("Kissa ei ole koira. Koira ei ole kissa.");
        assertEquals(2, sentences.size());
        assertEquals("Kissa ei ole koira. ", sentences.get(0).getText());
        assertEquals(SentenceStartType.PROBABLE, sentences.get(0).getNextStartType());
        assertEquals("Koira ei ole kissa.", sentences.get(1).getText());
        assertEquals(SentenceStartType.NONE, sentences.get(1).getNextStartType());
    }
}
