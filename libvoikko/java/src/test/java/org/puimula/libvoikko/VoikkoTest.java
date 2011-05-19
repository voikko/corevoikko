/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2010 - 2011 Harri Pitkänen <hatapitk@iki.fi>
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

    private static final int DEPRECATED_MAX_ANALYSIS_COUNT = 31;
    private static final int DEPRECATED_MAX_WORD_CHARS = 255;
    private Voikko voikko;

    @Before
    public void setUp() {
        voikko = new Voikko("fi");
    }

    @After
    public void tearDown() {
        voikko.terminate();
        // Do garbage collection after every test method. This will make errors
        // in native memory management (double frees etc.) more likely to show up.
        voikko = null;
        System.gc();
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
            assertEquals("Specified dictionary variant was not found", e.getMessage());
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
        assertTrue(voikko.suggest("lasjkblvankirknaslvethikertvhgn").isEmpty());
    }
    
    @Test
    public void suggestGc() {
        assertTrue(voikko.suggest("määärä").contains("määrä"));
        System.gc();
        assertTrue(voikko.suggest("määärä").contains("määrä"));
        System.gc();
        assertTrue(voikko.suggest("määärä").contains("määrä"));
        System.gc();
        assertTrue(voikko.suggest("määärä").contains("määrä"));
        System.gc();
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
    
    @Test
    public void hyphenationPattern() {
        assertEquals("   - ", voikko.getHyphenationPattern("kissa"));
        assertEquals("   - ", voikko.getHyphenationPattern("määrä"));
        assertEquals("    - =  - ", voikko.getHyphenationPattern("kuorma-auto"));
        assertEquals("   =  ", voikko.getHyphenationPattern("vaa'an"));
    }
    
    @Test
    public void hyphenate() {
        assertEquals("kis-sa", voikko.hyphenate("kissa"));
        assertEquals("mää-rä", voikko.hyphenate("määrä"));
        assertEquals("kuor-ma-au-to", voikko.hyphenate("kuorma-auto"));
        assertEquals("vaa-an", voikko.hyphenate("vaa'an"));
    }
    
    @Test
    public void setIgnoreDot() {
        voikko.setIgnoreDot(false);
        assertFalse(voikko.spell("kissa."));
        voikko.setIgnoreDot(true);
        assertTrue(voikko.spell("kissa."));
    }
    
    @Test
    public void setIgnoreNumbers() {
        voikko.setIgnoreNumbers(false);
        assertFalse(voikko.spell("kissa2"));
        voikko.setIgnoreNumbers(true);
        assertTrue(voikko.spell("kissa2"));
    }
    
    @Test
    public void setIgnoreUppercase() {
        voikko.setIgnoreUppercase(false);
        assertFalse(voikko.spell("KAAAA"));
        voikko.setIgnoreUppercase(true);
        assertTrue(voikko.spell("KAAAA"));
    }
    
    @Test
    public void setAcceptFirstUppercase() {
        voikko.setAcceptFirstUppercase(false);
        assertFalse(voikko.spell("Kissa"));
        voikko.setAcceptFirstUppercase(true);
        assertTrue(voikko.spell("Kissa"));
    }
    
    @Test
    public void upperCaseScandinavianLetters() {
        assertTrue(voikko.spell("Äiti"));
        assertFalse(voikko.spell("Ääiti"));
        assertTrue(voikko.spell("š"));
        assertTrue(voikko.spell("Š"));
    }
    
    @Test
    public void acceptAllUppercase() {
        voikko.setIgnoreUppercase(false);
        voikko.setAcceptAllUppercase(false);
        assertFalse(voikko.spell("KISSA"));
        voikko.setAcceptAllUppercase(true);
        assertTrue(voikko.spell("KISSA"));
        assertFalse(voikko.spell("KAAAA"));
    }
    
    @Test
    public void ignoreNonwords() {
        voikko.setIgnoreNonwords(false);
        assertFalse(voikko.spell("hatapitk@iki.fi"));
        voikko.setIgnoreNonwords(true);
        assertTrue(voikko.spell("hatapitk@iki.fi"));
        assertFalse(voikko.spell("ashdaksd"));
    }
    
    @Test
    public void acceptExtraHyphens() {
        voikko.setAcceptExtraHyphens(false);
        assertFalse(voikko.spell("kerros-talo"));
        voikko.setAcceptExtraHyphens(true);
        assertTrue(voikko.spell("kerros-talo"));
    }
    
    @Test
    public void acceptMissingHyphens() {
        voikko.setAcceptMissingHyphens(false);
        assertFalse(voikko.spell("sosiaali"));
        voikko.setAcceptMissingHyphens(true);
        assertTrue(voikko.spell("sosiaali"));
    }
    
    @Test
    public void setAcceptTitlesInGc() {
        voikko.setAcceptTitlesInGc(false);
        assertEquals(1, voikko.grammarErrors("Kissa on eläin").size());
        voikko.setAcceptTitlesInGc(true);
        assertEquals(0, voikko.grammarErrors("Kissa on eläin").size());
    }
    
    @Test
    public void setAcceptUnfinishedParagraphsInGc() {
        voikko.setAcceptUnfinishedParagraphsInGc(false);
        assertEquals(1, voikko.grammarErrors("Kissa on ").size());
        voikko.setAcceptUnfinishedParagraphsInGc(true);
        assertEquals(0, voikko.grammarErrors("Kissa on ").size());
    }
    
    @Test
    public void setAcceptBulletedListsInGc() {
        voikko.setAcceptBulletedListsInGc(false);
        assertFalse(voikko.grammarErrors("kissa").isEmpty());
        voikko.setAcceptBulletedListsInGc(true);
        assertTrue(voikko.grammarErrors("kissa").isEmpty());
    }
    
    @Test
    public void setNoUglyHyphenation() {
        voikko.setNoUglyHyphenation(false);
        assertEquals("i-va", voikko.hyphenate("iva"));
        voikko.setNoUglyHyphenation(true);
        assertEquals("iva", voikko.hyphenate("iva"));
    }
    
    @Test
    public void setHyphenateUnknownWordsWorks() {
        voikko.setHyphenateUnknownWords(false);
        assertEquals("kirjutepo", voikko.hyphenate("kirjutepo"));
        voikko.setHyphenateUnknownWords(true);
        assertEquals("kir-ju-te-po", voikko.hyphenate("kirjutepo"));
    }
    
    @Test
    public void setMinHyphenatedWordLength() {
        voikko.setMinHyphenatedWordLength(6);
        assertEquals("koira", voikko.hyphenate("koira"));
        voikko.setMinHyphenatedWordLength(2);
        assertEquals("koi-ra", voikko.hyphenate("koira"));
    }
    
    @Test
    public void increaseSpellerCacheSize() {
        // TODO: this only tests that nothing breaks, not that cache is actually increased
        voikko.setSpellerCacheSize(3);
        assertTrue(voikko.spell("kissa"));
    }
    
    @Test
    public void disableSpellerCache() {
        // TODO: this only tests that nothing breaks, not that cache is actually disabled
        voikko.setSpellerCacheSize(-1);
        assertTrue(voikko.spell("kissa"));
    }
    
    @Test
    public void setSuggestionStrategy() {
        voikko.setSuggestionStrategy(SuggestionStrategy.OCR);
        assertFalse(voikko.suggest("koari").contains("koira"));
        assertTrue(voikko.suggest("koir_").contains("koira"));
        voikko.setSuggestionStrategy(SuggestionStrategy.TYPO);
        assertTrue(voikko.suggest("koari").contains("koira"));
    }
    
    @Test
    public void maxAnalysisCountIsNotPassed() {
        String complexWord = "lumenerolumenerolumenerolumenerolumenero";
        assertTrue(voikko.analyze(complexWord).size() <= DEPRECATED_MAX_ANALYSIS_COUNT);
    }
    
    @Test
    public void morPruningWorks() {
        // TODO: this test will not fail, it just takes very long time
        // if pruning does not work.
        StringBuilder complexWord = new StringBuilder();
        for (int i = 0; i < 20; i++) {
            complexWord.append("lumenero");
        }
        assertTrue(complexWord.length() < DEPRECATED_MAX_WORD_CHARS);
        voikko.analyze(complexWord.toString());
    }
    
    @Test
    public void overLongWordIsRejectedDuringSpellCheck() {
        // This tests backend specific deprecated functionality.
        StringBuilder complexWord = new StringBuilder();
        for (int i = 0; i < 25; i++) {
            complexWord.append("kuraattori");
        }
        assertTrue(complexWord.length() < DEPRECATED_MAX_WORD_CHARS);
        assertTrue(voikko.spell(complexWord.toString()));
        
        complexWord.append("kuraattori");
        assertTrue(complexWord.length() > DEPRECATED_MAX_WORD_CHARS);
        assertFalse(voikko.spell(complexWord.toString()));
    }
    
    @Test
    public void OverLongWordsAreNotAnalyzed() {
        // This tests backend specific deprecated functionality.
        StringBuilder complexWord = new StringBuilder();
        for (int i = 0; i < 25; i++) {
            complexWord.append("kuraattori");
        }
        assertTrue(complexWord.length() < DEPRECATED_MAX_WORD_CHARS);
        assertEquals(1, voikko.analyze(complexWord.toString()).size());
        
        complexWord.append("kuraattori");
        assertTrue(complexWord.length() > DEPRECATED_MAX_WORD_CHARS);
        assertEquals(0, voikko.analyze(complexWord.toString()).size());
    }
    
    @Test
    public void embeddedNullsAreNotAccepted() {
        assertFalse(voikko.spell("kissa\0asdasd"));
        assertTrue(voikko.suggest("kisssa\0koira").isEmpty());
        assertEquals("kissa\0koira", voikko.hyphenate("kissa\0koira"));
        assertEquals(0, voikko.grammarErrors("kissa\0koira").size());
        assertEquals(0, voikko.analyze("kissa\0koira").size());
    }

    @Test
    public void nullCharMeansSingleSentence() {
        List<Sentence> sentences = voikko.sentences("kissa\0koira");
        assertEquals(1, sentences.size());
        assertEquals(SentenceStartType.NONE, sentences.get(0).getNextStartType());
        assertEquals("kissa\0koira", sentences.get(0).getText());
    }
    
    @Test
    public void nullCharIsUnknownToken() {
        {
            List<Token> tokens = voikko.tokens("kissa\0koira");
            assertEquals(3, tokens.size());
            assertEquals(TokenType.WORD, tokens.get(0).getType());
            assertEquals("kissa", tokens.get(0).getText());
            assertEquals(TokenType.UNKNOWN, tokens.get(1).getType());
            assertEquals("\0", tokens.get(1).getText());
            assertEquals(TokenType.WORD, tokens.get(2).getType());
            assertEquals("koira", tokens.get(2).getText());
        }
        {
            List<Token> tokens = voikko.tokens("kissa\0\0koira");
            assertEquals(4, tokens.size());
            assertEquals(TokenType.WORD, tokens.get(0).getType());
            assertEquals("kissa", tokens.get(0).getText());
            assertEquals(TokenType.UNKNOWN, tokens.get(1).getType());
            assertEquals("\0", tokens.get(1).getText());
            assertEquals(TokenType.UNKNOWN, tokens.get(2).getType());
            assertEquals("\0", tokens.get(2).getText());
            assertEquals(TokenType.WORD, tokens.get(3).getType());
            assertEquals("koira", tokens.get(3).getText());
        }
        {
            List<Token> tokens = voikko.tokens("kissa\0");
            assertEquals(2, tokens.size());
            assertEquals(TokenType.WORD, tokens.get(0).getType());
            assertEquals("kissa", tokens.get(0).getText());
            assertEquals(TokenType.UNKNOWN, tokens.get(1).getType());
            assertEquals("\0", tokens.get(1).getText());
        }
        {
            List<Token> tokens = voikko.tokens("\0kissa");
            assertEquals(2, tokens.size());
            assertEquals(TokenType.UNKNOWN, tokens.get(0).getType());
            assertEquals("\0", tokens.get(0).getText());
            assertEquals(TokenType.WORD, tokens.get(1).getType());
            assertEquals("kissa", tokens.get(1).getText());
        }
        {
            List<Token> tokens = voikko.tokens("\0");
            assertEquals(1, tokens.size());
            assertEquals(TokenType.UNKNOWN, tokens.get(0).getType());
            assertEquals("\0", tokens.get(0).getText());
        }
        assertEquals(0, voikko.tokens("").size());
    }
}
