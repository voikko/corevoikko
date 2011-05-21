using System;
using NUnit.Framework;
using System.Collections.Generic;
namespace libvoikko
{
	[TestFixture]
	public class Test
	{
		private Voikko voikko;

		[SetUp]
		public void setUp()
		{
			voikko = new Voikko("fi");
		}

		[TearDown]
		public void tearDown()
		{
			voikko.Dispose();
			// Do garbage collection after every test method. This will make errors
			// in native memory management (double frees etc.) more likely to show up.
			voikko = null;
			GC.Collect();
			GC.WaitForPendingFinalizers();
		}

		[Test]
		public void initAndTerminate()
		{
			// do nothing, just check that setUp and tearDown complete successfully
		}

		[Test]
		public void terminateCanBeCalledMultipleTimes()
		{
			voikko.Dispose();
			voikko.Dispose();
		}

		[Test]
		public void anotherObjectCanBeCreatedUsedAndDeletedInParallel()
		{
			Voikko medicalVoikko = new Voikko("fi-x-medicine");
			Assert.IsTrue(medicalVoikko.Spell("amifostiini"));
			Assert.IsFalse(voikko.Spell("amifostiini"));
			medicalVoikko.Dispose();
			Assert.IsFalse(voikko.Spell("amifostiini"));
		}

		[Test]
		public void dictionaryComparisonWorks()
		{
			VoikkoDictionary d1 = new VoikkoDictionary("fi", "a", "b");
			VoikkoDictionary d2 = new VoikkoDictionary("fi", "a", "c");
			VoikkoDictionary d3 = new VoikkoDictionary("fi", "c", "b");
			VoikkoDictionary d4 = new VoikkoDictionary("fi", "a", "b");
			VoikkoDictionary d5 = new VoikkoDictionary("sv", "a", "b");
			Assert.IsFalse(d1.Equals("kissa"));
			Assert.IsFalse("kissa".Equals(d1));
			Assert.IsFalse(d1.Equals(d2));
			Assert.IsFalse(d1.Equals(d3));
			Assert.IsFalse(d4.Equals(d5));
			Assert.IsTrue(d1.Equals(d4));
			Assert.IsTrue(d1.CompareTo(d2) < 0);
			Assert.IsTrue(d2.CompareTo(d3) < 0);
			Assert.IsTrue(d4.CompareTo(d5) < 0);
		}

		[Test]
		public void dictionaryHashCodeWorks()
		{
			VoikkoDictionary d1 = new VoikkoDictionary("fi", "a", "b");
			VoikkoDictionary d2 = new VoikkoDictionary("fi", "a", "c");
			VoikkoDictionary d3 = new VoikkoDictionary("fi", "c", "b");
			VoikkoDictionary d4 = new VoikkoDictionary("fi", "a", "b");
			VoikkoDictionary d5 = new VoikkoDictionary("sv", "a", "b");
			Assert.AreNotEqual(d1.GetHashCode(), d2.GetHashCode());
			Assert.AreNotEqual(d1.GetHashCode(), d3.GetHashCode());
			Assert.AreNotEqual(d4.GetHashCode(), d5.GetHashCode());
			Assert.AreEqual(d1.GetHashCode(), d4.GetHashCode());
		}

		[Test]
		public void listDictsWithoutPath()
		{
			List<VoikkoDictionary> dicts = Voikko.listDicts();
			Assert.IsTrue(dicts.Count > 0);
			VoikkoDictionary standard = dicts[0];
			Assert.AreEqual("standard", standard.Variant);
		}

		//[Test] TODO: should work, write test
		public void listDictsWithPathAndAttributes()
		{
			
		}

		[Test]
		public void initWithCorrectDictWorks()
		{
			voikko.Dispose();
			voikko = new Voikko("fi-x-standard");
			Assert.IsFalse(voikko.Spell("amifostiini"));
			voikko.Dispose();
			voikko = new Voikko("fi-x-medicine");
			Assert.IsTrue(voikko.Spell("amifostiini"));
		}

		[Test]
		public void initWithNonExistentDictThrowsException()
		{
			voikko.Dispose();
			try
			{
				voikko = new Voikko("fi-x-non-existent-variant");
			} catch (VoikkoException e)
			{
				Assert.AreEqual("Specified dictionary variant was not found", e.Message);
				return;
			}
			Assert.Fail("Expected exception not thrown");
		}

		[Test]
		public void initWithPathWorks()
		{
			// TODO: better test
			voikko.Dispose();
			voikko = new Voikko("fi", "/path/to/nowhere");
			Assert.IsTrue(voikko.Spell("kissa"));
		}

		[Test]
		public void spellAfterTerminateThrowsException()
		{
			voikko.Dispose();
			try
			{
				voikko.Spell("kissa");
			} catch (VoikkoException)
			{
				return;
			}
			Assert.Fail("Expected exception not thrown");
		}

		[Test]
		public void spell()
		{
			Assert.IsTrue(voikko.Spell("määrä"));
			Assert.IsFalse(voikko.Spell("määä"));
		}

		[Test]
		public void suggest()
		{
			Assert.IsTrue(voikko.Suggest("koirra").Contains("koira"));
			Assert.IsTrue(voikko.Suggest("määärä").Contains("määrä"));
			Assert.AreEqual(0, voikko.Suggest("lasjkblvankirknaslvethikertvhgn").Count);
		}

		[Test]
		public void suggestGc()
		{
			for (int i = 0; i < 10; i++)
			{
				Assert.IsTrue(voikko.Suggest("määärä").Contains("määrä"));
				GC.Collect();
				GC.WaitForPendingFinalizers();
			}
		}

		[Test]
		public void suggestReturnsArgumentIfWordIsCorrect()
		{
			List<string> suggestions = voikko.Suggest("koira");
			Assert.AreEqual(1, suggestions.Count);
			Assert.AreEqual("koira", suggestions[0]);
		}

		[Test]
		public void grammarErrorsAndExplanation()
		{
			List<GrammarError> errors = voikko.GrammarErrors("Minä olen joten kuten kaunis.");
			Assert.AreEqual(1, errors.Count);
			GrammarError error = errors[0];
			Assert.AreEqual(10, error.StartPos);
			Assert.AreEqual(11, error.ErrorLen);
			Assert.AreEqual(1, error.Suggestions.Count);
			Assert.AreEqual("jotenkuten", error.Suggestions[0]);
			int code = error.ErrorCode;
			Assert.AreEqual("Virheellinen kirjoitusasu", voikko.GrammarErrorExplanation(code, "fi"));
			Assert.AreEqual("Incorrect spelling of word(s)", voikko.GrammarErrorExplanation(code, "en"));
		}

		[Test]
		public void noGrammarErrorsInEmptyParagraph()
		{
			List<GrammarError> errors = voikko.GrammarErrors("Olen täi.\n\nOlen täi.");
			Assert.AreEqual(0, errors.Count);
		}

		[Test]
		public void grammarErrorOffsetsInMultipleParagraphs()
		{
			List<GrammarError> errors = voikko.GrammarErrors("Olen täi.\n\nOlen joten kuten.");
			Assert.AreEqual(1, errors.Count);
			Assert.AreEqual(16, errors[0].StartPos);
			Assert.AreEqual(11, errors[0].ErrorLen);
		}

		[Test]
		public void analyze()
		{
			List<Analysis> analysisList = voikko.Analyze("kansaneläkehakemus");
			Assert.AreEqual(1, analysisList.Count);
			Assert.AreEqual("=pppppp=ppppp=ppppppp", analysisList[0]["STRUCTURE"]);
		}

		[Test]
		public void tokens()
		{
			List<Token> tokens = voikko.Tokens("kissa ja koira sekä härkä");
			Assert.AreEqual(9, tokens.Count);
			Assert.AreEqual(TokenType.WORD, tokens[2].Type);
			Assert.AreEqual("ja", tokens[2].Text);
			Assert.AreEqual(TokenType.WHITESPACE, tokens[7].Type);
			Assert.AreEqual(" ", tokens[7].Text);
			Assert.AreEqual(TokenType.WORD, tokens[8].Type);
			Assert.AreEqual("härkä", tokens[8].Text);
		}

		[Test]
		public void sentences()
		{
			List<Sentence> sentences = voikko.Sentences("Kissa ei ole koira. Koira ei ole kissa.");
			Assert.AreEqual(2, sentences.Count);
			Assert.AreEqual("Kissa ei ole koira. ", sentences[0].Text);
			Assert.AreEqual(SentenceStartType.PROBABLE, sentences[0].NextStartType);
			Assert.AreEqual("Koira ei ole kissa.", sentences[1].Text);
			Assert.AreEqual(SentenceStartType.NONE, sentences[1].NextStartType);
		}

		[Test]
		public void hyphenationPattern()
		{
			Assert.AreEqual("   - ", voikko.GetHyphenationPattern("kissa"));
			Assert.AreEqual("   - ", voikko.GetHyphenationPattern("määrä"));
			Assert.AreEqual("    - =  - ", voikko.GetHyphenationPattern("kuorma-auto"));
			Assert.AreEqual("   =  ", voikko.GetHyphenationPattern("vaa'an"));
		}

		[Test]
		public void hyphenate()
		{
			Assert.AreEqual("kis-sa", voikko.Hyphenate("kissa"));
			Assert.AreEqual("mää-rä", voikko.Hyphenate("määrä"));
			Assert.AreEqual("kuor-ma-au-to", voikko.Hyphenate("kuorma-auto"));
			Assert.AreEqual("vaa-an", voikko.Hyphenate("vaa'an"));
		}
		
	}
}

