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
			Dictionary d1 = new Dictionary("fi", "a", "b");
			Dictionary d2 = new Dictionary("fi", "a", "c");
			Dictionary d3 = new Dictionary("fi", "c", "b");
			Dictionary d4 = new Dictionary("fi", "a", "b");
			Dictionary d5 = new Dictionary("sv", "a", "b");
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
			Dictionary d1 = new Dictionary("fi", "a", "b");
			Dictionary d2 = new Dictionary("fi", "a", "c");
			Dictionary d3 = new Dictionary("fi", "c", "b");
			Dictionary d4 = new Dictionary("fi", "a", "b");
			Dictionary d5 = new Dictionary("sv", "a", "b");
			Assert.AreNotEqual(d1.GetHashCode(), d2.GetHashCode());
			Assert.AreNotEqual(d1.GetHashCode(), d3.GetHashCode());
			Assert.AreNotEqual(d4.GetHashCode(), d5.GetHashCode());
			Assert.AreEqual(d1.GetHashCode(), d4.GetHashCode());
		}

		[Test]
		public void listDictsWithoutPath()
		{
			List<Dictionary> dicts = Voikko.listDicts();
			Assert.IsTrue(dicts.Count > 0);
			Dictionary standard = dicts[0];
			Assert.AreEqual("standard", standard.Variant);
		}

		//[Test] TODO: should work, write test
		public void listDictsWithPathAndAttributes()
		{
			
		}

		[Test]
		public void spell()
		{
			Assert.IsTrue(voikko.Spell("määrä"));
			Assert.IsFalse(voikko.Spell("määä"));
		}
	}
}

