using System;
using NUnit.Framework;
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
		public void anotherObjectCanBeCreatedUsedAndDeletedInParallel() {
			Voikko medicalVoikko = new Voikko("fi-x-medicine");
			Assert.IsTrue(medicalVoikko.Spell("amifostiini"));
			Assert.IsFalse(voikko.Spell("amifostiini"));
			medicalVoikko.Dispose();
			Assert.IsFalse(voikko.Spell("amifostiini"));
		}
		
		[Test]
		public void spell()
		{
			Assert.IsTrue(voikko.Spell("määrä"));
			Assert.IsFalse(voikko.Spell("määä"));
		}
	}
}

