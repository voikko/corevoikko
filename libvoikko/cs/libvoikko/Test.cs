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
	}
}

