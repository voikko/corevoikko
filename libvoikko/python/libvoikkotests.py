# -*- coding: utf-8 -*-

# Copyright 2009 Harri Pitkänen (hatapitk@iki.fi)
# Test suite for the Python interface to libvoikko

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

import unittest
import libvoikko

class LibvoikkoTest(unittest.TestCase):
	def setUp(self):
		self.voikko = libvoikko.Voikko()
		self.voikko.init()
	
	def tearDown(self):
		self.voikko.terminate()
	
	def testInitAndTerminate(self):
		def trySpell():
			self.voikko.spell(u"kissa")
		# Init can be called multiple times
		self.voikko.init()
		self.failUnless(self.voikko.spell(u"kissa"))
		# Terminate can be called multiple times
		self.voikko.terminate()
		self.voikko.terminate()
		self.assertRaises(libvoikko.VoikkoException, trySpell)
		# Initialization can be done again
		self.voikko.init()
		self.failUnless(self.voikko.spell(u"kissa"))
	
	def testSpell(self):
		self.failUnless(self.voikko.spell(u"määrä"))
		self.failIf(self.voikko.spell(u"määä"))
	
	def testSuggest(self):
		suggs = self.voikko.suggest(u"koirra")
		self.failUnless(u"koira" in suggs)
	
	def testGrammarErrorsAndExplanation(self):
		errors = self.voikko.grammarErrors(u"Minä olen joten kuten kaunis.")
		self.assertEqual(1, len(errors))
		error = errors[0]
		self.assertEqual(10, error.startPos)
		self.assertEqual(11, error.errorLen)
		self.assertEqual([u"jotenkuten"], error.suggestions)
		code = error.errorCode
		errorFi = self.voikko.grammarErrorExplanation(code, "fi")
		self.assertEqual(u"Virheellinen kirjoitusasu.", errorFi)
		errorEn = self.voikko.grammarErrorExplanation(code, "en")
		self.assertEqual(u"Incorrect spelling of word(s).", errorEn)
	
	def testAnalyze(self):
		analysisList = self.voikko.analyze(u"kansaneläkelaitos")
		self.assertEqual(1, len(analysisList))
		analysis = analysisList[0]
		self.assertEqual(u"=pppppp=ppppp=pppppp", analysis["STRUCTURE"])
	
	def testTokens(self):
		tokenList = self.voikko.tokens(u"kissa ja koira")
		self.assertEqual(5, len(tokenList))
		tokenJa = tokenList[2]
		self.assertEqual(libvoikko.Token.WORD, tokenJa.tokenType)
		self.assertEqual(u"ja", tokenJa.tokenText)
	
	def testHyphenationPattern(self):
		pattern = self.voikko.getHyphenationPattern(u"kissa")
		self.assertEqual("   - ", pattern)
		pattern = self.voikko.getHyphenationPattern(u"määrä")
		self.assertEqual("   - ", pattern)
		pattern = self.voikko.getHyphenationPattern(u"kuorma-auto")
		self.assertEqual("    - =  - ", pattern)
		pattern = self.voikko.getHyphenationPattern(u"vaa'an")
		self.assertEqual("   =  ", pattern)
	
	def testHyphenate(self):
		self.assertEqual(u"kis-sa", self.voikko.hyphenate(u"kissa"))
		self.assertEqual(u"mää-rä", self.voikko.hyphenate(u"määrä"))
		self.assertEqual(u"kuor-ma-au-to", self.voikko.hyphenate(u"kuorma-auto"))
		self.assertEqual(u"vaa-an", self.voikko.hyphenate(u"vaa'an"))
	
	def testSetIgnoreDot(self):
		self.voikko.setIgnoreDot(False)
		self.failIf(self.voikko.spell(u"kissa."))
		self.voikko.setIgnoreDot(True)
		self.failUnless(self.voikko.spell(u"kissa."))
	
	def testSetIgnoreNumbers(self):
		self.voikko.setIgnoreNumbers(False)
		self.failIf(self.voikko.spell(u"kissa2"))
		self.voikko.setIgnoreNumbers(True)
		self.failUnless(self.voikko.spell(u"kissa2"))
	
	def testSetIgnoreUppercase(self):
		self.voikko.setIgnoreUppercase(False)
		self.failIf(self.voikko.spell(u"KAAAA"))
		self.voikko.setIgnoreUppercase(True)
		self.failUnless(self.voikko.spell(u"KAAAA"))
	
	def testAcceptFirstUppercase(self):
		self.voikko.setAcceptFirstUppercase(False)
		self.failIf(self.voikko.spell("Kissa"))
		self.voikko.setAcceptFirstUppercase(True)
		self.failUnless(self.voikko.spell("Kissa"))
	
	def testAcceptAllUppercase(self):
		self.voikko.setIgnoreUppercase(False)
		self.voikko.setAcceptAllUppercase(False)
		self.failIf(self.voikko.spell("KISSA"))
		self.voikko.setAcceptAllUppercase(True)
		self.failUnless(self.voikko.spell("KISSA"))
		self.failIf(self.voikko.spell("KAAAA"))
	
	def testIgnoreNonwords(self):
		self.voikko.setIgnoreNonwords(False)
		self.failIf(self.voikko.spell("hatapitk@iki.fi"))
		self.voikko.setIgnoreNonwords(True)
		self.failUnless(self.voikko.spell("hatapitk@iki.fi"))
		self.failIf(self.voikko.spell("ashdaksd"))
	
	def testAcceptExtraHyphens(self):
		self.voikko.setAcceptExtraHyphens(False)
		self.failIf(self.voikko.spell("kerros-talo"))
		self.voikko.setAcceptExtraHyphens(True)
		self.failUnless(self.voikko.spell("kerros-talo"))
	
	def testAcceptMissingHyphens(self):
		self.voikko.setAcceptMissingHyphens(False)
		self.failIf(self.voikko.spell("sosiaali"))
		self.voikko.setAcceptMissingHyphens(True)
		self.failUnless(self.voikko.spell("sosiaali"))
	
	def testSetAcceptTitlesInGc(self):
		self.voikko.setAcceptTitlesInGc(False)
		self.assertEqual(1, len(self.voikko.grammarErrors(u"Kissa on eläin")))
		self.voikko.setAcceptTitlesInGc(True)
		self.assertEqual(0, len(self.voikko.grammarErrors(u"Kissa on eläin")))
	
	def testSetAcceptUnfinishedParagraphsInGc(self):
		self.voikko.setAcceptUnfinishedParagraphsInGc(False)
		self.assertEqual(1, len(self.voikko.grammarErrors(u"Kissa on ")))
		self.voikko.setAcceptUnfinishedParagraphsInGc(True)
		self.assertEqual(0, len(self.voikko.grammarErrors(u"Kissa on ")))
	
	def testSetAcceptBulletedListsInGc(self):
		self.voikko.setAcceptBulletedListsInGc(False)
		self.assertNotEqual(0, len(self.voikko.grammarErrors(u"kissa")))
		self.voikko.setAcceptBulletedListsInGc(True)
		self.assertEqual(0, len(self.voikko.grammarErrors(u"kissa")))
	
	def testSetNoUglyHyphenation(self):
		self.voikko.setNoUglyHyphenation(False)
		self.assertEqual(u"i-va", self.voikko.hyphenate(u"iva"))
		self.voikko.setNoUglyHyphenation(True)
		self.assertEqual(u"iva", self.voikko.hyphenate(u"iva"))
	
	def testSetMinHyphenatedWordLength(self):
		self.voikko.setMinHyphenatedWordLength(6)
		self.assertEqual(u"koira", self.voikko.hyphenate(u"koira"))
		self.voikko.setMinHyphenatedWordLength(2)
		self.assertEqual(u"koi-ra", self.voikko.hyphenate(u"koira"))
	
	def testSetSuggestionStrategy(self):
		self.voikko.setSuggestionStrategy(libvoikko.SuggestionStrategy.OCR)
		self.failIf(u"koira" in self.voikko.suggest(u"koari"))
		self.failUnless(u"koira" in self.voikko.suggest(u"koir_"))
		self.voikko.setSuggestionStrategy(libvoikko.SuggestionStrategy.TYPO)
		self.failUnless(u"koira" in self.voikko.suggest(u"koari"))



if __name__ == "__main__":
	unittest.main()
