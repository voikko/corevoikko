# -*- coding: utf-8 -*-

# Copyright 2010 Harri Pitkänen (hatapitk@iki.fi)
# Test suite for null linguistic components.

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
from TestUtils import MorphologyInfo, TestDataDir

VARIANT_NAME = u"NullComponentTest"

class NullComponentTest(unittest.TestCase):
	def setUp(self):
		info = MorphologyInfo()
		info.variant = VARIANT_NAME
		info.morphology = u"null"
		info.speller = u"AllOk"
		info.suggestion = u"null"
		self.dataDir = TestDataDir()
		self.dataDir.createMorphology(VARIANT_NAME, info)
		self.voikko = libvoikko.Voikko("fi-x-" + VARIANT_NAME, path = self.dataDir.getDirectory())
	
	def tearDown(self):
		self.voikko.terminate()
		self.dataDir.tearDown()
	
	def testNullAnalyzerWorks(self):
		analysisList = self.voikko.analyze(u"koira")
		self.assertEqual(0, len(analysisList))
	
	def testNullSuggestionGeneratorWorks(self):
		suggestionList = self.voikko.suggest(u"koirra")
		self.assertEqual(0, len(suggestionList))

	def testAllOkSpellerWorks(self):
		self.failUnless(self.voikko.spell(u"koirra"))


if __name__ == "__main__":
	unittest.main()
