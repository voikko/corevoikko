# -*- coding: utf-8 -*-

# Copyright 2009 Harri Pitkänen (hatapitk@iki.fi)
# Test suite for morphological analysis features in libvoikko that
# require non-standard dictionary configuration.

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

class ExtendedMorphologyTest(unittest.TestCase):
	def setUp(self):
		self.voikko = libvoikko.Voikko()
		self.voikko.init(variant = "standard+allextensions")
	
	def tearDown(self):
		self.voikko.terminate()
	
	def _assertSingletonAndGetItem(self, itemList):
		self.assertEqual(1, len(itemList), u"One item expected")
		return itemList[0]
	
	def testBaseForm(self):
		analysisList = self.voikko.analyze(u"kissoille")
		analysis = self._assertSingletonAndGetItem(analysisList)
		self.assertEqual(u"kissa", analysis["BASEFORM"])
	
	def testBaseFormForCompoundWord(self):
		analysisList = self.voikko.analyze(u"vatsaneläkeruokaa")
		analysis = self._assertSingletonAndGetItem(analysisList)
		self.assertEqual(u"vatsaneläkeruoka", analysis["BASEFORM"])
	
	def testBaseFormForNounDerivedFromVerb(self):
		analysisList = self.voikko.analyze(u"hyppijässä")
		analysis = self._assertSingletonAndGetItem(analysisList)
		self.assertEqual(u"hyppijä", analysis["BASEFORM"])
	
	def testBaseFormForNumeral(self):
		analysisList = self.voikko.analyze(u"kahdellakymmenelläseitsemällä")
		analysis = self._assertSingletonAndGetItem(analysisList)
		self.assertEqual(u"kaksikymmentäseitsemän", analysis["BASEFORM"])
	
	def testBaseFormForWordHavingNoInflectins(self):
		analysisList = self.voikko.analyze(u"koska")
		analysis = self._assertSingletonAndGetItem(analysisList)
		self.assertEqual(u"koska", analysis["BASEFORM"])


if __name__ == "__main__":
	unittest.main()
