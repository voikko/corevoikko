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
		self.assertEqual(u"+kissa(w505527)", analysis["WORDIDS"])
		self.assertEqual(u"+kissa(kissa)", analysis["WORDBASES"])
	
	def testBaseFormForCompoundWord1(self):
		analysisList = self.voikko.analyze(u"vatsaneläkeruokaa")
		analysis = self._assertSingletonAndGetItem(analysisList)
		self.assertEqual(u"vatsaneläkeruoka", analysis["BASEFORM"])
		self.assertEqual(u"+vatsa(w517091)+n+eläke(w501226)+ruoka", analysis["WORDIDS"])
		self.assertEqual(u"+vatsa(vatsa)+n+eläke(eläke)+ruoka(ruoka)", analysis["WORDBASES"])
	
	def testBaseFormForCompoundWord1(self):
		analysisList = self.voikko.analyze(u"hevosrakenteinen")
		analysis = self._assertSingletonAndGetItem(analysisList)
		self.assertEqual(u"hevosrakenteinen", analysis["BASEFORM"])
		self.assertEqual(u"+hevos(w502479)+rakenteinen(w528468)", analysis["WORDIDS"])
		self.assertEqual(u"+hevos(hevonen)+rakenteinen(rakenteinen)", analysis["WORDBASES"])
	
	def testBaseFormForNounDerivedFromVerb(self):
		analysisList = self.voikko.analyze(u"hyppijässä")
		analysis = self._assertSingletonAndGetItem(analysisList)
		self.assertEqual(u"hyppijä", analysis["BASEFORM"])
		self.assertEqual(u"+hyppi(w503034)+jä", analysis["WORDIDS"])
		self.assertEqual(u"+hyppi(hyppijä)+jä", analysis["WORDBASES"])
	
	def testBaseFormForNumeral(self):
		analysisList = self.voikko.analyze(u"kahdellakymmenelläseitsemällä")
		analysis = self._assertSingletonAndGetItem(analysisList)
		self.assertEqual(u"kaksikymmentäseitsemän", analysis["BASEFORM"])
		self.assertFalse("WORDIDS" in analysis)
		self.assertFalse("WORDBASES" in analysis)
	
	def testBaseFormForWordHavingNoInflectins(self):
		analysisList = self.voikko.analyze(u"koska")
		analysis = self._assertSingletonAndGetItem(analysisList)
		self.assertEqual(u"koska", analysis["BASEFORM"])
		self.assertFalse("WORDIDS" in analysis)
		self.assertEqual(u"+koska(koska)", analysis["WORDBASES"])


if __name__ == "__main__":
	unittest.main()
