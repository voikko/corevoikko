# -*- coding: utf-8 -*-

# Copyright 2010 Harri Pitkänen (hatapitk@iki.fi)
# Test suite for functions that return information about available dictionaries.
#
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
from TestUtils import MorphologyInfo, TestDataDir, getVoikkoCLibrary
from ctypes import c_int, c_char_p, byref

VARIANT_NAME = u"dictionaryinfotest"

class DictionaryInfoTest(unittest.TestCase):
	def setUp(self):
		info = MorphologyInfo()
		info.variant = VARIANT_NAME
		info.morphology = u"null"
		self.dataDir = TestDataDir()
		self.dataDir.createMorphology(VARIANT_NAME, info)
	
	def tearDown(self):
		self.dataDir.tearDown()
	
	def testListSupportedLanguagesReturnsFinnish(self):
		languages = libvoikko.Voikko.listSupportedLanguages(self.dataDir.getDirectory())
		self.assertEqual(1, len(languages))
		self.assertEqual(u"fi", languages[0])
	
	def __tryOpenWithOldApi(self, variant):
		lib = getVoikkoCLibrary()
		handle = c_int(-1)
		error = lib.voikko_init_with_path(byref(handle), variant,
		        0, self.dataDir.getDirectory())
		isOk = not bool(error)
		lib.voikko_terminate(handle)
		return isOk
	
	def __tryOpenWithNewApi(self, variant):
		lib = getVoikkoCLibrary()
		error = c_char_p()
		handle = lib.voikkoInit(byref(error), variant,
		         0, self.dataDir.getDirectory())
		if error.value == None:
			lib.voikkoTerminate(handle)
			return True
		else:
			return False
	
	def testVariantCanBeOpenedWithOldApiUsingFullNameAsIs(self):
		self.assertTrue(self.__tryOpenWithOldApi(VARIANT_NAME))
	
	def testDefaultVariantCanBeOpenedWithOldApiUsingValuesSpecifiedInApi(self):
		self.assertTrue(self.__tryOpenWithOldApi(u""))
		self.assertTrue(self.__tryOpenWithOldApi(u"default"))
		self.assertTrue(self.__tryOpenWithOldApi(u"fi_FI"))
	
	def testVariantCannotBeOpenedWithOldApiUsingOtherValues(self):
		self.assertFalse(self.__tryOpenWithOldApi(u"ksajdlaksjdkasl"))
	
	def testVariantCanBeOpenedWithNewApiUsingBCP47LanguageTag(self):
		self.assertTrue(self.__tryOpenWithNewApi(u"fi-x-dictiona-ryinfote-st"))
		self.assertTrue(self.__tryOpenWithNewApi(u"fi-x-Dictiona-ryinfote-st"))
		self.assertTrue(self.__tryOpenWithNewApi(u"Fi-x-dictiona-ryinfote-st"))
		self.assertTrue(self.__tryOpenWithNewApi(u"fi-x-dictio-nary-info-test"))
		self.assertTrue(self.__tryOpenWithNewApi(u"fi-FI-x-dictiona-ryinfote-st"))
		self.assertTrue(self.__tryOpenWithNewApi(u"fi"))
		self.assertTrue(self.__tryOpenWithNewApi(u"fi-FI"))
	
	def testVariantCannotBeOpenedWithNewApiUsingOtherValues(self):
		self.assertFalse(self.__tryOpenWithNewApi(VARIANT_NAME))
		self.assertFalse(self.__tryOpenWithNewApi(u"ksajdlaksjdkasl"))
		self.assertFalse(self.__tryOpenWithNewApi(u"sv"))
		self.assertFalse(self.__tryOpenWithNewApi(u"fi-x-askldjaslkdj"))
		self.assertFalse(self.__tryOpenWithNewApi(u""))
		self.assertFalse(self.__tryOpenWithNewApi(None))


if __name__ == "__main__":
	unittest.main()
