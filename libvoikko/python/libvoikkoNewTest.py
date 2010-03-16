# -*- coding: utf-8 -*-

# Copyright 2009 - 2010 Harri Pitkänen (hatapitk@iki.fi)
# Test suite for testing public API of libvoikko and the Python interface.

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
from libvoikkoNew import *

class LibvoikkoTest(unittest.TestCase):
	def setUp(self):
		self.voikko = Voikko()
	
	def tearDown(self):
		self.voikko.terminate()
	
	def testInitAndTerminate(self):
		pass # do nothing, just check that setUp and tearDown complete succesfully
	
	def testTerminateCanBeCalledMultipleTimes(self):
		self.voikko.terminate()
		self.voikko.terminate()
	
	def TODOtestInitAndTerminate(self):
		def trySpell():
			self.voikko.spell(u"kissa")
		# Init can be called multiple times
		voikko2 = Voikko()
		self.failUnless(self.voikko.spell(u"kissa"))
		self.voikko.terminate()
		self.assertRaises(VoikkoException, trySpell)
		# Initialization can be done again
		self.voikko.init()
		self.failUnless(self.voikko.spell(u"kissa"))
	
	def testDictionaryComparisonWorks(self):
		d1 = Dictionary("a", u"b")
		d2 = Dictionary("a", u"c")
		d3 = Dictionary("c", u"b")
		d4 = Dictionary("a", u"b")
		self.assertNotEqual(u"kissa", d1)
		self.assertNotEqual(d1, u"kissa")
		self.assertNotEqual(d1, d2)
		self.assertNotEqual(d1, d3)
		self.assertEqual(d1, d4)
		self.failUnless(d1 < d2)
		self.failUnless(d2 < d3)
	
	def testDictionaryHashCodeWorks(self):
		d1 = Dictionary("a", u"b")
		d2 = Dictionary("a", u"c")
		d3 = Dictionary("c", u"b")
		d4 = Dictionary("a", u"b")
		self.assertNotEqual(hash(d1), hash(d2))
		self.assertNotEqual(hash(d1), hash(d3))
		self.assertEqual(hash(d1), hash(d4))
	
	def testInitWithCorrectDictWorks(self):
		self.voikko.terminate()
		self.voikko = Voikko(variant = "standard")
		self.failIf(self.voikko.spell(u"amifostiini"))
		self.voikko.terminate()
		self.voikko = Voikko(variant = "medicine")
		self.failUnless(self.voikko.spell(u"amifostiini"))
	
	def testInitWithNonExistentDictThrowsException(self):
		def tryInit():
			self.voikko = Voikko(variant = "nonexistentvariant")
		self.voikko.terminate()
		self.assertRaises(VoikkoException, tryInit)
	
	def TODOtestInitWithCacheSizeWorks(self):
		# TODO: better test
		self.voikko.terminate()
		self.voikko.init(cacheSize = 3)
		self.failUnless(self.voikko.spell(u"kissa"))
	
	def TODOtestInitWithPathWorks(self):
		# TODO: better test
		self.voikko.terminate()
		self.voikko.init(path = "/path/to/nowhere")
		self.failUnless(self.voikko.spell(u"kissa"))

if __name__ == "__main__":
	unittest.main()
