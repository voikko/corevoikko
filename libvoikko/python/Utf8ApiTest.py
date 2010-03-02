# -*- coding: utf-8 -*-

# Copyright 2010 Harri Pitkänen (hatapitk@iki.fi)
# Test suite for *_cstr functions in libvoikko which are not normally
# used through Python API.

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

from ctypes import c_int
from ctypes import c_char_p
import unittest
from libvoikko import Voikko

voikko = Voikko()
voikko.lib.voikko_spell_cstr.argtypes = [c_int, c_char_p]
voikko.lib.voikko_spell_cstr.restype = c_int

def spellCstr(word):
	return voikko.lib.voikko_spell_cstr(voikko.handle, word.encode("UTF-8"))

class Utf8ApiTest(unittest.TestCase):
	def setUp(self):
		voikko.init()
		voikko.setAcceptFirstUppercase(True)
	
	def tearDown(self):
		voikko.terminate()
	
	def testSpellingWorksWithCapitalScandinavianLetters(self):
		self.failIf(spellCstr(u"Ääiti"))
		self.failUnless(spellCstr(u"Äiti"))
	
	def testSpellingWorksWithSmallScandinavianLetters(self):
		self.failIf(spellCstr(u"ääiti"))
		self.failUnless(spellCstr(u"äiti"))

if __name__ == "__main__":
	unittest.main()
