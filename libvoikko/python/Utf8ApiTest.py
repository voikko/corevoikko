# -*- coding: utf-8 -*-

# Copyright 2010 Harri Pitkänen (hatapitk@iki.fi)
# Test suite for *_cstr functions in libvoikko which are not normally
# used through Python API. Only non-deprecated functions are tested here.
# Tests for deprecated functions should be placed in DeprecatedApiTest.

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

from ctypes import byref
from ctypes import c_int
from ctypes import c_char_p
from ctypes import c_void_p
from ctypes import CDLL
from ctypes import POINTER
import unittest
import os

lib = None
handle = None

def spellCstr(word):
	return lib.voikkoSpellCstr(handle, word.encode("UTF-8"))

class Utf8ApiTest(unittest.TestCase):
	def setUp(self):
		global lib
		global handle
		if os.name == 'nt':
			lib = CDLL("libvoikko-1.dll")
		else:
			lib = CDLL("libvoikko.so.1")
		lib.voikkoInit.argtypes = [POINTER(c_char_p), c_char_p, c_int, c_char_p]
		lib.voikkoInit.restype = c_void_p
		
		lib.voikkoTerminate.argtypes = [c_void_p]
		lib.voikkoTerminate.restype = None
		
		lib.voikkoSpellCstr.argtypes = [c_void_p, c_char_p]
		lib.voikkoSpellCstr.restype = c_int
		
		error = c_char_p()
		handle = lib.voikkoInit(byref(error), "fi_FI", 0, None)
		if error.value != None:
			raise Exception(u"Initialization of Voikko failed: " + unicode(error.value, "UTF-8"))
	
	def tearDown(self):
		lib.voikkoTerminate(handle)
	
	def testSpellingWorksWithCapitalScandinavianLetters(self):
		self.failIf(spellCstr(u"Ääiti"))
		self.failUnless(spellCstr(u"Äiti"))
	
	def testSpellingWorksWithSmallScandinavianLetters(self):
		self.failIf(spellCstr(u"ääiti"))
		self.failUnless(spellCstr(u"äiti"))

if __name__ == "__main__":
	suite = unittest.TestLoader().loadTestsFromTestCase(Utf8ApiTest)
	unittest.TextTestRunner(verbosity=1).run(suite)
