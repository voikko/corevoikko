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
from ctypes import c_char
from ctypes import c_char_p
from ctypes import c_size_t
from ctypes import c_void_p
from ctypes import string_at
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
		
		lib.voikkoSuggestCstr.argtypes = [c_void_p, c_char_p]
		lib.voikkoSuggestCstr.restype = POINTER(c_char_p)
		
		lib.voikkoFreeCstrArray.argtypes = [POINTER(c_char_p)]
		lib.voikkoFreeCstrArray.restype = None
		
		lib.voikkoHyphenateCstr.argtypes = [c_void_p, c_char_p]
		lib.voikkoHyphenateCstr.restype = POINTER(c_char)
		
		lib.voikkoFreeCstr.argtypes = [POINTER(c_char)]
		lib.voikkoFreeCstr.restype = None
		
		lib.voikkoNextTokenCstr.argtypes = [c_void_p, c_char_p, c_size_t, POINTER(c_size_t)]
		lib.voikkoNextTokenCstr.restype = c_int
		
		lib.voikkoNextSentenceStartCstr.argtypes = [c_void_p, c_char_p, c_size_t, POINTER(c_size_t)]
		lib.voikkoNextSentenceStartCstr.restype = c_int
		
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
	
	def testSuggestCstrWorks(self):
		cSuggestions = lib.voikkoSuggestCstr(handle, u"koirra")
		pSuggestions = []
		
		if not bool(cSuggestions):
			return pSuggestions
		
		i = 0
		while bool(cSuggestions[i]):
			pSuggestions.append(unicode(cSuggestions[i], "UTF-8"))
			i = i + 1
		
		lib.voikkoFreeCstrArray(cSuggestions)
		self.failUnless(u"koira" in pSuggestions)
	
	def testHyphenateCstrWorks(self):
		cHyphenationPattern = lib.voikkoHyphenateCstr(handle, u"koira".encode("UTF-8"))
		hyphenationPattern = string_at(cHyphenationPattern)
		lib.voikkoFreeCstr(cHyphenationPattern)
		self.assertEqual(u"   - ", hyphenationPattern)
	
	def testNextTokenCstrWorks(self):
		tokenLen = c_size_t()
		text = u"Kissa ja koira".encode("UTF-8")
		tokenType = lib.voikkoNextTokenCstr(handle,
			            text, len(text), byref(tokenLen))
		self.assertEqual(5, tokenLen.value)
		self.assertEqual(1, tokenType)
	
	def testNextSentenceStartCstrWorks(self):
		sentenceLen = c_size_t()
		text = u"Kissa ei ole koira. Koira ei ole kissa.".encode("UTF-8")
		sentenceType = lib.voikkoNextSentenceStartCstr(handle,
			            text, len(text), byref(sentenceLen))
		self.assertEqual(20, sentenceLen.value)
		self.assertEqual(2, sentenceType) # SENTENCE_PROBABLE

if __name__ == "__main__":
	suite = unittest.TestLoader().loadTestsFromTestCase(Utf8ApiTest)
	unittest.TextTestRunner(verbosity=1).run(suite)
