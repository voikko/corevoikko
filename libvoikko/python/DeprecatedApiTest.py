# -*- coding: utf-8 -*-

# Copyright 2010 Harri Pitkänen (hatapitk@iki.fi)
# Test suite for deprecated functions in libvoikko. Uses the OLD version
# of libvoikko Python module since that module (and its test) already
# cover large part of the deprecated API.

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
from libvoikkoOld import Token
from libvoikkoOld import Voikko
from ctypes import byref
from ctypes import c_int
from ctypes import c_char
from ctypes import c_char_p
from ctypes import c_size_t
from ctypes import c_wchar_p
from ctypes import c_void_p
from ctypes import string_at
from ctypes import POINTER
from ctypes import Structure

class CGrammarError(Structure):
	_fields_ = [("errorCode", c_int),
	            ("errorLevel", c_int),
	            ("errorDescription", c_char_p),
	            ("startPos", c_size_t),
	            ("errorLen", c_size_t),
	            ("suggestions", POINTER(c_char_p))]

voikko = Voikko()
voikko.lib.voikko_set_string_option.argtypes = [c_int, c_int, c_char_p]
voikko.lib.voikko_set_string_option.restype = c_int

voikko.lib.voikko_spell_cstr.argtypes = [c_int, c_char_p]
voikko.lib.voikko_spell_cstr.restype = c_int

voikko.lib.voikko_suggest_cstr.argtypes = [c_int, c_char_p]
voikko.lib.voikko_suggest_cstr.restype = POINTER(c_char_p)

voikko.lib.voikko_hyphenate_cstr.argtypes = [c_int, c_char_p]
voikko.lib.voikko_hyphenate_cstr.restype = POINTER(c_char)

voikko.lib.voikko_next_token_cstr.argtypes = [c_int, c_char_p, c_size_t, POINTER(c_size_t)]
voikko.lib.voikko_next_token_cstr.restype = c_int

voikko.lib.voikko_next_sentence_start_ucs4.argtypes = [c_int, c_wchar_p, c_size_t, POINTER(c_size_t)]
voikko.lib.voikko_next_sentence_start_ucs4.restype = c_int

voikko.lib.voikko_next_sentence_start_cstr.argtypes = [c_int, c_char_p, c_size_t, POINTER(c_size_t)]
voikko.lib.voikko_next_sentence_start_cstr.restype = c_int

voikko.lib.voikko_next_grammar_error_cstr.argtypes = [c_int, c_char_p, c_size_t, c_size_t, c_int]
voikko.lib.voikko_next_grammar_error_cstr.restype = CGrammarError

voikko.lib.voikko_free_suggest_cstr.argtypes = [POINTER(c_char_p)]
voikko.lib.voikko_free_suggest_cstr.restype = None

voikko.lib.voikko_analyze_word_cstr.argtypes = [c_int, c_char_p]
voikko.lib.voikko_analyze_word_cstr.restype = POINTER(c_void_p)

voikko.lib.voikko_free_mor_analysis.argtypes = [POINTER(c_void_p)]
voikko.lib.voikko_free_mor_analysis.restype = None

voikko.lib.voikko_mor_analysis_value_ucs4.argtypes = [c_void_p, c_char_p]
voikko.lib.voikko_mor_analysis_value_ucs4.restype = c_wchar_p

class DeprecatedApiTest(unittest.TestCase):
	def setUp(self):
		voikko.init()
	
	def tearDown(self):
		voikko.terminate()
	
	def testIntersectCompoundLevelCanBeSet(self):
		# This will do nothing but should not fail
		voikko.lib.voikko_set_int_option(voikko.handle, 5, 56)
	
	def testEncodingCanBeSet(self):
		self.assertEquals(1, voikko.lib.voikko_set_string_option(voikko.handle, 2, "UTF-8"))
		self.assertEquals(0, voikko.lib.voikko_set_string_option(voikko.handle, 2, "iso-8859-1"))
		self.assertEquals(0, voikko.lib.voikko_set_string_option(voikko.handle, 1, "UTF-8"))
	
	def test_spell_cstr_works(self):
		self.assertEquals(1, voikko.lib.voikko_spell_cstr(voikko.handle, u"kissa".encode("UTF-8")))
		self.assertEquals(0, voikko.lib.voikko_spell_cstr(voikko.handle, u"koirra".encode("UTF-8")))
	
	def test_suggest_cstr_works(self):
		cSuggestions = voikko.lib.voikko_suggest_cstr(voikko.handle, u"koirra")
		pSuggestions = []
		
		if not bool(cSuggestions):
			return pSuggestions
		
		i = 0
		while bool(cSuggestions[i]):
			pSuggestions.append(unicode(cSuggestions[i], "UTF-8"))
			i = i + 1
		
		voikko.lib.voikko_free_suggest_cstr(cSuggestions)
		self.failUnless(u"koira" in pSuggestions)
	
	def test_hyphenate_cstr_works(self):
		cHyphenationPattern = voikko.lib.voikko_hyphenate_cstr(voikko.handle, u"koira".encode("UTF-8"))
		hyphenationPattern = string_at(cHyphenationPattern)
		voikko.lib.voikko_free_hyphenate(cHyphenationPattern)
		self.assertEqual(u"   - ", hyphenationPattern)
	
	def test_next_token_cstr_works(self):
		tokenLen = c_size_t()
		text = u"Kissa ja koira".encode("UTF-8")
		tokenType = voikko.lib.voikko_next_token_cstr(voikko.handle,
			            text, len(text), byref(tokenLen))
		self.assertEqual(5, tokenLen.value)
		self.assertEqual(Token.WORD, tokenType)
	
	def test_next_sentece_start_cstr_works(self):
		sentenceLen = c_size_t()
		text = u"Kissa ei ole koira. Koira ei ole kissa.".encode("UTF-8")
		sentenceType = voikko.lib.voikko_next_sentence_start_cstr(voikko.handle,
			            text, len(text), byref(sentenceLen))
		self.assertEqual(20, sentenceLen.value)
		self.assertEqual(2, sentenceType) # SENTENCE_PROBABLE
	
	def test_next_sentence_start_ucs4_works(self):
		sentenceLen = c_size_t()
		text = u"Kissa ei ole koira. Koira ei ole kissa."
		sentenceType = voikko.lib.voikko_next_sentence_start_ucs4(voikko.handle,
			            text, len(text), byref(sentenceLen))
		self.assertEqual(20, sentenceLen.value)
		self.assertEqual(2, sentenceType) # SENTENCE_PROBABLE
	
	def test_voikko_next_grammar_error_cstr_works(self):
		text = u"Osaan joten kuten ajaa autoa.".encode("UTF-8")
		error = voikko.lib.voikko_next_grammar_error_cstr(voikko.handle, text, len(text), 0, 0)
		self.assertEqual(1, error.errorCode)
		self.assertEqual(6, error.startPos)
		self.assertEqual(11, error.errorLen)
		self.assertEqual(u"jotenkuten", error.suggestions[0])
		voikko.lib.voikko_free_suggest_cstr(error.suggestions)
	
	def test_voikko_analyze_word_cstr_works(self):
		analysis = voikko.lib.voikko_analyze_word_cstr(voikko.handle, u"kansaneläkelaitos".encode("UTF-8"))
		structure = voikko.lib.voikko_mor_analysis_value_ucs4(analysis[0], "STRUCTURE")
		self.assertEqual(u"=pppppp=ppppp=pppppp", structure)
		voikko.lib.voikko_free_mor_analysis(analysis)

if __name__ == "__main__":
	unittest.main()
