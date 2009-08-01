# -*- coding: utf-8 -*-

# Copyright 2009 Harri Pitkänen (hatapitk@iki.fi)
# Python interface to libvoikko, library of Finnish language tools.
# This library requires Python version 2.5 or newer.

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
from ctypes import CDLL
from ctypes import c_int
from ctypes import c_char_p
from ctypes import c_wchar_p
from ctypes import c_size_t
from ctypes import c_void_p
from ctypes import pointer
from ctypes import POINTER
from ctypes import Structure

class Token:
	NONE = 0
	WORD = 1
	PUNCTUATION = 2
	WHITESPACE = 3
	UNKNOWN = 4
	
	TYPE_NAMES = ["NONE", "WORD", "PUNCTUATION", "WHITESPACE", "UNKNOWN"]
	
	def __init__(self, tokenText, tokenType):
		self.tokenText = tokenText
		self.tokenType = tokenType
	
	def __repr__(self):
		return (u"<" + self.tokenText + u"," + \
		       Token.TYPE_NAMES[self.tokenType] + u">").encode("UTF-8")

class CGrammarError(Structure):
	_fields_ = [("errorCode", c_int),
	            ("errorLevel", c_int),
	            ("errorDescription", c_char_p),
	            ("startPos", c_size_t),
	            ("errorLen", c_size_t),
	            ("suggestions", POINTER(c_char_p))]

class GrammarError:
	def __init__(self, cGrammarError):
		self.errorCode = cGrammarError.errorCode
		self.startPos = cGrammarError.startPos
		self.errorLen = cGrammarError.errorLen
		self.suggestions = []
		if bool(cGrammarError.suggestions):
			i = 0
			while bool(cGrammarError.suggestions[i]):
				self.suggestions.append(
				     unicode(cGrammarError.suggestions[i], "UTF-8"))
				i = i + 1
	
	def __repr__(self):
		return u"<" + self.toString() + u">"
	
	def toString(self):
		s = u'[code=%i, level=0, descr="", stpos=%i, len=%i, suggs={' \
		    % (self.errorCode, self.startPos, self.errorLen)
		first = True
		for suggestion in self.suggestions:
			if not first:
				s = s + u','
				first = False
			s = s + u'"' + suggestion + u'"'
		s = s + u"}]"
		return s

def _checkInited(voikko):
	if voikko.handle.value < 0:
		raise Exception("Voikko not initialized")


def _boolToInt(bool):
	if bool:
		return 1
	else:
		return 0

class Voikko:
	def __init__(self):
		self.handle = c_int(-1)
		self.lib = CDLL("libvoikko.so.1")
		
		self.lib.voikko_init.argtypes = [POINTER(c_int), c_char_p, c_int]
		self.lib.voikko_init.restype = c_char_p
		
		self.lib.voikko_terminate.argtypes = [c_int]
		self.lib.voikko_terminate.restype = c_int
		
		self.lib.voikko_spell_ucs4.argtypes = [c_int, c_wchar_p]
		self.lib.voikko_spell_ucs4.restype = c_int
		
		self.lib.voikko_suggest_ucs4.argtypes = [c_int, c_wchar_p]
		self.lib.voikko_suggest_ucs4.restype = POINTER(c_wchar_p)
		
		self.lib.voikko_free_suggest_ucs4.argtypes = [POINTER(c_wchar_p)]
		self.lib.voikko_free_suggest_ucs4.restype = None
		
		self.lib.voikko_free_suggest_cstr.argtypes = [POINTER(c_char_p)]
		self.lib.voikko_free_suggest_cstr.restype = None
		
		self.lib.voikko_analyze_word_ucs4.argtypes = [c_int, c_wchar_p]
		self.lib.voikko_analyze_word_ucs4.restype = POINTER(c_void_p)
		
		self.lib.voikko_free_mor_analysis.argtypes = [POINTER(c_void_p)]
		self.lib.voikko_free_mor_analysis.restype = None
		
		self.lib.voikko_mor_analysis_keys.argtypes = [c_void_p]
		self.lib.voikko_mor_analysis_keys.restype = POINTER(c_char_p)
		
		self.lib.voikko_mor_analysis_value_ucs4.argtypes = [c_void_p, c_char_p]
		self.lib.voikko_mor_analysis_value_ucs4.restype = c_wchar_p
		
		self.lib.voikko_next_token_ucs4.argtypes = [c_int, c_wchar_p, c_size_t,
		                                            POINTER(c_size_t)]
		self.lib.voikko_next_token_ucs4.restype = c_int
		
		self.lib.voikko_set_bool_option.argtypes = [c_int, c_int, c_int]
		self.lib.voikko_set_bool_option.restype = c_int
		
		self.lib.voikko_next_grammar_error_ucs4.argtypes = [c_int, c_wchar_p,
		                                   c_size_t, c_size_t, c_int]
		self.lib.voikko_next_grammar_error_ucs4.restype = CGrammarError
	
	def init(self):
		if self.handle.value < 0:
			error = self.lib.voikko_init(byref(self.handle), "fi_FI", 0)
			if error != None:
				raise Exception("Initialization of Voikko failed: " + error)
	
	def terminate(self):
		if self.handle.value >= 0:
			self.lib.voikko_terminate(self.handle)
			self.handle.value = -1
	
	def spell(self, word):
		_checkInited(self)
		result = self.lib.voikko_spell_ucs4(self.handle, word)
		if result == 0:
			return False
		elif result == 1:
			return True
		else:
			raise Exception("Internal error returned from libvoikko")
	
	def suggest(self, word):
		_checkInited(self)
		cSuggestions = self.lib.voikko_suggest_ucs4(self.handle, word)
		pSuggestions = []
		
		if not bool(cSuggestions):
			return pSuggestions
		
		i = 0
		while bool(cSuggestions[i]):
			pSuggestions.append(cSuggestions[i])
			i = i + 1
		
		self.lib.voikko_free_suggest_ucs4(cSuggestions)
		return pSuggestions
	
	def grammarErrors(self, paragraph):
		_checkInited(self)
		paragraphUnicode = unicode(paragraph)
		paragraphLen = len(paragraphUnicode)
		skipErrors = 0
		errorList = []
		while True:
			error = self.lib.voikko_next_grammar_error_ucs4(self.handle,
			        paragraphUnicode, paragraphLen, 0, skipErrors)
			if (error.errorCode == 0):
				return errorList
			errorList.append(GrammarError(error))
			self.lib.voikko_free_suggest_cstr(error.suggestions)
			skipErrors = skipErrors + 1
	
	def analyze(self, word):
		_checkInited(self)
		cAnalysisList = self.lib.voikko_analyze_word_ucs4(self.handle, word)
		pAnalysisList = []
		
		if not bool(cAnalysisList):
			return pAnalysisList
		
		i = 0
		while bool(cAnalysisList[i]):
			cAnalysis = cAnalysisList[i]
			cKeys = self.lib.voikko_mor_analysis_keys(cAnalysis)
			pAnalysis = {}
			j = 0
			while bool(cKeys[j]):
				key = cKeys[j]
				pAnalysis[key] = self.lib.voikko_mor_analysis_value_ucs4(
				                 cAnalysis, key)
				j = j + 1
			pAnalysisList.append(pAnalysis)
			i = i + 1
		
		self.lib.voikko_free_mor_analysis(cAnalysisList)
		return pAnalysisList
	
	def tokens(self, text):
		_checkInited(self)
		uniText = unicode(text)
		result = []
		textLen = len(uniText)
		tokenLen = c_size_t()
		position = 0
		while textLen > 0:
			tokenType = self.lib.voikko_next_token_ucs4(self.handle,
			            uniText[position:], textLen, byref(tokenLen))
			if tokenType == Token.NONE:
				break
			tokenText = uniText[position:position+tokenLen.value]
			result.append(Token(tokenText, tokenType))
			position = position + tokenLen.value
			textLen = textLen - tokenLen.value
		return result
	
	def setIgnoreDot(self, ignore):
		_checkInited(self)
		self.lib.voikko_set_bool_option(self.handle, 0, _boolToInt(ignore))
