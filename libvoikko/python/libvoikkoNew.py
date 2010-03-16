# -*- coding: utf-8 -*-
"""
Python interface to libvoikko, library of Finnish language tools.
This module can be used to perform various natural language analysis
tasks on Finnish text.

An example session demonstrating the use of this module:

 >>> import libvoikko
 >>> v = libvoikko.Voikko()
 >>> v.init()
 >>> v.analyze(u"kissa")
 [{'SIJAMUOTO': u'nimento', 'CLASS': u'nimisana', 'STRUCTURE': u'=ppppp'}]
 >>> v.spell(u"kissa")
 True
 >>> v.suggest(u"kisssa")
 [u'kissa', u'kissaa', u'kisassa', u'kisussa']
 >>> v.hyphenate(u"kissa")
 u'kis-sa'
 >>> v.terminate()

"""

# Copyright 2009 - 2010 Harri Pitkänen (hatapitk@iki.fi)
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
from ctypes import c_char
from ctypes import c_char_p
from ctypes import c_wchar_p
from ctypes import c_size_t
from ctypes import c_void_p
from ctypes import pointer
from ctypes import POINTER
from ctypes import string_at
from ctypes import Structure
import os

"""Maximum number of characters in a valid word"""
MAX_WORD_CHARS = 255

"""Maximum number of analyses that can be produced for a word"""
MAX_ANALYSIS_COUNT = 31

class Dictionary:
	"""Represents a morphological dictionary."""
	def __init__(self, variant, description):
		self.variant = variant
		self.description = description
	
	def __repr__(self):
		return u"<" + self.variant + u"," + self.description + u">"
	
	def __lt__(self, other):
		if not isinstance(other, Dictionary):
			return False
		if self.variant < other.variant:
			return True
		return self.description < other.description
	
	def __eq__(self, other):
		return isinstance(other, Dictionary) and \
		       self.variant == other.variant and \
		       self.description == other.description
	
	def __hash__(self):
		return hash(self.variant) ^ hash(self.description)

class Token:
	"""Represents a token in tokenized natural language text."""
	NONE = 0
	WORD = 1
	PUNCTUATION = 2
	WHITESPACE = 3
	UNKNOWN = 4
	
	_TYPE_NAMES = ["NONE", "WORD", "PUNCTUATION", "WHITESPACE", "UNKNOWN"]
	
	def __init__(self, tokenText, tokenType):
		self.tokenText = tokenText
		self.tokenType = tokenType
	
	def __repr__(self):
		return (u"<" + self.tokenText + u"," + \
		       Token._TYPE_NAMES[self.tokenType] + u">").encode("UTF-8")

class SuggestionStrategy:
	"""Strategies for generating suggestions for incorrectly spelled words."""
	TYPO = 0
	"""Suggestion strategy for correcting human typing errors."""
	OCR = 1
	"""Suggestion strategy for correcting errors in text produced by
	optical character recognition software."""

class _CGrammarError(Structure):
	_fields_ = [("errorCode", c_int),
	            ("errorLevel", c_int),
	            ("errorDescription", c_char_p),
	            ("startPos", c_size_t),
	            ("errorLen", c_size_t),
	            ("suggestions", POINTER(c_char_p))]

class GrammarError:
	"""Grammar error from grammar checker."""
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

class VoikkoException(Exception):
	"""Thrown when someting exceptional happens within libvoikko."""
	pass

def _boolToInt(bool):
	if bool:
		return 1
	else:
		return 0

def _setBoolOption(voikko, option, value):
	_checkInited(voikko)
	voikko.lib.voikko_set_bool_option(voikko.handle, option, _boolToInt(value))

class Voikko:
	"""Represents an instance of Voikko. The instance has state, such as
	settings related to spell checking and hyphenation, and methods for performing
	various natural language analysis operations. One instance should not be
	used simultaneously from multiple threads.
	
	Currently no more than one instance can be in initialized state. This is because
	libvoikko is not yet thread safe. This restriction should go away in future
	releases.
	"""
	def __init__(self, path = None, variant = "fi_FI", cacheSize = 0):
		"""Creates a new Voikko instance with the following optional parameters:
		   path      Extra path that will be checked first when looking for linguistic
		             resources.
		   variant   Variant of morphological dictionary to use.
		   cacheSize Parameter that controls the size of in memory cache for
		             spell checking results. 0 is the default size, 1 is twice as large
		             as 0 etc. -1 disables the spell checking cache entirely."""
		if os.name == 'nt':
			self.lib = CDLL("libvoikko-1.dll")
		else:
			self.lib = CDLL("libvoikko.so.1")
		
		self.lib.voikkoInit.argtypes = [POINTER(c_char_p), c_char_p, c_int, c_char_p]
		self.lib.voikkoInit.restype = c_void_p
		
		self.lib.voikkoTerminate.argtypes = [c_void_p]
		self.lib.voikkoTerminate.restype = None
		
		self.lib.voikkoSpellUcs4.argtypes = [c_void_p, c_wchar_p]
		self.lib.voikkoSpellUcs4.restype = c_int
		
		error = c_char_p()
		self.handle = self.lib.voikkoInit(byref(error), variant, cacheSize, path)
		if self.handle == 0:
			raise VoikkoException(u"Initialization of Voikko failed: " + unicode(error, "UTF-8"))
	
	
	def terminate(self):
		"""Uninitialize this Voikko instance. The instance cannot be used anymore
		after this method has been called.
		"""
		if (self.handle != 0):
			self.lib.voikkoTerminate(self.handle)
			self.handle = 0
	
	def spell(self, word):
		"""Check the spelling of given word. Return true if the word is correct,
		false if it is incorrect.
		"""
		result = self.lib.voikkoSpellUcs4(self.handle, word)
		if result == 0:
			return False
		elif result == 1:
			return True
		else:
			raise VoikkoException("Internal error returned from libvoikko")
