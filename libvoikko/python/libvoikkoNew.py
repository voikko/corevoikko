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

class Voikko(object):
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
			self.__lib = CDLL("libvoikko-1.dll")
		else:
			self.__lib = CDLL("libvoikko.so.1")
		
		self.__lib.voikkoInit.argtypes = [POINTER(c_char_p), c_char_p, c_int, c_char_p]
		self.__lib.voikkoInit.restype = c_void_p
		
		self.__lib.voikkoTerminate.argtypes = [c_void_p]
		self.__lib.voikkoTerminate.restype = None
		
		self.__lib.voikkoSpellUcs4.argtypes = [c_void_p, c_wchar_p]
		self.__lib.voikkoSpellUcs4.restype = c_int
		
		error = c_char_p()
		self.__handle = self.__lib.voikkoInit(byref(error), variant, cacheSize, path)
		if error.value != None:
			self.__handle = 0
			raise VoikkoException(u"Initialization of Voikko failed: " + unicode(error.value, "UTF-8"))
	
	def __del__(self):
		# Ensure that resources are freed before this object is deleted.
		self.terminate()
	
	def __setBoolOption(self, option, value):
		result = self.__lib.voikkoSetBooleanOption(self.__handle, option, _boolToInt(value))
		if result == 0:
			raise VoikkoException(u"Could not set boolean option " + str(option) + u" to value " + str(value) + u".")
	
	def terminate(self):
		"""Releases the resources allocated by libvoikko for this instance. The instance cannot be used anymore
		after this method has been called. The resources are released automatically when the Python object is
		deleted. This method may be used to make sure that the resources are immediately released since they
		may take significant amount of memory and timely object deletion by Python runtime cannot always be
		relied upon.
		"""
		if (self.__handle != 0):
			self.__lib.voikkoTerminate(self.__handle)
			self.__handle = 0
			# Replace __lib with a dummy object that throws exception when any method is called. This ensures
			# that nothing bad happens if methods of a Voikko instance are called after termination.
			class DummyLib:
				def __getattr__(obj, name):
					raise VoikkoException("Attempt to use Voikko instance after terminate() was called")
			self.__lib = DummyLib()
	
	def spell(self, word):
		"""Check the spelling of given word. Return true if the word is correct,
		false if it is incorrect.
		"""
		result = self.__lib.voikkoSpellUcs4(self.__handle, word)
		if result == 0:
			return False
		elif result == 1:
			return True
		else:
			raise VoikkoException("Internal error returned from libvoikko")
	
	def setIgnoreDot(self, value):
		"""Ignore dot at the end of the word (needed for use in some word processors).
		If this option is set and input word ends with a dot, spell checking and
		hyphenation functions try to analyse the word without the dot if no results
		can be obtained for the original form. Also with this option, string tokenizer
		will consider trailing dot of a word to be a part of that word.
		Default: false
		"""
		self.__setBoolOption(0, value)
	
	def setIgnoreNumbers(self, value):
		"""Ignore words containing numbers.
		Default: false
		"""
		self.__setBoolOption(1, value)
	
	def setIgnoreUppercase(self, value):
		"""Accept words that are written completely in uppercase letters without checking
		them at all.
		Default: false
		"""
		self.__setBoolOption(3, value)
	
	def setAcceptFirstUppercase(self, value):
		"""Accept words even when the first letter is in uppercase (start of sentence etc.)
		Default: true
		"""
		self.__setBoolOption(6, value)
	
	def setAcceptAllUppercase(self, value):
		"""Accept words even when all of the letters are in uppercase. Note that this is
		not the same as setIgnoreUppercase: with this option the word is still
		checked, only case differences are ignored.
		Default: true
		"""
		self.__setBoolOption(7, value)
	
	def setIgnoreNonwords(self, value):
		"""(Spell checking only): Ignore non-words such as URLs and email addresses.
		Default: true
		"""
		self.__setBoolOption(10, value)
	
	def setAcceptExtraHyphens(self, value):
		"""(Spell checking only): Allow some extra hyphens in words. This option relaxes
		hyphen checking rules to work around some unresolved issues in the underlying
		morphology, but it may cause some incorrect words to be accepted. The exact
		behaviour (if any) of this option is not specified.
		Default: false
		"""
		self.__setBoolOption(11, value)
	
	def setAcceptMissingHyphens(self, value):
		"""(Spell checking only): Accept missing hyphens at the start and end of the word.
		Some application programs do not consider hyphens to be word characters. This
		is reasonable assumption for many languages but not for Finnish. If the
		application cannot be fixed to use proper tokenisation algorithm for Finnish,
		this option may be used to tell libvoikko to work around this defect.
		Default: false
		"""
		self.__setBoolOption(12, value)
	
	def setAcceptTitlesInGc(self, value):
		"""(Grammar checking only): Accept incomplete sentences that could occur in
		titles or headings. Set this option to true if your application is not able
		to differentiate titles from normal text paragraphs, or if you know that
		you are checking title text.
		Default: false
		"""
		self.__setBoolOption(13, value)
	
	def setAcceptUnfinishedParagraphsInGc(self, value):
		"""(Grammar checking only): Accept incomplete sentences at the end of the
		paragraph. These may exist when text is still being written.
		Default: false
		"""
		self.__setBoolOption(14, value)
	
	def setAcceptBulletedListsInGc(self, value):
		"""(Grammar checking only): Accept paragraphs if they would be valid within
		bulleted lists.
		Default: false
		"""
		self.__setBoolOption(16, value)
	
	def setNoUglyHyphenation(self, value):
		"""Do not insert hyphenation positions that are considered to be ugly but correct
		Default: false
		"""
		self.__setBoolOption(4, value)
	
	def setHyphenateUnknownWords(self, value):
		"""(Hyphenation only): Hyphenate unknown words.
		Default: true
		"""
		self.__setBoolOption(15, value)
	
	def setMinHyphenatedWordLength(self, value):
		"""The minumum length for words that may be hyphenated. This limit is also enforced on
		individual parts of compound words.
		Default: 2
		"""
		self.__lib.voikkoSetIntegerOption(self.__handle, 9, value)
	
	def setSuggestionStrategy(self, value):
		"""Set the suggestion strategy to be used when generating spelling suggestions.
		Default: SuggestionStrategy.TYPO
		"""
		if value == SuggestionStrategy.OCR:
			self.__setBoolOption(8, True)
		elif value == SuggestionStrategy.TYPO:
			self.__setBoolOption(8, False)
		else:
			raise VoikkoException("Invalid suggestion strategy")
