# -*- coding: utf-8 -*-
"""Python interface to libvoikko, library of Finnish language tools.
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

# Copyright 2009 Harri Pitkänen (hatapitk@iki.fi)
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
	
	def __cmp__(self, other):
		if not isinstance(other, Dictionary):
			return -1
		variantOrder = cmp(self.variant, other.variant)
		if variantOrder != 0:
			return variantOrder
		return cmp(self.description, other.description)

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

def _checkInited(voikko):
	if voikko.handle.value < 0:
		raise VoikkoException("Voikko not initialized")


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
	def __init__(self):
		"""Creates a new Voikko instance."""
		self.handle = c_int(-1)
		if os.name == 'nt':
			self.lib = CDLL("libvoikko-1.dll")
		else:
			self.lib = CDLL("libvoikko.so.1")
		
		self.lib.voikko_init_with_path.argtypes = [POINTER(c_int), c_char_p, c_int, c_char_p]
		self.lib.voikko_init_with_path.restype = c_char_p
		
		self.lib.voikko_terminate.argtypes = [c_int]
		self.lib.voikko_terminate.restype = c_int
		
		self.lib.voikko_list_dicts.argtypes = [c_char_p]
		self.lib.voikko_list_dicts.restype = POINTER(c_void_p)
		
		self.lib.voikko_free_dicts.argtypes = [POINTER(c_void_p)]
		self.lib.voikko_free_dicts.restype = None
		
		self.lib.voikko_dict_variant.argtypes = [c_void_p]
		self.lib.voikko_dict_variant.restype = c_char_p
		
		self.lib.voikko_dict_description.argtypes = [c_void_p]
		self.lib.voikko_dict_description.restype = c_char_p
		
		self.lib.voikko_spell_ucs4.argtypes = [c_int, c_wchar_p]
		self.lib.voikko_spell_ucs4.restype = c_int
		
		self.lib.voikko_suggest_ucs4.argtypes = [c_int, c_wchar_p]
		self.lib.voikko_suggest_ucs4.restype = POINTER(c_wchar_p)
		
		self.lib.voikko_free_suggest_ucs4.argtypes = [POINTER(c_wchar_p)]
		self.lib.voikko_free_suggest_ucs4.restype = None
		
		self.lib.voikko_free_suggest_cstr.argtypes = [POINTER(c_char_p)]
		self.lib.voikko_free_suggest_cstr.restype = None
		
		self.lib.voikko_hyphenate_ucs4.argtypes = [c_int, c_wchar_p]
		self.lib.voikko_hyphenate_ucs4.restype = POINTER(c_char)
		
		self.lib.voikko_free_hyphenate.argtypes = [POINTER(c_char)]
		self.lib.voikko_free_hyphenate.restype = None
		
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
		
		self.lib.voikko_set_int_option.argtypes = [c_int, c_int, c_int]
		self.lib.voikko_set_int_option.restype = c_int
		
		self.lib.voikko_next_grammar_error_ucs4.argtypes = [c_int, c_wchar_p,
		                                   c_size_t, c_size_t, c_int]
		self.lib.voikko_next_grammar_error_ucs4.restype = _CGrammarError
		
		self.lib.voikko_error_message_cstr.argtypes = [c_int, c_char_p]
		self.lib.voikko_error_message_cstr.restype = c_char_p
	
	def init(self, path = None, variant = "fi_FI", cacheSize = 0):
		"""Initialize the Voikko instance with the following optional parameters:
		   path      Extra path that will be checked first when looking for linguistic
		             resources.
		   variant   Variant of morphological dictionary to use.
		   cacheSize Parameter that controls the size of in memory cache for
		             spell checking results. 0 is the default size, 1 is twice as large
		             as 0 etc. -1 disables the spell checking cache entirely.
		"""
		if self.handle.value < 0:
			error = self.lib.voikko_init_with_path(byref(self.handle), variant,
			                                       cacheSize, path)
			if error != None:
				raise VoikkoException("Initialization of Voikko failed: " + error)
	
	def terminate(self):
		"""Uninitialize this Voikko instance. The instance must be initialized again
		before it can be used.
		"""
		if self.handle.value >= 0:
			self.lib.voikko_terminate(self.handle)
			self.handle.value = -1
	
	def listDicts(self, path = None):
		"""Return a list of Dictionary objects representing the available
		dictionary variants. If path is specified, it will be searched first
		before looking from the standard locations. This method can be called
		even if the Voikko instance has not yet been initialized.
		"""
		cDicts = self.lib.voikko_list_dicts(path)
		dicts = []
		i = 0
		while bool(cDicts[i]):
			cDict = cDicts[i]
			variant = self.lib.voikko_dict_variant(cDict)
			description = unicode(self.lib.voikko_dict_description(cDict), "UTF-8")
			dicts.append(Dictionary(variant, description))
			i = i + 1
		self.lib.voikko_free_dicts(cDicts)
		return dicts
	
	def spell(self, word):
		"""Check the spelling of given word. Return true if the word is correct,
		false if it is incorrect.
		"""
		_checkInited(self)
		result = self.lib.voikko_spell_ucs4(self.handle, word)
		if result == 0:
			return False
		elif result == 1:
			return True
		else:
			raise VoikkoException("Internal error returned from libvoikko")
	
	def suggest(self, word):
		"""Generate a list of suggested spellings for given (misspelled) word.
		If the given word is correct, the list contains only the word itself.
		"""
		_checkInited(self)
		# FIXME: This should be done directly within libvoikko
		if self.spell(word):
			return [unicode(word)]
		
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
	
	def _grammarParagraph(self, paragraph, offset):
		paragraphLen = len(paragraph)
		skipErrors = 0
		errorList = []
		while True:
			error = self.lib.voikko_next_grammar_error_ucs4(self.handle,
			        paragraph, paragraphLen, 0, skipErrors)
			if (error.errorCode == 0):
				return errorList
			gError = GrammarError(error)
			gError.startPos = offset + gError.startPos
			errorList.append(gError)
			self.lib.voikko_free_suggest_cstr(error.suggestions)
			skipErrors = skipErrors + 1
	
	def grammarErrors(self, text):
		"""Check the given text for grammar errors and return a
		list of GrammarError objects representing the errors that were found.
		Unlike the C based API this method accepts multiple paragraps
		separated by newline characters.
		"""
		_checkInited(self)
		textUnicode = unicode(text)
		errorList = []
		offset = 0
		for paragraph in textUnicode.split(u"\n"):
			errorList = errorList + self._grammarParagraph(paragraph, offset)
			offset = offset + len(paragraph) + 1
		return errorList
	
	def grammarErrorExplanation(self, errorCode, language):
		"""Return a human readable explanation for grammar error code in
		given language.
		"""
		explanation = self.lib.voikko_error_message_cstr(errorCode, language)
		return unicode(explanation, "UTF-8")
	
	def analyze(self, word):
		"""Analyze the morphology of given word and return the list of
		analysis results. The results are represented as maps having property
		names as keys and property values as values.
		"""
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
		"""Split the given natural language text into a list of Token objects."""
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
	
	def getHyphenationPattern(self, word):
		"""Return a character pattern that describes the hyphenation of given word.
		  ' ' = no hyphenation at this character,
		  '-' = hyphenation point (character at this position
		        is preserved in the hyphenated form),
		  '=' = hyphentation point (character at this position
		        is replaced by the hyphen.)
		"""
		_checkInited(self)
		cHyphenationPattern = self.lib.voikko_hyphenate_ucs4(self.handle, word)
		hyphenationPattern = string_at(cHyphenationPattern)
		self.lib.voikko_free_hyphenate(cHyphenationPattern)
		return hyphenationPattern
	
	def hyphenate(self, word):
		"""Return the given word in fully hyphenated form."""
		pattern = self.getHyphenationPattern(word)
		hyphenated = u""
		for i in range(len(pattern)):
			patternC = pattern[i]
			if patternC == ' ':
				hyphenated = hyphenated + word[i]
			elif patternC == '-':
				hyphenated = hyphenated + u"-" + word[i]
			elif patternC == '=':
				hyphenated = hyphenated + u"-"
		return hyphenated
	
	def setIgnoreDot(self, value):
		"""Ignore dot at the end of the word (needed for use in some word processors).
		If this option is set and input word ends with a dot, spell checking and
		hyphenation functions try to analyse the word without the dot if no results
		can be obtained for the original form. Also with this option, string tokenizer
		will consider trailing dot of a word to be a part of that word.
		Default: false
		"""
		_setBoolOption(self, 0, value)
	
	def setIgnoreNumbers(self, value):
		"""Ignore words containing numbers.
		Default: false
		"""
		_setBoolOption(self, 1, value)
	
	def setIgnoreUppercase(self, value):
		"""Accept words that are written completely in uppercase letters without checking
		them at all.
		Default: false
		"""
		_setBoolOption(self, 3, value)
	
	def setAcceptFirstUppercase(self, value):
		"""Accept words even when the first letter is in uppercase (start of sentence etc.)
		Default: true
		"""
		_setBoolOption(self, 6, value)
	
	def setAcceptAllUppercase(self, value):
		"""Accept words even when all of the letters are in uppercase. Note that this is
		not the same as setIgnoreUppercase: with this option the word is still
		checked, only case differences are ignored.
		Default: true
		"""
		_setBoolOption(self, 7, value)
	
	def setIgnoreNonwords(self, value):
		"""(Spell checking only): Ignore non-words such as URLs and email addresses.
		Default: true
		"""
		_setBoolOption(self, 10, value)
	
	def setAcceptExtraHyphens(self, value):
		"""(Spell checking only): Allow some extra hyphens in words. This option relaxes
		hyphen checking rules to work around some unresolved issues in the underlying
		morphology, but it may cause some incorrect words to be accepted. The exact
		behaviour (if any) of this option is not specified.
		Default: false
		"""
		_setBoolOption(self, 11, value)
	
	def setAcceptMissingHyphens(self, value):
		"""(Spell checking only): Accept missing hyphens at the start and end of the word.
		Some application programs do not consider hyphens to be word characters. This
		is reasonable assumption for many languages but not for Finnish. If the
		application cannot be fixed to use proper tokenisation algorithm for Finnish,
		this option may be used to tell libvoikko to work around this defect.
		Default: false
		"""
		_setBoolOption(self, 12, value)
	
	def setAcceptTitlesInGc(self, value):
		"""(Grammar checking only): Accept incomplete sentences that could occur in
		titles or headings. Set this option to true if your application is not able
		to differentiate titles from normal text paragraphs, or if you know that
		you are checking title text.
		Default: false
		"""
		_setBoolOption(self, 13, value)
	
	def setAcceptUnfinishedParagraphsInGc(self, value):
		"""(Grammar checking only): Accept incomplete sentences at the end of the
		paragraph. These may exist when text is still being written.
		Default: false
		"""
		_setBoolOption(self, 14, value)
	
	def setAcceptBulletedListsInGc(self, value):
		"""(Grammar checking only): Accept paragraphs if they would be valid within
		bulleted lists.
		Default: false
		"""
		_setBoolOption(self, 16, value)
	
	def setNoUglyHyphenation(self, value):
		"""Do not insert hyphenation positions that are considered to be ugly but correct
		Default: false
		"""
		_setBoolOption(self, 4, value)
	
	def setHyphenateUnknownWords(self, value):
		"""(Hyphenation only): Hyphenate unknown words.
		Default: true
		"""
		_setBoolOption(self, 15, value)
	
	def setMinHyphenatedWordLength(self, value):
		"""The minumum length for words that may be hyphenated. This limit is also enforced on
		individual parts of compound words.
		Default: 2
		"""
		_checkInited(self)
		self.lib.voikko_set_int_option(self.handle, 9, value)
	
	def setSuggestionStrategy(self, value):
		"""Set the suggestion strategy to be used when generating spelling suggestions.
		Default: SuggestionStrategy.TYPO
		"""
		if value == SuggestionStrategy.OCR:
			_setBoolOption(self, 8, True)
		elif value == SuggestionStrategy.TYPO:
			_setBoolOption(self, 8, False)
		else:
			raise VoikkoException("Invalid suggestion strategy")
