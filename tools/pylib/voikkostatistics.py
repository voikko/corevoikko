# -*- coding: utf-8 -*-

# Copyright 2011 Harri Pitkänen (hatapitk@iki.fi)
# Module for analyzing readability of text.
# This program requires Python and Python module of libvoikko from
# libvoikko 3.0 or later.

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

# References:
# http://en.wikipedia.org/wiki/Flesch%E2%80%93Kincaid_readability_test
# http://media.tkk.fi/GTTS/Suomi/dt&raportit/DI_J_Haataja.pdf

from libvoikko import Sentence, Token

def getSyllablesInWord(word, voikko):
	hyphens = len(voikko.getHyphenationPattern(word).replace(u" ", u""))
	return hyphens + 1

def getSyllablesInBaseform(word, voikko):
	# TODO: this will use only the first analysis
	for analysis in voikko.analyze(word):
		if "BASEFORM" in analysis:
			return getSyllablesInWord(analysis["BASEFORM"], voikko)
	return 0

def __insertToHistogram(histogram, bin):
	if bin in histogram:
		histogram[bin] = histogram[bin] + 1
	else:
		histogram[bin] = 1

def __getBin(histogram, bin):
	if bin in histogram:
		return histogram[bin]
	else:
		return 0

def __getUpper(histogram, lowerBound):
	total = 0
	for (bin, value) in histogram.items():
		if bin >= lowerBound:
			total = total + value
	return total

def getStatistics(text, voikko):
	sentenceCount = 0
	for sentence in voikko.sentences(text):
		if sentence.nextStartType in [Sentence.NONE, Sentence.PROBABLE]:
			sentenceCount = sentenceCount + 1
	
	wordCount = 0
	knownWords = 0
	syllableCount = 0
	characterCount = 0
	punctuationCount = 0
	baseformLengthHistogram = {}
	for token in voikko.tokens(text):
		if token.tokenType == Token.WORD:
			wordCount = wordCount + 1
			syllableCount = syllableCount + getSyllablesInWord(token.tokenText, voikko)
			characterCount = characterCount + len(token.tokenText)
			sylsInBaseform = getSyllablesInBaseform(token.tokenText, voikko)
			__insertToHistogram(baseformLengthHistogram, sylsInBaseform)
			if sylsInBaseform > 0:
				knownWords = knownWords + 1
		elif token.tokenType == Token.PUNCTUATION:
			punctuationCount = punctuationCount + 1
	
	result = {}
	
	# measured statistics
	result[u"SENTENCES"] = sentenceCount
	result[u"WORDS"] = wordCount
	result[u"KNOWN_WORDS"] = knownWords
	result[u"BASEFORM_SYLLABLES_HISTOGRAM"] = baseformLengthHistogram
	result[u"SYLLABLES"] = syllableCount
	result[u"LETTERS"] = characterCount
	result[u"PUNCTUATION"] = punctuationCount
	
	# derived statistics
	if knownWords == 0 or sentenceCount == 0:
		result[u"FLESCH_READING_EASE"] = 0
		result[u"FLESCH_KINCAID_GRADE_LEVEL"] = 0
		result[u"WIIO_SIMPLE"] = 0
	else:
		result[u"FLESCH_READING_EASE"] = 206.823 - 1.015 * wordCount / sentenceCount - 84.6 * syllableCount / wordCount
		result[u"FLESCH_KINCAID_GRADE_LEVEL"] = 0.39 * wordCount / sentenceCount + 11.8 * syllableCount / wordCount - 15.59
		result[u"WIIO_SIMPLE"] = 2.7 + 30.0 * __getUpper(baseformLengthHistogram, 4) / knownWords
	
	return result
