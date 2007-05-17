# -*- coding: utf-8 -*-

# Copyright 2007 Harri Pitkänen (hatapitk@iki.fi)

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

# This module contains general helper functions and classes for use
# with Python and Voikko.

import codecs
import os
import locale

# Vowel types
VOWEL_DEFAULT=0
VOWEL_FRONT=1
VOWEL_BACK=2
VOWEL_BOTH=3

class FlagAttribute:
	"Vocabulary flag attribute"
	joukahainen = 0
	xmlGroup = None
	xmlFlag = None
	malagaFlag = None
	description = None

# Remove comments from a given line of text.
def removeComments(line):
	comment_start = line.find(u'#')
	if comment_start == -1:
		return line
	if comment_start == 0:
		return u''
	return line[:comment_start]

# Returns a list of flag attributes from given file
def readFlagAttributes(filename):
	inputfile = codecs.open(filename, 'r', 'UTF-8')
	flags = []
	fileCont = True
	while fileCont:
		line = inputfile.readline()
		fileCont = line.endswith('\n')
		line = removeComments(line).strip()
		if len(line) > 0:
			f = FlagAttribute()
			endind = line.find(u' ')
			f.joukahainen = int(line[:endind])
			line = line[endind:].strip()
			endind = line.find(u'/')
			f.xmlGroup = line[:endind]
			line = line[endind + 1:]
			endind = line.find(u' ')
			f.xmlFlag = line[:endind]
			line = line[endind:].strip()
			endind = line.find(u' ')
			if line[:endind] != u'-': f.malagaFlag = line[:endind]
			line = line[endind:].strip()
			if len(line) > 0: f.description = line
			flags.append(f)
	inputfile.close()
	return flags

# Function that returns the type of vowels that are allowed in the suffixes for
# given simple word.
# The possible values are VOWEL_FRONT, VOWEL_BACK and VOWEL_BOTH.
def _simple_vowel_type(word):
	word = word.lower()
	last_back = max(word.rfind(u'a'), word.rfind(u'o'), word.rfind(u'å'), word.rfind(u'u'))
	last_ord_front = max(word.rfind(u'ä'), word.rfind(u'ö'))
	last_y = word.rfind(u'y')
	if last_back > -1 and max(last_ord_front, last_y) == -1:
		return VOWEL_BACK
	if last_back == -1 and max(last_ord_front, last_y) > -1:
		return VOWEL_FRONT
	if max(last_back, last_ord_front, last_y) == -1:
		return VOWEL_FRONT
	if last_y < max(last_back, last_ord_front):
		if last_back > last_ord_front: return VOWEL_BACK
		else: return VOWEL_FRONT
	else:
		return VOWEL_BOTH

# Returns autodetected vowel type of infection suffixes for a word.
# If word contains character '=', automatic detection is only performed on the
# trailing part. If word contains character '|', automatic detection is performed
# on the trailing part and the whole word, and the union of accepted vowel types is returned.
def get_wordform_infl_vowel_type(wordform):
	# Search for last '=' or '-', check the trailing part using recursion
	startind = max(wordform.rfind(u'='), wordform.rfind(u'-'))
	if startind == len(wordform) - 1: return VOWEL_BOTH # Not allowed
	if startind != -1: return get_wordform_infl_vowel_type(wordform[startind+1:])
	
	# Search for first '|', check the trailing part using recursion
	startind = wordform.find(u'|')
	if startind == len(wordform) - 1: return VOWEL_BOTH # Not allowed
	vtype_whole = _simple_vowel_type(wordform)
	if startind == -1: return vtype_whole
	vtype_part = get_wordform_infl_vowel_type(wordform[startind+1:])
	if vtype_whole == vtype_part: return vtype_whole
	else: return VOWEL_BOTH

def get_preference(prefname):
	u'Returns the value of given preference'
	try:
		import voikko_dev_prefs
		if prefname == 'svnroot' and hasattr(voikko_dev_prefs, 'svnroot'):
			return voikko_dev_prefs.svnroot
		if prefname == 'voikkotest_dir' and hasattr(voikko_dev_prefs, 'prefname'):
			return voikko_dev_prefs.voikkotest_dir
		if prefname == 'encoding' and hasattr(voikko_dev_prefs, 'encoding'):
			return voikko_dev_prefs.encoding
		if prefname == 'libvoikko_bin' and hasattr(voikko_dev_prefs, 'libvoikko_bin'):
			return voikko_dev_prefs.libvoikko_bin
	except ImportError:
		pass
	if prefname == 'svnroot': return os.environ['HOME'] + '/svn/voikko'
	if prefname == 'voikkotest_dir': return os.environ['HOME'] + '/tmp/voikkotest'
	if prefname == 'encoding': return locale.getpreferredencoding()
	if prefname == 'libvoikko_bin': return '/usr/bin'
	return None

