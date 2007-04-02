#!/usr/bin/python
# -*- coding: utf-8 -*-

# Copyright 2007 Harri Pitkänen (hatapitk@iki.fi)
# Program to generate lexicon files for Suomi-malaga Voikko edition

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

import os

# Path to module directory
MODULE_PATH_HFTOOLS = os.environ["HOME"] + u"/svn/voikko/trunk/tools/pylib"

# Path to data directory
VOIKKO_DATA = os.environ["HOME"] + u"/svn/voikko/trunk/data"

import sys
sys.path.append(MODULE_PATH_HFTOOLS)
import hfconv
import voikkoutils
import xml.dom.minidom
import codecs

flag_attributes = voikkoutils.readFlagAttributes(VOIKKO_DATA + u"/words/flags.txt")

def print_header():
	print "This is automatically generated intermediate lexicon file for"
	print "Suomi-malaga Voikko edition. The original source data is"
	print "distributed under the GNU General Public License, version 2 or"
	print "later, as published by the Free Software Foundation. You should"
	print "have received the original data, tools and instructions to"
	print "generate this file (or instructions to obtain them) wherever"
	print "you got this file from."

def tValue(element):
	rc = ""
	for node in element.childNodes:
		if node.nodeType == node.TEXT_NODE:
			rc = rc + node.data
	return rc

def tValues(group, element_name):
	values = []
	for element in group.getElementsByTagName(element_name):
		values.append(tValue(element))
	return values

def frequency(word):
	fclass = word.getElementsByTagName("fclass")
	if len(fclass) == 0: return 7
	return int(tValue(fclass[0]))

def vowel_type(group):
	vtypes = group.getElementsByTagName("vtype")
	if len(vtypes) != 1: return voikkoutils.VOWEL_DEFAULT
	else:
		vtypes = tValue(vtypes[0])
		if vtypes == u'a': return voikkoutils.VOWEL_BACK
		elif vtypes == u'ä': return voikkoutils.VOWEL_FRONT
		else: return voikkoutils.VOWEL_BOTH

def has_flag(word, flag):
	if word in tValues(word, "flag"): return True
	return False

# Returns tuple (alku, jatko) for given word in Joukahainen
def get_malaga_inflection_class(wordform, j_infclass, j_wordclasses):
	classparts = j_infclass.split(u'-')
	if len(classparts) == 1:
		classparts.append(None)
		gradtypes = [None]
	else:
		gradtypes = []
		for grad in hfconv.grads:
			if grad[2] == classparts[1]: gradtypes.append(grad[1])
	
	# Determine the word class for the given word
	if "adjective" in j_wordclasses: wclass = hfconv.ADJ
	elif "noun" in j_wordclasses or "pnoun_firstname" in j_wordclasses or \
	     "pnoun_lastname" in j_wordclasses or "pnoun_place" in j_wordclasses or \
	     "pnoun_misc" in j_wordclasses: wclass = hfconv.SUBST
	elif "verb" in j_wordclasses: wclass = hfconv.VERB 
	else: return (None, None)
	
	for infclass in hfconv.classmap:
		if infclass[0] != classparts[0]: continue
		for subclass in infclass[2]:
			if len(subclass) > 3 and not wclass in subclass[3]: continue
			if not subclass[0] in gradtypes: continue
			alku = hfconv.match_re(wordform, subclass[1])
			if alku != None: return (alku, subclass[2])
	
	return (None, None)

# Returns malaga word class for given word in Joukahainen
def get_malaga_word_class(j_wordclasses):
	if "pnoun_place" in j_wordclasses: return u"paikannimi"
	if "pnoun_firstname" in j_wordclasses: return u"etunimi"
	if "pnoun_lastname" in j_wordclasses: return u"sukunimi"
	if "pnoun_misc" in j_wordclasses: return u"nimi"
	if "verb" in j_wordclasses: return u"teonsana"
	if "adjective" in j_wordclasses and "noun" in j_wordclasses: return u"nimi_laatusana"
	if "adjective" in j_wordclasses: return u"laatusana"
	if "noun" in j_wordclasses: return u"nimisana"
	return None

# Returns malaga flags for given word in Joukahainen
def get_malaga_flags(word):
	global flag_attributes
	malaga_flags = []
	for flag_attribute in flag_attributes:
		group = word.getElementsByTagName(flag_attribute.xmlGroup)
		if len(group) == 0: continue
		if flag_attribute.xmlFlag in tValues(group[0], "flag") and \
		   flag_attribute.malagaFlag != None:
			malaga_flags.append(flag_attribute.malagaFlag)
	return malaga_flags

def handle_word(word):
	# Drop words that are not needed in the Voikko lexicon
	if has_flag(word, "not_voikko"): return
	if has_flag(word, "incorrect"): return
	if has_flag(word, "foreign"): return
	if has_flag(word, "dialect"): return
	if frequency(word) >= 10: return
	if frequency(word) == 9 and has_flag(word, "confusing"): return
	
	# Get the inflection class. Exactly one inflection class is needed
	infclasses = word.getElementsByTagName("infclass")
	voikko_infclass = None
	for infclass in word.getElementsByTagName("infclass"):
		if infclass.getAttribute("type") != "historical":
			voikko_infclass = tValue(infclass)
			break
	if voikko_infclass == None: return
	if voikko_infclass == u"poikkeava": return
	
	# Get the word classes
	wordclasses = tValues(word.getElementsByTagName("classes")[0], "wclass")
	malaga_word_class = get_malaga_word_class(wordclasses)
	if malaga_word_class == None: return
	
	# Get malaga flags
	malaga_flags = get_malaga_flags(word)
	
	# Get forced vowel type
	forced_inflection_vtype = vowel_type(word.getElementsByTagName("inflection")[0])
	
	# Process all alternative forms
	for altform in tValues(word.getElementsByTagName("forms")[0], "form"):
		wordform = altform.replace(u'|', u'').replace(u'=', u'')
		(alku, jatko) = get_malaga_inflection_class(wordform, voikko_infclass, wordclasses)
		if alku == None:
			print (u"#Malaga class not found for (%s, %s)\n" \
			       % (wordform, voikko_infclass)).encode('UTF-8')
			continue
		print altform.encode("UTF-8"), alku.encode("UTF-8"), jatko.encode("UTF-8")


listfile = open(VOIKKO_DATA + u'/words/fi_FI.xml', 'r')

line = ""
while line != '<wordlist xml:lang="fi">\n': line = listfile.readline()

while True:
	wordstr = ""
	line = listfile.readline()
	if line == "</wordlist>\n": break
	while line != '</word>\n':
		wordstr = wordstr + line
		line = listfile.readline()
	word = xml.dom.minidom.parseString(wordstr + line)
	handle_word(word)

listfile.close()
