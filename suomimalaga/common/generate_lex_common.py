# -*- coding: utf-8 -*-

# Copyright 2007 Harri Pitkänen (hatapitk@iki.fi)
#                Hannu Väisänen (Etunimi.Sukunimi@joensuu.fi)
#
# Functions and variables that are common to Sukija and Voikko versions.
#
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

import hfconv
import voikkoutils
import codecs


# Path to source data directory
VOCABULARY_DATA = u"vocabulary"

# Vocabulary entries that should be saved to different files
# (group, name, file)
SPECIAL_VOCABULARY = [
	('usage', 'it', 'atk.lex'),
	('usage', 'medicine', 'laaketiede.lex'),
	('usage', 'science', 'matluonnontiede.lex'),
	('usage', 'education', 'kasvatustiede.lex'),
	('style', 'foreign', 'vieraskieliset.lex')]

def open_lex(path, filename):
	file = codecs.open(path + u"/" + filename, 'w', 'UTF-8')
	file.write(u"# This is automatically generated intermediate lexicon file for\n")
	file.write(u"# Suomi-malaga Voikko edition. The original source data is\n")
	file.write(u"# distributed under the GNU General Public License, version 2 or\n")
	file.write(u"# later, as published by the Free Software Foundation. You should\n")
	file.write(u"# have received the original data, tools and instructions to\n")
	file.write(u"# generate this file (or instructions to obtain them) wherever\n")
	file.write(u"# you got this file from.\n\n")
	return file

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
	if len(malaga_flags) == 0: return u""
	flag_string = u", tiedot: <"
	for flag in malaga_flags:
		flag_string = flag_string + flag + u","
	flag_string = flag_string[:-1] + u">"
	return flag_string

flag_attributes = voikkoutils.readFlagAttributes(VOCABULARY_DATA + u"/flags.txt")

def vowel_type(group):
	vtypes = group.getElementsByTagName("vtype")
	if len(vtypes) != 1: return voikkoutils.VOWEL_DEFAULT
	else:
		vtypes = tValue(vtypes[0])
		if vtypes == u'a': return voikkoutils.VOWEL_BACK
		elif vtypes == u'ä': return voikkoutils.VOWEL_FRONT
		else: return voikkoutils.VOWEL_BOTH

def has_flag(word, flag):
	if flag in tValues(word, "flag"): return True
	return False

# Returns tuple (alku, jatko) for given word in Joukahainen
def get_malaga_inflection_class(wordform, j_infclass, j_wordclasses, j_classmap):
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
	
	for infclass in j_classmap:
		if infclass[0] != classparts[0]: continue
		for subclass in infclass[2]:
			if len(subclass) > 3 and not wclass in subclass[3]: continue
			if not subclass[0] in gradtypes: continue
			alku = hfconv.match_re(wordform, subclass[1])
			if alku != None: return (alku, subclass[2])
	
	return (None, None)


# Returns a string describing the structure of a word, if necessary for the spellchecker
# or hyphenator
def get_structure(wordform, malaga_word_class):
	needstructure = False
	if malaga_word_class in [u'nimi', u'etunimi', u'sukunimi', 'paikannimi']: ispropernoun = True
	else: ispropernoun = False
	structstr = u', rakenne: "='
	for i in range(len(wordform)):
		c = wordform[i]
		if c == u'-':
			structstr = structstr + u"-="
			needstructure = True
		elif c == u'|': structstr = structstr
		elif c == u'=':
			structstr = structstr + u"="
			needstructure = True
		elif c == u':':
			structstr = structstr + u":"
			needstructure = True
		elif c.isupper():
			structstr = structstr + u"i"
			if not (ispropernoun and i == 0): needstructure = True
		else: structstr = structstr + u"p"
	if needstructure: return structstr + u'"'
	else: return u""

# Writes the vocabulary entry to a suitable file
def write_entry(main_vocabulary,vocabulary_files,word, entry):
	special = False
	for voc in SPECIAL_VOCABULARY:
		group = word.getElementsByTagName(voc[0])
		if len(group) == 0: continue
		if has_flag(group[0], voc[1]):
			vocabulary_files[voc[2]].write(entry + u"\n")
			special = True
	if not special:
		main_vocabulary.write(entry + u"\n")
