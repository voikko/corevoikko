# -*- coding: utf-8 -*-

# Copyright 2007 - 2011 Harri Pitkänen (hatapitk@iki.fi)
#           2007        Hannu Väisänen (Etunimi.Sukunimi@joensuu.fi)
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
import getopt
import sys
from xml.dom import Node

# Path to source data directory
VOCABULARY_DATA = "vocabulary"

def tValue(element):
	return element.firstChild.wholeText

# Returns a list of text values with given element name under DOM element "group"
def tValues(group, element_name):
	values = []
	for element in group.getElementsByTagName(element_name):
		values.append(tValue(element))
	return values

# Returns flag names from given group for word in Joukahainen
def get_flags_from_group(word, groupName):
	flags = []
	for group in word.childNodes:
		if group.nodeType != Node.ELEMENT_NODE or group.tagName != groupName:
			continue
		for flag in group.childNodes:
			if flag.nodeType != Node.ELEMENT_NODE:
				continue
			if flag.tagName != "flag":
				continue
			flags.append(flag.firstChild.wholeText)
	return flags

flag_attributes = voikkoutils.readFlagAttributes(VOCABULARY_DATA + "/flags.txt")

def vowel_type(group):
	vtypes = group.getElementsByTagName("vtype")
	if len(vtypes) != 1: return voikkoutils.VOWEL_DEFAULT
	else:
		vtypes = tValue(vtypes[0])
		if vtypes == 'a': return voikkoutils.VOWEL_BACK
		elif vtypes == 'ä': return voikkoutils.VOWEL_FRONT
		else: return voikkoutils.VOWEL_BOTH

def has_flag(word, flag):
	if flag in tValues(word, "flag"): return True
	return False

# Returns tuple (alku, jatko) for given word in Joukahainen
def get_malaga_inflection_class(wordform, j_infclass, j_wordclasses, j_classmap):
	if j_infclass is None:
		return (wordform, "loppu")
	(infclass, gradclass) = (list(j_infclass.split('-')) + [None])[:2]
	
	if gradclass == None: gradtypes = [None]
	else: gradtypes = [grad[1] for grad in hfconv.grads if grad[2] == gradclass]
	
	# Determine the word class for the given word
	if "adjective" in j_wordclasses: wclass = hfconv.ADJ
	elif "noun" in j_wordclasses or "pnoun_firstname" in j_wordclasses or \
	     "pnoun_lastname" in j_wordclasses or "pnoun_place" in j_wordclasses or \
	     "pnoun_misc" in j_wordclasses: wclass = hfconv.SUBST
	elif "verb" in j_wordclasses: wclass = hfconv.VERB 
	else: return (None, None)
	
	for (m_infclass, m_infclass_gradation, m_smclasses) in j_classmap:
		if m_infclass != infclass: continue
		for m_smclass in m_smclasses:
			(m_gradtype, pattern, jatko, wclasses) = (list(m_smclass) + [None])[:4]
			if wclasses != None and not wclass in wclasses: continue
			if not m_gradtype in gradtypes: continue
			alku = hfconv.match_re(wordform, pattern)
			if alku != None: return (alku, jatko)
	
	return (None, None)


# Returns a string describing the structure of a word, if necessary for the spellchecker
# or hyphenator
def get_structure(wordform, malaga_word_class):
	needstructure = False
	if malaga_word_class in ['nimi', 'etunimi', 'sukunimi', 'paikannimi']: ispropernoun = True
	else: ispropernoun = False
	if malaga_word_class == 'lyhenne':
		i = "j"
		p = "q"
	else:
		i = "i"
		p = "p"
	structstr = ', rakenne: "='
	for idx in range(len(wordform)):
		c = wordform[idx]
		if c == '-':
			structstr = structstr + "-="
			needstructure = True
		elif c == '|': structstr = structstr
		elif c == '=':
			structstr = structstr + "="
			needstructure = True
		elif c == ':':
			structstr = structstr + ":"
			needstructure = True
		elif c.isupper():
			structstr = structstr + i
			if not (ispropernoun and idx == 0):
				needstructure = True
		else:
			structstr = structstr + p
			if ispropernoun and idx == 0:
				needstructure = True
	if needstructure: return structstr + '"'
	else: return ""

# Writes the vocabulary entry to a suitable file
def write_entry(main_vocabulary,vocabulary_files,word, entry):
	main_vocabulary.write(entry + "\n")

# Parse command line options and return them in a dictionary
def get_options():
	try:
		optlist = ["min-frequency=", "extra-usage=", "style=", "destdir=", "no-baseform", "sourceid", "vanhat", "sukija", "sukija-ys"]
		(opts, args) = getopt.getopt([f for f in sys.argv[1:] if f.startswith("--")], "", optlist)
	except getopt.GetoptError:
		sys.stderr.write("Invalid option list for %s\n" % sys.argv[0])
		sys.exit(1)
	options = {"frequency": 9,
	           "extra-usage": [],
	           "style": ["old", "international", "inappropriate"],
	           "sourceid": False,
	           "vanhat": False,
	           "destdir": None,
	           "no-baseform": False,
		   "sukija": False,
		   "sukija-ys": False}
	for (name, value) in opts:
		if name == "--min-frequency":
			options["frequency"] = int(value)
		elif name == "--extra-usage":
			options["extra-usage"] = value.split(",")
		elif name == "--style":
			options["style"] = value.split(",")
		elif name == "--destdir":
			options["destdir"] = value
		elif name == "--no-baseform":
			options["no-baseform"] = True
		elif name == "--sourceid":
			options["sourceid"] = True
		elif name == "--vanhat":
			options["vanhat"] = True
		elif name == "--sukija":
			options["sukija"] = True
		elif name == "--sukija-ys":
			options["sukija-ys"] = True
	return options

# Strip whitespace and comments from LEXC input file
def stripWhitespaceAndComments(line):
	if "!" in line:
		line = line[0:line.find("!")]
	return line.strip()

# Filter LEXC input according to options
def filterVfstInput(line_orig, OPTIONS):
	if line_orig.startswith('?Sukija'):
		if OPTIONS["sukija"]:
			line_orig = line_orig[7:]
		else:
			return None
	if line_orig.startswith('?Murre'):
		if "dialect" in OPTIONS["style"] or OPTIONS["sukija"]:
			line_orig = line_orig[6:]
		else:
			return None
	if line_orig.startswith('?Vanha'):
		if OPTIONS["vanhat"] or OPTIONS["sukija"]:
			line_orig = line_orig[6:]
		else:
			return None
	return stripWhitespaceAndComments(line_orig)
