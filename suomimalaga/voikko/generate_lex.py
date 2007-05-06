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

# Path to source data directory
VOIKKO_DATA = u"vocabulary"

# Path to target directory
VOIKKO_LEX = u"voikko"

# Vocabulary entries that should be saved to different files
# (group, name, file)
SPECIAL_VOCABULARY = [
	('usage', 'it', 'atk.lex'),
	('usage', 'medicine', 'laaketiede.lex'),
	('usage', 'science', 'matluonnontiede.lex'),
	('usage', 'education', 'kasvatustiede.lex'),
	('style', 'foreign', 'vieraskieliset.lex')]


import hfconv
import voikkoutils
import xml.dom.minidom
import codecs
import sys

flag_attributes = voikkoutils.readFlagAttributes(VOIKKO_DATA + u"/flags.txt")

def open_lex(filename):
	file = codecs.open(VOIKKO_LEX + u"/" + filename, 'w', 'UTF-8')
	file.write(u"# This is automatically generated intermediate lexicon file for\n")
	file.write(u"# Suomi-malaga Voikko edition. The original source data is\n")
	file.write(u"# distributed under the GNU General Public License, version 2 or\n")
	file.write(u"# later, as published by the Free Software Foundation. You should\n")
	file.write(u"# have received the original data, tools and instructions to\n")
	file.write(u"# generate this file (or instructions to obtain them) wherever\n")
	file.write(u"# you got this file from.\n\n")
	return file

main_vocabulary = open_lex("joukahainen.lex")
vocabulary_files = {}
for voc in SPECIAL_VOCABULARY:
	vocabulary_files[voc[2]] = open_lex(voc[2])

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
	if flag in tValues(word, "flag"): return True
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
	if len(malaga_flags) == 0: return u""
	flag_string = u", tiedot: <"
	for flag in malaga_flags:
		flag_string = flag_string + flag + u","
	flag_string = flag_string[:-1] + u">"
	return flag_string

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
def write_entry(word, entry):
	global vocabulary_files
	global main_vocabulary
	special = False
	for voc in SPECIAL_VOCABULARY:
		group = word.getElementsByTagName(voc[0])
		if len(group) == 0: continue
		if has_flag(group[0], voc[1]):
			vocabulary_files[voc[2]].write(entry + u"\n")
			special = True
	if not special:
		main_vocabulary.write(entry + u"\n")

def handle_word(word):
	# Drop words that are not needed in the Voikko lexicon
	if has_flag(word, "not_voikko"): return
	if has_flag(word, "incorrect"): return
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
		if forced_inflection_vtype == voikkoutils.VOWEL_DEFAULT:
			vtype = voikkoutils.get_wordform_infl_vowel_type(altform)
		else: vtype = forced_inflection_vtype
		if vtype == voikkoutils.VOWEL_FRONT: malaga_vtype = u'ä'
		elif vtype == voikkoutils.VOWEL_BACK: malaga_vtype = u'a'
		elif vtype == voikkoutils.VOWEL_BOTH: malaga_vtype = u'aä'
		rakenne = get_structure(altform, malaga_word_class)
		if alku == None:
			write_entry(word, u"#Malaga class not found for (%s, %s)\n" \
			            % (wordform, voikko_infclass))
			continue
		entry = u'[perusmuoto: "%s", alku: "%s", luokka: %s, jatko: <%s>, äs: %s%s%s];' \
		          % (wordform, alku, malaga_word_class, jatko, malaga_vtype, malaga_flags,
				   get_structure(altform, malaga_word_class))
		write_entry(word, entry)


listfile = open(VOIKKO_DATA + u'/joukahainen.xml', 'r')

line = ""
while line != '<wordlist xml:lang="fi">\n':
	line = listfile.readline()
	if line == '':
		sys.stderr.write("Malformed file " + VOIKKO_DATA + "/joukahainen.xml\n")
		sys.exit(1)

wcount = 0
while True:
	wordstr = ""
	line = listfile.readline()
	if line == "</wordlist>\n": break
	while line != '</word>\n':
		wordstr = wordstr + line
		line = listfile.readline()
	word = xml.dom.minidom.parseString(wordstr + line)
	handle_word(word)
	wcount = wcount + 1
	if wcount % 1000 == 0:
		sys.stdout.write("#")
		sys.stdout.flush()

sys.stdout.write("\n")
listfile.close()
main_vocabulary.close()
for (name, file) in vocabulary_files.iteritems():
	file.close()

