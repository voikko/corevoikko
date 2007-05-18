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

# Path to target directory
VOIKKO_LEX_DIR = u"voikko"

import sys
sys.path.append("common")
import hfconv
import generate_lex_common
import voikkoutils
import xml.dom.minidom
import codecs

flag_attributes = voikkoutils.readFlagAttributes(generate_lex_common.VOCABULARY_DATA + u"/flags.txt")

main_vocabulary = generate_lex_common.open_lex(VOIKKO_LEX_DIR, "joukahainen.lex")
vocabulary_files = {}
for voc in generate_lex_common.SPECIAL_VOCABULARY:
	vocabulary_files[voc[2]] = generate_lex_common.open_lex(VOIKKO_LEX_DIR, voc[2])

def frequency(word):
	fclass = word.getElementsByTagName("fclass")
	if len(fclass) == 0: return 7
	return int(generate_lex_common.tValue(fclass[0]))


# Writes the vocabulary entry to a suitable file
def write_entry(word, entry):
	global vocabulary_files
	global main_vocabulary
	special = False
	for voc in generate_lex_common.SPECIAL_VOCABULARY:
		group = word.getElementsByTagName(voc[0])
		if len(group) == 0: continue
		if generate_lex_common.has_flag(group[0], voc[1]):
			vocabulary_files[voc[2]].write(entry + u"\n")
			special = True
	if not special:
		main_vocabulary.write(entry + u"\n")

def handle_word(word):
	# Drop words that are not needed in the Voikko lexicon
	if generate_lex_common.has_flag(word, "not_voikko"): return
	if generate_lex_common.has_flag(word, "incorrect"): return
	if generate_lex_common.has_flag(word, "dialect"): return
	if frequency(word) >= 10: return
	if frequency(word) == 9 and generate_lex_common.has_flag(word, "confusing"): return
	
	# Get the inflection class. Exactly one inflection class is needed
	infclasses = word.getElementsByTagName("infclass")
	voikko_infclass = None
	for infclass in word.getElementsByTagName("infclass"):
		if infclass.getAttribute("type") != "historical":
			voikko_infclass = generate_lex_common.tValue(infclass)
			break
	if voikko_infclass == None: return
	if voikko_infclass == u"poikkeava": return
	
	# Get the word classes
	wordclasses = generate_lex_common.tValues(word.getElementsByTagName("classes")[0], "wclass")
	malaga_word_class = generate_lex_common.get_malaga_word_class(wordclasses)
	if malaga_word_class == None: return
	
	# Get malaga flags
	malaga_flags = generate_lex_common.get_malaga_flags(word)
	
	# Get forced vowel type
	forced_inflection_vtype = generate_lex_common.vowel_type(word.getElementsByTagName("inflection")[0])
	
	# Process all alternative forms
	for altform in generate_lex_common.tValues(word.getElementsByTagName("forms")[0], "form"):
		wordform = altform.replace(u'|', u'').replace(u'=', u'')
		(alku, jatko) = generate_lex_common.get_malaga_inflection_class(wordform, voikko_infclass, wordclasses)
		if forced_inflection_vtype == voikkoutils.VOWEL_DEFAULT:
			vtype = voikkoutils.get_wordform_infl_vowel_type(altform)
		else: vtype = forced_inflection_vtype
		if vtype == voikkoutils.VOWEL_FRONT: malaga_vtype = u'ä'
		elif vtype == voikkoutils.VOWEL_BACK: malaga_vtype = u'a'
		elif vtype == voikkoutils.VOWEL_BOTH: malaga_vtype = u'aä'
		rakenne = generate_lex_common.get_structure(altform, malaga_word_class)
		if alku == None:
			write_entry(word, u"#Malaga class not found for (%s, %s)\n" \
			            % (wordform, voikko_infclass))
			continue
		entry = u'[perusmuoto: "%s", alku: "%s", luokka: %s, jatko: <%s>, äs: %s%s%s];' \
		          % (wordform, alku, malaga_word_class, jatko, malaga_vtype, malaga_flags,
				   generate_lex_common.get_structure(altform, malaga_word_class))
		write_entry(word, entry)


listfile = open(generate_lex_common.VOCABULARY_DATA + u'/joukahainen.xml', 'r')

line = ""
while line != '<wordlist xml:lang="fi">\n':
	line = listfile.readline()
	if line == '':
		sys.stderr.write("Malformed file " + generate_lex_common.VOCABULARY_DATA + \
		                 "/joukahainen.xml\n")
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

