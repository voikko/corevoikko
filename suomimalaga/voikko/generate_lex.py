# -*- coding: utf-8 -*-

# Copyright 2007 - 2008 Harri Pitkänen (hatapitk@iki.fi)
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

# Get command line options
OPTIONS = generate_lex_common.get_options()

# According to the options, remove some special vocabularies
old_special = generate_lex_common.SPECIAL_VOCABULARY
generate_lex_common.SPECIAL_VOCABULARY = []
for s in old_special:
	if s[0] != "usage": continue
	if s[1] in OPTIONS["extra-usage"]: continue
	generate_lex_common.SPECIAL_VOCABULARY.append(s)

main_vocabulary = generate_lex_common.open_lex(VOIKKO_LEX_DIR, "joukahainen.lex")
vocabulary_files = {}
for voc in generate_lex_common.SPECIAL_VOCABULARY:
	vocabulary_files[voc[2]] = generate_lex_common.open_lex(VOIKKO_LEX_DIR, voc[2])


def frequency(word):
	fclass = word.getElementsByTagName("fclass")
	if len(fclass) == 0: return 7
	return int(generate_lex_common.tValue(fclass[0]))

# Check the style flags of the word according to current options.
# Returns True if the word is acceptable, otherwise returns false.
def check_style(word):
	global OPTIONS
	for styleE in word.getElementsByTagName("style"):
		for style in generate_lex_common.tValues(styleE, "flag"):
			if not style in OPTIONS["style"]: return False
	return True

def handle_word(word):
	global OPTIONS
	# Drop words that are not needed in the Voikko lexicon
	if generate_lex_common.has_flag(word, "not_voikko"): return
	if not check_style(word): return
	if frequency(word) >= OPTIONS["frequency"] + 1: return
	if frequency(word) == OPTIONS["frequency"] and generate_lex_common.has_flag(word, "confusing"): return
	
	# Get the inflection class. Exactly one inflection class is needed
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
		(alku, jatko) = generate_lex_common.get_malaga_inflection_class(wordform, voikko_infclass, wordclasses, hfconv.modern_classmap)
		if forced_inflection_vtype == voikkoutils.VOWEL_DEFAULT:
			vtype = voikkoutils.get_wordform_infl_vowel_type(altform)
		else: vtype = forced_inflection_vtype
		if vtype == voikkoutils.VOWEL_FRONT: malaga_vtype = u'ä'
		elif vtype == voikkoutils.VOWEL_BACK: malaga_vtype = u'a'
		elif vtype == voikkoutils.VOWEL_BOTH: malaga_vtype = u'aä'
		rakenne = generate_lex_common.get_structure(altform, malaga_word_class)
		if alku == None:
			generate_lex_common.write_entry(main_vocabulary, vocabulary_files,
				word, u"#Malaga class not found for (%s, %s)\n" \
				% (wordform, voikko_infclass))
			continue
		entry = u'[perusmuoto: "%s", alku: "%s", luokka: %s, jatko: <%s>, äs: %s%s%s];' \
		          % (wordform, alku, malaga_word_class, jatko, malaga_vtype, malaga_flags,
				   generate_lex_common.get_structure(altform, malaga_word_class))
		generate_lex_common.write_entry(main_vocabulary, vocabulary_files, word, entry)


voikkoutils.process_wordlist(generate_lex_common.VOCABULARY_DATA + u'/joukahainen.xml', \
                             handle_word, True)

main_vocabulary.close()
for (name, file) in vocabulary_files.iteritems():
	file.close()

