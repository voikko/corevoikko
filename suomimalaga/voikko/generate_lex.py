# -*- coding: utf-8 -*-

# Copyright 2007 - 2011 Harri Pitkänen (hatapitk@iki.fi)
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

import sys
sys.path.append("common")
import hfconv
import generate_lex_common
import voikkoutils
import xml.dom.minidom
import codecs
from string import rfind

flag_attributes = voikkoutils.readFlagAttributes(generate_lex_common.VOCABULARY_DATA + u"/flags.txt")

# Get command line options
OPTIONS = generate_lex_common.get_options()

# Inflection class map
CLASSMAP = hfconv.compileClassmapREs(hfconv.modern_classmap)

# No special vocabularies are built for Voikko
generate_lex_common.SPECIAL_VOCABULARY = []

main_vocabulary = generate_lex_common.open_lex(OPTIONS["destdir"], "joukahainen.lex")

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
			if style == "foreignloan":
				continue
			if not style in OPTIONS["style"]: return False
	return True

# Returns True if the word is acceptable according to its usage flags.
def check_usage(word):
	global OPTIONS
	wordUsage = word.getElementsByTagName("usage")
	if len(wordUsage) == 0: return True
	for usageE in wordUsage:
		for usage in generate_lex_common.tValues(usageE, "flag"):
			if usage in OPTIONS["extra-usage"]: return True
	return False

def get_prefix_jatko(word):
	flags = generate_lex_common.get_flags_from_group(word, u"compounding")
	prefixJatko = u""
	for flag in flags:
		if flag in [u"eln", u"ell", u"elt", u"eltj"]:
			if (len(prefixJatko) > 0):
				prefixJatko = prefixJatko + u" + "
			prefixJatko = prefixJatko + u"@" + flag
	return prefixJatko

def handle_word(word):
	global OPTIONS
	global CLASSMAP
	# Drop words that are not needed in the Voikko lexicon
	if generate_lex_common.has_flag(word, "not_voikko") and "sukija" not in OPTIONS["extra-usage"]:
		return
	if not check_style(word): return
	if not check_usage(word): return
	if frequency(word) >= OPTIONS["frequency"] + 1: return
	if frequency(word) == OPTIONS["frequency"] and generate_lex_common.has_flag(word, "confusing"): return
	
	# Get the inflection class. Exactly one inflection class is needed
	voikko_infclass = None
	for infclass in word.getElementsByTagName("infclass"):
		if infclass.getAttribute("type") != "historical":
			voikko_infclass = generate_lex_common.tValue(infclass)
			break
	if voikko_infclass == u"poikkeava": return
	
	# Get the word classes
	wordclasses = generate_lex_common.tValues(word.getElementsByTagName("classes")[0], "wclass")
	if wordclasses[0] not in [u"interjection", u"prefix"] and voikko_infclass == None:
		return
	malaga_word_class = generate_lex_common.get_malaga_word_class(wordclasses)
	if malaga_word_class == None: return
	
	baseformTags = word.getElementsByTagName("baseform")
	if len(baseformTags) > 0:
		baseform = generate_lex_common.tValue(baseformTags[0])
	else:
		baseform = None
	
	# Get malaga flags
	malaga_flags = generate_lex_common.get_malaga_flags(word)
	
	# Get forced vowel type
	if voikko_infclass == None:
		forced_inflection_vtype = voikkoutils.VOWEL_DEFAULT
	else:
		forced_inflection_vtype = generate_lex_common.vowel_type(word.getElementsByTagName("inflection")[0])
	
	# Construct debug information
	debug_info = u""
	if OPTIONS["sourceid"]:
		debug_info = u', sourceid: "%s"' % word.getAttribute("id")
	
	# Process all alternative forms
	singlePartForms = []
	multiPartForms = []
	for altform in generate_lex_common.tValues(word.getElementsByTagName("forms")[0], "form"):
		wordform = altform.replace(u'|', u'').replace(u'=', u'')
		if len(altform) == len(wordform.replace(u'-', u'')):
			singlePartForms.append(altform)
		else:
			multiPartForms.append(altform)
		(alku, jatko) = generate_lex_common.get_malaga_inflection_class(wordform, voikko_infclass, wordclasses, CLASSMAP)
		if malaga_word_class == u"etuliite":
			vtype = voikkoutils.VOWEL_BOTH
			malaga_jatko = get_prefix_jatko(word)
		else:
			if forced_inflection_vtype == voikkoutils.VOWEL_DEFAULT:
				vtype = voikkoutils.get_wordform_infl_vowel_type(altform)
			else:
				vtype = forced_inflection_vtype
			malaga_jatko = u"<" + jatko + u">"
		if vtype == voikkoutils.VOWEL_FRONT: malaga_vtype = u'ä'
		elif vtype == voikkoutils.VOWEL_BACK: malaga_vtype = u'a'
		elif vtype == voikkoutils.VOWEL_BOTH: malaga_vtype = u'aä'
		rakenne = generate_lex_common.get_structure(altform, malaga_word_class)
		if alku == None:
			errorstr = u"ERROR: Malaga class not found for (%s, %s)\n" \
				% (wordform, voikko_infclass)
			generate_lex_common.write_entry(main_vocabulary, {}, word, errorstr)
			sys.stderr.write(errorstr.encode(u"UTF-8"))
			sys.exit(1)
		if baseform is None:
			baseform = wordform
		entry = u'[perusmuoto: "%s", alku: "%s", luokka: %s, jatko: %s, äs: %s%s%s%s];' \
		          % (baseform, alku, malaga_word_class, malaga_jatko, malaga_vtype, malaga_flags,
			   generate_lex_common.get_structure(altform, malaga_word_class),
			   debug_info)
		generate_lex_common.write_entry(main_vocabulary, {}, word, entry)
	
	# Sanity check for alternative forms: if there are both multi part forms and single part forms
	# then all multi part forms must end with a part contained in the single part set.
	if singlePartForms:
		for multiPartForm in multiPartForms:
			lastPart = multiPartForm[max(rfind(multiPartForm, u"="), rfind(multiPartForm, u"|"), rfind(multiPartForm, u"-")) + 1:]
			if lastPart not in singlePartForms:
				sys.stderr.write(u"ERROR: suspicious alternative spelling: %s\n" % multiPartForm)
				sys.exit(1)


voikkoutils.process_wordlist(generate_lex_common.VOCABULARY_DATA + u'/joukahainen.xml', \
                             handle_word, True)

main_vocabulary.close()
