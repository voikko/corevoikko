# Copyright 2007 - 2015 Harri Pitkänen (hatapitk@iki.fi)
# Program to generate lexicon files for voikko-fi Malaga edition

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

flag_attributes = voikkoutils.readFlagAttributes(generate_lex_common.VOCABULARY_DATA + "/flags.txt")

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
	flags = generate_lex_common.get_flags_from_group(word, "compounding")
	prefixJatko = ""
	for flag in flags:
		if flag in ["eln", "ell", "elt", "eltj"]:
			if (len(prefixJatko) > 0):
				prefixJatko = prefixJatko + " + "
			prefixJatko = prefixJatko + "@" + flag
	return prefixJatko

def get_adverb_jatko(word):
	flags = generate_lex_common.get_flags_from_group(word, "inflection")
	prefixJatko = ""
	loppu = True
	for flag in flags:
		if flag in ["liitesana", "omistusliite"]:
			prefixJatko = prefixJatko + ", " + flag
		elif flag == "ulkopaikallissijat_yks":
			prefixJatko = prefixJatko + ", ulkopaikallissija_llA"
		elif flag == "required":
			loppu = False;
	if loppu:
		prefixJatko = prefixJatko + ", loppu"
	if prefixJatko.startswith(", "):
		prefixJatko = prefixJatko[2:]
	return prefixJatko

def get_abbreviation_jatko(word, wordform):
	flags = generate_lex_common.get_flags_from_group(word, "inflection")
	if wordform.endswith(".") or "none" in flags:
		return "loppu"
	else:
		return "tavuviiva, kaksoispiste, loppu"

def get_additional_attributes(word):
	flags = generate_lex_common.get_flags_from_group(word, "compounding")
	result = ""
	if "el_altark" in flags:
		result = result + ", aluetta_tarkentava_etuliite: yes"
	if "geo_suffix" in flags:
		result = result + ", paikannimen_jälkiliite: yes"
	if "org_suffix" in flags:
		result = result + ", erisnimen_pääte: yes"
	if "free_suffix" in flags:
		result = result + ", vapaa_jälkiosa: yes"
	flags = generate_lex_common.get_flags_from_group(word, "grammar")
	if "require_following_a" in flags:
		result = result + ", vaatii_tapaluokan: nimitapa_1"
	if "require_following_ma" in flags:
		result = result + ", vaatii_tapaluokan: nimitapa_3"
	return result

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
	if voikko_infclass == "poikkeava": return
	
	# Get the word classes
	wordclasses = generate_lex_common.tValues(word.getElementsByTagName("classes")[0], "wclass")
	if wordclasses[0] not in ["interjection", "prefix", "abbreviation", "conjunction", "adverb"] and voikko_infclass == None:
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
	if voikko_infclass == None and malaga_word_class != "lyhenne":
		forced_inflection_vtype = voikkoutils.VOWEL_DEFAULT
	else:
		inflectionElement = word.getElementsByTagName("inflection")
		if len(inflectionElement) > 0:
			forced_inflection_vtype = generate_lex_common.vowel_type(inflectionElement[0])
		else:
			forced_inflection_vtype = voikkoutils.VOWEL_DEFAULT
	
	# Construct debug information and additional attributes
	additional_attributes = get_additional_attributes(word)
	if OPTIONS["sourceid"]:
		additional_attributes = additional_attributes + ', sourceid: "%s"' % word.getAttribute("id")
	
	# Process all alternative forms
	singlePartForms = []
	multiPartForms = []
	for altform in generate_lex_common.tValues(word.getElementsByTagName("forms")[0], "form"):
		wordform = altform.replace('|', '').replace('=', '')
		if len(altform) == len(wordform.replace('-', '')):
			singlePartForms.append(altform)
		else:
			multiPartForms.append(altform)
		(alku, jatko) = generate_lex_common.get_malaga_inflection_class(wordform, voikko_infclass, wordclasses, CLASSMAP)
		if alku == None:
			errorstr = "ERROR: Malaga class not found for (%s, %s)\n" \
				% (wordform, voikko_infclass)
			generate_lex_common.write_entry(main_vocabulary, {}, word, errorstr)
			sys.stderr.write(errorstr.encode("UTF-8"))
			sys.exit(1)
		if malaga_word_class == "lyhenne":
			jatko = get_abbreviation_jatko(word, altform)
		elif malaga_word_class == "seikkasana":
			jatko = get_adverb_jatko(word)
		if malaga_word_class == "etuliite":
			vtype = voikkoutils.VOWEL_BOTH
			malaga_jatko = get_prefix_jatko(word)
		else:
			if forced_inflection_vtype == voikkoutils.VOWEL_DEFAULT:
				vtype = voikkoutils.get_wordform_infl_vowel_type(altform)
			else:
				vtype = forced_inflection_vtype
			malaga_jatko = "<" + jatko + ">"
		if vtype == voikkoutils.VOWEL_FRONT: malaga_vtype = 'ä'
		elif vtype == voikkoutils.VOWEL_BACK: malaga_vtype = 'a'
		elif vtype == voikkoutils.VOWEL_BOTH: malaga_vtype = 'aä'
		rakenne = generate_lex_common.get_structure(altform, malaga_word_class)
		if baseform is None:
			altBaseform = altform
		else:
			altBaseform = baseform
		if malaga_word_class == "lyhenne":
			perusmuotoEntry = ""
		else:
			perusmuotoEntry = 'perusmuoto: "%s", ' % altBaseform
		entry = '[%salku: "%s", luokka: %s, jatko: %s, äs: %s%s%s%s];' \
		          % (perusmuotoEntry, alku, malaga_word_class, malaga_jatko, malaga_vtype, malaga_flags,
			   generate_lex_common.get_structure(altform, malaga_word_class),
			   additional_attributes)
		generate_lex_common.write_entry(main_vocabulary, {}, word, entry)
	
	# Sanity check for alternative forms: if there are both multi part forms and single part forms
	# then all multi part forms must end with a part contained in the single part set.
	if singlePartForms:
		for multiPartForm in multiPartForms:
			lastPart = multiPartForm[max(multiPartForm.rfind("="), multiPartForm.rfind("|"), multiPartForm.rfind("-")) + 1:]
			if lastPart not in singlePartForms:
				sys.stderr.write("ERROR: suspicious alternative spelling: %s\n" % multiPartForm)
				sys.exit(1)


voikkoutils.process_wordlist(generate_lex_common.VOCABULARY_DATA + '/joukahainen.xml', \
                             handle_word, True)

main_vocabulary.close()
