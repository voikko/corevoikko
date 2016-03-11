# Copyright 2007 - 2015 Harri Pitkänen (hatapitk@iki.fi)
# Program to generate lexicon files for VFST variant of voikko-fi

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
from xml.dom import Node

flag_attributes = voikkoutils.readFlagAttributes(generate_lex_common.VOCABULARY_DATA + "/flags.txt")

# Get command line options
OPTIONS = generate_lex_common.get_options()

# Inflection class map
CLASSMAP = hfconv.compileClassmapREs(hfconv.modern_classmap)

# No special vocabularies are built for Voikko
generate_lex_common.SPECIAL_VOCABULARY = []

vocabularyFileSuffixes = ["ep", "ee", "es", "em", "t", "nl", "l", "n", "h", "p", "a", "s", "c"]
vocabularyFiles = {}
for fileSuffix in vocabularyFileSuffixes:
	vocFile = codecs.open(OPTIONS["destdir"] + "/joukahainen-" + fileSuffix + ".lexc", 'w', 'UTF-8')
	vocFile.write("! This is automatically generated intermediate lexicon file for\n")
	vocFile.write("! VVFST morphology of voikko-fi. The original source data is\n")
	vocFile.write("! distributed under the GNU General Public License, version 2 or\n")
	vocFile.write("! later, as published by the Free Software Foundation. You should\n")
	vocFile.write("! have received the original data, tools and instructions to\n")
	vocFile.write("! generate this file (or instructions to obtain them) wherever\n")
	vocFile.write("! you got this file from.\n\n")
	vocFile.write("LEXICON Joukahainen_" + fileSuffix + "\n")
	vocabularyFiles[fileSuffix] = vocFile


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

# Returns VFST word class for given word in Joukahainen
def get_vfst_word_class(j_wordclasses):
	if "pnoun_place" in j_wordclasses: return "[Lep]"
	if "pnoun_firstname" in j_wordclasses: return "[Lee]"
	if "pnoun_lastname" in j_wordclasses: return "[Les]"
	if "pnoun_misc" in j_wordclasses: return "[Lem]"
	if "verb" in j_wordclasses: return "[Lt]"
	if "adjective" in j_wordclasses and "noun" in j_wordclasses: return "[Lnl]"
	if "adjective" in j_wordclasses: return "[Ll]"
	if "noun" in j_wordclasses: return "[Ln]"
	if "interjection" in j_wordclasses: return "[Lh]"
	if "prefix" in j_wordclasses: return "[Lp]"
	if "abbreviation" in j_wordclasses: return "[La]"
	if "adverb" in j_wordclasses: return "[Ls]"
	if "conjunction" in j_wordclasses: return "[Lc]"
	return None

# Returns a string describing the structure of a word, if necessary for the spellchecker
# or hyphenator
def get_structure(wordform, vfst_word_class, alku):
	needstructure = False
	ispropernoun = vfst_word_class[0:3] == '[Le'
	structstr = '[Xr]'
	oldAlku = alku
	newAlku = ""
	if vfst_word_class == '[La]':
		i = "j"
		p = "q"
	else:
		i = "i"
		p = "p"
	for idx in range(len(wordform)):
		c = wordform[idx]
		if c == '-':
			structstr = structstr + "-="
			if (len(oldAlku) > 0):
				newAlku = newAlku + '-[Bm]'
				oldAlku = oldAlku[1:]
		elif c == '|':
			structstr = structstr
		elif c == '=':
			structstr = structstr + "="
			newAlku = newAlku + "[Bm]"
		elif c == ':':
			structstr = structstr + ":"
			needstructure = True
			if (len(oldAlku) > 0):
				newAlku = newAlku + ':'
				oldAlku = oldAlku[1:]
		elif c.isupper():
			structstr = structstr + i
			if not (ispropernoun and idx == 0):
				needstructure = True
			if (len(oldAlku) > 0):
				newAlku = newAlku + oldAlku[0]
				oldAlku = oldAlku[1:]
		else:
			structstr = structstr + p
			if ispropernoun and idx == 0:
				needstructure = True
			if (len(oldAlku) > 0):
				newAlku = newAlku + oldAlku[0]
				oldAlku = oldAlku[1:]
	if needstructure:
		returnedLength = len(structstr)
		while structstr[returnedLength - 1] == p:
			returnedLength = returnedLength - 1
		return (structstr[0:returnedLength] + '[X]', alku)
	else:
		return ("", newAlku)

def get_diacritics(word, altforms, vfst_word_class):
	diacritics = []
	for group in word.childNodes:
		if group.nodeType != Node.ELEMENT_NODE:
			continue
		for flag in group.childNodes:
			if flag.nodeType != Node.ELEMENT_NODE:
				continue
			if flag.tagName != "flag":
				continue
			flagName = flag.firstChild.wholeText
			if flagName == "ei_yks":
				diacritics.append("@P.EI_YKS.ON@")
			elif flagName == "ysj":
				diacritics.append("@R.YS_ALKANUT@")
			elif flagName == "inen":
				diacritics.append("@P.INEN_SALLITTU.ON@")
			elif flagName == "ei_inen":
				diacritics.append("@P.INEN_KIELLETTY.ON@")
			elif flagName == "ei_mainen":
				diacritics.append("@P.EI_MAINEN.ON@")
			elif flagName == "ei_lainen":
				diacritics.append("@P.EI_LAINEN.ON@")
			elif flagName == "ei_vertm":
				diacritics.append("@P.EI_VERTM.ON@")
			elif flagName == "ym3":
				diacritics.append("@P.VAIN_YM3.ON@")
			elif flagName == "yt":
				diacritics.append("@P.YKSITEKIJÄINEN.ON@")
			elif flagName == "geo_suffix":
				diacritics.append("@C.PAIKANNIMEN_JL@")
			if flagName in ["ei_ys", "ei_ysa"]:
				diacritics.append("@P.YS_EI_JATKOA.ON@")
			if flagName in ["ei_ys", "ei_ysj"]:
				diacritics.append("@D.YS_ALKANUT@")
	if vfst_word_class in ["[Ln]", "[Lnl]"] and (altforms[0].endswith("lainen") or altforms[0].endswith("läinen")):
		diacritics.append("@P.LAINEN.ON@@C.LAINEN_VAADITTU@@C.VAIN_NIMISANA@")
	return diacritics

def get_info_flags(word):
	flags = ""
	for group in word.childNodes:
		if group.nodeType != Node.ELEMENT_NODE:
			continue
		for flag in group.childNodes:
			if flag.nodeType != Node.ELEMENT_NODE:
				continue
			if flag.tagName != "flag":
				continue
			flagName = flag.firstChild.wholeText
			if flagName == "paikannimi_ulkopaikallissijat":
				flags = flags + "[Ipu]"
			elif flagName == "paikannimi_sisäpaikallissijat":
				flags = flags + "[Ips]"
			elif flagName == "foreignloan":
				flags = flags + "[Isf]"
			elif flagName == "el_altark":
				flags = flags + "[De]"
			elif flagName == "geo_suffix":
				flags = flags + "[Ica]"
			elif flagName == "org_suffix":
				flags = flags + "[Ion]"
			elif flagName == "free_suffix":
				flags = flags + "[Ivj]"
			elif flagName == "require_following_a":
				flags = flags + "[Ira]"
			elif flagName == "require_following_ma":
				flags = flags + "[Irm]"
	return flags

def get_vfst_class_prefix(vfst_class):
	if vfst_class == "[Ln]":
		return "Nimisana"
	elif vfst_class == "[Lee]":
		return "Etunimi"
	elif vfst_class == "[Lep]":
		return "Paikannimi"
	elif vfst_class == "[Les]":
		return "Sukunimi"
	elif vfst_class == "[Lem]":
		return "Nimi"
	elif vfst_class == "[Ll]":
		return "Laatusana"
	elif vfst_class == "[Lnl]":
		return "NimiLaatusana"
	else:
		return ""

def vowel_type_for_derived_verb(wordform):
	for char in reversed(wordform):
		if char in "yäö":
			return "@P.V_SALLITTU.E@"
		if char in "uao":
			return "@P.V_SALLITTU.T@"
		if char in "]":
			break
	return "@P.V_SALLITTU.T@"

def get_prefix_jatko(word, altform):
	flags = generate_lex_common.get_flags_from_group(word, "compounding")
	prefixJatko = ""
	for flag in sorted(flags):
		if flag in ["eln", "ell", "elt", "eltj"]:
			prefixJatko = prefixJatko + flag
	if altform.endswith("-"):
		prefixJatko = prefixJatko + "H"
	return prefixJatko

def get_adverb_jatko(word, altform):
	flags = generate_lex_common.get_flags_from_group(word, "inflection")
	loppu = True
	adverbJatko = ""
	for flag in sorted(flags):
		if flag in ["liitesana", "ulkopaikallissijat_yks"]:
			adverbJatko = adverbJatko + flag.title()
		elif flag == "omistusliite":
			if altform[-1] in "aäe" and altform[-1] != altform[-2]:
				adverbJatko = adverbJatko + "OlV"
			else:
				adverbJatko = adverbJatko + "Omistusliite"
		elif flag == "required":
			loppu = False;
	if loppu:
		adverbJatko = "Loppu" + adverbJatko
	return adverbJatko

def get_abbreviation_jatko(word, wordform):
	flags = generate_lex_common.get_flags_from_group(word, "inflection")
	if wordform.endswith(".") or "none" in flags:
		return "PisteellisenLyhenteenJatko"
	else:
		return "Lyhenne"

def injectBaseformToStructure(baseform, structure):
	if baseform is None or baseform == "":
		return structure
	i = 0
	# The following requires an incompatible format change but would
	# make the standard dictionary about 30 % smaller.
	# while i < len(baseform) and i + 1 < len(structure):
	#	if baseform[i] != structure[i]:
	#		break
	#	i = i + 1
	if i == 0:
		return "[Xp]" + baseform + "[X]" + structure
	return structure[:i] + "[Xp]" + baseform[i:] + "[X]" + structure[i:]

def handle_word(word):
	global OPTIONS
	global CLASSMAP
	# Drop words that are not needed in the Voikko lexicon
	# but only if not generating Sukija lexicon.
	if generate_lex_common.has_flag(word, "not_voikko") and not OPTIONS["sukija"]: return
	if not check_style(word): return
	if not check_usage(word): return
	if frequency(word) >= OPTIONS["frequency"] + 1: return
	if frequency(word) == OPTIONS["frequency"] and generate_lex_common.has_flag(word, "confusing"): return
	
	# Get the inflection class. Exactly one inflection class is needed
	voikko_infclass = None
	if OPTIONS["sukija"]:
		for infclass in word.getElementsByTagName("infclass"):
			if infclass.getAttribute("type") == "historical":
				voikko_infclass = generate_lex_common.tValue(infclass)
				if voikko_infclass == "banaali":   # Banaali taipuu kuten paperi.
					voikko_infclass = "paperi"
				elif voikko_infclass == "pasuuna":
					voikko_infclass = "peruna"
				if voikko_infclass not in ["aavistaa-av1", "arvelu", "arvelu-av1", "haravoida-av2", "karahka", "matala", "paperi", "paperi-av1", "peruna"]:
					voikko_infclass = None
				break
	if voikko_infclass == None:
		for infclass in word.getElementsByTagName("infclass"):
			if infclass.getAttribute("type") != "historical":
				voikko_infclass = generate_lex_common.tValue(infclass)
				break
	if voikko_infclass == "poikkeava": return
	
	# Get the word classes
	wordclasses = generate_lex_common.tValues(word.getElementsByTagName("classes")[0], "wclass")
	if wordclasses[0] not in ["interjection", "prefix", "abbreviation", "conjunction", "adverb"] and voikko_infclass == None:
		return
	vfst_word_class = get_vfst_word_class(wordclasses)
	if vfst_word_class == None: return
	
	# Get diacritics
	altforms = generate_lex_common.tValues(word.getElementsByTagName("forms")[0], "form")
	diacritics = "".join(get_diacritics(word, altforms, vfst_word_class))
	
	# Get forced vowel type
	if voikko_infclass == None and vfst_word_class != "[La]":
		forced_inflection_vtype = voikkoutils.VOWEL_DEFAULT
	else:
		inflectionElement = word.getElementsByTagName("inflection")
		if len(inflectionElement) > 0:
			forced_inflection_vtype = generate_lex_common.vowel_type(inflectionElement[0])
		else:
			forced_inflection_vtype = voikkoutils.VOWEL_DEFAULT
	
	# Construct debug information
	debug_info = ""
	if OPTIONS["sourceid"]:
		debug_info = '[Xs]%s[X]' % word.getAttribute("id")[1:].replace("0", "%0")
	
	infoFlags = get_info_flags(word)
	
	# Process all alternative forms
	singlePartForms = []
	multiPartForms = []
	for altform in altforms:
		outputBaseform = altform.replace('|', '')
		wordform = outputBaseform.replace('=', '')
		if len(altform) == len(wordform.replace('-', '')):
			singlePartForms.append(altform)
		else:
			multiPartForms.append(altform)
		(alku, jatko) = generate_lex_common.get_malaga_inflection_class(wordform, voikko_infclass, wordclasses, CLASSMAP)
		if alku == None:
			errorstr = "ERROR: VFST class not found for (%s, %s)\n" % (wordform, voikko_infclass)
			sys.stderr.write(errorstr.encode("UTF-8"))
			sys.exit(1)
		if vfst_word_class == "[La]":
			jatko = get_abbreviation_jatko(word, altform)
		elif vfst_word_class == "[Ls]":
			jatko = get_adverb_jatko(word, altform)
		else:
			jatko = jatko.title()
		if vfst_word_class in ["[Ls]", "[Lc]", "[Lh]"]:
			for element in word.getElementsByTagName("baseform"):
				wordform = generate_lex_common.tValue(element)
				outputBaseform = wordform.replace('|', '')
		if forced_inflection_vtype == voikkoutils.VOWEL_DEFAULT:
			vtype = voikkoutils.get_wordform_infl_vowel_type(altform)
		else: vtype = forced_inflection_vtype
		if vtype == voikkoutils.VOWEL_FRONT: vfst_vtype = 'ä'
		elif vtype == voikkoutils.VOWEL_BACK: vfst_vtype = 'a'
		elif vtype == voikkoutils.VOWEL_BOTH: vfst_vtype = 'aä'
		vocabularyFile = vocabularyFiles[vfst_word_class.replace("[L", "").replace("]", "")]
		if alku == None:
			errorstr = "ERROR: Malaga class not found for (%s, %s)\n" \
				% (wordform, voikko_infclass)
			generate_lex_common.write_entry(vocabularyFile, {}, word, errorstr)
			sys.stderr.write(errorstr.encode("UTF-8"))
			sys.exit(1)
		alku = alku.lower()
		(rakenne, alkuWithTags) = get_structure(altform, vfst_word_class, alku)
		
		if OPTIONS["no-baseform"]:
			outputBaseform = ""
		
		if vfst_word_class == "[Lh]":
			entry = '%s%s%s%s:%s # ;' % (vfst_word_class, debug_info, rakenne, injectBaseformToStructure(outputBaseform, alkuWithTags), alku)
			vocabularyFile.write(entry + "\n")
			continue
		vfst_class_prefix = get_vfst_class_prefix(vfst_word_class)
		
		# Vowel type in derived verbs
		if jatko in ["Heittää", "Muistaa", "Juontaa", "Hohtaa", "Murtaa", "Nousta", "Loistaa", "Jättää", "Kihistä"]:
			diacritics = diacritics + vowel_type_for_derived_verb(alkuWithTags)
			if jatko == "Kihistä" and vtype == voikkoutils.VOWEL_FRONT and "y" not in alku and "ä" not in alku and "ö" not in alku and "e" in alku:
				jatko = "Helistä"
		
		if jatko == "Nainen" and vfst_class_prefix in ["Laatusana", "NimiLaatusana"] and altform.endswith("inen"):
			jatko = "NainenInen"
		
		if vfst_word_class == "[Lp]":
			entry = '[Lp]%s%s%s%s%s:%s%s EtuliitteenJatko_%s;' \
			        % (debug_info, rakenne, alkuWithTags, diacritics, infoFlags, alku, diacritics, get_prefix_jatko(word, altform))
		else:
			entry = '%s%s%s%s%s%s:%s%s %s%s_%s ;' \
			        % (vfst_word_class, debug_info, rakenne, infoFlags, injectBaseformToStructure(outputBaseform, alkuWithTags),
			        diacritics, alku, diacritics, vfst_class_prefix, jatko, vfst_vtype)
		vocabularyFile.write(entry + "\n")
	
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

for fileSuffix in vocabularyFileSuffixes:
	vocabularyFiles[fileSuffix].write("\n\n") # Extra line feeds needed to avoid mixed lines in concatenated lexc file
	vocabularyFiles[fileSuffix].close()
