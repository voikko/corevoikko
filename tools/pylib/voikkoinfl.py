# -*- coding: utf-8 -*-

# Copyright 2005-2010 Harri Pitkänen (hatapitk@iki.fi)
# Library for inflecting words for Voikko project.
# This library requires Python version 2.4 or newer.

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

import voikkoutils
import codecs
import sys
import re


# Size of the affix file is controlled by the following parameter. Only affix rules
# having priority lower or equal to MAX_AFFIX_PRIORITY are written to the
# affix file. Values from 1 to 3 are currently used.
MAX_AFFIX_PRIORITY=2

class InflectionRule:
	"Rule for word inflection"
	def __init__(self):
		self.name = ""
		self.isCharacteristic = False
		self.rulePriority = 1
		self.delSuffix = ""
		self.addSuffix = ""
		self.gradation = voikkoutils.GRAD_WEAK

class InflectionType:
	"Word inflection type"
	def __init__(self):
		self.kotusClasses = []
		self.joukahainenClasses = []
		self.rmsfx = ""
		self.matchWord = ""
		self.gradation = voikkoutils.GRAD_NONE
		self.note = ""
		self.inflectionRules = []
	
	"Return the given word with suffix removed"
	def removeSuffix(self, word):
		l = len(self.rmsfx)
		if l == 0: return word
		elif len(word) <= l: return ""
		else: return word[:-l]
	
	"Return the Kotus gradation class for word and gradation class in Joukahainen"
	def kotusGradClass(self, word, grad_type):
		if not grad_type in ["av1", "av2", "av3", "av4", "av5", "av6"]:
			return ""
		word = self.removeSuffix(word)
		if len(word) == 0: return ""
		if voikkoutils.is_consonant(word[-1]) and not voikkoutils.is_consonant(word[-2]):
			if word[-4:-2] == 'ng': return 'G'
			if word[-4:-2] == 'mm': return 'H'
			if word[-4:-2] == 'nn': return 'J'
			if word[-4:-2] == 'll': return 'I'
			if word[-4:-2] == 'rr': return 'K'
			if word[-3] == 'd': return 'F'
			if word[-3] == 't': return 'C'
			if word[-3] == 'k': return 'A'
			if word[-3] == 'p': return 'B'
			if word[-3] == 'v': return 'E'
		if grad_type == 'av1':
			if word[-3:-1] == 'tt': return 'C'
			if word[-3:-1] == 'kk': return 'A'
			if word[-3:-1] == 'pp': return 'B'
			if word[-3:-1] == 'mp': return 'H'
			if word[-2] == 'p' and not voikkoutils.is_consonant(word[-1]):
				return 'E'
			if word[-3:-1] == 'nt': return 'J'
			if word[-3:-1] == 'lt': return 'I'
			if word[-3:-1] == 'rt': return 'K'
			if word[-2] == 't': return 'F'
			if word[-3:-1] == 'nk': return 'G'
			if word[-3:] == 'uku': return 'M'
			if word[-3:] == 'yky': return 'M'
		if grad_type == 'av2':
			if word[-4:-2] == 'ng': return 'G'
			if word[-4:-2] == 'mm': return 'H'
			if word[-4:-2] == 'nn': return 'J'
			if word[-4:-2] == 'll': return 'I'
			if word[-4:-2] == 'rr': return 'K'
			if word[-3] == 'd': return 'F'
			if word[-3] == 't': return 'C'
			if word[-3] == 'k': return 'A'
			if word[-3] == 'p': return 'B'
			if word[-3] == 'b': return 'O' # Unofficial, not in Kotus
			if word[-3] == 'g': return 'P' # Unofficial, not in Kotus
			if word[-3] == 'v': return 'E'
		if grad_type == 'av3': # k -> j
			if word[-2] == 'k': return 'L'
		if grad_type == 'av4': # j -> k
			if word[-2] == 'j': return 'L'
			if word[-3] == 'j': return 'L'
		if grad_type == 'av5': # k -> -
			if word[-2] == 'k': return 'D'
		if grad_type == 'av6': # - -> k
			return 'D'
		return ''

class InflectedWord:
	"Word in inflected form"
	def __init__(self):
		self.formName = ""
		self.inflectedWord = ""
		self.isCharacteristic = False
		self.priority = 1
	def __str__(self):
		return self.formName + "\t" + self.inflectedWord

## Function to convert a string containing back vowels to an equivalent string containing
# front vowels.
def __convert_tv_ev(text):
	return text.replace('a', 'ä').replace('o', 'ö').replace('u', 'y')


## Applies given gradation type to given word. Returns a tuple in the form
# (strong, weak) or None if this is not possible. Conditional aposthrope
# is represented by $.
def __apply_gradation(word, grad_type):
	if grad_type == '-':
		return (word, word)
	
	if voikkoutils.is_consonant(word[-1]) and not voikkoutils.is_consonant(word[-2]) and len(word) >= 3:
		if word[-4:-2] == 'ng':
			return (word[:-4]+'nk'+word[-2:], word)
		# uvu/yvy->uku/yky not possible?
		if word[-4:-2] == 'mm':
			return (word[:-4]+'mp'+word[-2:], word)
		if word[-4:-2] == 'nn':
			return (word[:-4]+'nt'+word[-2:], word)
		if word[-4:-2] == 'll':
			return (word[:-4]+'lt'+word[-2:], word)
		if word[-4:-2] == 'rr':
			return (word[:-4]+'rt'+word[-2:], word)
		if word[-3] == 'd':
			return (word[:-3]+'t'+word[-2:], word)
		if word[-3] in 'tkp':
			return (word[:-2]+word[-3:], word)
		if word[-3] == 'v':
			return (word[:-3]+'p'+word[-2:], word)
	
	if grad_type == 'av1' and len(word) >= 3:
		if word[-3:-1] in ('tt','kk','pp'):
			return (word, word[:-2]+word[-1])
		if word[-3:-1] == 'mp':
			return (word, word[:-3]+'mm'+word[-1])
		if word[-2] == 'p' and not voikkoutils.is_consonant(word[-1]):
			return (word, word[:-2]+'v'+word[-1])
		if word[-3:-1] == 'nt':
			return (word, word[:-3]+'nn'+word[-1])
		if word[-3:-1] == 'lt':
			return (word, word[:-3]+'ll'+word[-1])
		if word[-3:-1] == 'rt':
			return (word, word[:-3]+'rr'+word[-1])
		if word[-2] == 't':
			return (word, word[:-2]+'d'+word[-1])
		if word[-3:-1] == 'nk':
			return (word, word[:-3]+'ng'+word[-1])
		if word[-3:] == 'uku':
			return (word, word[:-3]+'uvu')
		if word[-3:] == 'yky':
			return (word, word[:-3]+'yvy')
	if grad_type == 'av2' and len(word) >= 2:
		if word[-3:-1] == 'ng':
			return (word[:-3]+'nk'+word[-1], word)
		# uvu/yvy->uku/yky not possible?
		if word[-3:-1] == 'mm':
			return (word[:-3]+'mp'+word[-1], word)
		if word[-3:-1] == 'nn':
			return (word[:-3]+'nt'+word[-1], word)
		if word[-3:-1] == 'll':
			return (word[:-3]+'lt'+word[-1], word)
		if word[-3:-1] == 'rr':
			return (word[:-3]+'rt'+word[-1], word)
		if word[-2] == 'd':
			return (word[:-2]+'t'+word[-1], word)
		if word[-2] in 'tkpbg':
			return (word[:-1]+word[-2:], word)
		if word[-2] == 'v':
			return (word[:-2]+'p'+word[-1], word)
	if grad_type == 'av3' and len(word) >= 3: # k -> j
		if word[-2] == 'k':
			if voikkoutils.is_consonant(word[-3]):
				return (word, word[:-2]+'j'+word[-1])
			else:
				return (word, word[:-3]+'j'+word[-1])
	if grad_type == 'av4' and len(word) >= 3: # j -> k
		if word[-2] == 'j':
			return (word[:-2]+'k'+word[-1], word)
		if word[-3] == 'j':
			return (word[:-3]+'k'+word[-2]+word[-1], word)
	if grad_type == 'av5' and len(word) >= 2: # k -> -
		if word[-2] == 'k':
			return (word, word[:-2]+'$'+word[-1])
	if grad_type == 'av6' and len(word) >= 1: # - -> k
		if voikkoutils.is_consonant(word[-1]): # FIXME: hack
			return (word[:-2]+'k'+word[-2:], word)
		else:
			return (word[:-1]+'k'+word[-1], word)
	return None


# Read header line from a file. Return value will be a tuple in the form (name, value) or
# None if the end of file was reached.
def __read_header(file):
	while True:
		line = file.readline()
		if not line.endswith('\n'): return None
		strippedLine = voikkoutils.removeComments(line).strip()
		if len(strippedLine) == 0: continue
		valStart = strippedLine.find(':')
		if valStart < 1:
			print('Malformed input file: the problematic line was')
			print(line)
			return None
		return (strippedLine[:valStart].strip(), strippedLine[valStart+1:].strip())


# Read an option "name" from string "options". If it does not exist, then default will be returned.
def __read_option(options, name, default):
	parts = options.split(',');
	for part in parts:
		nameval = part.split('=')
		if len(nameval) == 2 and nameval[0] == name: return nameval[1]
		if len(nameval) == 1 and nameval[0] == name: return '1'
	return default

# Read and return an inflection rule from a file. Returns None, if
# the end of group or file was reached.
def __read_inflection_rule(file):
	while True:
		line = file.readline()
		if not line.endswith('\n'): return None
		strippedLine = voikkoutils.removeComments(line).strip()
		if len(strippedLine) == 0: continue
		columns = strippedLine.split()
		if columns[0] == 'end:': return None
		
		r = InflectionRule()
		if columns[0].startswith('!'):
			r.name = columns[0][1:]
			r.isCharacteristic = True
		else:
			r.name = columns[0]
			if columns[0] in ['nominatiivi', 'genetiivi', 'partitiivi', 'illatiivi',
				'nominatiivi_mon', 'genetiivi_mon', 'partitiivi_mon', 'illatiivi_mon',
				'infinitiivi_1', 'preesens_yks_1', 'imperfekti_yks_3',
				'kondit_yks_3', 'imperatiivi_yks_3', 'partisiippi_2',
				'imperfekti_pass']: r.isCharacteristic = True
			else: r.isCharacteristic = False
		if columns[1] != '0': r.delSuffix = columns[1]
		if columns[2] != '0': r.addSuffix = columns[2]
		if columns[3] == 's': r.gradation = voikkoutils.GRAD_STRONG
		if len(columns) > 4:
			if __read_option(columns[4], 'ps', '') == 'r': continue
			r.rulePriority = int(__read_option(columns[4], 'prio', '1'))
		
		return r



# Read and return an inflection type from a file.
# If the end of file is reached, this function returns None.
def __read_inflection_type(file):
	header_tuple = __read_header(file)
	if header_tuple == None: return None
	if header_tuple[0] != 'class' or len(header_tuple[1]) == 0:
		print('Class definition expected.')
		return None
	
	t = InflectionType()
	t.kotusClasses = header_tuple[1].split(",")
	while True:
		header_tuple = __read_header(file)
		if header_tuple == None:
			print('Unexpected end of file.')
			return None
		if header_tuple[0] == 'sm-class': t.joukahainenClasses = header_tuple[1].split(' ')
		if header_tuple[0] == 'rmsfx': t.rmsfx = header_tuple[1]
		if header_tuple[0] == 'match-word': t.matchWord = header_tuple[1]
		if header_tuple[0] == 'consonant-gradation':
			if header_tuple[1] == '-': t.gradation = voikkoutils.GRAD_NONE
			if header_tuple[1] == 'sw': t.gradation = voikkoutils.GRAD_SW
			if header_tuple[1] == 'ws': t.gradation = voikkoutils.GRAD_WS
		if header_tuple[0] == 'note':
			t.note = header_tuple[1]
		if header_tuple[0] == 'rules':
			rule = __read_inflection_rule(file)
			while rule != None:
				t.inflectionRules.append(rule)
				rule = __read_inflection_rule(file)
		if header_tuple[0] == 'end' and header_tuple[1] == 'class':
			return t


# Convert a Hunspell-fi -style pair of regular expression and replacement string to a list
# of tuples containing corresponding Hunspell affix rule elements (strip_str, affix, condition).
def __regex_to_hunspell(exp, repl):
	# TODO: implement more regular expressions
	rulelist = []
	wchars = "[a-zäöé]"
	if exp == "": exp = "0"
	if repl == "": repl = "0"
	if exp == "0":
		strip_str = "0"
		condition = "."
		affix = repl
		rulelist.append((strip_str, affix, condition))
		return rulelist
	if re.compile("^(?:%s)+$" % wchars).match(exp) != None: # string of letters
		strip_str = exp
		condition = exp
		affix = repl
		rulelist.append((strip_str, affix, condition))
		return rulelist
	m = re.compile("^((?:%s)*)\\(\\[((?:%s)*)\\]\\)((?:%s)*)$" % (wchars, wchars, wchars) \
	              ).match(exp)
	if m != None: # exp is of form 'ab([cd])ef'
		start_letters = m.group(1)
		alt_letters = m.group(2)
		end_letters = m.group(3)
		for alt_char in alt_letters:
			strip_str = start_letters + alt_char + end_letters
			condition = start_letters + alt_char + end_letters
			affix = repl.replace('(1)', alt_char)
			rulelist.append((strip_str, affix, condition))
		return rulelist
	m = re.compile("^((?:%s)*)\\[((?:%s)*)\\]((?:%s)*)$" % (wchars, wchars, wchars)).match(exp)
	if m != None: # exp is of form 'ab[cd]ef'
		start_letters = m.group(1)
		alt_letters = m.group(2)
		end_letters = m.group(3)
		for alt_char in alt_letters:
			strip_str = start_letters + alt_char + end_letters
			condition = start_letters + alt_char + end_letters
			affix = repl
			rulelist.append((strip_str, affix, condition))
		return rulelist
	print('Unsupported regular expression: exp=\'' + exp + '\', repl=\'' + repl + '\'')
	return []


# Translates word match pattern to a Perl-compatible regular expression
def __word_pattern_to_pcre(pattern):
	return '.*' + voikkoutils.capital_char_regexp(pattern) + '$'


# Public functions


# Reads and returns a list of word classes from a file named file_name.
def readInflectionTypes(file_name):
	inflection_types = []
	inputfile = codecs.open(file_name, 'r', 'UTF-8')
	inftype = __read_inflection_type(inputfile)
	while inftype != None:
		inflection_types.append(inftype)
		inftype = __read_inflection_type(inputfile)
	inputfile.close()
	return inflection_types


def _replace_conditional_aposthrope(word):
	ind = word.find('$')
	if ind == -1: return word
	if ind == 0 or ind == len(word) - 1: return word.replace('$', '')
	if word[ind-1] == word[ind+1]:
		if word[ind-1] in ['i', 'o', 'u', 'y', 'ö']:
			return word.replace('$', '\'')
		if word[ind-1] in ['a', 'ä'] and ind > 1 and word[ind-2] == word[ind-1]:
			return word.replace('$', '\'')
	return word.replace('$', '')

DERIVS_VOWEL_HARMONY_SPECIAL_CLASS_1 = ['subst_tO', 'subst_Os']
DERIVS_VOWEL_HARMONY_SPECIAL_CLASS_2 = ['verbi_AjAA', 'verbi_AhtAA', 'verbi_AUttAA', 'verbi_AUtellA']

def _normalize_base(base):
	if base.find('=') != -1:
		base = base[base.find('=') + 1:]
	return base.lower()

def _vtype_special_class_1(base):
	base = _normalize_base(base)
	last_back = max(base.rfind('a'), base.rfind('o'), base.rfind('å'), base.rfind('u'))
	last_front = max(base.rfind('ä'), base.rfind('ö'), base.rfind('y'))
	if last_front > last_back: return voikkoutils.VOWEL_FRONT
	else: return voikkoutils.VOWEL_BACK

def _vtype_special_class_2(base):
	base = _normalize_base(base)
	last_back = max(base.rfind('a'), base.rfind('o'), base.rfind('å'), base.rfind('u'))
	last_front = max(base.rfind('ä'), base.rfind('ö'), base.rfind('y'))
	if last_front > last_back: return voikkoutils.VOWEL_FRONT
	elif last_front < last_back: return voikkoutils.VOWEL_BACK
	else:
		# no front or back vowels
		if base.rfind('e') >= 0:
			# "hel|istä" -> "heläjää"
			return voikkoutils.VOWEL_FRONT
		else:
			# "kih|istä" -> "kihajaa"
			return voikkoutils.VOWEL_BACK

def _vtype_meri_partitive(base):
	return voikkoutils.VOWEL_BACK

def _removeStructure(word):
	return word.replace("=", "").replace("|", "")

## Returns a list of InflectedWord objects for given word and inflection type.
def inflectWordWithType(word, inflection_type, infclass, gradclass, vowel_type = voikkoutils.VOWEL_DEFAULT):
	if not infclass in inflection_type.joukahainenClasses: return []
	word_no_sfx = inflection_type.removeSuffix(word)
	word_grad = __apply_gradation(word_no_sfx, gradclass)
	if word_grad == None: return []
	if gradclass == '-': grad_type = voikkoutils.GRAD_NONE
	elif gradclass in ['av1', 'av3', 'av5']: grad_type = voikkoutils.GRAD_SW
	elif gradclass in ['av2', 'av4', 'av6']: grad_type = voikkoutils.GRAD_WS
	if grad_type != voikkoutils.GRAD_NONE and grad_type != inflection_type.gradation: return []
	if not re.compile(__word_pattern_to_pcre(inflection_type.matchWord),
	                  re.IGNORECASE).match(word): return []
	inflection_list = []
	if vowel_type == voikkoutils.VOWEL_DEFAULT:
		vowel_type = voikkoutils.get_wordform_infl_vowel_type(word)
	for rule in inflection_type.inflectionRules:
		if rule.gradation == voikkoutils.GRAD_STRONG: word_base = word_grad[0]
		else: word_base = word_grad[1]
		hunspell_rules = __regex_to_hunspell(rule.delSuffix, rule.addSuffix)
		for hunspell_rule in hunspell_rules:
			if hunspell_rule[0] == '0': word_stripped_base = word_base
			else: word_stripped_base = word_base[:-len(hunspell_rule[0])]
			if hunspell_rule[1] == '0': affix = ''
			else: affix = hunspell_rule[1]
			if hunspell_rule[2] == '.': pattern = ''
			else: pattern = hunspell_rule[2]
			
			infl = InflectedWord()
			infl.formName = rule.name
			infl.isCharacteristic = rule.isCharacteristic
			infl.priority = rule.rulePriority
			
			vowel_harmony_rule = None
			if rule.name in DERIVS_VOWEL_HARMONY_SPECIAL_CLASS_1:
				vowel_harmony_rule = _vtype_special_class_1
			elif rule.name in DERIVS_VOWEL_HARMONY_SPECIAL_CLASS_2:
				vowel_harmony_rule = _vtype_special_class_2
			elif rule.name == 'partitiivi' and infclass == 'meri':
				vowel_harmony_rule = _vtype_meri_partitive
			final_base = _removeStructure(word_stripped_base)
			if vowel_harmony_rule != None:
				if vowel_harmony_rule(word_stripped_base) == voikkoutils.VOWEL_FRONT:
					infl.inflectedWord = final_base + __convert_tv_ev(affix)
				else:
					infl.inflectedWord = final_base + affix
				inflection_list.append(infl)
				continue
			
			if vowel_type in [voikkoutils.VOWEL_BACK, voikkoutils.VOWEL_BOTH] and \
			   word_base.endswith(pattern):
				infl.inflectedWord = _replace_conditional_aposthrope(
				                     final_base + affix)
				inflection_list.append(infl)
				infl = InflectedWord()
				infl.formName = rule.name
				infl.isCharacteristic = rule.isCharacteristic
				infl.priority = rule.rulePriority
			if vowel_type in [voikkoutils.VOWEL_FRONT, voikkoutils.VOWEL_BOTH] and \
			   word_base.endswith(__convert_tv_ev(pattern)):
				infl.inflectedWord = _replace_conditional_aposthrope(
				                     final_base + __convert_tv_ev(affix))
				inflection_list.append(infl)
	return inflection_list

## Returns a list of InflectedWord objects for given word.
def inflectWord(word, jo_infclass, inflection_types, vowel_type = voikkoutils.VOWEL_DEFAULT):
	dash = jo_infclass.find('-')
	if dash == -1:
		infclass = jo_infclass
		gradclass = '-'
	else:
		infclass = jo_infclass[:dash]
		gradclass = jo_infclass[dash+1:]
		if not gradclass in ['av1', 'av2', 'av3', 'av4', 'av5', 'av6', '-']:
			return []
	
	for inflection_type in inflection_types:
		inflection = inflectWordWithType(word, inflection_type, infclass, gradclass, vowel_type)
		if len(inflection) > 0: return inflection
	return []
