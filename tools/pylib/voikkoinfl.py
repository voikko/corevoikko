# -*- coding: utf-8 -*-

# Copyright 2005-2007 Harri Pitkänen (hatapitk@iki.fi)
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

import hfutils
import voikkoutils
import codecs
import sys
import re


# Size of the affix file is controlled by the following parameter. Only affix rules
# having priority lower or equal to MAX_AFFIX_PRIORITY are written to the
# affix file. Values from 1 to 3 are currently used.
MAX_AFFIX_PRIORITY=2


# Function to convert a string containing back vowels to an equivalent string containing
# front vowels. Possessive suffix flags will also be converted.
def __convert_tv_ev(text):
	return text.replace('a', u'ä').replace('o', u'ö').replace('u', u'y').replace('A0', 'A1')


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
			print 'Malformed input file: the problematic line was'
			print line
			sys.exit(1)
		return (strippedLine[:valStart].strip(), strippedLine[valStart+1:].strip())


# Read an affix rule line from a file. Returns a list containing the columns or None, if
# the end of group or file was reached.
def __read_list_line(file):
	while True:
		line = file.readline()
		if not line.endswith('\n'): return None
		strippedLine = voikkoutils.removeComments(line).strip()
		if len(strippedLine) == 0: continue
		columns = strippedLine.split()
		if columns[0] == 'end:': return None
		else: return columns



# Read an inflection class from a file. Returns the following dictionary of items:
# cname: class name
# smcnames: Suomi-Malaga class names
# rmsfx: suffix to be removed from every word before applying inflections
# match: regular expression that all words in this class must match
# gradation: consonant gradation type.
# group: FIXME: should be t
# tgroup: FIXME: should be e
# note: free form description of the class
# rules: list of lists containing the inflection rules for this class
# If the end of file is reached, this function returns None.
def __read_inflection_class(file):
	header_tuple = __read_header(file)
	if header_tuple == None: return None
	if header_tuple[0] != 'class' or len(header_tuple[1]) == 0:
		print 'Class definition expected.'
		sys.exit(1)
	cname = header_tuple[1]
	smcnames = []
	rmsfx = u''
	match = '.*'
	gradation = voikkoutils.GRAD_NONE
	group = 't'
	tgroup = '-'
	note = u''
	rules = []
	while True:
		header_tuple = __read_header(file)
		if header_tuple == None:
			print 'Unexpected end of file.'
			sys.exit(1)
		if header_tuple[0] == 'sm-class': smcnames = header_tuple[1].split(' ')
		if header_tuple[0] == u'rmsfx': rmsfx = header_tuple[1]
		if header_tuple[0] == 'match-word': match = header_tuple[1]
		if header_tuple[0] == 'consonant-gradation':
			if header_tuple[1] == '-': gradation = voikkoutils.GRAD_NONE
			if header_tuple[1] == 'sw': gradation = voikkoutils.GRAD_SW
			if header_tuple[1] == 'ws': gradation = voikkoutils.GRAD_WS
		if header_tuple[0] == 'group':
			if header_tuple[1] == 't': group = 't'
			if header_tuple[1] == 'e': group = 'e'
		if header_tuple[0] == 'transform-group':
			if header_tuple[1] == '-': tgroup = '-'
			if header_tuple[1] == 't': tgroup = 't'
			if header_tuple[1] == 'e': tgroup = 'e'
		if header_tuple[0] == 'note':
			note = header_tuple[1]
		if header_tuple[0] == 'rules':
			rule = __read_list_line(file)
			while rule != None:
				rules.append(rule)
				rule = __read_list_line(file)
		if header_tuple[0] == 'end' and header_tuple[1] == 'class':
			return { 'cname':cname, 'smcnames':smcnames, 'match':match, \
			         'gradation':gradation, 'note':note, \
			         'group':group, 'tgroup':tgroup, 'rules':rules, 'rmsfx':rmsfx }


# Convert a Hunspell-fi -style pair of regular expression and replacement string to a list
# of tuples containing corresponding Hunspell affix rule elements (strip_str, affix, condition).
def __regex_to_hunspell(exp, repl):
	# TODO: implement more regular expressions
	rulelist = []
	wchars = u"[a-zäöé]"
	if exp == "0":
		strip_str = "0"
		condition = "."
		affix = repl
		rulelist.append((strip_str, affix, condition))
		return rulelist
	if re.compile(u"^(?:%s)+$" % wchars).match(exp) != None: # string of letters
		strip_str = exp
		condition = exp
		affix = repl
		rulelist.append((strip_str, affix, condition))
		return rulelist
	m = re.compile(u"^((?:%s)*)\\(\\[((?:%s)*)\\]\\)((?:%s)*)$" % (wchars, wchars, wchars) \
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
	print 'Unsupported regular expression: exp=\'' + exp + '\', repl=\'' + repl + '\''
	return None


# Translates word match pattern to a Perl-compatible regular expression
def __word_pattern_to_pcre(pattern):
	return '.*' + hfutils.hf_regexp(pattern) + '$'


# Public functions

# Writes the contents of file named inputfile_name to a file descriptor outputfile.
# Comments are removed.
def write_affix_base(inputfile_name, outputfile):
	inputfile=codecs.open(inputfile_name, 'r', hfutils.INPUT_ENCODING)
	fileCont = True
	while fileCont:
		line = inputfile.readline()
		fileCont = line.endswith('\n')
		line = voikkoutils.removeComments(line).strip()
		if len(line) > 0: outputfile.write(line+'\n')
	inputfile.close()


# Reads and returns a list of word classes from a file named file_name.
def read_word_classes(file_name):
	word_classes = []
	inputfile = codecs.open(file_name, 'r', hfutils.INPUT_ENCODING)
	infclass = __read_inflection_class(inputfile)
	while infclass != None:
		word_classes.append(infclass)
		infclass = __read_inflection_class(inputfile)
	inputfile.close()
	return word_classes


# Returns a noun class that matches the given word or None, if such class is not found.
def get_matching_noun_class(word, cname, grad_exact_type, noun_classes):
	word_grad = hfutils.apply_gradation(word, grad_exact_type)
	if grad_exact_type == '-': grad_type = voikkoutils.GRAD_NONE
	elif grad_exact_type in ['av1', 'av3', 'av5']: grad_type = voikkoutils.GRAD_SW
	elif grad_exact_type in ['av2', 'av4', 'av6']: grad_type = voikkoutils.GRAD_WS
	for noun_class in noun_classes:
		if 'subst-' + cname != noun_class['cname']: continue
		if grad_type != voikkoutils.GRAD_NONE and grad_type != noun_class['gradation']: continue
		if not re.compile(__word_pattern_to_pcre(noun_class['match']),
		                  re.IGNORECASE).match(word): continue
		return noun_class
	return None

def _replace_conditional_aposthrope(word):
	ind = word.find(u'$')
	if ind == -1: return word
	if ind == 0 or ind == len(word) - 1: return word.replace(u'$', u'')
	if word[ind-1] == word[ind+1]:
		if word[ind-1] in [u'i', u'o', u'u', u'y', u'ö']:
			return word.replace(u'$', u'\'')
		if word[ind-1] in [u'a', u'ä'] and ind > 1 and word[ind-2] == word[ind-1]:
			return word.replace(u'$', u'\'')
	return word.replace(u'$', u'')

def _vtype_subst_tO(base):
	base = base.lower()
	last_back = max(base.rfind(u'a'), base.rfind(u'o'), base.rfind(u'å'), base.rfind(u'u'))
	last_front = max(base.rfind(u'ä'), base.rfind(u'ö'), base.rfind(u'y'))
	if last_front > last_back: return voikkoutils.VOWEL_FRONT
	else: return voikkoutils.VOWEL_BACK

# Returns a list of inflected forms for a given word or None, if word cannot be
# inflected in the given class. The list will contain tuples in the form
# (form_name, word, flags).
def inflect_word(word, grad_exact_type, noun_class, vowel_type = voikkoutils.VOWEL_DEFAULT):
	l = len(noun_class['rmsfx'])
	if l == 0: word_no_sfx = word
	else: word_no_sfx = word[:-l]
	word_grad = hfutils.apply_gradation(word_no_sfx, grad_exact_type)
	if word_grad == None: return None
	if grad_exact_type == '-': grad_type = voikkoutils.GRAD_NONE
	elif grad_exact_type in ['av1', 'av3', 'av5']: grad_type = voikkoutils.GRAD_SW
	elif grad_exact_type in ['av2', 'av4', 'av6']: grad_type = voikkoutils.GRAD_WS
	if grad_type != voikkoutils.GRAD_NONE and grad_type != noun_class['gradation']: return None
	if not re.compile(__word_pattern_to_pcre(noun_class['match']),
	                  re.IGNORECASE).match(word): return None
	inflection_list = []
	if vowel_type == voikkoutils.VOWEL_DEFAULT: vowel_type = hfutils.vowel_type(word)
	if vowel_type in [voikkoutils.VOWEL_BACK, voikkoutils.VOWEL_BOTH] and \
	   noun_class['group'] != 't': return None # FIXME
	if vowel_type in [voikkoutils.VOWEL_FRONT, voikkoutils.VOWEL_BOTH] and \
	   noun_class['tgroup'] != 'e': return None # FIXME
	for rule in noun_class['rules']:
		if len(rule) == 5:
			optflags = rule[4]
			if int(hfutils.read_option(optflags, 'prio', '1')) > MAX_AFFIX_PRIORITY: continue
		else: optflags = ""
		if rule[3] == 's': word_base = word_grad[0]
		else: word_base = word_grad[1]
		hunspell_rules = __regex_to_hunspell(rule[1], rule[2])
		for hunspell_rule in hunspell_rules:
			if hunspell_rule[0] == '0': word_stripped_base = word_base
			else: word_stripped_base = word_base[:-len(hunspell_rule[0])]
			if hunspell_rule[1] == '0': affix = ''
			else: affix = hunspell_rule[1]
			if hunspell_rule[2] == '.': pattern = ''
			else: pattern = hunspell_rule[2]
			if rule[0].endswith('subst_tO') or rule[0].endswith('subst_Os'):
				if _vtype_subst_tO(word_stripped_base) == voikkoutils.VOWEL_FRONT:
					word_infl = word_stripped_base + __convert_tv_ev(affix)
				else:
					word_infl = word_stripped_base + affix
				inflection_list.append((rule[0], word_infl, optflags))
				continue
			if vowel_type in [voikkoutils.VOWEL_BACK, voikkoutils.VOWEL_BOTH] and \
			   word_base.endswith(pattern):
				word_infl = _replace_conditional_aposthrope(
				             word_stripped_base + affix)
				inflection_list.append((rule[0], word_infl, optflags))
			if vowel_type in [voikkoutils.VOWEL_FRONT, voikkoutils.VOWEL_BOTH] and \
			   word_base.endswith(__convert_tv_ev(pattern)):
				word_infl = _replace_conditional_aposthrope(
				             word_stripped_base + __convert_tv_ev(affix))
				inflection_list.append((rule[0], word_infl, optflags))
	return inflection_list

