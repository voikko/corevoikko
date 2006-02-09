#!/usr/bin/env python2.4
# -*- coding: utf-8 -*-

# Copyright 2005-2006 Harri Pitkänen (hatapitk@cc.jyu.fi)
# Library for reading and writing affix data for Hunspell-fi -project.
# This program requires Python version 2.4 or newer.

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
import codecs
import sys
import re


# Size of the affix file is controlled by the following parameter. Only affix rules
# having priority lower or equal to MAX_AFFIX_PRIORITY are written to the
# affix file. Values from 1 to 3 are currently used.
MAX_AFFIX_PRIORITY=2

# Two-character Hunspell affix class identifier flags are used. The follwing strings contain
# allowed first and second characters for these identifiers.
FLAG_CHARS_1 = 'QRST'
FLAG_CHARS_2 = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'

# Function to get the next available affix flag
def __next_affix_flag(previous_flag):
	n1 = FLAG_CHARS_1.find(previous_flag[0])
	n2 = FLAG_CHARS_2.find(previous_flag[1])
	n2 = n2 + 1
	if n2 == len(FLAG_CHARS_2):
		n2 = 0
		n1 = n1 + 1
	return FLAG_CHARS_1[n1]+FLAG_CHARS_2[n2]

# Function to convert a string containing back vowels to an equivalent string containing
# front vowels. Possessive suffix flags will also be converted.
def __convert_tv_ev(text):
	return text.replace('a', u'ä').replace('o', u'ö').replace('u', u'y').replace('A0', 'A1')

# Write an affix class to a file.
#def __write_affix_class(lines, file):
#	lines[0] = lines[0].replace('$$', `len(lines)-1`)
#	for r in lines:
#		pRow = ''
#		for s in r.split():
#			pRow = pRow + s + ' '
#		file.write(pRow.strip()+'\n')

# Read header line from a file. Return value will be a tuple in the form (name, value) or
# None if the end of file was reached.
def __read_header(file):
	while True:
		line = file.readline()
		if not line.endswith('\n'): return None
		strippedLine = hfutils.remove_comments(line).strip()
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
		strippedLine = hfutils.remove_comments(line).strip()
		if len(strippedLine) == 0: continue
		columns = strippedLine.split()
		if columns[0] == 'end:': return None
		else: return columns



# Read an inflection class from a file. Returns the following dictionary of items:
# cname: class name
# match: regular expression that all words in this class must match
# gradation: consonant gradation type. 0 = none, 1 = strong->weak, 2 = weak->strong
# group: FIXME: should be t
# tgroup: FIXME: should be e
# rules: list of lists containing the inflection rules for this class
# If the end of file is reached, this function returns None.
def __read_inflection_class(file):
	header_tuple = __read_header(file)
	if header_tuple == None: return None
	if header_tuple[0] != 'class' or len(header_tuple[1]) == 0:
		print 'Class definition expected.'
		sys.exit(1)
	cname = header_tuple[1]
	match = '.*'
	gradation = 0
	group = 't'
	tgroup = '-'
	rules = []
	while True:
		header_tuple = __read_header(file)
		if header_tuple == None:
			print 'Unexpected end of file.'
			sys.exit(1)
		if header_tuple[0] == 'match-word': match = header_tuple[1]
		if header_tuple[0] == 'consonant-gradation':
			if header_tuple[1] == '-': gradation = 0
			if header_tuple[1] == 'sw': gradation = 1
			if header_tuple[1] == 'ws': gradation = 2
		if header_tuple[0] == 'group':
			if header_tuple[1] == 't': group = 't'
			if header_tuple[1] == 'e': group = 'e'
		if header_tuple[0] == 'transform-group':
			if header_tuple[1] == '-': tgroup = '-'
			if header_tuple[1] == 't': tgroup = 't'
			if header_tuple[1] == 'e': tgroup = 'e'
		if header_tuple[0] == 'rules':
			rule = __read_list_line(file)
			while rule != None:
				rules.append(rule)
				rule = __read_list_line(file)
		if header_tuple[0] == 'end' and header_tuple[1] == 'class':
			return { 'cname':cname, 'match':match, 'gradation':gradation, \
			         'group':group, 'tgroup':tgroup, 'rules':rules }


# Convert a Hunspell-fi -style pair of regular expression and replacement string to a list
# of tuples containing corresponding Hunspell affix rule elements (strip_str, affix, condition).
def __regex_to_hunspell(exp, repl):
	# TODO: implement more regular expressions
	rulelist = []
	if exp == "0":
		strip_str = "0"
		condition = "."
		affix = repl
		rulelist.append((strip_str, affix, condition))
		return rulelist
	if re.compile("^(?:[a-z])+$").match(exp) != None: # string of letters
		strip_str = exp
		condition = exp
		affix = repl
		rulelist.append((strip_str, affix, condition))
		return rulelist
	m = re.compile("^((?:[a-z])*)\\(\\[((?:[a-z])*)\\]\\)((?:[a-z])*)$").match(exp)
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
	m = re.compile("^((?:[a-z])*)\\[((?:[a-z])*)\\]((?:[a-z])*)$").match(exp)
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
	sys.exit(1)



# Read an option "name" from string "option". If it does not exist, then default will be returned.
def __read_option(options, name, default):
	parts = options.split(',');
	for part in parts:
		nameval = part.split('=')
		if len(nameval) == 2 and nameval[0] == name: return nameval[1]
	return default


# Write an affix class to a file "file". Affix data is given in list of lines "lines". If
# transformation=='e', back to front vowel transfromation is performed on the data.
# Affixflag is the affix flag to be used for this class.
# TODO: lines are still assumed to be in back vowel format.
def __write_affix_class(lines, transformation, affixflag, file):
	linestrings = []
	pos_suffixes = { '-':'/A0', 'f':'', 'r':'/A0F0' }
	for line in lines:
		possufx = '-'
		if len(line) == 5: possufx = __read_option(line[4], 'ps', '-')
		for rule in __regex_to_hunspell(line[1], line[2]):
			linestring = 'SFX ' + affixflag + ' ' + rule[0] + ' ' + rule[1] + \
			             pos_suffixes[possufx] + ' ' + rule[2] + ' +' + line[0].upper()
			if transformation == 'e': linestrings.append(__convert_tv_ev(linestring))
			else: linestrings.append(linestring)
	number_of_lines = len(linestrings)
	file.write('SFX ' + affixflag + ' Y ' + `number_of_lines` + '\n')
	for linestring in linestrings: file.write(linestring + '\n')


# Write an inflection class to a file. Returns the next unused affix flag
# FIXME: writing to a class file.
def __write_inflection_class(classinfo, first_affixflag, affixfile):
	affixflag = first_affixflag
	if classinfo['group'] == 't': # FIXME: other possibilities
		if classinfo['gradation'] == 0: # no gradation
			rulelist = []
			for rule in classinfo['rules']:
				if len(rule) == 4 or \
				   int(__read_option(rule[4], 'prio', '1')) <= MAX_AFFIX_PRIORITY:
					rulelist.append(rule)
			__write_affix_class(rulelist, '-', affixflag, affixfile)
			affixflag = __next_affix_flag(affixflag)
			if classinfo['tgroup'] == 'e':
				__write_affix_class(rulelist, 'e', affixflag, affixfile)
				affixflag = __next_affix_flag(affixflag)
		if classinfo['gradation'] in [1, 2]: # strong <-> weak
			if classinfo['gradation'] == 1: # strong -> weak
				orig_grad = 's'
				trans_grad = 'w'
			else: # weak -> strong
				orig_grad = 'w'
				trans_grad = 's'
			rulelist = []
			for rule in classinfo['rules']:
				if rule[3] == orig_grad and (len(rule) == 4 or \
				   int(__read_option(rule[4], 'prio', '1')) <= MAX_AFFIX_PRIORITY):
					rulelist.append(rule)
			__write_affix_class(rulelist, '-', affixflag, affixfile)
			affixflag = __next_affix_flag(affixflag)
			if classinfo['tgroup'] == 'e':
				__write_affix_class(rulelist, 'e', affixflag, affixfile)
				affixflag = __next_affix_flag(affixflag)
			rulelist = []
			for rule in classinfo['rules']:
				if rule[3] == trans_grad and (len(rule) == 4 or \
				   int(__read_option(rule[4], 'prio', '1')) <= MAX_AFFIX_PRIORITY):
					rulelist.append(rule)
			__write_affix_class(rulelist, '-', affixflag, affixfile)
			affixflag = __next_affix_flag(affixflag)
			if classinfo['tgroup'] == 'e':
				__write_affix_class(rulelist, 'e', affixflag, affixfile)
				affixflag = __next_affix_flag(affixflag)
	return affixflag


# Public functions

# Writes the contents of file named inputfile_name to a file descriptor outputfile.
# Comments are removed.
def write_affix_base(inputfile_name, outputfile):
	inputfile=codecs.open(inputfile_name, 'r', 'utf-8')
	fileCont = True
	while fileCont:
		line = inputfile.readline()
		fileCont = line.endswith('\n')
		line = hfutils.remove_comments(line).strip()
		if len(line) > 0: outputfile.write(line+'\n')
	inputfile.close()


# Reads and returns a list of noun classes from a file named file_name.
def read_noun_classes(file_name):
	noun_classes = []
	inputfile=codecs.open(file_name, 'r', 'utf-8')
	infclass = __read_inflection_class(inputfile)
	while infclass != None:
		noun_classes.append(infclass)
		infclass = __read_inflection_class(inputfile)
	inputfile.close()
	return noun_classes


# Writes the given list of noun classes to the file descriptor.
def write_noun_classes(noun_classes, file):
	affixflag = 'Q0'
	for noun_class in noun_classes:
		affixflag = __write_inflection_class(noun_class, affixflag, file)




