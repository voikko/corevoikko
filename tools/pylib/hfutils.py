# -*- coding: utf-8 -*-

# Copyright 2005, 2006 Harri Pitkänen (hatapitk@cc.jyu.fi)
# Utility functions for Hunspell-fi tools

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

import codecs

# Text encodings
INPUT_ENCODING = 'UTF-8'
OUTPUT_ENCODING = 'ISO-8859-15'

# Gradation types
GRAD_NONE = 0
GRAD_SW = 1
GRAD_WS = 2

# Vowel types
VOWEL_DEFAULT=0
VOWEL_FRONT=1
VOWEL_BACK=2
VOWEL_BOTH=3

# Returns a connection to a vocabulary database (or None, if the database is not available).
# Connection parameters are read from file .pg_connect_string in the current working directory.
# The contents of the file must be in format host:database:username:password .
def get_db():
	try:
		inputfile = codecs.open('.pg_connect_string');
		pg_connect_string = inputfile.readline().strip()
		inputfile.close()
	except IOError:
		return None
	parts = pg_connect_string.split(':')
	import _pg
	return _pg.connect(parts[1], parts[0], 5432, None, None, parts[2], parts[3])


# Returns True, if given character is a consonant, otherwise retuns False.
def consonant(letter):
	if letter.lower() in u'qwrtpsdfghjklzxcvbnm':
		return True
	else:
		return False


# Function that returns the type of vowels that are allowed in the affixes for given word.
# The possible values are VOWEL_FRONT, VOWEL_BACK and VOWEL_BOTH.
def vowel_type(word):
	word = word.lower()
	last_back = max(word.rfind(u'a'), word.rfind(u'o'), word.rfind(u'å'), word.rfind(u'u'))
	last_ord_front = max(word.rfind(u'ä'), word.rfind(u'ö'))
	last_y = word.rfind(u'y')
	if last_back > -1 and max(last_ord_front, last_y) == -1:
		return VOWEL_BACK
	if last_back == -1 and max(last_ord_front, last_y) > -1:
		return VOWEL_FRONT
	if max(last_back, last_ord_front, last_y) == -1:
		return VOWEL_FRONT
	if last_y < max(last_back, last_ord_front):
		if last_back > last_ord_front: return VOWEL_BACK
		else: return VOWEL_FRONT
	else:
		return VOWEL_BOTH


# Remove comments from a given line of text.
def remove_comments(line):
	comment_start = line.find(u'#')
	if comment_start == -1:
		return line
	if comment_start == 0:
		return u''
	return line[:comment_start]


# Applies given gradation type to given word. Returns a tuple in the form
# (strong, weak) or None if this is not possible. Conditional aposthrope
# is represented by $.
def apply_gradation(word, grad_type):
	if grad_type == u'-': return (word, word)
	if consonant(word[-1]) and not consonant(word[-2]):
		if word[-4:-2] == u'ng':
			return (word[:-4]+u'nk'+word[-2:], word)
		# uvu/yvy->uku/yky not possible?
		if word[-4:-2] == u'mm':
			return (word[:-4]+u'mp'+word[-2:], word)
		if word[-4:-2] == u'nn':
			return (word[:-4]+u'nt'+word[-2:], word)
		if word[-4:-2] == u'll':
			return (word[:-4]+u'lt'+word[-2:], word)
		if word[-4:-2] == u'rr':
			return (word[:-4]+u'rt'+word[-2:], word)
		if word[-3] == u'd':
			return (word[:-3]+u't'+word[-2:], word)
		if word[-3] in u'tkp':
			return (word[:-2]+word[-3:], word)
		if word[-3] == u'v':
			return (word[:-3]+u'p'+word[-2:], word)
	
	# This can be used to carry suffixes. Not currently needed.
	last_letter = u''
	
	if grad_type == u'av1':
		if word[-3:-1] in (u'tt',u'kk',u'pp'):
			return (word+last_letter, word[:-2]+word[-1]+last_letter)
		if word[-3:-1] == u'mp':
			return (word+last_letter, word[:-3]+u'mm'+word[-1]+last_letter)
		if word[-2] == u'p' and not consonant(word[-1]):
			return (word+last_letter, word[:-2]+u'v'+word[-1]+last_letter)
		if word[-3:-1] == u'nt':
			return (word+last_letter, word[:-3]+u'nn'+word[-1]+last_letter)
		if word[-3:-1] == u'lt':
			return (word+last_letter, word[:-3]+u'll'+word[-1]+last_letter)
		if word[-3:-1] == u'rt':
			return (word+last_letter, word[:-3]+u'rr'+word[-1]+last_letter)
		if word[-2] == u't':
			return (word+last_letter, word[:-2]+u'd'+word[-1]+last_letter)
		if word[-3:-1] == u'nk':
			return (word+last_letter, word[:-3]+u'ng'+word[-1]+last_letter)
		if word[-3:] == u'uku':
			return (word+last_letter, word[:-3]+u'uvu'+last_letter)
		if word[-3:] == u'yky':
			return (word+last_letter, word[:-3]+u'yvy'+last_letter)
	if grad_type == u'av2':
		if word[-3:-1] == u'ng':
			return (word[:-3]+u'nk'+word[-1]+last_letter, word+last_letter)
		# uvu/yvy->uku/yky not possible?
		if word[-3:-1] == u'mm':
			return (word[:-3]+u'mp'+word[-1]+last_letter, word+last_letter)
		if word[-3:-1] == u'nn':
			return (word[:-3]+u'nt'+word[-1]+last_letter, word+last_letter)
		if word[-3:-1] == u'll':
			return (word[:-3]+u'lt'+word[-1]+last_letter, word+last_letter)
		if word[-3:-1] == u'rr':
			return (word[:-3]+u'rt'+word[-1]+last_letter, word+last_letter)
		if word[-2] == u'd':
			return (word[:-2]+u't'+word[-1]+last_letter, word+last_letter)
		if word[-2] in u'tkpbg':
			return (word[:-1]+word[-2:]+last_letter, word+last_letter)
		if word[-2] == u'v':
			return (word[:-2]+u'p'+word[-1]+last_letter, word+last_letter)
	if grad_type == u'av3': # k -> j
		if word[-2] == u'k':
			return (word+last_letter, word[:-2]+u'j'+word[-1]+last_letter)
	if grad_type == u'av4': # j -> k
		if word[-2] == u'j':
			return (word[:-2]+u'k'+word[-1]+last_letter, word+last_letter)
		if word[-3] == u'j':
			return (word[:-3]+u'k'+word[-2]+word[-1]+last_letter, word+last_letter)
	if grad_type == u'av5': # k -> -
		if word[-2] == u'k':
			return (word+last_letter, word[:-2]+u'$'+word[-1]+last_letter)
	if grad_type == u'av6': # - -> k
		if consonant(word[-1]): # FIXME: hack
			return (word[:-2]+u'k'+word[-2:]+last_letter, word+last_letter)
		else:
			return (word[:-1]+u'k'+word[-1]+last_letter, word+last_letter)
	return None


# Read an option "name" from string "options". If it does not exist, then default will be returned.
def read_option(options, name, default):
	parts = options.split(u',');
	for part in parts:
		nameval = part.split(u'=')
		if len(nameval) == 2 and nameval[0] == name: return nameval[1]
		if len(nameval) == 1 and nameval[0] == name: return u'1'
	return default


# Expands capital letters to useful character classes for regular expressions
def hf_regexp(pattern):
	pattern = pattern.replace('V', u'(?:a|e|i|o|u|y|ä|ö|é|è|á|ó|â)')
	pattern = pattern.replace('C', u'(?:b|c|d|f|g|h|j|k|l|m|n|p|q|r|s|t|v|w|x|z|š|ž)')
	pattern = pattern.replace('A', u'(?:a|ä)')
	pattern = pattern.replace('O', u'(?:o|ö)')
	pattern = pattern.replace('U', u'(?:u|y)')
	return pattern
