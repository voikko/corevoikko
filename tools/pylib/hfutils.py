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
	if letter.lower() in ('q','w','r','t','p','s','d','f','g','h','j','k','l','z','x','c','v','b','n','m'):
		return True
	else:
		return False


# Function that returns the type of vowels that are allowed in the affixes for given word.
# The possible values are VOWEL_FRONT, VOWEL_BACK and VOWEL_BOTH.
VOWEL_FRONT=1
VOWEL_BACK=2
VOWEL_BOTH=3
def vowel_type(word):
	word = word.lower()
	last_back = max(word.rfind('a'), word.rfind('o'), word.rfind('u'))
	last_ord_front = max(word.rfind(u'ä'), word.rfind(u'ö'))
	last_y = word.rfind('y')
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
	comment_start = line.find('#')
	if comment_start == -1:
		return line
	if comment_start == 0:
		return ''
	return line[:comment_start]


# Applies given gradation type to given word. Returns a tuple in the form
# (strong, weak) or None if this is not possible.
def apply_gradation(word, grad_type):
	if grad_type == '-': return (word, word)
	if consonant(word[-1]) and not consonant(word[-2]):
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
		if word[-3] in ('t','k','p'):
			return (word[:-2]+word[-3:], word)
		if word[-3] == 'v':
			return (word[:-3]+'p'+word[-2:], word)
	if grad_type == 'av1':
		if word[-3:-1] in ('tt','kk','pp'):
			return (word, word[:-2]+word[-1])
		if word[-3:-1] == 'mp':
			return (word, word[:-3]+'mm'+word[-1])
		if word[-2] == 'p' and not consonant(word[-1]):
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
	if grad_type == 'av2':
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
		if word[-2] in ('t','k','p'):
			return (word[:-1]+word[-2:], word)
		if word[-2] == 'v':
			return (word[:-2]+'p'+word[-1], word)
	if grad_type == 'av3': # k -> j
		if word[-2] == 'k':
			return (word, word[:-2]+'j'+word[-1])
	if grad_type == 'av4': # j -> k
		if word[-2] == 'j':
			return (word[:-2]+'k'+word[-1], word)
	if grad_type == 'av5': # k -> -
		if word[-2] == 'k':
			if word[-3] == word[-1]: # ruoko, vaaka
				return (word, word[:-2]+'\''+word[-1])
			else:
				return (word, word[:-2]+word[-1])
	if grad_type == 'av6': # - -> k
		if consonant(word[-1]): # FIXME: hack
			return (word[:-2]+'k'+word[-2:], word)
		else:
			return (word[:-1]+'k'+word[-1], word)
	return None


# Read an option "name" from string "options". If it does not exist, then default will be returned.
def read_option(options, name, default):
	parts = options.split(',');
	for part in parts:
		nameval = part.split('=')
		if len(nameval) == 2 and nameval[0] == name: return nameval[1]
	return default