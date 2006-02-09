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
