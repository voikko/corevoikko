# -*- coding: utf-8 -*-

# Copyright 2007 Harri Pitkänen (hatapitk@iki.fi)

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

# This module contains general helper functions and classes for use
# with Python and Voikko.

import codecs

# Vowel types
VOWEL_DEFAULT=0
VOWEL_FRONT=1
VOWEL_BACK=2
VOWEL_BOTH=3

class FlagAttribute:
	"Vocabulary flag attribute"
	joukahainen = 0
	xmlGroup = None
	xmlFlag = None
	malagaFlag = None
	description = None

# Remove comments from a given line of text.
def removeComments(line):
	comment_start = line.find(u'#')
	if comment_start == -1:
		return line
	if comment_start == 0:
		return u''
	return line[:comment_start]

# Returns a list of flag attributes from given file
def readFlagAttributes(filename):
	inputfile = codecs.open(filename, 'r', 'UTF-8')
	flags = []
	fileCont = True
	while fileCont:
		line = inputfile.readline()
		fileCont = line.endswith('\n')
		line = removeComments(line).strip()
		if len(line) > 0:
			f = FlagAttribute()
			endind = line.find(u' ')
			f.joukahainen = int(line[:endind])
			line = line[endind:].strip()
			endind = line.find(u'/')
			f.xmlGroup = line[:endind]
			line = line[endind + 1:]
			endind = line.find(u' ')
			f.xmlFlag = line[:endind]
			line = line[endind:].strip()
			endind = line.find(u' ')
			if line[:endind] != u'-': f.malagaFlag = line[:endind]
			line = line[endind:].strip()
			if len(line) > 0: f.description = line
			flags.append(f)
	inputfile.close()
	return flags

