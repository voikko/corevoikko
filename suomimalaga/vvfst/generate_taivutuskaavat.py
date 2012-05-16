# -*- coding: utf-8 -*-

# Copyright 2012 Harri Pitkänen (hatapitk@iki.fi)
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
import codecs
from string import rfind


def stripWhitespaceAndComments(line):
	if u"!" in line:
		line = line[0:line.find(u"!")]
	return line.strip()

def addDiacritics(line):
	if u"[Ny]" in line:
		middle = line.find(u":")
		return u"@D.EI_YKS@" + line[0:middle+1] + u"@D.EI_YKS@" + line[middle+1:]
	return line

def replacementsFront(line):
	return addDiacritics(line.replace(u"<A>", u"ä").replace(u"<U>", u"y"))


def replacementsBack(line):
	return addDiacritics(line.replace(u"<A>", u"a").replace(u"<U>", u"u"))


def appendLines(lexiconName, lines, lexcFile):
	lexcFile.write(u"LEXICON " + lexiconName + u"_a\n")
	for line in lines:
		lexcFile.write(replacementsBack(line) + u"\n")
	lexcFile.write(u"LEXICON " + lexiconName + u"_ä\n")
	for line in lines:
		lexcFile.write(replacementsFront(line) + u"\n")
	lexcFile.write(u"LEXICON " + lexiconName + u"_aä\n")
	for line in lines:
		lexcFile.write(replacementsBack(line) + u"\n")
		if u"<A>" in line:
			lexcFile.write(replacementsFront(line) + u"\n")


# Get command line options
OPTIONS = generate_lex_common.get_options()

lexcFile = codecs.open(OPTIONS["destdir"] + u"/" + "taivutuskaavat.lexc", 'w', 'UTF-8')

infile = codecs.open(u"vvfst/taivutuskaavat.lexc.in", "r", "UTF-8")

lexicon = u""
lexcLines = []
linecount = 0
while True:
	line_orig = infile.readline()
	linecount = linecount + 1
	if line_orig == u'':
		break
	line = stripWhitespaceAndComments(line_orig)
	if line.startswith(u'LEXICON '):
		if lexicon != u"":
			appendLines(u"Nom" + lexicon, lexcLines, lexcFile)
		lexicon = line[8:]
		lexcLines = []
		continue
	lexcLines.append(line)

appendLines(u"Nom" + lexicon, lexcLines, lexcFile)

lexcFile.close()
