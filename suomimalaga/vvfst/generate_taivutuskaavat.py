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
	return addDiacritics(line.replace(u"<A>", u"ä").replace(u"<O>", u"ö").replace(u"<U>", u"y"))


def replacementsBack(line):
	return addDiacritics(line.replace(u"<A>", u"a").replace(u"<O>", u"o").replace(u"<U>", u"u"))

def filterLines(lines, lexiconPrefix):
	for line in lines:
		if line.startswith("?Laatusana"):
			if lexiconPrefix in [u"Laatusana", u"NimiLaatusana"]:
				yield line[10:].replace(u"<VC>", lexiconPrefix)
		elif line.startswith("?Nimisana"):
			if lexiconPrefix in [u"Nimisana", u"NimiLaatusana"]:
				yield line[9:].replace(u"<VC>", lexiconPrefix)
		elif line.startswith("?NotLaatusana"):
			if lexiconPrefix != u"Laatusana":
				yield line[13:].replace(u"<VC>", lexiconPrefix)
		elif line.startswith("?Paikannimi"):
			if lexiconPrefix == u"Paikannimi":
				yield line[11:].replace(u"<VC>", lexiconPrefix)
		elif line.startswith("?Sukunimi"):
			if lexiconPrefix == u"Sukunimi":
				yield line[9:].replace(u"<VC>", lexiconPrefix)
		else:
			yield line.replace(u"<WC>", lexiconPrefix)

def appendLines(lexiconPrefix, lexiconName, lines, lexcFile):
	lexcFile.write(u"LEXICON " + lexiconPrefix + lexiconName + u"_a\n")
	for line in filterLines(lines, lexiconPrefix):
		lexcFile.write(replacementsBack(line) + u"\n")
	lexcFile.write(u"LEXICON " + lexiconPrefix + lexiconName + u"_ä\n")
	for line in filterLines(lines, lexiconPrefix):
		lexcFile.write(replacementsFront(line) + u"\n")
	lexcFile.write(u"LEXICON " + lexiconPrefix + lexiconName + u"_aä\n")
	for line in filterLines(lines, lexiconPrefix):
		lexcFile.write(replacementsBack(line) + u"\n")
		if u"<A>" in line:
			lexcFile.write(replacementsFront(line) + u"\n")

def appendLexicon(lexiconName, lines, lexcFile):
	if lexiconName.startswith(u"NOUN "):
		appendLines(u"Nimisana", lexiconName[5:], lines, lexcFile)
		appendLines(u"Laatusana", lexiconName[5:], lines, lexcFile)
		appendLines(u"Etunimi", lexiconName[5:], lines, lexcFile)
		appendLines(u"Sukunimi", lexiconName[5:], lines, lexcFile)
		appendLines(u"Paikannimi", lexiconName[5:], lines, lexcFile)
		appendLines(u"Nimi", lexiconName[5:], lines, lexcFile)
		appendLines(u"NimiLaatusana", lexiconName[5:], lines, lexcFile)
	else:
		appendLines(u"", lexiconName, lines, lexcFile)

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
			appendLexicon(lexicon, lexcLines, lexcFile)
		lexicon = line[8:]
		lexcLines = []
		continue
	lexcLines.append(line)

appendLexicon(lexicon, lexcLines, lexcFile)

lexcFile.close()
