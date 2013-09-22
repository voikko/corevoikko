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
	if u"[Nm]" in line:
		middle = line.find(u":")
		return u"@C.EI_YKS@" + line[0:middle+1] + u"@C.EI_YKS@" + line[middle+1:]
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
		elif line.startswith("?NimisanaOnly"):
			if lexiconPrefix == u"Nimisana":
				yield line[13:].replace(u"<VC>", lexiconPrefix)
		elif line.startswith("?NotLaatusana"):
			if lexiconPrefix != u"Laatusana":
				yield line[13:].replace(u"<VC>", lexiconPrefix)
		elif line.startswith("?Paikannimi"):
			if lexiconPrefix == u"Paikannimi":
				yield line[11:].replace(u"<VC>", lexiconPrefix)
		elif line.startswith("?Sukunimi"):
			if lexiconPrefix == u"Sukunimi":
				yield line[9:].replace(u"<VC>", lexiconPrefix)
		elif line.startswith("?Etunimi"):
			if lexiconPrefix == u"Etunimi":
				yield line[8:].replace(u"<VC>", lexiconPrefix)
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
		realName = lexiconName[5:]
		appendLines(u"Nimisana", realName, lines, lexcFile)
		appendLines(u"Laatusana", realName, lines, lexcFile)
		appendLines(u"Etunimi", realName, lines, lexcFile)
		appendLines(u"Sukunimi", realName, lines, lexcFile)
		appendLines(u"Paikannimi", realName, lines, lexcFile)
		appendLines(u"Nimi", realName, lines, lexcFile)
		appendLines(u"NimiLaatusana", realName, lines, lexcFile)
		if realName in [u"Vieras", u"Vieras_s", u"Vieras_w", u"YhteisetMonikonPaikallissijat", \
		                u"YhteisetYksikönPaikallissijat", u"LiOlN", u"YhteisetMonikonSijat2", \
		                u"LiOlI", u"YksikönGenetiivinJatko", u"LiOlV", u"MonikonGenetiiviEnJatko", \
		                u"NormaaliYsJatko", u"Nainen", u"NainenYhteiset", u"Autio", \
		                u"NormaaliYsJatkoOl", u"Luku", u"Valo_w", u"Valo_s", u"Valo_sl", \
		                u"Koira", u"Koira_w", u"Koira_s", u"MonikonGenetiiviInJatko", \
		                u"Koira_w_monikko", u"Koira_w_yksikkö", u"Puu", u"Maa_l", u"Maa_s"]:
			appendLines(u"LukusananJälkiliite", realName, lines, lexcFile)
			appendLines(u"Asemosana", realName, lines, lexcFile)
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
	if line_orig.startswith(u'?Sukija'):
		if OPTIONS["sukija"]:
			line_orig = line_orig[7:]
		else:
			continue
	line = stripWhitespaceAndComments(line_orig)
	if line.startswith(u'LEXICON '):
		if lexicon != u"":
			appendLexicon(lexicon, lexcLines, lexcFile)
		lexicon = line[8:]
		lexcLines = []
		continue
	lexcLines.append(line)
infile.close()
	
appendLexicon(lexicon, lexcLines, lexcFile)

# Generate lexicons for numerals

MULTI = {
	u"SgNy": [u"kymmenen", u"sadan", u"tuhannen", u"miljoonan"],
	u"SpNy": [u"kymmentä", u"sataa", u"tuhatta", u"miljoonaa"],
	u"StrNy": [u"kymmeneksi", u"sadaksi", u"tuhanneksi", u"miljoonaksi"],
	u"SesNy": [u"kymmenenä", u"satana", u"tuhantena", u"miljoonana"],
	u"SineNy": [u"kymmenessä", u"sadassa", u"tuhannessa", u"miljoonassa"],
	u"SelaNy": [u"kymmenestä", u"sadasta", u"tuhannesta", u"miljoonasta"],
	u"SillNy": [u"kymmeneen", u"sataan", u"tuhanteen", u"miljoonaan"],
	u"SadeNy": [u"kymmenellä", u"sadalla", u"tuhannella", u"miljoonalla"],
	u"SablNy": [u"kymmeneltä", u"sadalta", u"tuhannelta", u"miljoonalta"],
	u"SallNy": [u"kymmenelle", u"sadalle", u"tuhannelle", u"miljoonalle"],
	u"SabNy": [u"kymmenettä", u"sadatta", u"tuhannetta", u"miljoonatta"],
	u"SgNm": [u"kymmenien", u"satojen", u"tuhansien", u"miljoonien"],
	u"SpNm": [u"kymmeniä", u"satoja", u"tuhansia", u"miljoonia"],
	u"StrNm": [u"kymmeniksi", u"sadoiksi", u"tuhansiksi", u"miljooniksi"],
	u"SesNm": [u"kymmeninä", u"satoina", u"tuhansina", u"miljoonina"],
	u"SineNm": [u"kymmenissä", u"sadoissa", u"tuhansissa", u"miljoonissa"],
	u"SelaNm": [u"kymmenistä", u"sadoista", u"tuhansista", u"miljoonista"],
	u"SillNm": [u"kymmeniin", u"satoihin", u"tuhansiin", u"miljooniin"],
	u"SadeNm": [u"kymmenillä", u"sadoilla", u"tuhansilla", u"miljoonilla"],
	u"SablNm": [u"kymmeniltä", u"sadoilta", u"tuhansilta", u"miljoonilta"],
	u"SallNm": [u"kymmenille", u"sadoille", u"tuhansille", u"miljoonille"],
	u"SabNm": [u"kymmenittä", u"sadoitta", u"tuhansitta", u"miljoonitta"],
	u"SinNm": [u"kymmenin", u"sadoin", u"tuhansin", u"miljoonin"],
	u"SkoNm": [u"kymmenine", u"satoine", u"tuhansine", u"miljoonine"]
}

MULTI_VOWELS = [u"_ä", u"_a", u"_a", u"_a"]

for sija in MULTI.keys():
	diacritic = u"@U.LS." + sija.upper() + u"@"
	lexiconName = u"Lukusana" + sija + u"29"
	
	lexcFile.write(u"LEXICON " + lexiconName + u"Kertoimet\n")
	for i in range(len(MULTI_VOWELS)):
		lexcFile.write(u"[Bc]" + diacritic + MULTI[sija][i] + u":" + diacritic + MULTI[sija][i] + u"\t" + lexiconName + MULTI_VOWELS[i] + u"\t;\n")
	
	numeralLines = []
	numeralLines.append(diacritic + u":" + diacritic + u"\tLukusanaLiitesana_<A>\t;")
	numeralLines.append(diacritic + u":" + diacritic + u"\tLukusanaToista\t;")
	numeralLines.append(lexiconName + u"Kertoimet\t;")
	appendLines(u"Lukusana", sija + u"29", numeralLines, lexcFile)
	
	numeralLines = []
	numeralLines.append(diacritic + u":" + diacritic + u"\tLiitesana_<A>\t;")
	numeralLines.append(diacritic + u":" + diacritic + u"\tLukusanaToista\t;")
	numeralLines.append(diacritic + u":" + diacritic + u"\tLukusananJälkiliite\t;")
	appendLines(u"Lukusana", sija + u"1", numeralLines, lexcFile)

MULTI_ORDINALS = {
	u"SgNy": [u"kymmenennen", u"sadannen", u"tuhannennen"],
	u"SpNy": [u"kymmenettä", u"sadannetta", u"tuhannetta"],
	u"StrNy": [u"kymmenenneksi", u"sadanneksi", u"tuhannenneksi"],
	u"SesNy": [u"kymmenentenä", u"sadantena", u"tuhannentena"],
	u"SineNy": [u"kymmenennessä", u"sadannessa", u"tuhannennessa"],
	u"SelaNy": [u"kymmenennestä", u"sadannesta", u"tuhannennesta"],
	u"SillNy": [u"kymmenenteen", u"sadanteen", u"tuhannenteen"],
	u"SadeNy": [u"kymmenennellä", u"sadannella", u"tuhannennella"],
	u"SablNy": [u"kymmenenneltä", u"sadannelta", u"tuhannennelta"],
	u"SallNy": [u"kymmenennelle", u"sadannelle", u"tuhannennelle"],
	u"SabNy": [u"kymmenennettä", u"sadannetta", u"tuhannennetta"]
}

for sija in MULTI_ORDINALS.keys():
	diacritic = u"@U.LS." + sija.upper() + u"@"
	lexiconName = u"Järjestysluku" + sija + u"39"
	
	lexcFile.write(u"LEXICON " + lexiconName + u"Kertoimet\n")
	lexcFile.write(u"[Bc]" + diacritic + MULTI_ORDINALS[sija][0] + u":" + diacritic + MULTI_ORDINALS[sija][0] + u"\t" + lexiconName + u"_ä\t;\n")
	lexcFile.write(u"[Bc]" + diacritic + MULTI_ORDINALS[sija][1] + u":" + diacritic + MULTI_ORDINALS[sija][1] + u"\t" + lexiconName + u"_a\t;\n")
	lexcFile.write(u"[Bc]" + diacritic + MULTI_ORDINALS[sija][2] + u":" + diacritic + MULTI_ORDINALS[sija][2] + u"\t" + lexiconName + u"_a\t;\n")
	
	numeralLines = []
	numeralLines.append(diacritic + u":" + diacritic + u"\tJärjestyslukuLiitesana_<A>\t;")
	#numeralLines.append(diacritic + u":" + diacritic + u"\tLukusanaToista\t;")
	numeralLines.append(lexiconName + u"Kertoimet\t;")
	appendLines(u"Järjestysluku", sija + u"39", numeralLines, lexcFile)
	
	numeralLines = []
	numeralLines.append(diacritic + u":" + diacritic + u"\tLiitesana_<A>\t;")
	#numeralLines.append(diacritic + u":" + diacritic + u"\tLukusanaToista\t;")
	#numeralLines.append(diacritic + u":" + diacritic + u"\tLukusananJälkiliite\t;")
	appendLines(u"Järjestysluku", sija + u"1", numeralLines, lexcFile)


lexcFile.close()
