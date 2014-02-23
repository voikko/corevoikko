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
	if u"[Nm]" in line or (u"[Sn][Ny]" in line and u"Omistusliite" in line):
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
				yield line[10:].replace(u"<WC>", lexiconPrefix)
		elif line.startswith("?Nimisana"):
			if lexiconPrefix in [u"Nimisana", u"NimiLaatusana"]:
				yield line[9:].replace(u"<WC>", lexiconPrefix)
		elif line.startswith("?NimisanaOnly"):
			if lexiconPrefix == u"Nimisana":
				yield line[13:].replace(u"<WC>", lexiconPrefix)
		elif line.startswith("?NotLaatusana"):
			if lexiconPrefix != u"Laatusana":
				yield line[13:].replace(u"<WC>", lexiconPrefix)
		elif line.startswith("?Paikannimi"):
			if lexiconPrefix == u"Paikannimi":
				yield line[11:].replace(u"<WC>", lexiconPrefix)
		elif line.startswith("?Sukunimi"):
			if lexiconPrefix == u"Sukunimi":
				yield line[9:].replace(u"<WC>", lexiconPrefix)
		elif line.startswith("?Etunimi"):
			if lexiconPrefix == u"Etunimi":
				yield line[8:].replace(u"<WC>", lexiconPrefix)
		elif line.startswith("?Nimi"):
			if lexiconPrefix == u"Nimi":
				yield line[5:].replace(u"<WC>", lexiconPrefix)
		elif line.startswith("?Asemosana"):
			if lexiconPrefix == u"Asemosana":
				yield line[10:].replace(u"<WC>", lexiconPrefix)
		elif line.startswith("?NotAsemosana"):
			if lexiconPrefix != u"Asemosana":
				yield line[13:].replace(u"<WC>", lexiconPrefix)
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
		                u"LiOlI", u"YksikönGenetiivinJatko", u"LiOlV", u"LiOlAA", u"MonikonGenetiiviEnJatko", \
		                u"NormaaliYsJatko", u"Nainen", u"NainenYhteiset", u"NainenYsJatko", u"Autio", \
		                u"NormaaliYsJatkoOl", u"Luku", u"Valo_w", u"Valo_s", u"Valo_sl", \
		                u"Koira", u"Koira_w", u"Koira_s", u"MonikonGenetiiviInJatko", \
		                u"Koira_w_monikko", u"Koira_w_yksikkö", u"Puu", u"Maa_l", u"Maa_s"]:
			appendLines(u"LukusananJälkiliite", realName, lines, lexcFile)
			appendLines(u"Asemosana", realName, lines, lexcFile)
		if OPTIONS["sukija"] and realName in [u"SukijaYhteisetHMuodot"]:
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
	u"SgNy": [u"kymmenen", u"sadan", u"tuhannen", u"miljoonan", u"miljardin", u"biljoonan", u"triljoonan", u"kvadriljoonan", u"kvintiljoonan", u"sekstiljoonan", u"septiljoonan", u"sentiljoonan"],
	u"SpNy": [u"kymmentä", u"sataa", u"tuhatta", u"miljoonaa", u"miljardia", u"biljoonaa", u"triljoonaa", u"kvadriljoonaa", u"kvintiljoonaa", u"sekstiljoonaa", u"septiljoonaa", u"sentiljoonaa"],
	u"StrNy": [u"kymmeneksi", u"sadaksi", u"tuhanneksi", u"miljoonaksi", u"miljardiksi", u"biljoonaksi", u"triljoonaksi", u"kvadriljoonaksi", u"kvintiljoonaksi", u"sekstiljoonaksi", u"septiljoonaksi", u"sentiljoonaksi"],
	u"SesNy": [u"kymmenenä", u"satana", u"tuhantena", u"miljoonana", u"miljardina", u"biljoonana", u"triljoonana", u"kvadriljoonana", u"kvintiljoonana", u"sekstiljoonana", u"septiljoonana", u"sentiljoonana"],
	u"SineNy": [u"kymmenessä", u"sadassa", u"tuhannessa", u"miljoonassa", u"miljardissa", u"biljoonassa", u"triljoonassa", u"kvadriljoonassa", u"kvintiljoonassa", u"sekstiljoonassa", u"septiljoonassa", u"sentiljoonassa"],
	u"SelaNy": [u"kymmenestä", u"sadasta", u"tuhannesta", u"miljoonasta", u"miljardista", u"biljoonasta", u"triljoonasta", u"kvadriljoonasta", u"kvintiljoonasta", u"sekstiljoonasta", u"septiljoonasta", u"sentiljoonasta"],
	u"SillNy": [u"kymmeneen", u"sataan", u"tuhanteen", u"miljoonaan", u"miljardiin", u"biljoonaan", u"triljoonaan", u"kvadriljoonaan", u"kvintiljoonaan", u"sekstiljoonaan", u"septiljoonaan", u"sentiljoonaan"],
	u"SadeNy": [u"kymmenellä", u"sadalla", u"tuhannella", u"miljoonalla", u"miljardilla", u"biljoonalla", u"triljoonalla", u"kvadriljoonalla", u"kvintiljoonalla", u"sekstiljoonalla", u"septiljoonalla", u"sentiljoonalla"],
	u"SablNy": [u"kymmeneltä", u"sadalta", u"tuhannelta", u"miljoonalta", u"miljardilta", u"biljoonalta", u"triljoonalta", u"kvadriljoonalta", u"kvintiljoonalta", u"sekstiljoonalta", u"septiljoonalta", u"sentiljoonalta"],
	u"SallNy": [u"kymmenelle", u"sadalle", u"tuhannelle", u"miljoonalle", u"miljardille", u"biljoonalle", u"triljoonalle", u"kvadriljoonalle", u"kvintiljoonalle", u"sekstiljoonalle", u"septiljoonalle", u"sentiljoonalle"],
	u"SabNy": [u"kymmenettä", u"sadatta", u"tuhannetta", u"miljoonatta", u"miljarditta", u"biljoonatta", u"triljoonatta", u"kvadriljoonatta", u"kvintiljoonatta", u"sekstiljoonatta", u"septiljoonatta", u"sentiljoonatta"],
	u"SgNm": [u"kymmenien", u"satojen", u"tuhansien", u"miljoonien", u"miljardien", u"biljoonien", u"triljoonien", u"kvadriljoonien", u"kvintiljoonien", u"sekstiljoonien", u"septiljoonien", u"sentiljoonien"],
	u"SpNm": [u"kymmeniä", u"satoja", u"tuhansia", u"miljoonia", u"miljardeja", u"biljoonia", u"triljoonia", u"kvadriljoonia", u"kvintiljoonia", u"sekstiljoonia", u"septiljoonia", u"sentiljoonia"],
	u"StrNm": [u"kymmeniksi", u"sadoiksi", u"tuhansiksi", u"miljooniksi", u"miljardeiksi", u"biljooniksi", u"triljooniksi", u"kvadriljooniksi", u"kvintiljooniksi", u"sekstiljooniksi", u"septiljooniksi", u"sentiljooniksi"],
	u"SesNm": [u"kymmeninä", u"satoina", u"tuhansina", u"miljoonina", u"miljardeina", u"biljoonina", u"triljoonina", u"kvadriljoonina", u"kvintiljoonina", u"sekstiljoonina", u"septiljoonina", u"sentiljoonina"],
	u"SineNm": [u"kymmenissä", u"sadoissa", u"tuhansissa", u"miljoonissa", u"miljardeissa", u"biljoonissa", u"triljoonissa", u"kvadriljoonissa", u"kvintiljoonissa", u"sekstiljoonissa", u"septiljoonissa", u"sentiljoonissa"],
	u"SelaNm": [u"kymmenistä", u"sadoista", u"tuhansista", u"miljoonista", u"miljardeista", u"biljoonista", u"triljoonista", u"kvadriljoonista", u"kvintiljoonista", u"sekstiljoonista", u"septiljoonista", u"sentiljoonista"],
	u"SillNm": [u"kymmeniin", u"satoihin", u"tuhansiin", u"miljooniin", u"miljardeihin", u"biljooniin", u"triljooniin", u"kvadriljooniin", u"kvintiljooniin", u"sekstiljooniin", u"septiljooniin", u"sentiljooniin"],
	u"SadeNm": [u"kymmenillä", u"sadoilla", u"tuhansilla", u"miljoonilla", u"miljardeilla", u"biljoonilla", u"triljoonilla", u"kvadriljoonilla", u"kvintiljoonilla", u"sekstiljoonilla", u"septiljoonilla", u"sentiljoonilla"],
	u"SablNm": [u"kymmeniltä", u"sadoilta", u"tuhansilta", u"miljoonilta", u"miljardeilta", u"biljoonilta", u"triljoonilta", u"kvadriljoonilta", u"kvintiljoonilta", u"sekstiljoonilta", u"septiljoonilta", u"sentiljoonilta"],
	u"SallNm": [u"kymmenille", u"sadoille", u"tuhansille", u"miljoonille", u"miljardeille", u"biljoonille", u"triljoonille", u"kvadriljoonille", u"kvintiljoonille", u"sekstiljoonille", u"septiljoonille", u"sentiljoonille"],
	u"SabNm": [u"kymmenittä", u"sadoitta", u"tuhansitta", u"miljoonitta", u"miljardeitta", u"biljoonitta", u"triljoonitta", u"kvadriljoonitta", u"kvintiljoonitta", u"sekstiljoonitta", u"septiljoonitta", u"sentiljoonitta"],
	u"SinNm": [u"kymmenin", u"sadoin", u"tuhansin", u"miljoonin", u"miljardein", u"biljoonin", u"triljoonin", u"kvadriljoonin", u"kvintiljoonin", u"sekstiljoonin", u"septiljoonin", u"sentiljoonin"],
	u"SkoNm": [u"kymmenine", u"satoine", u"tuhansine", u"miljoonine", u"miljardeine", u"biljoonine", u"triljoonine", u"kvadriljoonine", u"kvintiljoonine", u"sekstiljoonine", u"septiljoonine", u"sentiljoonine"],
	u"SstiNy": [u"kymmenesti", u"sadasti", u"tuhannesti", u"miljoonasti", u"miljardisti", u"biljoonasti", u"triljoonasti", u"kvadriljoonasti", u"kvintiljoonasti", u"sekstiljoonasti", u"septiljoonasti", u"sentiljoonasti"]
}

MULTI_VOWELS = [u"_ä", u"_a", u"_a", u"_a", u"_a", u"_a", u"_a", u"_a", u"_a", u"_a", u"_a", u"_a"]

for sija in MULTI.keys():
	diacritic = u"@U.LS." + sija.upper() + u"@"
	lexiconName = u"Lukusana" + sija + u"29"
	tagName = u"[" + sija.replace(u"N", u"][N") + u"]"
	
	lexcFile.write(u"LEXICON " + lexiconName + u"Kertoimet\n")
	for i in range(len(MULTI_VOWELS)):
		lexcFile.write(diacritic + tagName + MULTI[sija][i] + u":" + diacritic + MULTI[sija][i] + u"\t" + lexiconName + MULTI_VOWELS[i] + u"\t;\n")
	
	numeralLines = []
	numeralLines.append(diacritic + u":" + diacritic + u"\tLukusanaLiitesana_<A>\t;")
	numeralLines.append(diacritic + u":" + diacritic + u"\tLukusanaToista\t;")
	if OPTIONS["sukija"]:
		numeralLines.append(diacritic + u":" + diacritic + u"\tSukijaLukusanaKolmattaYhdeksättä\t;")
	if sija in [u"SelaNy", u"SelaNm"]:
		numeralLines.append(diacritic + u":" + diacritic + u"\tOlV_<A>\t;")
	appendLines(u"Lukusana", sija + u"29", numeralLines, lexcFile)

	numeralLines = []
	numeralLines.append(diacritic + u":" + diacritic + u"\tLiitesana_<A>\t;")
	numeralLines.append(diacritic + u":" + diacritic + u"\tLukusanaToista\t;")
	if OPTIONS["sukija"]:
		numeralLines.append(diacritic + u":" + diacritic + u"\tSukijaLukusanaKolmattaYhdeksättä\t;")
	numeralLines.append(diacritic + u":" + diacritic + u"\tLukusananJälkiliite\t;")
	if sija == u"SelaNm":
		numeralLines.append(diacritic + u":" + diacritic + u"\tOlV_<A>\t;")
	if sija == u"StrNm":
		numeralLines.append(diacritic + u":" + diacritic + u"\tOlI_<A>\t;")
	appendLines(u"Lukusana", sija + u"1", numeralLines, lexcFile)

MULTI_ORDINALS = {
	u"SgNy": [u"kymmenennen", u"sadannen", u"tuhannennen", u"miljoonannen"],
	u"SpNy": [u"kymmenettä", u"sadannetta", u"tuhannetta", u"miljoonannetta"],
	u"StrNy": [u"kymmenenneksi", u"sadanneksi", u"tuhannenneksi", u"miljoonanneksi"],
	u"SesNy": [u"kymmenentenä", u"sadantena", u"tuhannentena", u"miljoonantena"],
	u"SineNy": [u"kymmenennessä", u"sadannessa", u"tuhannennessa", u"miljoonannessa"],
	u"SelaNy": [u"kymmenennestä", u"sadannesta", u"tuhannennesta", u"miljoonannesta"],
	u"SillNy": [u"kymmenenteen", u"sadanteen", u"tuhannenteen", u"miljoonanteen"],
	u"SadeNy": [u"kymmenennellä", u"sadannella", u"tuhannennella", u"miljoonannella"],
	u"SablNy": [u"kymmenenneltä", u"sadannelta", u"tuhannennelta", u"miljoonannelta"],
	u"SallNy": [u"kymmenennelle", u"sadannelle", u"tuhannennelle", u"miljoonannelle"],
	u"SabNy": [u"kymmenennettä", u"sadannetta", u"tuhannennetta", u"miljoonannetta"],
	u"SgNm": [u"kymmenensien", u"sadansien", u"tuhannensien", u"miljoonansien"],
	u"SpNm": [u"kymmenensiä", u"sadansia", u"tuhannensia", u"miljoonansia"],
	u"StrNm": [u"kymmenensiksi", u"sadansiksi", u"tuhannensiksi", u"miljoonansiksi"],
	u"SesNm": [u"kymmenensinä", u"sadansina", u"tuhannensina", u"miljoonansina"],
	u"SineNm": [u"kymmenensissä", u"sadansissa", u"tuhannensissa", u"miljoonansissa"],
	u"SelaNm": [u"kymmenensistä", u"sadansista", u"tuhannensista", u"miljoonansista"],
	u"SillNm": [u"kymmenensiin", u"sadansiin", u"tuhannensiin", u"miljoonansiin"],
	u"SadeNm": [u"kymmenensillä", u"sadansilla", u"tuhannensilla", u"miljoonansilla"],
	u"SablNm": [u"kymmenensiltä", u"sadansilta", u"tuhannensilta", u"miljoonansilta"],
	u"SallNm": [u"kymmenensille", u"sadansille", u"tuhannensille", u"miljoonansille"],
	u"SabNm": [u"kymmenensittä", u"sadansitta", u"tuhannensitta", u"miljoonansitta"],
	u"SinNm": [u"kymmenensin", u"sadansin", u"tuhannensin", u"miljoonansin"],
	u"SkoNm": [u"kymmenensine", u"sadansine", u"tuhannensine", u"miljoonansine"]
}

for sija in MULTI_ORDINALS.keys():
	diacritic = u"@U.LS." + sija.upper() + u"@"
	lexiconName = u"Järjestysluku" + sija + u"39"
	
	lexcFile.write(u"LEXICON " + lexiconName + u"Kertoimet\n")
	lexcFile.write(u"[Bc]" + diacritic + MULTI_ORDINALS[sija][0] + u":" + diacritic + MULTI_ORDINALS[sija][0] + u"\t" + lexiconName + u"_ä\t;\n")
	lexcFile.write(u"[Bc]" + diacritic + MULTI_ORDINALS[sija][1] + u":" + diacritic + MULTI_ORDINALS[sija][1] + u"\t" + lexiconName + u"_a\t;\n")
	lexcFile.write(u"[Bc]" + diacritic + MULTI_ORDINALS[sija][2] + u":" + diacritic + MULTI_ORDINALS[sija][2] + u"\t" + lexiconName + u"_a\t;\n")
	lexcFile.write(u"[Bc]" + diacritic + MULTI_ORDINALS[sija][3] + u":" + diacritic + MULTI_ORDINALS[sija][3] + u"\t" + lexiconName + u"_a\t;\n")
	
	numeralLines = []
	numeralLines.append(diacritic + u":" + diacritic + u"\tJärjestyslukuLiitesana_<A>\t;")
	numeralLines.append(diacritic + u":" + diacritic + u"\tJärjestyslukuToista\t;")
	if OPTIONS["sukija"]:
		numeralLines.append(diacritic + u":" + diacritic + u"\tSukijaJärjestyslukuKolmattaYhdeksättä\t;")
	numeralLines.append(lexiconName + u"Kertoimet\t;")
	appendLines(u"Järjestysluku", sija + u"39", numeralLines, lexcFile)
	
	numeralLines = []
	numeralLines.append(diacritic + u":" + diacritic + u"\tLiitesana_<A>\t;")
	numeralLines.append(diacritic + u":" + diacritic + u"\tJärjestyslukuToista\t;")
	if OPTIONS["sukija"]:
		numeralLines.append(diacritic + u":" + diacritic + u"\tSukijaJärjestyslukuKolmattaYhdeksättä\t;")
	numeralLines.append(diacritic + u":" + diacritic + u"\tLukusananJälkiliiteJl\t;")
	appendLines(u"Järjestysluku", sija + u"1", numeralLines, lexcFile)


lexcFile.close()
