# Copyright 2012 - 2015 Harri Pitkänen (hatapitk@iki.fi)
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


def addDiacritics(line):
	if "[Nm]" in line or ("[Sn][Ny]" in line and "Omistusliite" in line):
		middle = line.find(":")
		return "@C.EI_YKS@" + line[0:middle+1] + "@C.EI_YKS@" + line[middle+1:]
	return line

def replacementsFront(line):
	return addDiacritics(line.replace("<A>", "ä").replace("<O>", "ö").replace("<U>", "y"))


def replacementsBack(line):
	return addDiacritics(line.replace("<A>", "a").replace("<O>", "o").replace("<U>", "u"))

def filterLines(lines, lexiconPrefix):
	for line in lines:
		if line.startswith("?Laatusana"):
			if lexiconPrefix in ["Laatusana", "NimiLaatusana"]:
				yield line[10:].replace("<WC>", lexiconPrefix)
		elif line.startswith("?NimiLaatusana"):
			if lexiconPrefix == "NimiLaatusana":
				yield line[14:].replace("<WC>", lexiconPrefix)
		elif line.startswith("?NimisanaOnly"):
			if lexiconPrefix == "Nimisana":
				yield line[13:].replace("<WC>", lexiconPrefix)
		elif line.startswith("?Nimisana"):
			if lexiconPrefix in ["Nimisana", "NimiLaatusana"]:
				yield line[9:].replace("<WC>", lexiconPrefix)
		elif line.startswith("?NotLaatusana"):
			if lexiconPrefix not in ["Laatusana", "Asemosana"]:
				yield line[13:].replace("<WC>", lexiconPrefix)
		elif line.startswith("?Paikannimi"):
			if lexiconPrefix == "Paikannimi":
				yield line[11:].replace("<WC>", lexiconPrefix)
		elif line.startswith("?Sukunimi"):
			if lexiconPrefix == "Sukunimi":
				yield line[9:].replace("<WC>", lexiconPrefix)
		elif line.startswith("?Etunimi"):
			if lexiconPrefix == "Etunimi":
				yield line[8:].replace("<WC>", lexiconPrefix)
		elif line.startswith("?Nimi"):
			if lexiconPrefix == "Nimi":
				yield line[5:].replace("<WC>", lexiconPrefix)
		elif line.startswith("?Erisnimi"):
			if lexiconPrefix in ["Sukunimi", "Etunimi", "Paikannimi", "Nimi"]:
				yield line[9:].replace("<WC>", lexiconPrefix)
		elif line.startswith("?Yleisnimi"):
			if lexiconPrefix in ["Nimisana", "Laatusana", "NimiLaatusana"]:
				yield line[10:].replace("<WC>", lexiconPrefix)
		elif line.startswith("?ErisYleisOnly"):
			if lexiconPrefix in ["Nimisana", "Sukunimi", "Etunimi", "Paikannimi", "Nimi"]:
				yield line[14:].replace("<WC>", lexiconPrefix)
		elif line.startswith("?Asemosana"):
			if lexiconPrefix == "Asemosana":
				yield line[10:].replace("<WC>", lexiconPrefix)
		elif line.startswith("?NotAsemosana"):
			if lexiconPrefix != "Asemosana":
				yield line[13:].replace("<WC>", lexiconPrefix)
		else:
			yield line.replace("<WC>", lexiconPrefix)

def appendLines(lexiconPrefix, lexiconName, lines, lexcFile):
	lexcFile.write("LEXICON " + lexiconPrefix + lexiconName + "_a\n")
	for line in filterLines(lines, lexiconPrefix):
		lexcFile.write(replacementsBack(line) + "\n")
	lexcFile.write("LEXICON " + lexiconPrefix + lexiconName + "_ä\n")
	for line in filterLines(lines, lexiconPrefix):
		lexcFile.write(replacementsFront(line) + "\n")
	lexcFile.write("LEXICON " + lexiconPrefix + lexiconName + "_aä\n")
	for line in filterLines(lines, lexiconPrefix):
		lexcFile.write(replacementsBack(line) + "\n")
		if "<A>" in line:
			lexcFile.write(replacementsFront(line) + "\n")

def appendLexicon(lexiconName, lines, lexcFile):
	if lexiconName.startswith("NOUN "):
		realName = lexiconName[5:]
		appendLines("Nimisana", realName, lines, lexcFile)
		appendLines("Laatusana", realName, lines, lexcFile)
		appendLines("Etunimi", realName, lines, lexcFile)
		appendLines("Sukunimi", realName, lines, lexcFile)
		appendLines("Paikannimi", realName, lines, lexcFile)
		appendLines("Nimi", realName, lines, lexcFile)
		appendLines("NimiLaatusana", realName, lines, lexcFile)
		if realName in ["Vieras", "Vieras_s", "Vieras_w", "YhteisetMonikonPaikallissijat", \
		                "YhteisetYksikönPaikallissijat", "LiOlN", "YhteisetMonikonSijat2", \
		                "LiOlI", "YksikönGenetiivinJatko", "LiOlV", "LiOlAA", "MonikonGenetiiviEnJatko", \
		                "NormaaliYsJatko", "Nainen", "NainenYhteiset", "NainenYsJatko", "Autio", \
		                "NormaaliYsJatkoOl", "Luku", "Valo_w", "Valo_s", "Valo_sl", \
		                "Koira", "Koira_w", "Koira_s", "MonikonGenetiiviInJatko", \
		                "Koira_w_monikko", "Koira_w_yksikkö", "Puu", "Maa_l", "Maa_s", \
		                "OlVLoppu", "MonikonNominatiivinJatko", "YksikönGenetiivinJatkoEiYs"]:
			appendLines("LukusananJälkiliite", realName, lines, lexcFile)
			appendLines("Asemosana", realName, lines, lexcFile)
		if (OPTIONS["sukija"] or OPTIONS["vanhat"]) and realName in ["YhteisetHTaivutusmuodot"]:
			appendLines("LukusananJälkiliite", realName, lines, lexcFile)
			appendLines("Asemosana", realName, lines, lexcFile)
	else:
		appendLines("", lexiconName, lines, lexcFile)

# Get command line options
OPTIONS = generate_lex_common.get_options()

lexcFile = codecs.open(OPTIONS["destdir"] + "/" + "taivutuskaavat.lexc", 'w', 'UTF-8')

infile = codecs.open("vvfst/taivutuskaavat.lexc.in", "r", "UTF-8")

lexicon = ""
lexcLines = []
linecount = 0
while True:
	line_orig = infile.readline()
	linecount = linecount + 1
	if line_orig == '':
		break
	line = generate_lex_common.filterVfstInput(line_orig, OPTIONS)
	if line is None:
		continue
	if line.startswith('LEXICON '):
		if lexicon != "":
			appendLexicon(lexicon, lexcLines, lexcFile)
		lexicon = line[8:]
		lexcLines = []
		continue
	lexcLines.append(line)
infile.close()
	
appendLexicon(lexicon, lexcLines, lexcFile)

# Generate lexicons for numerals

MULTI = {
	"SnNy": ["kymmenen", "sata", "tuhat", "miljoona", "miljardi", "biljoona", "triljoona", "kvadriljoona", "kvintiljoona", "sekstiljoona", "septiljoona", "sentiljoona"],
	"SgNy": ["kymmenen", "sadan", "tuhannen", "miljoonan", "miljardin", "biljoonan", "triljoonan", "kvadriljoonan", "kvintiljoonan", "sekstiljoonan", "septiljoonan", "sentiljoonan"],
	"SpNy": ["kymmentä", "sataa", "tuhatta", "miljoonaa", "miljardia", "biljoonaa", "triljoonaa", "kvadriljoonaa", "kvintiljoonaa", "sekstiljoonaa", "septiljoonaa", "sentiljoonaa"],
	"StrNy": ["kymmeneksi", "sadaksi", "tuhanneksi", "miljoonaksi", "miljardiksi", "biljoonaksi", "triljoonaksi", "kvadriljoonaksi", "kvintiljoonaksi", "sekstiljoonaksi", "septiljoonaksi", "sentiljoonaksi"],
	"SesNy": ["kymmenenä", "satana", "tuhantena", "miljoonana", "miljardina", "biljoonana", "triljoonana", "kvadriljoonana", "kvintiljoonana", "sekstiljoonana", "septiljoonana", "sentiljoonana"],
	"SineNy": ["kymmenessä", "sadassa", "tuhannessa", "miljoonassa", "miljardissa", "biljoonassa", "triljoonassa", "kvadriljoonassa", "kvintiljoonassa", "sekstiljoonassa", "septiljoonassa", "sentiljoonassa"],
	"SelaNy": ["kymmenestä", "sadasta", "tuhannesta", "miljoonasta", "miljardista", "biljoonasta", "triljoonasta", "kvadriljoonasta", "kvintiljoonasta", "sekstiljoonasta", "septiljoonasta", "sentiljoonasta"],
	"SillNy": ["kymmeneen", "sataan", "tuhanteen", "miljoonaan", "miljardiin", "biljoonaan", "triljoonaan", "kvadriljoonaan", "kvintiljoonaan", "sekstiljoonaan", "septiljoonaan", "sentiljoonaan"],
	"SadeNy": ["kymmenellä", "sadalla", "tuhannella", "miljoonalla", "miljardilla", "biljoonalla", "triljoonalla", "kvadriljoonalla", "kvintiljoonalla", "sekstiljoonalla", "septiljoonalla", "sentiljoonalla"],
	"SablNy": ["kymmeneltä", "sadalta", "tuhannelta", "miljoonalta", "miljardilta", "biljoonalta", "triljoonalta", "kvadriljoonalta", "kvintiljoonalta", "sekstiljoonalta", "septiljoonalta", "sentiljoonalta"],
	"SallNy": ["kymmenelle", "sadalle", "tuhannelle", "miljoonalle", "miljardille", "biljoonalle", "triljoonalle", "kvadriljoonalle", "kvintiljoonalle", "sekstiljoonalle", "septiljoonalle", "sentiljoonalle"],
	"SabNy": ["kymmenettä", "sadatta", "tuhannetta", "miljoonatta", "miljarditta", "biljoonatta", "triljoonatta", "kvadriljoonatta", "kvintiljoonatta", "sekstiljoonatta", "septiljoonatta", "sentiljoonatta"],
	"SgNm": ["kymmenien", "satojen", "tuhansien", "miljoonien", "miljardien", "biljoonien", "triljoonien", "kvadriljoonien", "kvintiljoonien", "sekstiljoonien", "septiljoonien", "sentiljoonien"],
	"SpNm": ["kymmeniä", "satoja", "tuhansia", "miljoonia", "miljardeja", "biljoonia", "triljoonia", "kvadriljoonia", "kvintiljoonia", "sekstiljoonia", "septiljoonia", "sentiljoonia"],
	"StrNm": ["kymmeniksi", "sadoiksi", "tuhansiksi", "miljooniksi", "miljardeiksi", "biljooniksi", "triljooniksi", "kvadriljooniksi", "kvintiljooniksi", "sekstiljooniksi", "septiljooniksi", "sentiljooniksi"],
	"SesNm": ["kymmeninä", "satoina", "tuhansina", "miljoonina", "miljardeina", "biljoonina", "triljoonina", "kvadriljoonina", "kvintiljoonina", "sekstiljoonina", "septiljoonina", "sentiljoonina"],
	"SineNm": ["kymmenissä", "sadoissa", "tuhansissa", "miljoonissa", "miljardeissa", "biljoonissa", "triljoonissa", "kvadriljoonissa", "kvintiljoonissa", "sekstiljoonissa", "septiljoonissa", "sentiljoonissa"],
	"SelaNm": ["kymmenistä", "sadoista", "tuhansista", "miljoonista", "miljardeista", "biljoonista", "triljoonista", "kvadriljoonista", "kvintiljoonista", "sekstiljoonista", "septiljoonista", "sentiljoonista"],
	"SillNm": ["kymmeniin", "satoihin", "tuhansiin", "miljooniin", "miljardeihin", "biljooniin", "triljooniin", "kvadriljooniin", "kvintiljooniin", "sekstiljooniin", "septiljooniin", "sentiljooniin"],
	"SadeNm": ["kymmenillä", "sadoilla", "tuhansilla", "miljoonilla", "miljardeilla", "biljoonilla", "triljoonilla", "kvadriljoonilla", "kvintiljoonilla", "sekstiljoonilla", "septiljoonilla", "sentiljoonilla"],
	"SablNm": ["kymmeniltä", "sadoilta", "tuhansilta", "miljoonilta", "miljardeilta", "biljoonilta", "triljoonilta", "kvadriljoonilta", "kvintiljoonilta", "sekstiljoonilta", "septiljoonilta", "sentiljoonilta"],
	"SallNm": ["kymmenille", "sadoille", "tuhansille", "miljoonille", "miljardeille", "biljoonille", "triljoonille", "kvadriljoonille", "kvintiljoonille", "sekstiljoonille", "septiljoonille", "sentiljoonille"],
	"SabNm": ["kymmenittä", "sadoitta", "tuhansitta", "miljoonitta", "miljardeitta", "biljoonitta", "triljoonitta", "kvadriljoonitta", "kvintiljoonitta", "sekstiljoonitta", "septiljoonitta", "sentiljoonitta"],
	"SinNm": ["kymmenin", "sadoin", "tuhansin", "miljoonin", "miljardein", "biljoonin", "triljoonin", "kvadriljoonin", "kvintiljoonin", "sekstiljoonin", "septiljoonin", "sentiljoonin"],
	"SkoNm": ["kymmenine", "satoine", "tuhansine", "miljoonine", "miljardeine", "biljoonine", "triljoonine", "kvadriljoonine", "kvintiljoonine", "sekstiljoonine", "septiljoonine", "sentiljoonine"],
	"SstiNy": ["kymmenesti", "sadasti", "tuhannesti", "miljoonasti", "miljardisti", "biljoonasti", "triljoonasti", "kvadriljoonasti", "kvintiljoonasti", "sekstiljoonasti", "septiljoonasti", "sentiljoonasti"]
}

MULTI_VOWELS = ["_ä", "_a", "_a", "_a", "_a", "_a", "_a", "_a", "_a", "_a", "_a", "_a"]

for sija in MULTI.keys():
	if sija == "SnNy":
		continue
	diacritic = "@U.LS." + sija.upper() + "@"
	lexiconName = "Lukusana" + sija + "29"
	tagName = "[" + sija.replace("N", "][N") + "]"
	
	lexcFile.write("LEXICON " + lexiconName + "Kertoimet\n")
	for i in range(len(MULTI_VOWELS)):
		lexcFile.write(diacritic + "[Xp]" + MULTI["SpNy"][i] + "[X]" + tagName + MULTI[sija][i] + ":" + diacritic + MULTI[sija][i] + "\t" + lexiconName + MULTI_VOWELS[i] + "\t;\n")
		if MULTI[sija][i].endswith("joonien"):
			altSija = MULTI[sija][i].replace("joonien", "joonain")
			lexcFile.write(diacritic + tagName + altSija + ":" + diacritic + altSija + "\t" + lexiconName + MULTI_VOWELS[i] + "\t;\n")
	
	lexcFile.write("LEXICON " + lexiconName + "Alut\n")
	for i in range(len(MULTI_VOWELS)):
		lexcFile.write(diacritic + "[Xp]" + MULTI["SnNy"][i] + "[X]" + tagName + MULTI[sija][i] + ":" + diacritic + MULTI[sija][i] + "\t" + lexiconName + MULTI_VOWELS[i] + "\t;\n")
		if MULTI[sija][i].endswith("joonien"):
			altSija = MULTI[sija][i].replace("joonien", "joonain")
			lexcFile.write(diacritic + tagName + altSija + ":" + diacritic + altSija + "\t" + lexiconName + MULTI_VOWELS[i] + "\t;\n")
	
	numeralLines = []
	numeralLines.append(diacritic + ":" + diacritic + "\tLukusanaLiitesana_<A>\t;")
	numeralLines.append(diacritic + ":" + diacritic + "\tLukusanaToista\t;")
	if OPTIONS["sukija"]:
		numeralLines.append(diacritic + ":" + diacritic + "\tSukijaLukusanaKolmattaYhdeksättä\t;")
	if sija in ["SelaNy", "SelaNm"]:
		numeralLines.append(diacritic + ":" + diacritic + "\tOlV_<A>\t;")
	appendLines("Lukusana", sija + "29", numeralLines, lexcFile)

	numeralLines = []
	numeralLines.append(diacritic + ":" + diacritic + "\tLiitesana_<A>\t;")
	numeralLines.append(diacritic + ":" + diacritic + "\tLukusanaToista\t;")
	if OPTIONS["sukija"]:
		numeralLines.append(diacritic + ":" + diacritic + "\tSukijaLukusanaKolmattaYhdeksättä\t;")
	numeralLines.append(diacritic + ":" + diacritic + "\tLukusananJälkiliite\t;")
	if sija == "SelaNm":
		numeralLines.append(diacritic + ":" + diacritic + "\tOlV_<A>\t;")
	if sija == "StrNm":
		numeralLines.append(diacritic + ":" + diacritic + "\tOlI_<A>\t;")
	appendLines("Lukusana", sija + "1", numeralLines, lexcFile)

MULTI_ORDINAL_BASES = ["kymmenes", "sadas", "tuhannes", "miljoonas", "miljardis"]

MULTI_ORDINALS = {
	"SgNy": ["kymmenennen", "sadannen", "tuhannennen", "miljoonannen", "miljardinnen"],
	"SpNy": ["kymmenettä", "sadannetta", "tuhannetta", "miljoonannetta", "miljardinnetta"],
	"StrNy": ["kymmenenneksi", "sadanneksi", "tuhannenneksi", "miljoonanneksi", "miljardinneksi"],
	"SesNy": ["kymmenentenä", "sadantena", "tuhannentena", "miljoonantena", "miljardintena"],
	"SineNy": ["kymmenennessä", "sadannessa", "tuhannennessa", "miljoonannessa", "miljardinnessa"],
	"SelaNy": ["kymmenennestä", "sadannesta", "tuhannennesta", "miljoonannesta", "miljardinnesta"],
	"SillNy": ["kymmenenteen", "sadanteen", "tuhannenteen", "miljoonanteen", "miljardinteen"],
	"SadeNy": ["kymmenennellä", "sadannella", "tuhannennella", "miljoonannella", "miljardinnella"],
	"SablNy": ["kymmenenneltä", "sadannelta", "tuhannennelta", "miljoonannelta", "miljardinnelta"],
	"SallNy": ["kymmenennelle", "sadannelle", "tuhannennelle", "miljoonannelle", "miljardinnelle"],
	"SabNy": ["kymmenennettä", "sadannetta", "tuhannennetta", "miljoonannetta", "miljardinnetta"],
	"SgNm": ["kymmenensien", "sadansien", "tuhannensien", "miljoonansien", "miljardinsien"],
	"SpNm": ["kymmenensiä", "sadansia", "tuhannensia", "miljoonansia", "miljardinsia"],
	"StrNm": ["kymmenensiksi", "sadansiksi", "tuhannensiksi", "miljoonansiksi", "miljardinsiksi"],
	"SesNm": ["kymmenensinä", "sadansina", "tuhannensina", "miljoonansina", "miljardinsina"],
	"SineNm": ["kymmenensissä", "sadansissa", "tuhannensissa", "miljoonansissa", "miljardinsissa"],
	"SelaNm": ["kymmenensistä", "sadansista", "tuhannensista", "miljoonansista", "miljardinsista"],
	"SillNm": ["kymmenensiin", "sadansiin", "tuhannensiin", "miljoonansiin", "miljardinsiin"],
	"SadeNm": ["kymmenensillä", "sadansilla", "tuhannensilla", "miljoonansilla", "miljardinsilla"],
	"SablNm": ["kymmenensiltä", "sadansilta", "tuhannensilta", "miljoonansilta", "miljardinsilta"],
	"SallNm": ["kymmenensille", "sadansille", "tuhannensille", "miljoonansille", "miljardinsille"],
	"SabNm": ["kymmenensittä", "sadansitta", "tuhannensitta", "miljoonansitta", "miljardinsitta"],
	"SinNm": ["kymmenensin", "sadansin", "tuhannensin", "miljoonansin", "miljardinsin"],
	"SkoNm": ["kymmenensine", "sadansine", "tuhannensine", "miljoonansine", "miljardinsine"]
}

for sija in MULTI_ORDINALS.keys():
	diacritic = "@U.LS." + sija.upper() + "@"
	lexiconName = "Järjestysluku" + sija + "39"
	tagName = "[" + sija.replace("N", "][N") + "]"
	
	lexcFile.write("LEXICON " + lexiconName + "Kertoimet\n")
	lexcFile.write("[Bc]" + diacritic + "[Xp]" + MULTI_ORDINAL_BASES[0] + "[X]" + tagName + MULTI_ORDINALS[sija][0] + ":" + diacritic + MULTI_ORDINALS[sija][0] + "\t" + lexiconName + "_ä\t;\n")
	lexcFile.write("[Bc]" + diacritic + "[Xp]" + MULTI_ORDINAL_BASES[1] + "[X]" + tagName + MULTI_ORDINALS[sija][1] + ":" + diacritic + MULTI_ORDINALS[sija][1] + "\t" + lexiconName + "_a\t;\n")
	lexcFile.write("[Bc]" + diacritic + "[Xp]" + MULTI_ORDINAL_BASES[2] + "[X]" + tagName + MULTI_ORDINALS[sija][2] + ":" + diacritic + MULTI_ORDINALS[sija][2] + "\t" + lexiconName + "_a\t;\n")
	lexcFile.write("[Bc]" + diacritic + "[Xp]" + MULTI_ORDINAL_BASES[3] + "[X]" + tagName + MULTI_ORDINALS[sija][3] + ":" + diacritic + MULTI_ORDINALS[sija][3] + "\t" + lexiconName + "_a\t;\n")
	lexcFile.write("[Bc]" + diacritic + "[Xp]" + MULTI_ORDINAL_BASES[4] + "[X]" + tagName + MULTI_ORDINALS[sija][4] + ":" + diacritic + MULTI_ORDINALS[sija][4] + "\t" + lexiconName + "_a\t;\n")
	
	numeralLines = []
	if sija in ["SgNy", "SgNm"]:
		numeralLines.append(diacritic + ":" + diacritic + "\tJärjestyslukuLiitesanaJl_<A>\t;")
	else:
		numeralLines.append(diacritic + ":" + diacritic + "\tJärjestyslukuLiitesana_<A>\t;")
	numeralLines.append(diacritic + ":" + diacritic + "\tJärjestyslukuToista\t;")
	if OPTIONS["sukija"]:
		numeralLines.append(diacritic + ":" + diacritic + "\tSukijaJärjestyslukuKolmattaYhdeksättä\t;")
	numeralLines.append(lexiconName + "Kertoimet\t;")
	appendLines("Järjestysluku", sija + "39", numeralLines, lexcFile)
	
	numeralLines = []
	numeralLines.append(diacritic + ":" + diacritic + "\tLiitesana_<A>\t;")
	numeralLines.append(diacritic + ":" + diacritic + "\tJärjestyslukuToista\t;")
	if OPTIONS["sukija"]:
		numeralLines.append(diacritic + ":" + diacritic + "\tSukijaJärjestyslukuKolmattaYhdeksättä\t;")
	numeralLines.append(diacritic + ":" + diacritic + "\tLukusananJälkiliiteJl\t;")
	appendLines("Järjestysluku", sija + "1", numeralLines, lexcFile)


lexcFile.close()
