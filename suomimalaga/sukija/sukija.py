# -*- coding: utf-8 -*-

# Copyright 2007-2008 Hannu Väisänen (Etunimi.Sukunimi@joensuu.fi
# Program to generate lexicon files for Suomi-malaga Sukija edition.

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


# This code is heavily based on code written by Harri Pitkänen.

import codecs
import generate_lex_common
import hfconv
import voikkoutils
import re
import sys

# Historical inflections in alphabetical order.
historical = [(u'aavistaa', u'sw', [(u'tt',u'(.*O)ittAA',u'kirjoittaa'),
				    (u'tt',u'(.*O)ttAA',u'ammottaa'),
				    (None,u'(.*t)AA',u'aavistaa'),
				    (u'tt',u'(.*[AeiU]t)tAA',u'alittaa'),
				    (u't',u'(.*h)tAA',u'astahtaa')]),
	      (u'arvailla',   u'-',  [(None,u'(.*[AOU]])illA',u'arvailla')]),
	      (u'hohtaa',     u'sw', [(u'tt',u'(.*t)tAA',u'heittää')]),
	      (u'iäkäs',      u'ws', [(u'k',u'(.*k)As',u'iäkäs')]),
	      (u'katsella',   u'ws', [(None,u'(.*[AOU])illA',u'arvailla')]),
	      (u'kaivaa',     u'sw', [(None,u'(.*aj)AA',u'ajaa')]),
	      (u'kirjoittaa', u'sw', [(u'tt',u'(.*O)ittAA',u'kirjoittaa'),
				      (u'tt',u'(.*O)ttAA',u'ammottaa'),
				      (u'tt',u'(.*[AeiU]t)tAA',u'asettaa')]),
	      (u'kuollut', u'-', [(None,u'(.*neits)yt',u'neitsyt'),
				  (None,u'(.*C)lUt',u'kuollut'),
				  (None,u'(.*)nUt', u'punonut'),
				  (None,u'(.*C)rUt',u'purrut'),
				  (None,u'(.*C)sUt',u'juossut')]),
	      (u'onneton', u'ws', [(None,u'(.*)tOn',u'alaston'),
				   (u't',u'(.*)tOn',u'onneton')]),
	      (u'punoa',   u'sw', [(u't',u'(...*AU)tUA',u'antautua')]),
##	      (u'vieras',  u'ws', [(u'k',u'(.*k)As',u'iäkäs'),
##				   (None,u'(.*[lr]iA)s',u'utelias'),
##				   (u'>k',u'(.*)As',u'vilkas')]),
	(u'ahven',    u'ws', [(None,u'(.*CVC)',u'ahven')]),
        (u'antautua', u'sw', [(u't',u'(.*)tUA',u'antautua')]),
	(u'altis',    u'ws', [(None, u'(.*t)is', u'altis')]),
        (u'banaali',  u'sw', [(None,u'(.*)i',u'banaali'),
                              (u'nt',u'(.*n)ti',u'hollanti'),
			      (u'nk',u'(.*n)ki',u'killinki'),
			      (u'kk',u'(.*k)ki',u'kajakki'),
			      (u'tt',u'(.*t)ti',u'salaatti'),
			      (u'pp',u'(.*p)pi',u'sinappi'),
			      (u't',u'(.*)ti',u'konvehti') ]),
        (u'bébé', u'-', [(None,u'(.*V)',u'bébé')]),
        (u'haastaa', u'sw', [(None,u'(.*C)AA',u'haastaa')]),
        (u'kaihtaa', u'sw', [(u't',u'(.*)tAA',u'kaihtaa')]),
        (u'kantaja', u'-', [(None,u'(.*)jA',u'johdin_jA_kantaja')]),
        (u'kirjoitella', u'ws', [(None,u'(...*O)itellA',u'kilvoitella'),
                                 (None,u'(.*O)tellA',u'ilotella')]),
	(u'tulla', u'ws', [(None,u'(.*Vl)lA',u'tulla')]),
        (u'koiras', u'ws', [(None,u'(.*A)s',u'koiras')]),
	(u'kutiaa', u'-',  [(None,u'(.*Cia)a',u'kutiaa')]),
        (u'laittaa', u'sw', [(u'tt',u'(.*t)tAA',u'laittaa')]),
	(u'neiti', u'sw', [(u't',u'(.*)ti',u'neiti')]),
        (u'nuori', u'-', [(None,u'(.*C)i',u'nuori')]),
        (u'paahtaa', u'sw', [(u't',u'(.*)tAA',u'paahtaa')]),
        (u'paistaa', u'sw', [(None,u'(.*C)AA',u'paistaa')]),
        (u'palata', u'ws', [(None,u'(.*)AtA',u'palata')]),
        (u'palaa', u'ws', [(None,u'(.*C)AA',u'palaa')]),
        (u'pasuuna', u'sw', [(None,u'(.*)A',u'pasuuna')]),
        (u'siivota', u'ws', [(None,u'(.*O)tA',u'siivota')]),
	(u'sydän', u'-', [(None,u'(.*A)n',u'sydän')]),
        (u'taittaa', u'sw', [(u'tt',u'(.*t)tAA',u'taittaa')]),
        (u'tuomi', u'-', [(None,u'(.*V)mi',u'tuomi')]),
	(u'uros', u'-', [(None,u'(.*)s',u'uros')]),
	(u'valmis',u'ws', [(None,u'(.*)is',u'valmis')]),
	(u'veranta', u'sw', [(u'nt',u'(.*n)tA',u'veranta')]),
	(u'vihanta', u'sw', [(u'nt',u'(.*n)tA',u'vihanta')]),
        (u'virkkaa', u'sw', [(u'kk',u'(.*k)kAA',u'jakaa')])
        ]

classmap = historical
classmap.extend(hfconv.modern_classmap)

pattern = u"^(?P<alku>.*)(?:" + \
          u"(?P<ammottaa>OttAA)|" + \
          u"(?P<kirjoittaa>OittAA)|" + \
          u"(?P<jaotus>OtUs)|" + \
          u"(?P<aivoitus>OitUs)|" + \
          u"(?P<nuolaista>AistA)|" + \
          u"(?P<utelias_ankerias>[lr]iAs)|" + \
          u"(?P<obligaatio>Caatio)|" + \
          u"(?P<resoluutio>Cuutio)|" + \
          u"(?P<illuusio>Cuusio)|" + \
          u"(?P<traditio>Citio)|" + \
	  u"(?P<funktio>.ktio)|" + \
	  u"(?P<symboli_ym>[^aeouyäö]o[dfglmnrv]i)|" + \
	  u"(?P<balladi>..adi)|" + \
	  u"(?P<logia_ym>gogia|logia|sofia)|" + \
	  u"(?P<loginen_ym>goginen|loginen|sofinen)|" + \
	  u"(?P<grafia>grafia)|" + \
	  u"(?P<grafinen>.grafinen)|" + \
	  u"(?P<ironinen_ym>Co[nr]inen)|" + \
	  u"(?P<psyykkinen_ym>(?:aa|ee|ii|oo|uu|yy|ää|öö)(?:kk|pp|tt)inen)|" + \
	  u"(?P<relatiivinen>tiivinen)|" + \
	  u"(?P<aarteisto>eistO)|" + \
	  u"(?P<keltainen>C[aouyäö]i?nen)|" + \
	  u"(?P<antautua>CAUtUA)|" + \
	  u"(?P<maineikas>[mntv]eikAs)" + \
          u")$"

pattern = pattern.replace(u"A", u"[aä]")
pattern = pattern.replace(u"O", u"[oö]")
pattern = pattern.replace(u"U", u"[uy]")
pattern = pattern.replace(u"C", u"[bcdfghjklmnpqrstvwxzšžçðñþß]")
rx = re.compile(pattern, re.IGNORECASE)

begin = u"(amerikan|jälleen|tiibetin|uudelleen).+"
rx_begin = re.compile(begin, re.IGNORECASE)

end = u".+(herkkä|pöllö|rämeä|valmis)"
rx_end = re.compile(end, re.IGNORECASE)


#print pattern


# Sanat, jotka tunnistetaan Sukija-versiossa automaagisesti toisten
# sanojen johdoksina. Tällaiset sanat pitäisi merkitä Joukahaisen
# sanastoon lipulla ei kuulu indeksointisanastoon.
#
# Niiden lisäksi Sukijassa ei tarvita erisnimiä, jotka ovat myös
# yleisnimiä. Kuitenkin mukaan pitää ottaa sellaiset sanat, jotka
# taipuvat eri tavalla yleis- ja erisniminä. Esim. Lempi, Lempin;
# lempi, lemmen.
#
# Sanaluettelon saa näin:
# grep '<form>' ../*/*xml | sed -e "s@</\?form>@@g" | sort
#
words = []
inputfile = codecs.open ('sukija/ei-sukija.txt', 'r', 'UTF-8')
while True:
	word = inputfile.readline()
	if (len(word) == 0):
		break
	word = word[:-1]      # Poistetaan \n sanan lopusta.
#	print (word)
	words.append (word)


# Aksentilliset kirjaimet UTF-8 -merkistössä 0000-017F,
# ei kuitenkaan merkkejä š ja ž.
#
# C0 Controls and Basic Latin.        Range: 0000-007F
# C1 Controls and Latin-1 Supplement  Range: 0080-00FF
# Latin Extended-A                    Range: 0100-017F
#
# C0 on sama kuin ASCII, C0+C1 on sama kuin ISO-8859-1.
#
# Kirjaimet å, ä ja ö eivät ole aksentillisia kirjaimia suomen kielessä.
#
accents = u"ÀÁÂÃÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕØÙÚÛÜÝÞßàáâãæçèéêëìíîïðñòóôõøùúûüýþÿ" + \
          u"ĀāĂăĄąĆćĈĉĊċČčĎďĐđĒēĔĕĖėĘęĚěĜĝĞğĠġĢģĤĥĦħĨĩĪīĬĭĮįİıĲĳĴĵĶķĸ" + \
	  u"ĹĺĻļĽľĿŀŁłŃńŅņŇňŉŊŋŌōŎŏŐőŔŕŖŗŘřŚśŜŝŞşŢţŤťŦŧŨũŪūŬŭŮůŰűŲųŴŵŶŷŸŹźŻżſ"

replace = u"AAAAÆCEEEEIIIIDNOOOÖÖUUUUYÞßaaaaæceeeeiiiidnoooööuuuuyþy" + \
          u"AaAaAaCcCcCcCcDdDdEeEeEeEeEeGgGgGgGgHhHhIiIiIiIiIiĲĳJjKkk" + \
	  u"LlLlLlLlLlNnNnNnnNnOoOoÖöŒœRrRrSsSsSsTtTtTtUuUuUuUuYyUuWwYyYZzZzs"

rx_accents = re.compile (u"[" + accents + u"]")


# Jaetaan sana tavuihin. Esim.
# hyphenate(u"valkoinen") = val-koi-nen.
#
#
# Algoritmi: Facta-tietosanakirja (1970), osa 9, palsta 50.
#
# "(1) kaksi peräkkäistä vokaalikirjainta kuuluvat samaan tavuun
# jos ja vain jos ne ääntyvät pitkänä vokaalina tai diftongina.
#
# (2) jos konsonanttia seuraa vokaali, ne kuuluvat samaan tavuun,
# muutoin konsonantti kuuluu edellisen kirjaimen tavuun (kuitenkin
# vierasperäisen sanan kaikki alkukonsonantit kuuluvat samaan tavuun)."

# Kahden ääntiön yhdistelmät, jotka voivat olla tavussa.
A0 = [u"ei", u"ai", u"äi", u"ui", u"yi", u"oi", u"öi"]
A1 = [u"au", u"äy", u"ou", u"öy", u"iu", u"iy", u"eu", u"ey", u"uo", u"yö", u"ie"]
A1.extend(A0)
A2 = [u"aa", u"ee", u"ii", u"oo", u"uu", u"yy", u"ää", u"öö"]

V2 = A1
V2.extend(A2)

V = u"AÀÁÂÃEÈÉÊËŒÆIÌÍÎÏOÒÓÔUÙÚÛYÝÿÜÅÄÖØÕaàáâãeèéêëœæiìíîïoòóôuùúûyýÿüåäöøõ"
C = u"BCDFGHJKLMNŃPQRSTVWXZŠŽÇÐÑÞßbcdfghjklmnńpqrstvwxzšžçðñþß"


# Palautetaan True, jos sanassa on ainakin yksi ääntiö.
#
def has_vowel(s):
	for i in s:
		if (i in V):
			return 1
	return 0


def hyphenate(word):
	n = len(word)
	i = 0
	s = u""

	if (n <= 2):
		return word

#	print(word + u"\n");
	
	# Jos sanassa on joku merkeistä -':.
	# tavutetaan sanan molemmat puoliskot erikseen.
	#
	p = re.compile(u"[-':.]+").search(word)
	if (p != None):
		return hyphenate(word[:p.start(0)]) + u"-" + hyphenate(word[p.end(0):])

	# Kerakkeet sanan alussa.
	#
	while ((i < n) and (word[i] in C) and (word[i+1] in C)):
		s = s + word[i]
		i = i + 1

	if (i == n):
		return word  # Lyhenne, jossa on vain kerakkeita.

	while (i < n-1):
###		print (s + u" " + str(i) + u" " + str(n) + u"\n")
		if ((word[i] in V) and (word[i+1] in V)):
###			print (u"Foo 1 " + str(i) + u" " + word[i] + word[i+1] + u"\n")
			if ((i + 2 < n) and (word[i+1:i+3] in V2)):
				# Re-aa-li.
				s = s + word[i] + u"-" + word[i+1:i+3]
				i = i + 3
			elif (word[i:i+2] in V2):
				s = s + word[i:i+2]
				i = i + 2
			else:
				s = s + word[i] + u"-" + word[i+1]
				i = i + 2
		elif ((word[i] in C) and (word[i+1] in V)):
			if ((len(s) > 0) and (s[-1] != u"-") and (has_vowel(s))):
				s = s + u"-"
			s = s + word[i]
			i = i + 1
		elif ((word[i] in C) and (word[i+1] in C)):
			if (i+2 == n):  # Sanan lopussa on kaksi keraketta.
				s = s + word[i:i+2]
				i = i + 2
			elif (word[i+2] in V):
				s = s + word[i] + u"-"
				i = i + 1
			else:
				s = s + word[i]
				i = i + 1
		elif (word[i] in V):
			if ((i > 2) and (word[i-2:i] in V2)):
				s = s + u"-" + word[i]
			else:
				s = s + word[i]
			i = i + 1
		if (i+1 == n):
			if ((s[-1] in V) and (word[i] == V)):
				if (s[-1] + word[i] in V2):
					s = s + word[i]
				else:
					s = u"-" + word[i]
			else:
				s = s + word[i]
			i = i + 1
	
	return s


# Palautetaan sanassa olevien tavujen määrä.
#
def number_of_syllabels(word):
	n = 1
	s = hyphenate(word)
	for c in s:
		if (c == u"-"):
			n = n + 1
	return n;


# Korvataan sanasta 'word' aksenttimerkit
# aksentittomilla kohtien 'start' ja 'end' välistä.
#
def deaccent(word, start, end):
	s = u""
	for i in range(start, end):
		j = accents.find(word[i])
		if (j >= 0):
			s = s + replace[j]
		else:
			s = s + word[i]
	s = s + word[end:]
	return s


# Kirjoitetaan sana Malagan tietokantaan korvaamalla aksenttimerkit aksentittomilla (esim. á == a),
# mutta ei korvata kirjaimia š ja ž s:llä ja z:lla.
#
def write_word_without_accents(main_vocabulary, vocabulary_files, word, entry, wordform):
	if ((rx_accents.search(wordform) != None) and (wordform != u"šakki")):
		n = entry.find(u" luokka: ")
		if (n == -1):
			print("write_word_without_accents: Virhe Malaga-koodissa: " + entry + u"\n")
		entry2 = deaccent (entry, 0, n)
#		print (entry  + entry2 + u"\n")
		generate_lex_common.write_entry(main_vocabulary, vocabulary_files, word, entry2)


word_end = re.compile(u".+geeni(nen)?$")

# Hyväksytään esim. karsinogeenia ja karsinogeeniä.
#
def new_vtype (malaga_vtype, wordform):
	if (word_end.match(wordform)):
		return u"aä"
	else:
	       	return malaga_vtype
	

def handle_word(main_vocabulary,vocabulary_files,word):
	if generate_lex_common.has_flag(word, "not_sukija"): return

	# Get the inflection class. Exactly one inflection class is needed.
	infclasses = word.getElementsByTagName("infclass")
	voikko_infclass = None
	for infclass in word.getElementsByTagName("infclass"):
		if infclass.getAttribute("type") == "historical":
			voikko_infclass = generate_lex_common.tValue(infclass)
			break
	if (voikko_infclass in [u"antautua", u"kaihtaa", u"laittaa", u"paahtaa",
				u"taittaa", u"veranta", u"vihanta", u"virkkaa"]):
		voikko_infclass = voikko_infclass + u"-av1"
	
	if voikko_infclass == None:
		for infclass in word.getElementsByTagName("infclass"):
			if infclass.getAttribute("type") != "historical":
				voikko_infclass = generate_lex_common.tValue(infclass)
				break
	
	if voikko_infclass == None: return
	if voikko_infclass == u"poikkeava": return
	
	# Get the word classes
	wordclasses = generate_lex_common.tValues(word.getElementsByTagName("classes")[0], "wclass")
	malaga_word_class = generate_lex_common.get_malaga_word_class(wordclasses)
	if malaga_word_class == None: return
	
	# Get malaga flags
	malaga_flags = generate_lex_common.get_malaga_flags(word)
	
	# Get forced vowel type
	forced_inflection_vtype = generate_lex_common.vowel_type(word.getElementsByTagName("inflection")[0])
	
	# Process all alternative forms
	for altform in generate_lex_common.tValues(word.getElementsByTagName("forms")[0], "form"):
		wordform = altform.replace(u'|', u'').replace(u'=', u'')
		if (voikko_infclass == u"nuolaista-av2") and (wordform in [u"häväistä", u"vavista"]):
			voikko_infclass = u"nuolaista"
#		print (u"Hoo " + str(voikko_infclass) + u" " + u" " + wordform + u"\n")
#		print(u"Tavutus1 " + wordform + u" " + hyphenate(wordform.lower()) + u"\n")
		(alku, jatko) = generate_lex_common.get_malaga_inflection_class(wordform, voikko_infclass, wordclasses, classmap)
#		print (u"Huu " + wordform + u" " + str(alku) + u" " + str(jatko) + u" "  + str(voikko_infclass) + u"\n")
		if forced_inflection_vtype == voikkoutils.VOWEL_DEFAULT:
			vtype = voikkoutils.get_wordform_infl_vowel_type(altform)
		else: vtype = forced_inflection_vtype
		if vtype == voikkoutils.VOWEL_FRONT: malaga_vtype = u'ä'
		elif vtype == voikkoutils.VOWEL_BACK: malaga_vtype = u'a'
		elif vtype == voikkoutils.VOWEL_BOTH: malaga_vtype = u'aä'
		malaga_vtype = new_vtype (malaga_vtype, wordform)
		rakenne = generate_lex_common.get_structure(altform, malaga_word_class)
		if alku == None:
			generate_lex_common.write_entry(main_vocabulary, vocabulary_files, word, \
							u"#Malaga class not found for (%s, %s)\n" \
			                   % (wordform, voikko_infclass))
			continue
		if (wordform in words):
#			print ("Ei tarvita: " + wordform)
			continue
		if (rx_begin.match(wordform) != None):
#			print ("Ei tarvita: " + wordform)
			continue
		if (rx_end.match(wordform) != None):
#			print ("Ei tarvita: " + wordform)
			continue
		if ((wordform == u"ori") and (jatko == u"risti")):
#			print ("Ei tarvita: " + wordform)
			continue
		
		nsyl = number_of_syllabels(wordform)

		m = rx.match(wordform)
		d = None

		if (m != None):
			d = m.groupdict()
		
		alku2 = u""
		jatko2 = u""
		wordform2 = u""

		alku3 = u""
		jatko3 = u""
		wordform3 = u""

		alku4 = u""
		jatko4 = u""
		wordform4 = u""

		alku5 = u""
		jatko5 = u""
		wordform5 = u""

		alku6 = u""
		jatko6 = u""
		wordform6 = u""

		s = u"lähtösana: \"" + wordform + u"\", lähtöalku: \"" + alku + u"\""

		# Useimmat kolmitavuiset (i)nen-loppuiset sanat hyväksytään i:llisinä ja i:ttöminä.
		# Esim. kelta(i)nen, hevo(i)nen.
		#
		if ((jatko == u"nainen") and (nsyl == 3) and
		    (malaga_word_class in [u"nimisana", u"nimi_laatusana", u"laatusana"]) and
		    (rx != None) and (d != None) and (d['keltainen'] != None) and
		    (not (wordform in [u"armainen", u"lukunen", u"omainen", u"osanen", u"vastuinen"]))):
			if (wordform[-4] == u"i"):
				alku2 = alku[:-1]    # Keltainen => keltanen.
			else:
				alku2 = alku + u"i"  # Hevonen => hevoinen.
			jatko2 = jatko
			wordform2 = alku2 + u"nen"
##			if (wordform[-4] == u"i"):
##				alku = wordform[:-4]    # Keltainen => keltanen.
##				jatko = u"punainen"
##			else:
##				jatko = u"hevoinen"
##			print (u"Keltainen " + wordform + u" " + alku + u" " + jatko)
		elif (jatko in [u"asiakas", u"avokas"]):
#		if (jatko in [u"asiakas", u"avokas"]):
			jatko = u"iäkäs"
		elif (jatko == u"varas"):
			jatko = u"vilkas"
		elif ((jatko == u"vieras") and (d != None) and (d['utelias_ankerias'] != None)):
			jatko = u"utelias"
		#
		# Korjataan alku- ja jatko-kenttien arvoja.
		#
		elif (jatko == u"rakentaa"):
			alku = wordform[:-4]
		elif ((nsyl > 2) and (jatko == u"vastaus") and
		      (rx != None) and (d != None) and ((d['aivoitus'] != None) or (d['jaotus'] != None))):
			#
			# Aivoitus => aivotus. Jaotus => jaoitus.
			#
			if (d['aivoitus'] != None):
				alku2 = wordform[:-4] + wordform[-3:-1]           # Aivoitus => aivotus.
			else:
				alku2 = wordform[:-3] + u"i" + wordform[-3:-1]    # Jaotus => jaoitus.
			jatko2 = jatko
			wordform2 = alku2 + wordform[-1:]
#			print(u"J " + wordform2 + u" " + alku2 + u" " + s + u"\n")
		elif ((nsyl > 3) and (jatko == u"autio") and
		      (rx != None) and (d != None) and ((d['obligaatio'] != None) or (d['resoluutio'] != None))):
			#
			# Obligaatio => obligatsioni, obligatsiooni, obligatio, obligationi, obligatiooni.
			# Revoluutio => revolutsioni, revolutsiooni, revolutio, revolutioni, revolutiooni.
			# Tässä -ni ei ole omistusliite.
			#
			alku2 = wordform[:-4] + u"tsion"      # Obligatsioni, revolutsioni.
			jatko2 = u"paperi"
			wordform2 = alku2 + u"i"
				
			alku3 = wordform[:-4] + u"tsioon"     # Obligatsiooni, revolutsiooni.
			jatko3 = u"paperi"
			wordform3 = alku3 + u"i"
			
			alku4 = wordform[:-4] + wordform[-3:] # Obligatio, revolutio.
			jatko4 = jatko
			wordform4 = alku4
			
			alku5 = alku4 + u"n"                  # Obligationi, revolutioni.
			jatko5 = u"paperi"
			wordform5 = alku5 + u"i"
			
			alku6 = alku4 + u"on"                 # Obligatiooni, revolutiooni.
			jatko6 = u"paperi"
			wordform6 = alku6 + u"i"
       		elif ((nsyl > 3) and (jatko == u"autio") and
		      (rx != None) and (d != None) and (d['illuusio'] != None)):
			#
			# Illuusio => illusioni, illusiooni, illusio.
			#
			alku2 = wordform[:-4] + u"sion"     # Illusioni.
			jatko2 = u"paperi"
			wordform2 = alku2 + u"i"

			alku3 = wordform[:-4] + u"sioon"    # Illusiooni.
			jatko3 = u"paperi"
			wordform3 = alku3 + u"i"

			alku4 = wordform[:-4] + u"sio"      # Illusio.
			jatko4 = jatko
			wordform4 = alku4
		elif ((nsyl > 2) and (jatko == u"autio") and
		      (rx != None) and (d != None) and ((d['traditio'] != None) or (d['funktio'] != None))):
			#
			# Traditio => traditsioni, traditsiooni, traditsio.
			#
			alku2 = wordform[:-2] + u"sion"    # Traditsioni, funktsioni.
			jatko2 = u"paperi"
			wordform2 = alku2 + u"i"

			alku3 = wordform[:-2] + u"sioon"   # Traditsiooni, funktsiooni.
			jatko3 = u"paperi"
			wordform3 = alku2 + u"i"
			
			alku4 = wordform[:-2] + u"sio"      # Traditsio, funktsio.
			jatko4 = jatko
			wordform4 = alku4
		elif ((nsyl > 2) and (rx != None) and (d != None) and (d['symboli_ym'] != None) and
		      (malaga_word_class in [u"nimisana", u"nimi_laatusana", u"laatusana"])):
			#
			# Symboli => symbooli, atoomi, uniooni, tenoori, alkoovi, aploodi,
			# pedagoogi, psykoloogi, filosoofi, katastroofi, mutta ei esim. koni => kooni.
			#
			alku2 = wordform[:-2] + u"o" + wordform[-2]
			jatko2 = jatko
			wordform2 = alku2 + u"i"
		elif ((nsyl > 2) and (rx != None) and (d != None) and (d['balladi'] != None)):
			#
			# Balladi => ballaadi.
			#
			alku2 = wordform[:-2] + u"a" + wordform[-2]
			jatko2 = jatko
			wordform2 = alku2 + u"i"
		elif ((nsyl > 2) and (rx != None) and (d != None) and (d['logia_ym'] != None) and
		      (malaga_word_class in [u"nimisana", u"nimi_laatusana"])):
			#
			# Pedagogia => pedagoogia, psykolo(o)gia, filoso(o)fia.
			#
			alku2 = wordform[:-3] + u"o" + wordform[-3:-1]
			jatko2 = jatko
			wordform2 = alku2 + u"a"
		elif ((nsyl > 2) and (rx != None) and (d != None) and (d['loginen_ym'] != None) and
		      (malaga_word_class in [u"nimisana", u"nimi_laatusana"])):
			#
			# Pedagoginen => pedagooginen, psykolo(o)ginen, filoso(o)finen.
			#
			alku2 = wordform[:-5] + u"o" + wordform[-5:-3]
			jatko2 = jatko
			wordform2 = alku2 + u"nen"
		elif ((nsyl > 2) and (rx != None) and (d != None) and (d['grafia'] != None)):
			#
			# Topografia => topograafia.
			#
			alku2 = wordform[:-3] + u"a" + wordform[-3:-1]
			jatko2 = jatko
			wordform2 = alku2 + u"a"
		elif ((nsyl > 2) and (rx != None) and (d != None) and (d['grafinen'] != None)):
			#
			# Topografinen => topograafinen.
			#
			alku2 = wordform[:-5] + u"a" + wordform[-5:-3]
			jatko2 = jatko
			wordform2 = alku2 + u"nen"
		elif ((rx != None) and (d != None) and (d['ironinen_ym'] != None) and
		      (malaga_word_class in [u"nimisana", u"nimi_laatusana", u"laatusana"])):
			#
			# Ironinen => irooninen, allegorinen => allegoorinen.
			#
			alku2 = wordform[:-5] + u"o" + wordform[-5:-3]
			jatko2 = jatko
			wordform2 = alku2 + u"nen"
#			print (u"Ironinen " + wordform2 + u" " + alku2 + u" " + jatko2)
		elif ((rx != None) and (d != None) and (d['psyykkinen_ym'] != None) and
		      (malaga_word_class in [u"nimisana", u"nimi_laatusana", u"laatusana"]) and
		      (wordform != "eettinen")):  # "Eetillinen" on Joukahaisessa.
			#
			# Psyykkinen => psyykillinen, eeppinen => eepillinen, kriittinen => kriitillinen.
			#
			alku2 = wordform[:-5] + u"illi"
			jatko2 = jatko
			wordform2 = alku2 + u"nen"
		elif ((rx != None) and (d != None) and (d['relatiivinen'] != None)):
			#
			# Relatiivinen => relativinen.
			#
			alku2 = wordform[:-6] + u"vi"
			jatko2 = jatko
			wordform2 = alku2 + u"nen"
		elif ((rx != None) and (d != None) and (d['aarteisto'] != None)):
			#
			# Aarteisto => aartehisto.
			#
			alku2 = wordform[:-4] + u"h" + wordform[-4:]
			jatko2 = jatko
			wordform2 = alku2
#			print (u"Aarteisto " + wordform2 + u" " + alku2 + u" " + jatko2)
		elif ((jatko == u"iäkäs") and (malaga_word_class in [u"nimi_laatusana", u"laatusana"])):
			#
			# Iäkäs => iäkkäisyys.
			#
			if (wordform[-2] == u"a"):
				alku2 = alku + u"kaisuu";
			else:
				alku2 = alku + u"käisyy";
			jatko2 = u"kalleus"
			wordform2 = alku2 + u"s"

			if ((nsyl == 3) and (rx != None) and (d != None) and (d['maineikas'] != None)):
				#
				# Maineikas => mainehikas.
				#
				alku3 = wordform[:-4] + u"hik"
				wordform3 = alku3 + wordform[-2:]
				jatko3 = jatko
#				print ("Huuhaa " + wordform + u" " + wordform3 + u" " + alku3 + u"\n");
		
		# Tulostetaan.

#		print(u"Word   " + wordform + u"\n")
		entry = u'[perusmuoto: "%s", alku: "%s", luokka: %s, jatko: <%s>, äs: %s%s%s];' \
			% (wordform, alku, malaga_word_class, jatko, malaga_vtype, malaga_flags,
			   generate_lex_common.get_structure(altform, malaga_word_class))
		generate_lex_common.write_entry(main_vocabulary, vocabulary_files, word, entry)
		
		write_word_without_accents(main_vocabulary, vocabulary_files, word, entry, wordform)

		
		if (len(wordform2) > 0):
			entry = u'[perusmuoto: "%s", alku: "%s", luokka: %s, jatko: <%s>, äs: %s%s%s, %s];' \
				% (wordform2, alku2, malaga_word_class, jatko2, malaga_vtype, malaga_flags,
				   generate_lex_common.get_structure(altform, malaga_word_class), s)
			generate_lex_common.write_entry(main_vocabulary, vocabulary_files, word, entry)

		if (len(wordform3) > 0):
			entry = u'[perusmuoto: "%s", alku: "%s", luokka: %s, jatko: <%s>, äs: %s%s%s, %s];' \
				% (wordform3, alku3, malaga_word_class, jatko3, malaga_vtype, malaga_flags,
				   generate_lex_common.get_structure(altform, malaga_word_class), s)
			generate_lex_common.write_entry(main_vocabulary, vocabulary_files, word, entry)

		if (len(wordform4) > 0):
			entry = u'[perusmuoto: "%s", alku: "%s", luokka: %s, jatko: <%s>, äs: %s%s%s, %s];' \
				% (wordform4, alku4, malaga_word_class, jatko4, malaga_vtype, malaga_flags,
				   generate_lex_common.get_structure(altform, malaga_word_class), s)
			generate_lex_common.write_entry(main_vocabulary, vocabulary_files, word, entry)

		if (len(wordform5) > 0):
			entry = u'[perusmuoto: "%s", alku: "%s", luokka: %s, jatko: <%s>, äs: %s%s%s, %s];' \
				% (wordform5, alku5, malaga_word_class, jatko5, malaga_vtype, malaga_flags,
				   generate_lex_common.get_structure(altform, malaga_word_class), s)
			generate_lex_common.write_entry(main_vocabulary, vocabulary_files, word, entry)

		if (len(wordform6) > 0):
			entry = u'[perusmuoto: "%s", alku: "%s", luokka: %s, jatko: <%s>, äs: %s%s%s, %s];' \
				% (wordform6, alku6, malaga_word_class, jatko6, malaga_vtype, malaga_flags,
				   generate_lex_common.get_structure(altform, malaga_word_class), s)
			generate_lex_common.write_entry(main_vocabulary, vocabulary_files, word, entry)
