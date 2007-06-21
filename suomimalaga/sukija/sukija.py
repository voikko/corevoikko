# -*- coding: utf-8 -*-

# Copyright 2007 Hannu Väisänen (Etunimi.Sukunimi@joensuu.fi
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

import generate_lex_common
import hfconv
import voikkoutils
import re
import sys

# Historical inflections in alphabetical order.
historical = [(u'ahven', u'ws', [(None,u'(.*CVC)',u'ahven')]),
        (u'antautua', u'sw', [(u't',u'(.*)tUA',u'antautua')]),
        (u'banaali', u'sw', [(None,u'(.*)i',u'banaali'),
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
        (u'koiras', u'ws', [(None,u'(.*A)s',u'koiras')]),
        (u'laittaa', u'sw', [(u'tt',u'(.*t)tAA',u'laittaa')]),
	(u'neiti', u'sw', [(u't',u'(.*)ti',u'neiti')]),
        (u'nuori', u'-', [(None,u'(.*C)i',u'nuori')]),
        (u'paahtaa', u'sw', [(u't',u'(.*)tAA',u'paahtaa')]),
        (u'paistaa', u'sw', [(None,u'(.*C)AA',u'paistaa')]),
        (u'palata', u'ws', [(None,u'(.*)AtA',u'palata')]),
        (u'palaa', u'ws', [(None,u'(.*C)AA',u'palaa')]),
        (u'pasuuna', u'sw', [(None,u'(.*)A',u'pasuuna')]),
        (u'siivota', u'ws', [(None,u'(.*O)tA',u'siivota')]),
        (u'taittaa', u'sw', [(u'tt',u'(.*t)tAA',u'taittaa')]),
        (u'tuomi', u'-', [(None,u'(.*V)mi',u'tuomi')]),
	(u'veranta', u'sw', [(u'nt',u'(.*n)tA',u'veranta')]),
	(u'vihanta', u'sw', [(u'nt',u'(.*n)tA',u'vihanta')]),
        (u'virkkaa', u'sw', [(u'kk',u'(.*k)kAA',u'jakaa')])
        ]

classmap = hfconv.modern_classmap
classmap.extend(historical)


rx_ammottaa   = re.compile(u"(.*)([oö]t)(taa|tää)$")
rx_kirjoittaa = re.compile(u"(.*)([oö]it)(taa|tää)$")
rx_ammottaa_kirjoittaa = re.compile(u"(.*)([oö]i?t)(taa|tää)$")
rx_nuolaista_alku = re.compile(u"(.*)is$")
rx_utelias_ankerias_loppu = re.compile(u".*[lr]i[aä]s$")
rx_aivoitus_jaotus = re.compile(u"(.+)([oö]i?t)([uy]s)$")
rx_aatio_uutio = re.compile(u"(.*[bcdfghjklmnpqrstvwxzšžçðñþß])(aatio|uutio)$")

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
C = u"BCDFGHJKLMNPQRSTVWXZŠŽÇÐÑÞßbcdfghjklmnpqrstvwxzšžçðñþß"


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

#	sys.stdout.write(word + u"\n");
	
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
###		sys.stdout.write (s + u" " + str(i) + u" " + str(n) + u"\n")
		if ((word[i] in V) and (word[i+1] in V)):
###			sys.stdout.write (u"Foo 1 " + str(i) + u" " + word[i] + word[i+1] + u"\n")
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


def handle_word(main_vocabulary,vocabulary_files,word):
	# Get the inflection class. Exactly one inflection class is needed
	infclasses = word.getElementsByTagName("infclass")
	voikko_infclass = None
	for infclass in word.getElementsByTagName("infclass"):
		if infclass.getAttribute("type") == "historical":
			voikko_infclass = generate_lex_common.tValue(infclass)
			break

	if voikko_infclass == u"antautua":
		voikko_infclass = u"antautua-av1"
	elif voikko_infclass == u"kaihtaa":
		voikko_infclass = u"kaihtaa-av1"
	elif voikko_infclass == u"laittaa":
		voikko_infclass = u"laittaa-av1"
	elif voikko_infclass == u"paahtaa":
		voikko_infclass = u"paahtaa-av1"
	elif voikko_infclass == u"taittaa":
		voikko_infclass = u"taittaa-av1"
	elif voikko_infclass == u"veranta":
		voikko_infclass = u"veranta-av1"
	elif voikko_infclass == u"vihanta":
		voikko_infclass = u"vihanta-av1"
	elif voikko_infclass == u"virkkaa":
		voikko_infclass = u"virkkaa-av1"
	
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
#		sys.stdout.write (u"Hoo " + str(voikko_infclass) + u" " + u" " + wordform + u"\n")
#		sys.stdout.write(u"Tavutus1 " + wordform + u" " + hyphenate(wordform.lower()) + u"\n")
		(alku, jatko) = generate_lex_common.get_malaga_inflection_class(wordform, voikko_infclass, wordclasses, classmap)
#		sys.stdout.write (u"Huu " + wordform + u" " + str(alku) + u" " + str(jatko) + u" "  + str(voikko_infclass) + u"\n")
		if forced_inflection_vtype == voikkoutils.VOWEL_DEFAULT:
			vtype = voikkoutils.get_wordform_infl_vowel_type(altform)
		else: vtype = forced_inflection_vtype
		if vtype == voikkoutils.VOWEL_FRONT: malaga_vtype = u'ä'
		elif vtype == voikkoutils.VOWEL_BACK: malaga_vtype = u'a'
		elif vtype == voikkoutils.VOWEL_BOTH: malaga_vtype = u'aä'
		rakenne = generate_lex_common.get_structure(altform, malaga_word_class)
		if alku == None:
			generate_lex_common.write_entry(main_vocabulary, vocabulary_files, word, u"#Malaga class not found for (%s, %s)\n" \
			                   % (wordform, voikko_infclass))
			continue
#		entry = u'[perusmuoto: "%s", alku: "%s", luokka: %s, jatko: <%s>, äs: %s%s%s];' \
#		          % (wordform, alku, malaga_word_class, jatko, malaga_vtype, malaga_flags,
#				   generate_lex_common.get_structure(altform, malaga_word_class))
#		generate_lex_common.write_entry(main_vocabulary, vocabulary_files, word, entry)

		nsyl = number_of_syllabels(wordform)

		alku2 = u""
		jatko2 = u""
		wordform2 = u""

		alku3 = u""
		jatko3 = u""
		wordform3 = u""

		alku4 = u""
		jatko4 = u""
		wordform4 = u""

		s = u""
		
		# Kolmitavuiset, perusmuodossaan O(i)ttAA-loppuiset sanat
		# tunnistetaan Sukija-versiossa sekä i:n kanssa että ilman.
		#
		if ((jatko == u"alittaa") and (nsyl == 3)):
			p = rx_ammottaa_kirjoittaa.match(wordform)
			if (p != None):
				jatko = u"kirjoittaa"
				jatko2 = u"kirjoittaa"
				
				g = p.groups()
				if (g[1][1] == u"i"):
					alku2 = g[0] + g[1][0] + u"t"   # Kirjoittaa => kirjottaa.
				else:
					alku2 = g[0] + g[1][0] + u"it"  # Ammottaa => ammoittaa.
				wordform2 = alku2 + g[2]
				s = u"lähtösana: \"" + wordform + u"\", lähtöalku: \"" + alku + u"\""
#				sys.stdout.write ("Aa1 " + s + u" " + wordform2 + u" " + alku2 + u"\n")
		#
		# Muutetaan taivutus ammottaa tai kirjoittaa => alittaa tai kirjoittaa.
		#
		elif ((jatko in [u"ammottaa", u"kirjoittaa"]) and (nsyl > 2)):
			p = rx_ammottaa.match(wordform)
			q = rx_kirjoittaa.match(wordform)
			if (p != None):
				jatko = u"kirjoittaa"
				jatko2 = jatko
				
				g = p.groups()
				alku = g[0] + g[1][0] + u"t"
				alku2 = g[0] + g[1][0] + u"it"
				wordform2 = alku2 + g[2]
				s = u"lähtösana: \"" + wordform + u"\", lähtöalku: \"" + alku + u"\""
#				sys.stdout.write ("Aa3 " + s + u" " + wordform2 + u" " + alku2 + u"\n")
			elif (q != None):
				jatko = u"kirjoittaa"
				jatko2 = jatko
				
				g = q.groups()
				alku = g[0] + g[1][0] + u"it"
				alku2 = g[0] + g[1][0] + u"t"
				wordform2 = alku2 + g[2]
				s = u"lähtösana: \"" + wordform + u"\", lähtöalku: \"" + alku + u"\""
#				sys.stdout.write ("Aa4 " + s + u" " + wordform2 + u" " + alku2 + u"\n")
			elif (jatko == u"ammottaa"):
				jatko = u"kirjoittaa"
				alku = alku + u"t"
			else:
				jatko = u"alittaa"
				alku = alku + u"t"
		#
		# Kolmitavuiset nuolaista-tyyppiset sanat
		# tunnistetaan Sukija-versiossa sekä i:n kanssa että ilman.
		#
		elif ((jatko == u"nuolaista") and (nsyl == 3)):
			p = rx_nuolaista_alku.match(wordform)
			if (p != None):
				g = p.groups()
				alku2 = g[0] + u"s"
				jatko2 = jatko
				wordform2 = alku2 + wordform[-2:]
				s = u"lähtösana: \"" + wordform + u"\", lähtöalku: \"" + alku + u"\""
#				sys.stdout.write("N1 " + wordform2 + u" " + alku2 + u"\n")
			else:
				alku2 = alku + u"s"
				jatko2 = jatko
				alku = alku + u"is"
				wordform2 = alku2 + wordform[-2:]
				s = u"lähtösana: \"" + wordform + u"\", lähtöalku: \"" + alku + u"\""
#				sys.stdout.write("N2 " + wordform2 + u" " + alku2 + u"\n")
		elif ((wordform == u"ajaa") and (jatko == u"kaivaa")):
			jatko = u"ajaa"
		elif (jatko in [u"asiakas", u"avokas"]):
			jatko = u"iäkäs"
		elif (jatko == u"varas"):
			jatko = u"vilkas"
		elif ((jatko == u"vieras") and (rx_utelias_ankerias_loppu.match(wordform) != None)):
			jatko = u"utelias"
		elif ((nsyl > 2) and (jatko == u"vastaus")):
			#
			# Aivoitus => aivotus. Jaotus => jaoitus.
			#
			p = rx_aivoitus_jaotus.match(wordform)
			if (p != None):
				g = p.groups()
				if (g[1][1] == u"i"):
					alku2 = g[0] + g[1][0] + u"t" + g[2][0]    # Aivoitus => aivotus.
				else:
					alku2 = g[0] + g[1][0] + u"it" + g[2][0]   # Jaotus => jaoitus.
				jatko2 = jatko
				wordform2 = alku2 + wordform[-1:]
				s = u"lähtösana: \"" + wordform + u"\", lähtöalku: \"" + alku + u"\""
		elif ((nsyl > 3) and (jatko == u"autio")):
			#
			# Obligaatio => obligatsioni, obligatsiooni, obligatio.
			# Revoluutio => revolutsioni, revolutsiooni, revolutio.
			# Tässä -ni ei ole omistusliite.
			#
			p = rx_aatio_uutio.match(wordform)
			if (p != None):
				g = p.groups()
				alku2 = g[0] + g[1][0] + u"tsion"
				jatko2 = u"paperi"
				wordform2 = alku2 + "i"
				
				alku3 = g[0] + g[1][0] + u"tsioon"
				jatko3 = u"paperi"
				wordform3 = alku3 + "i"
				
				alku4 = g[0] + g[1][1:]
				jatko4 = jatko
				wordform4 = alku4
				
				s = u"lähtösana: \"" + wordform + u"\", lähtöalku: \"" + alku + u"\""
				


		# Tulostetaan.

#		sys.stdout.write("Wor1 " + wordform + u"\n")
		entry = u'[perusmuoto: "%s", alku: "%s", luokka: %s, jatko: <%s>, äs: %s%s%s];' \
			% (wordform, alku, malaga_word_class, jatko, malaga_vtype, malaga_flags,
			   generate_lex_common.get_structure(altform, malaga_word_class))
		generate_lex_common.write_entry(main_vocabulary, vocabulary_files, word, entry)

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
