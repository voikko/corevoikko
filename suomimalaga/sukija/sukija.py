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
        (u'nuori', u'-', [(None,u'(.*C)i',u'nuori')]),
        (u'paahtaa', u'sw', [(u't',u'(.*)tAA',u'paahtaa')]),
        (u'paistaa', u'sw', [(None,u'(.*C)AA',u'paistaa')]),
        (u'palata', u'ws', [(None,u'(.*)AtA',u'palata')]),
        (u'palaa', u'ws', [(None,u'(.*C)AA',u'palaa')]),
        (u'pasuuna', u'sw', [(None,u'(.*)A',u'pasuuna')]),
        (u'siivota', u'ws', [(None,u'(.*O)tA',u'siivota')]),
        (u'taittaa', u'sw', [(u'tt',u'(.*t)tAA',u'taittaa')]),
        (u'tuomi', u'-', [(None,u'(.*V)mi',u'tuomi')]),
	(u'vihanta', u'sw', [(u'nt',u'(.*n)tA',u'vihanta')]),
        (u'virkkaa', u'sw', [(u'kk',u'(.*k)kAA',u'jakaa')])
        ]

classmap = hfconv.modern_classmap
classmap.extend(historical)


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
		(alku, jatko) = generate_lex_common.get_malaga_inflection_class(wordform, voikko_infclass, wordclasses, classmap)
#		sys.stdout.write (u"Huu " + str(voikko_infclass) + u" " + str(alku) + u" " + str(jatko) + u" " + wordform + u"\n")
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
		entry = u'[perusmuoto: "%s", alku: "%s", luokka: %s, jatko: <%s>, äs: %s%s%s];' \
		          % (wordform, alku, malaga_word_class, jatko, malaga_vtype, malaga_flags,
				   generate_lex_common.get_structure(altform, malaga_word_class))
		generate_lex_common.write_entry(main_vocabulary, vocabulary_files, word, entry)
