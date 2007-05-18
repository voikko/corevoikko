# -*- coding: utf-8 -*-

# Copyright 2007 Harri Pitkänen (hatapitk@iki.fi)
# Program to generate lexicon files for Suomi-malaga

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

# Path to target directory
SUKIJA_LEX = u"sukija/voikonsanat"

import sys
sys.path.append("common")
sys.path.append("sukija")
import generate_lex_common
import voikkoutils
import sukija
import xml.dom.minidom
import codecs
import getopt

path = SUKIJA_LEX

flag_attributes = voikkoutils.readFlagAttributes(generate_lex_common.VOCABULARY_DATA + u"/flags.txt")

main_vocabulary = generate_lex_common.open_lex(path,"joukahainen.lex")
vocabulary_files = {}
for voc in generate_lex_common.SPECIAL_VOCABULARY:
	vocabulary_files[voc[2]] = generate_lex_common.open_lex(path,voc[2])


listfile = open(generate_lex_common.VOCABULARY_DATA + u'/joukahainen.xml', 'r')

line = ""
while line != '<wordlist xml:lang="fi">\n':
	line = listfile.readline()
	if line == '':
		sys.stderr.write("Malformed file " + generate_lex_common.VOCABULARY_DATA + \
		                 "/joukahainen.xml\n")
		sys.exit(1)

wcount = 0
while True:
	wordstr = ""
	line = listfile.readline()
	if line == "</wordlist>\n": break
	while line != '</word>\n':
		wordstr = wordstr + line
		line = listfile.readline()
	word = xml.dom.minidom.parseString(wordstr + line)
	sukija.handle_word(main_vocabulary, vocabulary_files, word)
	wcount = wcount + 1
	if wcount % 1000 == 0:
		sys.stdout.write("#")
		sys.stdout.flush()

sys.stdout.write("\n")
listfile.close()
main_vocabulary.close()
for (name, file) in vocabulary_files.iteritems():
	file.close()

