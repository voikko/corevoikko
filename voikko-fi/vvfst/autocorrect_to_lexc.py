# Copyright 2009 - 2015 Harri Pitkänen (hatapitk@iki.fi)

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

# This program converts an XML representation of autocorrect data
# into lexc format for autocorrect transducer.
#
# Usage: python triecompiler.py input.xml output.lexc

import xml.dom.minidom
import sys

# Open the XML file
xmlFile = open(sys.argv[1], "r", encoding="UTF-8")
autoCorrect = xml.dom.minidom.parseString(xmlFile.read())
xmlFile.close()

# Open the lexc file
outputFile = open(sys.argv[2], "w", encoding="UTF-8")
outputFile.write("Multichar_Symbols\n@_SPACE_@\n\nLEXICON Root\n")

def formatForLexc(s):
	return s.replace("=", "").replace(" ", "@_SPACE_@")

# Read entries to lexc
for replacement in autoCorrect.getElementsByTagName("replacement"):
	incorrect = formatForLexc(replacement.getElementsByTagName("incorrect")[0].firstChild.wholeText)
	correct = formatForLexc(replacement.getElementsByTagName("correct")[0].firstChild.wholeText)
	outputFile.write(incorrect + ":" + correct + "\t#\t;\n")

outputFile.close()
