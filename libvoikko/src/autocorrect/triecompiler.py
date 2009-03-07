# -*- coding: utf-8 -*-

# Copyright 2009 Harri Pitkänen (hatapitk@iki.fi)

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
# into a lookup trie in C++. The trie consist of an array of TrieNode
# objects (NODES). NODES[0] is the trie root. If x is a node, the first
# child of x is NODES[x.subtreeStart], the second is NODES[x.subtreeStart + 1]
# etc. The end of child list is represented by a null node (node.label == 0).
# If node has no children, node.subtreeStart == 0.
#
# All nodes representing an input string have node.replacementIndex != 0.
# The replacement index is an index of array REPLACEMENTS that contains
# the suggested replacement for the input string.

import xml.dom.minidom

class Node:
	def __init__(self, value = None):
		self.value = value
		self.valueIndex = 0
		self.children = { }

def appendToTrie(trie, key, value):
	if len(key) == 1:
		if key in trie.children:
			# Duplicate key will replace the old value
			trie.children[key].value = value
		else:
			trie.children[key] = Node(value)
	else:
		first = key[0]
		rest = key[1:]
		if first in trie.children:
			appendToTrie(trie.children[first], rest, value)
		else:
			node = Node()
			appendToTrie(node, rest, value)
			trie.children[first] = node

def indexTrie(trie, nextNodeIndex, nextValueIndex):
	for node in trie.children:
		trie.children[node].nodeIndex = nextNodeIndex
		nextNodeIndex += 1
		if trie.children[node].value != None:
			trie.children[node].valueIndex = nextValueIndex
			nextValueIndex += 1
	nextNodeIndex += 1 # skip terminating null node
	for node in trie.children:
		(nextNodeIndex, nextValueIndex) = indexTrie(trie.children[node], nextNodeIndex, nextValueIndex)
	return (nextNodeIndex, nextValueIndex)

def cHexCodeForChar(unicodeChar):
	ordinal = ord(unicodeChar)
	if (0x20 <= ordinal or ordinal <= 0x7f) and ordinal != 0x24 and ordinal != 0x40:
		# These characters cannot be represented as unicode literals in XC++
		return unicodeChar
	hexCode = hex(ordinal)[2:]
	return "\\u" + hexCode.rjust(4, "0")

def writeTrieNodes(trie, outputFile):
	for node in trie.children:
		outputFile.write(",TrieNode(")
		outputFile.write("L'%s'" % cHexCodeForChar(node))
		outputFile.write(",")
		outputFile.write(`trie.children[node].valueIndex`)
		outputFile.write(",")
		if len(trie.children[node].children) != 0:
			outputFile.write(`trie.children[node].children.itervalues().next().nodeIndex`)
		else:
			outputFile.write("0")
		outputFile.write(")")
	outputFile.write(",TrieNode(L'\\0',0,0)") # terminating null node
	for node in trie.children:
		writeTrieNodes(trie.children[node], outputFile)

def writeTrieValues(trie, outputFile):
	for node in trie.children.itervalues():
		if node.value != None:
			outputFile.write(',L"')
			outputFile.write(node.value)
			outputFile.write('"')
	for node in trie.children.itervalues():
		writeTrieValues(node, outputFile)

# Open the XML file
xmlFile = open("test.xml", "r")
autoCorrect = xml.dom.minidom.parseString(xmlFile.read())
xmlFile.close()

# Read entries to a trie
trieRoot = Node()
for replacement in autoCorrect.getElementsByTagName("replacement"):
	incorrect = replacement.getElementsByTagName("incorrect")[0].firstChild.wholeText
	correct = replacement.getElementsByTagName("correct")[0].firstChild.wholeText
	appendToTrie(trieRoot, incorrect.replace("=", ""), correct.replace("=", ""))
autoCorrect = None

# Index trie nodes
indexTrie(trieRoot, 1, 1)

# Write trie to a file as a C++ structure
outputFile = open("Data.cpp", "w")
outputFile.write('#include "TrieNode.hpp"\n')
outputFile.write('namespace libvoikko { namespace autocorrect {\n')

outputFile.write("static const TrieNode NODES[] = {\n")
outputFile.write("TrieNode(L'\\0',0,1)")
writeTrieNodes(trieRoot, outputFile)
outputFile.write('};\n')

outputFile.write("static const wchar_t * REPLACEMENTS[] = {\n")
outputFile.write("0")
writeTrieValues(trieRoot, outputFile)
outputFile.write('};\n')

outputFile.write('}}\n')
outputFile.close()
