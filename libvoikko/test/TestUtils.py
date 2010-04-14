# -*- coding: utf-8 -*-

# Copyright 2010 Harri Pitkänen (hatapitk@iki.fi)
# Utilities to help writing tests for libvoikko.

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

import tempfile
import os
import codecs

DICTIONARY_FORMAT_VERSION = "2"
INFO_FILE = "voikko-fi_FI.pro"

class MorphologyInfo:
	def __init__(self):
		self.language = u"fi_FI"
		self.variant = u"standard"
		self.description = u"Default description"
		self.morphology = None
		self.speller = None
		self.suggestion = None
		self.hyphenator = None
	
	def __writeLine(self, fileHandle, key, value):
		if value is not None:
			fileHandle.write(u"info: ")
			fileHandle.write(key)
			fileHandle.write(u": ")
			fileHandle.write(value)
			fileHandle.write(u"\n")
	
	def writeFileContent(self, fileHandle):
		self.__writeLine(fileHandle, u"Voikko-Dictionary-Format", DICTIONARY_FORMAT_VERSION)
		self.__writeLine(fileHandle, u"Language-Code", self.language)
		self.__writeLine(fileHandle, u"Language-Variant", self.variant)
		self.__writeLine(fileHandle, u"Description", self.description)
		self.__writeLine(fileHandle, u"Morphology-Backend", self.morphology)
		self.__writeLine(fileHandle, u"Speller-Backend", self.speller)
		self.__writeLine(fileHandle, u"Suggestion-Backend", self.suggestion)
		self.__writeLine(fileHandle, u"Hyphenator-Backend", self.hyphenator)


class TestDataDir:
	def __init__(self):
		self.tempDir = tempfile.mkdtemp()
		self.versionedDir = self.tempDir + os.sep + DICTIONARY_FORMAT_VERSION
		os.mkdir(self.versionedDir)
		self.subdirNames = []
	
	def tearDown(self):
		for subdirName in self.subdirNames:
			subdirPath = self.versionedDir + os.sep + subdirName
			os.remove(subdirPath + os.sep + INFO_FILE)
			os.rmdir(subdirPath)
		os.rmdir(self.versionedDir)
		os.rmdir(self.tempDir)
	
	def createMorphology(self, subdirName, morphology):
		self.subdirNames.append(subdirName)
		subdirPath = self.versionedDir + os.sep + subdirName
		os.mkdir(subdirPath)
		fileHandle = codecs.open(subdirPath + os.sep + INFO_FILE, "w", "UTF-8")
		morphology.writeFileContent(fileHandle)
		fileHandle.close()
