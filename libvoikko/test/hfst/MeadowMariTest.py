# -*- coding: utf-8 -*-

# Copyright 2010 - 2013 Harri Pitkänen (hatapitk@iki.fi)
# Test suite for HFST morphology backend.
# Meadow Mari (mhr) dictionary is used as test data.

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

import unittest
import libvoikko

class MeadowMariTest(unittest.TestCase):
	def setUp(self):
		self.voikko = libvoikko.Voikko("mhr")
	
	def tearDown(self):
		self.voikko.terminate()
	
	def testUnknowWordIsRejected(self):
		self.failIf(self.voikko.spell(u"skjdfhksdfgh"))
	
	def testValidWordIsAccepted(self):
		self.failUnless(self.voikko.spell(u"йылме"))
	
	def testProperNounIsAccepted(self):
		self.failUnless(self.voikko.spell(u"Марий"))
	
	def testSuggestionsAreReturned(self):
		self.failUnless(u"Такси" in self.voikko.suggest(u"Такаси"))


if __name__ == "__main__":
	unittest.main()
