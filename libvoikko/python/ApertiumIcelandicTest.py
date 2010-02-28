# -*- coding: utf-8 -*-

# Copyright 2010 Harri Pitkänen (hatapitk@iki.fi)
# Test suite for Lttoolbox (Apertium) morphology backend.
# Icelandic dictionary is used as test data.

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

class ApertiumIcelandicTest(unittest.TestCase):
	def setUp(self):
		self.voikko = libvoikko.Voikko()
		self.voikko.init(variant = "apertium")
	
	def tearDown(self):
		self.voikko.terminate()
	
	def testUnknowWordReturnsNoAnalyses(self):
		analysisList = self.voikko.analyze(u"skjdfhksdfgh")
		self.assertEqual(0, len(analysisList))
	
	def testSingleAdjectiveIsProperlyAnalyzed(self):
		# ^heilagur/heilagur<adj><pst><m><sg><nom><sta>$
		analysisList = self.voikko.analyze(u"heilagur")
		self.assertEqual(1, len(analysisList))
		analysis = analysisList[0]
		self.assertEqual(u"LAATUSANA", analysis["CLASS"])
		self.assertEqual(u"=pppppppp", analysis["STRUCTURE"])
	
	def testMultipleAnalysesCanBeReturned(self):
		# ^Björn/Björn<np><ant><m><sg><nom>/Björn<np><ant><m><sg><acc>/Björn<n><m><sg><nom><ind>/Björn<n><m><sg><acc><ind>$
		analysisList = self.voikko.analyze(u"Björn")
		self.failUnless(len(analysisList) > 1)


if __name__ == "__main__":
	unittest.main()
