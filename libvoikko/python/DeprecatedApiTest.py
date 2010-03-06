# -*- coding: utf-8 -*-

# Copyright 2010 Harri Pitkänen (hatapitk@iki.fi)
# Test suite for deprecated functions in libvoikko.

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
from libvoikko import Voikko

voikko = Voikko()

class Utf8ApiTest(unittest.TestCase):
	def setUp(self):
		voikko.init()
	
	def tearDown(self):
		voikko.terminate()
	
	def testIntersectCompoundLevelCanBeSet(self):
		# This will do nothing but should not fail
		voikko.lib.voikko_set_int_option(voikko.handle, 5, 56)

if __name__ == "__main__":
	unittest.main()
