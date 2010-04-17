# -*- coding: utf-8 -*-

# Copyright 2010 Harri Pitkänen (hatapitk@iki.fi)
# Test suite for libvoikko that includes all tests that can be run
# without assuming externally installed morphologies or other data.
#
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

import sys
import os
import ctypes
import unittest
import TestUtils

# Set up test environment
TEST_DIR = sys.path[0]
PYTHON_MODULE_DIR = TEST_DIR + os.sep + ".." + os.sep + "python"
sys.path.insert(0, PYTHON_MODULE_DIR)

# Import tests
from NullComponentTest import NullComponentTest

# Run all test suites
testCaseClasses = [NullComponentTest]
testCases = [unittest.TestLoader().loadTestsFromTestCase(caseClass) for caseClass in testCaseClasses]
testSuite = unittest.TestSuite(testCases)
unittest.TextTestRunner(verbosity=1).run(testSuite)
