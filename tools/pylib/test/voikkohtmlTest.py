# -*- coding: utf-8 -*-

# Copyright 2011 Harri Pitkänen (hatapitk@iki.fi)

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

from unittest import TestCase, TestLoader, TextTestRunner
from voikkohtml import parseHtml, SEGMENT_TYPE_HEADING, SEGMENT_TYPE_LIST_ITEM, SEGMENT_TYPE_PARAGRAPH
from HTMLParser import HTMLParseError

class VoikkoHtmlTest(TestCase):
	
	def assertParseError(self, html, lineno, offset):
		try:
			parseHtml(html)
		except HTMLParseError as e:
			self.assertEquals(lineno, e.lineno)
			self.assertEquals(offset, e.offset)
		else:
			self.fail("Expected exception")
	
	def testParseEmptyDocument(self):
		result = parseHtml(u"<html><head><title>kissa</title></head><body></body></html>")
		self.failUnless(len(result) == 0)
	
	def testParseInvalid(self):
		self.assertParseError(u"<htm<l>", 1, 4)
	
	def testParseTagMismatch(self):
		self.assertParseError(u"<html><head></html>", 1, 12)
	
	def testParseHeader(self):
		result = parseHtml(u"<html><body><h1>Kissan ruokkiminen</h1></body></html>")
		self.assertEquals([(SEGMENT_TYPE_HEADING, u"Kissan ruokkiminen")], result)
	
	def testParseListItems(self):
		result = parseHtml(u"<html><body><ul><li>kissa</li><li>koira</li></ul></body></html>")
		self.assertEquals([(SEGMENT_TYPE_LIST_ITEM, u"kissa"), (SEGMENT_TYPE_LIST_ITEM, u"koira")], result)
	
	def testParseParagraph(self):
		result = parseHtml(u"<html><body><p>Kissaa on ruokittava huolella.</p></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"Kissaa on ruokittava huolella.")], result)
	
	def testIgnoreBr(self):
		result = parseHtml(u"<html><body><p>Kissaa on ruokittava <br/>huolella.</p></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"Kissaa on ruokittava huolella.")], result)

if __name__ == "__main__":
	suite = TestLoader().loadTestsFromTestCase(VoikkoHtmlTest)
	TextTestRunner(verbosity=1).run(suite)
