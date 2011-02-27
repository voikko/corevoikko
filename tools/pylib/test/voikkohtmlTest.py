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
from voikkohtml import parseHtml, SEGMENT_TYPE_HEADING, SEGMENT_TYPE_LIST_ITEM, SEGMENT_TYPE_PARAGRAPH, getHtmlSafely
from HTMLParser import HTMLParseError
from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
from threading import Thread
from time import sleep

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
	
	def testIgnoreXhtmlBr(self):
		result = parseHtml(u"<html><body><p>Kissaa on ruokittava <br/>huolella.</p></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"Kissaa on ruokittava huolella.")], result)
	
	def testIgnoreTraditionalBr(self):
		result = parseHtml(u"<html><body><p>Kissaa on ruokittava <br>huolella.</p></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"Kissaa on ruokittava huolella.")], result)
	
	def testIgnoreImages(self):
		result = parseHtml(u"<html><body><p>Kissaa <img src='cat.jpg'>on ruokittava.</p></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"Kissaa on ruokittava.")], result)
	
	def testBrIsWhitespace(self):
		result = parseHtml(u"<html><body><p>Kissaa on ruokittava<br/>huolella.</p></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"Kissaa on ruokittava huolella.")], result)
	
	def testExtraWhitespaceIsRemoved(self):
		result = parseHtml(u"<html><body><p>\tKissaa  on \rruokittava huolella.  </p></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"Kissaa on ruokittava huolella.")], result)
	
	def testLineFeedIsJustSpace(self):
		result = parseHtml(u"<html><body><p>Kissaa\non\r\nruokittava\rhuolella.</p></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"Kissaa on ruokittava huolella.")], result)
	
	def testScriptsAreStripped(self):
		result = parseHtml(u"<html><body><p>Kissaa on ruokittava.</p><script>lksjdf</script></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"Kissaa on ruokittava.")], result)
	
	def testScriptsWithinPIsParseError(self):
		self.assertParseError(u"<html><body><p>Kissaa on <script>aksldj</script>ruokittava.</p></body></html>", 1, 25)
	
	def testLiWithinPIsParseError(self):
		self.assertParseError(u"<html><body><p>Kissaa on <li>aksldj</li>ruokittava.</p></body></html>", 1, 25)
	
	def testTablesAreIgnored(self):
		result = parseHtml(u"<html><body><p>Kissaa on ruokittava.</p><table><tr><td>sdsd</td></tr></table></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"Kissaa on ruokittava.")], result)
	
	def testTableWithinPIsParseError(self):
		self.assertParseError(u"<html><body><p><table><tr><td>sdsd</td></tr></table>ruokittava.</p></body></html>", 1, 15)
	
	def testStrongIsJustText(self):
		result = parseHtml(u"<html><body><p>Kissaa on <strong>ruokittava</strong>.</p></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"Kissaa on ruokittava.")], result)
	
	def testUnderlineInducesNoSpace(self):
		result = parseHtml(u"<html><body><h1>Libre<u>Office</u></h1></body></html>")
		self.assertEquals([(SEGMENT_TYPE_HEADING, u"LibreOffice")], result)
	
	def testNonAscii(self):
		result = parseHtml(u"<html><body><h1>Eläinlääk&auml;rissä käynti €</h1></body></html>")
		self.assertEquals([(SEGMENT_TYPE_HEADING, u"Eläinlääkärissä käynti €")], result)
	
	def testUnknownEntityIsParseError(self):
		self.assertParseError(u"<html><body><p>&kissa;</p></body></html>", 1, 15)
	
	def testCharacterReferences(self):
		result = parseHtml(u"<html><body><h1>&#33;</h1></body></html>")
		self.assertEquals([(SEGMENT_TYPE_HEADING, u"!")], result)
	
	def testUnknownCharacterReferenceIsParseError(self):
		self.assertParseError(u"<html><body><p>&#65534;</p></body></html>", 1, 15)
	
	def startServer(self, port, code, contentType, responseData):
		def runServer():
			class TestHttpServer(BaseHTTPRequestHandler):
				def do_GET(self):
					self.send_response(code)
					self.send_header("Content-Type", contentType)
					self.end_headers()
					self.wfile.write(responseData)
				def log_request(self, code, size=0):
					pass # no logging for tests
			server = HTTPServer(("", port), TestHttpServer)
			server.handle_request()
		t = Thread(target = runServer)
		t.start()
		sleep(0.01)
		return t
	
	def assertThreadExitsNormally(self, thread):
		thread.join(1)
		if thread.isAlive():
			self.fail(u"Thread did not exit normally")
	
	def xxtestGetHtmlSafely(self):
		t = self.startServer(3400, 200, "text/html; charset=UTF-8", "kissa")
		self.assertEquals(u"kissa", getHtmlSafely(u"http://localhost:3400"))
		assertThreadExitsNormally(t)
	
	def testSchemeIsOptional(self):
		t = self.startServer(3400, 200, "text/html; charset=UTF-8", "kissa")
		self.assertEquals(u"kissa", getHtmlSafely(u"127.0.0.1:3400"))
		assertThreadExitsNormally(t)

if __name__ == "__main__":
	suite = TestLoader().loadTestsFromTestCase(VoikkoHtmlTest)
	TextTestRunner(verbosity=1).run(suite)
