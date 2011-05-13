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
from voikkohtml import parseHtml, SEGMENT_TYPE_HEADING, SEGMENT_TYPE_LIST_ITEM, SEGMENT_TYPE_PARAGRAPH, SEGMENT_TYPE_OTHER
from voikkohtml import getHtmlSafely, HttpException, ERR_INVALID_ENCODING, ERR_FORBIDDEN_SCHEME, ERR_TOO_MANY_REDIRECTS, ERR_NOT_FOUND, USER_AGENT
from HTMLParser import HTMLParseError
from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
from threading import Thread
from time import sleep
import pycurl

class VoikkoHtmlTest(TestCase):
	
	def assertParseError(self, html, lineno, offset):
		try:
			parseHtml(html)
		except HTMLParseError, e:
			self.assertEquals(lineno, e.lineno)
			self.assertEquals(offset, e.offset)
		else:
			self.fail("Expected exception")
	
	def testParseEmptyDocument(self):
		result = parseHtml(u"<html><head></head><body></body></html>")
		self.failUnless(len(result) == 0)
	
	def testParseTitle(self):
		result = parseHtml(u"<html><head><title>kissa</title></head><body></body></html>")
		self.assertEquals([(SEGMENT_TYPE_OTHER, u"kissa")], result)
	
	def testParseInvalid(self):
		self.assertParseError(u"<htm<l>", 1, 4)
	
	def testParseTagMismatch(self):
		self.assertParseError(u"<html><head></html>", 1, 12)
	
	def testParseTagMismatch(self):
		self.assertParseError(u"<html></html></p>", 1, 13)
	
	def testParseHeader(self):
		result = parseHtml(u"<html><body><h1>Kissan ruokkiminen</h1></body></html>")
		self.assertEquals([(SEGMENT_TYPE_HEADING, u"Kissan ruokkiminen")], result)
	
	def testParseListItems(self):
		result = parseHtml(u"<html><body><ul><li>kissa</li><li>koira</li></ul></body></html>")
		self.assertEquals([(SEGMENT_TYPE_LIST_ITEM, u"kissa"), (SEGMENT_TYPE_LIST_ITEM, u"koira")], result)
	
	def testParseListItemsWithinA(self):
		result = parseHtml(u"<html><body><ul><li><a>kissa</a></li></ul></body></html>")
		self.assertEquals([(SEGMENT_TYPE_LIST_ITEM, u"kissa")], result)
	
	def testParseListItemsWithinEm(self):
		result = parseHtml(u"<html><body><ul><li>kis<em>sa</em></li></ul></body></html>")
		self.assertEquals([(SEGMENT_TYPE_LIST_ITEM, u"kissa")], result)
	
	def testParseUnclosedListItems(self):
		result = parseHtml(u"<html><body><ul><li>kissa<li>koira</ul></body></html>")
		self.assertEquals([(SEGMENT_TYPE_LIST_ITEM, u"kissa"), (SEGMENT_TYPE_LIST_ITEM, u"koira")], result)
	
	def testParseNestedLists(self):
		result = parseHtml(u"<html><body><ul><li>kissa<ul><li>koira</li></ul></li></ul></body></html>")
		self.assertEquals([(SEGMENT_TYPE_OTHER, u"kissa"), (SEGMENT_TYPE_LIST_ITEM, u"koira")], result)
	
	def testParseDefinitionLists(self):
		result = parseHtml(u"<html><body><dl><dt>kissa</dt><dd>jalo eläin</dd></dl></body></html>")
		self.assertEquals([(SEGMENT_TYPE_LIST_ITEM, u"kissa"), (SEGMENT_TYPE_LIST_ITEM, u"jalo eläin")], result)
	
	def testBrokenTrIsIgnored(self):
		result = parseHtml(u"<html><body><table><td><p>kissa</p></td></tr></table></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"kissa")], result)
	
	def testAttributesHavingNoSpaceInBetweenAreAccepted(self):
		result = parseHtml(u"<html><body><table><td><p border='0'width='200'>kissa</p></td></tr></table></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"kissa")], result)
	
	def testUnclosedTdIsNotError(self):
		result = parseHtml(u"<html><body><table><tr><td><p>kissa</p></tr></table></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"kissa")], result)
	
	def testTdAfterUnclosedTdIsNotError(self):
		result = parseHtml(u"<html><body><table><tr><td><td><p>kissa</p></tr></table></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"kissa")], result)
	
	def testTableCaptionAndThead(self):
		result = parseHtml(u"<html><body><div><table><caption>kissa</caption><thead><tr><th>a</th></tr></thead></table></div></body></html>")
		self.assertEquals([(SEGMENT_TYPE_HEADING, u"kissa"), (SEGMENT_TYPE_OTHER, u"a")], result)
	
	def testTableInDivInList(self):
		result = parseHtml(u"<html><body><ul><li><div><table></table></div></li></ul></body></html>")
		self.assertEquals([], result)
	
	def testUnclosedThIsNotError(self):
		result = parseHtml(u"<html><body><table><tr><th><th><p>kissa</p></tr></table></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"kissa")], result)
	
	def testCorrectCommentParsing(self):
		result = parseHtml(u"<html><head><style><!-- <Group></Group> --></style></head><body><p>kissa</p></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"kissa")], result)
	
	def testMultiLineComments(self):
		result = parseHtml(u"<html><head><style><!-- \n<Group></Group> --></style></head><body><p>kissa</p></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"kissa")], result)
	
	def testMultipleComments(self):
		result = parseHtml(u"<html><body><p><!-- eka -- >kissa<!-- toka -- > ja koira</p></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"kissa ja koira")], result)
	
	def testUnescapedTagsWithinScript(self):
		result = parseHtml(u"<html><body><scrIpt>document.write('<script '); '</scr' + 'ipt>' </sCript ><p>koira</p></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"koira")], result)
	
	def testEmptyScript(self):
		result = parseHtml(u"<html><body><script></script><p>koira</p></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"koira")], result)
	
	def testCommentsDoNotAffectLineNumberingInErrorMessages(self):
		self.assertParseError(u"<html><body><p><!-- e\n<\ne -- >\nkissa<!-- to<> -- > <p<", 4, 22)
	
	def testScriptsDoNotAffectLineNumberingInErrorMessages(self):
		self.assertParseError(u"<html><body><p><script>e\n<\ne </script>\nkissa<script> to<></script> <p<", 4, 30)
	
	def testUnclosedP(self):
		result = parseHtml(u"<html><body><p>kissa<p>koira<div><p>hevonen</div></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"kissa"), (SEGMENT_TYPE_PARAGRAPH, u"koira"), (SEGMENT_TYPE_PARAGRAPH, u"hevonen")], result)
	
	def testMetaAndLinkMayBeUnclosed(self):
		result = parseHtml(u"<html><head><meta><link></head><body><p>kissa</p></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"kissa")], result)
	
	def testBaseMayBeUnclosed(self):
		result = parseHtml(u"<html><head><base></head><body><p>kissa</p></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"kissa")], result)
	
	def testFontAndNoscriptMayBeUnclosed(self):
		result = parseHtml(u"<html><body><p><font><noscript>kissa</p></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"kissa")], result)
	
	def testImageMapsAreIgnored(self):
		result = parseHtml(u"<html><body><map><area></map><p>kissa</p></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"kissa")], result)
	
	def testIframeIsIgnored(self):
		result = parseHtml(u"<html><body><p>kissa<iframe></iframe></p></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"kissa")], result)
	
	def testParseParagraphInListItem(self):
		result = parseHtml(u"<html><body><ul><li><p>kissa</p></li></ul></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"kissa")], result)
	
	def testParseParagraphInDefinitionListItem(self):
		result = parseHtml(u"<html><body><dl><dd><p>kissa</p></dd></dl></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"kissa")], result)
	
	def testParseParagraphInFormWithinListItem(self):
		result = parseHtml(u"<html><body><ul><li><form><p>kissa</p></form></li></ul></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"kissa")], result)
	
	def testParseParagraphInFormWithTraditionalInput(self):
		result = parseHtml(u"<html><body><form><input><p>kissa</p></form></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"kissa")], result)
	
	def testClearBeforeParagraph(self):
		result = parseHtml(u"<html><head><title>koira</title></head><body><p>kissa</p></body></html>")
		self.assertEquals([(SEGMENT_TYPE_OTHER, u"koira"), (SEGMENT_TYPE_PARAGRAPH, u"kissa")], result)
	
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
	
	def testScriptsWithinPIsIgnoredAndContentStripped(self):
		result = parseHtml(u"<html><body><p>Kissaa on <script>aksldj</script>ruokittava.</p></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"Kissaa on ruokittava.")], result)
	
	def testH1WithinPClosesP(self):
		result = parseHtml(u"<html><body><p>Kissa<h1>Koira</h1>jotain muuta</p></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"Kissa"), (SEGMENT_TYPE_HEADING, u"Koira"), (SEGMENT_TYPE_OTHER, u"jotain muuta")], result)
	
	def testLiWithinPIsParseError(self):
		self.assertParseError(u"<html><body><p>Kissaa on <li>aksldj</li>ruokittava.</p></body></html>", 1, 25)
	
	def testTablesAreIgnored(self):
		result = parseHtml(u"<html><body><p>Kissaa on ruokittava.</p><table><tr><td>sdsd</td></tr></table></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"Kissaa on ruokittava."), (SEGMENT_TYPE_OTHER, u"sdsd")], result)
	
	def testTableWithinP(self):
		result = parseHtml(u"<html><body><p><table><tr><td>sdsd</td></tr></table>ruokittava.</p></body></html>")
		self.assertEquals([(SEGMENT_TYPE_OTHER, u"sdsd"), (SEGMENT_TYPE_OTHER, u"ruokittava.")], result)
	
	def testStrayTdCloseTag(self):
		result = parseHtml(u"<html><body><p>kissa</p></td></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"kissa")], result)
	
	def testStrongIsJustText(self):
		result = parseHtml(u"<html><body><p>Kissaa on <strong>ruokittava</strong>.</p></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"Kissaa on ruokittava.")], result)
	
	def testUnderlineInducesNoSpace(self):
		result = parseHtml(u"<html><body><h1>Libre<u>Office</u></h1></body></html>")
		self.assertEquals([(SEGMENT_TYPE_HEADING, u"LibreOffice")], result)
	
	def testNonAscii(self):
		result = parseHtml(u"<html><body><h1>Eläinlääk&auml;rissä käynti €</h1></body></html>")
		self.assertEquals([(SEGMENT_TYPE_HEADING, u"Eläinlääkärissä käynti €")], result)
	
	def testUnknownEntityIsAssumedToBeJustText(self):
		result = parseHtml(u"<html><body><p>Kissa & koira ja &kissa;</p></body></html>")
		self.assertEquals([(SEGMENT_TYPE_PARAGRAPH, u"Kissa & koira ja &kissa")], result)
	
	def testCharacterReferences(self):
		result = parseHtml(u"<html><body><h1>&#33;</h1></body></html>")
		self.assertEquals([(SEGMENT_TYPE_HEADING, u"!")], result)
	
	def testTextWithinBody(self):
		result = parseHtml(u"<html><body>kissa</body></html>");
		self.assertEquals([(SEGMENT_TYPE_OTHER, u"kissa")], result)
	
	def testUnknownCharacterReferenceIsParseError(self):
		self.assertParseError(u"<html><body><p>&#65534;</p></body></html>", 1, 15)
	
	def startServer(self, port, getFunct, requestCount):
		def runServer(tester):
			class TestHttpServer(BaseHTTPRequestHandler):
				def do_GET(self):
					getFunct(self, tester)
				def log_request(self, code, size=0):
					pass # no logging for tests
			server = HTTPServer(("", port), TestHttpServer)
			for i in range(requestCount):
				server.handle_request()
		t = Thread(target = runServer, args = (self,))
		t.start()
		sleep(0.01)
		return t
	
	def startNormalServer(self, port, code, contentType, responseData):
		def getFunct(slf, tester):
			tester.assertEquals(USER_AGENT, slf.headers["User-Agent"])
			tester.assertEquals("127.0.0.1", slf.headers["X-Forwarded-For"])
			slf.send_response(code)
			slf.send_header("Content-Type", contentType)
			slf.end_headers()
			slf.wfile.write(responseData)
		return self.startServer(port, getFunct, 1)
	
	def startRedirectServer(self, port):
		def getFunct(slf, tester):
			tester.assertEquals(USER_AGENT, slf.headers["User-Agent"])
			if slf.path.startswith("/1"):
				slf.send_response(301) # Moved permanently
				slf.send_header("Location", "/2")
				slf.end_headers()
			elif slf.path.startswith("/2"):
				slf.send_response(302) # Found
				slf.send_header("Location", "http://localhost:%i/3" % port)
				slf.end_headers()
				slf.wfile.write("do-not-display-this")
			elif slf.path.startswith("/3"):
				slf.send_response(200) # OK
				slf.send_header("Content-Type", "text/html")
				slf.end_headers()
				slf.wfile.write("hirvi")
		return self.startServer(port, getFunct, 3)
	
	def startRedirectNServer(self, port, numberOfRedirects, numberOfExpectedGets):
		def getFunct(slf, tester):
			tester.assertEquals(USER_AGENT, slf.headers["User-Agent"])
			step = int(slf.path[1:])
			if step <= numberOfRedirects:
				slf.send_response(302) # Found
				slf.send_header("Location", "/%i" % (step + 1))
				slf.end_headers()
			else:
				slf.send_response(200) # OK
				slf.send_header("Content-Type", "text/html")
				slf.end_headers()
				slf.wfile.write("hirvi")
		return self.startServer(port, getFunct, numberOfExpectedGets)
	
	def assertThreadExitsNormally(self, thread):
		thread.join(1)
		if thread.isAlive():
			self.fail(u"Thread did not exit normally")
	
	def testFollowRedirects(self):
		t = self.startRedirectServer(3400)
		self.assertEquals(u"hirvi", getHtmlSafely("http://localhost:3400/1"))
		self.assertThreadExitsNormally(t)
	
	def testMaxRedirectsIsReached(self):
		t = self.startRedirectNServer(3400, 4, 4)
		try:
			getHtmlSafely("http://localhost:3400/1")
		except HttpException, e:
			self.failUnless(ERR_TOO_MANY_REDIRECTS in e.parameter)
			self.assertThreadExitsNormally(t)
			return
		self.fail(u"Expected exception")
	
	def testRedirectToFtpIsNotAllowed(self):
		def getFunct(slf, tester):
			slf.send_response(302) # Found
			slf.send_header("Location", "ftp://localhost/")
			slf.end_headers()
		t = self.startServer(3400, getFunct, 1)
		try:
			getHtmlSafely("http://localhost:3400/")
		except HttpException, e:
			self.failUnless(ERR_FORBIDDEN_SCHEME in e.parameter)
			self.assertThreadExitsNormally(t)
			return
		self.fail(u"Expected exception")
	
	def testNotFound(self):
		def getFunct(slf, tester):
			slf.send_response(404)
			slf.end_headers()
			slf.wfile.write("kissa")
		t = self.startServer(3400, getFunct, 1)
		try:
			getHtmlSafely("http://localhost:3400/")
		except HttpException, e:
			self.failUnless(ERR_NOT_FOUND in e.parameter)
			self.assertThreadExitsNormally(t)
			return
		self.fail(u"Expected exception")
	
	def testPreviousProxiesAreRetained(self):
		def getFunct(slf, tester):
			tester.assertEquals("123.123.123.123, 93.35.124.35", slf.headers["X-Forwarded-For"])
			slf.send_response(200)
			slf.end_headers()
			slf.wfile.write("kissa")
		t = self.startServer(3400, getFunct, 1)
		html = getHtmlSafely("http://localhost:3400/", "93.35.124.35", ["X-Something: sfsf\n", "X-Forwarded-For: 123.123.123.123\n"])
		self.assertEquals(u"kissa", html)
		self.assertThreadExitsNormally(t)
	
	def testGetHtmlSafely(self):
		t = self.startNormalServer(3400, 200, "text/html; charset=UTF-8", "kissa")
		self.assertEquals(u"kissa", getHtmlSafely("http://localhost:3400"))
		self.assertThreadExitsNormally(t)
	
	def testSchemeIsOptional(self):
		t = self.startNormalServer(3400, 200, "text/html; charset=UTF-8", "kissa")
		self.assertEquals(u"kissa", getHtmlSafely("127.0.0.1:3400"))
		self.assertThreadExitsNormally(t)
	
	def testUtf8EncodingFromContentType(self):
		t = self.startNormalServer(3400, 200, "text/html; charset=UTF-8", u"täti".encode('UTF-8'))
		self.assertEquals(u"täti", getHtmlSafely("http://127.0.0.1:3400"))
		self.assertThreadExitsNormally(t)
	
	def testLatin1EncodingFromContentType(self):
		t = self.startNormalServer(3400, 200, "text/html; charset=ISO-8859-1", u"täti".encode('ISO-8859-1'))
		self.assertEquals(u"täti", getHtmlSafely("http://127.0.0.1:3400"))
		self.assertThreadExitsNormally(t)
	
	def testLatin1EncodingFromMetaTag(self):
		response = u'<html>\n<head>\n<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">\n</head><body>täti</body></html>'
		t = self.startNormalServer(3400, 200, "text/html", response.encode('ISO-8859-1'))
		self.assertEquals(response, getHtmlSafely("http://127.0.0.1:3400"))
	
	def testEncodingMismatchIsError(self):
		t = self.startNormalServer(3400, 200, "text/html; charset=UTF-8", u"täti".encode('ISO-8859-1'))
		try:
			getHtmlSafely("http://127.0.0.1:3400")
		except HttpException, e:
			self.failUnless(ERR_INVALID_ENCODING in e.parameter)
			return
		self.fail(u"Expected exception")
	
	def testInvalidEncodingNameIsError(self):
		t = self.startNormalServer(3400, 200, "text/html; charset=ksfhsdlfhhfsd", u"täti".encode('ISO-8859-1'))
		try:
			getHtmlSafely("http://127.0.0.1:3400")
		except HttpException, e:
			self.failUnless(ERR_INVALID_ENCODING in e.parameter)
			return
		self.fail(u"Expected exception")
	
	def testForbiddenScheme(self):
		try:
			getHtmlSafely("ftp://localhost")
		except HttpException, e:
			self.failUnless(ERR_FORBIDDEN_SCHEME in e.parameter)
			return
		self.fail(u"Expected exception")
	
	def testUnknownHost(self):
		try:
			getHtmlSafely("http://ksdjaksd.ajhsdjas.ajshd")
		except HttpException, e:
			self.failUnless(pycurl.E_COULDNT_RESOLVE_HOST in e.parameter)
			return
		self.fail(u"Expected exception")
	
	def testLocalFilesAreNotReadWithScheme(self):
		try:
			getHtmlSafely("file:/etc/passwd")
		except HttpException, e:
			return
		self.fail(u"Expected exception")
	
	def testLocalFilesAreNotRead(self):
		try:
			getHtmlSafely("/etc/passwd")
		except HttpException, e:
			return
		self.fail(u"Expected exception")
	
	def testFtpIsNotAllowed(self):
		try:
			getHtmlSafely("ftp:localhost")
		except HttpException, e:
			return
		self.fail(u"Expected exception")
	
	def testFtpIsNotAllowedWithProtocolAutoDetect(self):
		try:
			getHtmlSafely("ftp.funet.fi")
		except HttpException, e:
			self.failUnless(ERR_FORBIDDEN_SCHEME in e.parameter)
			return
		self.fail(u"Expected exception")

if __name__ == "__main__":
	suite = TestLoader().loadTestsFromTestCase(VoikkoHtmlTest)
	#suite = TestLoader().loadTestsFromName("voikkohtmlTest.VoikkoHtmlTest.testCorrectCommentParsing")
	TextTestRunner(verbosity=1).run(suite)
