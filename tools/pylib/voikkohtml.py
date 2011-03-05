# -*- coding: utf-8 -*-

# Copyright 2011 Harri Pitkänen (hatapitk@iki.fi)
# Module for analyzing html text.
# This program requires
# - Python 2.5 or later (3 and later are not yet supported)
# - Python module of libvoikko 3.0 or later
# - PyCurl (used because timeout options in built-in HTTP client libraries are insufficient)

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

from HTMLParser import HTMLParser, HTMLParseError
from re import sub, search
from htmlentitydefs import name2codepoint
from urlparse import urlparse
import pycurl

SEGMENT_TYPE_HEADING = 1
SEGMENT_TYPE_LIST_ITEM = 2
SEGMENT_TYPE_PARAGRAPH = 3

class VoikkoHTMLParser(HTMLParser):
	
	def getData(self):
		result = sub(u"\\s+", u" ", self.data).strip()
		self.data = u""
		return result
	
	def isContentTag(self, tag):
		return tag in ["h1", "h2", "h3", "h4", "h5", "h6", "li", "dt", "dd", "p"]
	
	def isNotAllowedInHeaderOrParagraph(self, tag):
		return tag in ["table"]
	
	def isNonContentTag(self, tag):
		return tag in ["script", "style", "iframe"]
	
	def isIgnorableTag(self, tag):
		return tag in ["hr", "img", "tr", "b", "i", "u", "span", "meta", "link", "input", "button", "map", "area"]
	
	def __init__(self):
		HTMLParser.__init__(self)
		self.tags = []
		self.segments = []
		self.data = u""
		self.acceptData = None
	
	def handle_data(self, data):
		self.data = self.data + data
	
	def handle_entityref(self, name):
		if name in name2codepoint:
			self.data = self.data + unichr(name2codepoint[name])
		else:
			raise HTMLParseError("Unknown entity reference", self.getpos())
	
	def handle_charref(self, name):
		nameInt = int(name)
		if 0 < nameInt and nameInt <= 65533:
			self.data = self.data + unichr(nameInt)
		else:
			raise HTMLParseError("Unknown character reference", self.getpos())
	
	def allowContentInList(self):
		for tag in reversed(self.tags):
			if tag == "li":
				return True
			if self.isContentTag(tag) or self.isNonContentTag(tag):
				return False
		return False
	
	def handle_starttag(self, tag, attrs):
		if self.isIgnorableTag(tag):
			return
		if tag in ["br"]:
			self.data = self.data + u" "
		elif self.isContentTag(tag):
			self.data = u""
			if self.allowContentInList():
				pass
			elif tag == "li" and len(self.tags) >= 1 and self.tags[-1] == "ul":
				pass
			elif self.acceptData is not None:
				raise HTMLParseError("Nesting error", self.getpos())
			self.acceptData = True
		elif self.isNonContentTag(tag):
			if self.acceptData is not None:
				raise HTMLParseError("Nesting error", self.getpos())
			self.acceptData = False
		elif self.isNotAllowedInHeaderOrParagraph(tag):
			if len(self.tags) == 0 or (self.tags[-1] != "li" and self.acceptData is not None):
				raise HTMLParseError("Nesting error", self.getpos())
		self.tags.append(tag)
	
	def handle_endtag(self, tag):
		if self.isIgnorableTag(tag):
			return
		if not self.tags:
			raise HTMLParseError("End tag without open elements", self.getpos())
		openTag = self.tags.pop()
		while tag != openTag and openTag in ["br"]:
			openTag = self.tags.pop()
		if tag != openTag:
			raise HTMLParseError("End tag does not match start tag", self.getpos())
		if openTag in ["h1", "h2", "h3", "h4", "h5", "h6"]:
			self.segments.append((SEGMENT_TYPE_HEADING, self.getData()))
		elif openTag in ["li", "dt", "dd"]:
			data = self.getData()
			if len(data) > 0:
				self.segments.append((SEGMENT_TYPE_LIST_ITEM, data))
		elif openTag == "p":
			self.segments.append((SEGMENT_TYPE_PARAGRAPH, self.getData()))
		if self.isContentTag(tag) or self.isNonContentTag(tag):
			self.acceptData = None
	
	def processInput(self, html):
		self.feed(html)
		self.close()
		return self.segments

class HttpResult:
	def __init__(self):
		self.contents = ''
	
	def body_callback(self, buf):
		self.contents = self.contents + buf 

def parseHtml(html):
	return VoikkoHTMLParser().processInput(html)

class HttpException(Exception):
	def __init__(self, msg):
		self.parameter = msg
	def __str__(self):
		return repr(self.parameter)

ERR_FORBIDDEN_SCHEME = u"Vain http-osoitteet ovat sallittuja."
ERR_INVALID_ENCODING = u"Sivu merkistökoodaus on määritelty väärin tai merkistö on tuntematon"
ERR_TOO_MANY_REDIRECTS = u"Liian monta http-uudelleenohjausta"
USER_AGENT = "WebVoikko language checker - see http://joukahainen.puimula.org/webvoikko/spell"
MAX_REDIRECTS = 3

def __checkValidUrl(url):
	if url.startswith('ftp') or (':/' in url and not url.startswith('http:')):
		raise HttpException(ERR_FORBIDDEN_SCHEME)

def getHtmlSafely(url):
	result = HttpResult()
	__checkValidUrl(url)
	urlParts = urlparse(url, u"http")
	c = pycurl.Curl()
	c.setopt(pycurl.URL, url)
	c.setopt(pycurl.WRITEFUNCTION, result.body_callback)
	c.setopt(pycurl.MAXFILESIZE, 50000)
	c.setopt(pycurl.TIMEOUT, 10)
	c.setopt(pycurl.USERAGENT, USER_AGENT)
	c.setopt(pycurl.FOLLOWLOCATION, False)
	found = False
	redirs = 0
	oldRedirUrl = None
	while True:
		try:
			c.perform()
		except Exception, e:
			c.close()
			raise HttpException(e)
		redirUrl = c.getinfo(pycurl.REDIRECT_URL)
		if redirUrl != None and redirUrl != oldRedirUrl:
			redirs = redirs + 1
			if redirs > MAX_REDIRECTS:
				break
			__checkValidUrl(redirUrl)
			c.setopt(pycurl.URL, redirUrl)
			oldRedirUrl = redirUrl
		else:
			found = True
			break
	if not found:
		raise HttpException(ERR_TOO_MANY_REDIRECTS)
	encoding = None
	contentType = c.getinfo(pycurl.CONTENT_TYPE)
	if 'charset=' in contentType:
		encoding = contentType[contentType.find('charset=') + 8:]
	c.close()
	text = result.contents
	if not encoding:
		# Check meta tags or similar for hints
		match = search('charset=.+"|\'', text)
		if match:
			encoding = match.group(0)[8:-1]
	if not encoding:
		encoding = 'UTF-8' # default
	try:
		return unicode(result.contents, encoding)
	except UnicodeDecodeError:
		raise HttpException(ERR_INVALID_ENCODING)
