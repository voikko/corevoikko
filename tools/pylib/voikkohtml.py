# -*- coding: utf-8 -*-

# Copyright 2011 Harri Pitkänen (hatapitk@iki.fi)
# Module for analyzing html text.
# This program requires Python and Python module of libvoikko from
# libvoikko 3.0 or later.

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
from re import sub

SEGMENT_TYPE_HEADING = 1
SEGMENT_TYPE_LIST_ITEM = 2
SEGMENT_TYPE_PARAGRAPH = 3

class VoikkoHTMLParser(HTMLParser):
	
	def getData(self):
		result = sub(u"\\s+", u" ", self.data).strip()
		self.data = u""
		return result
	
	def __init__(self):
		HTMLParser.__init__(self)
		self.tags = []
		self.segments = []
		self.data = u""
	
	def handle_data(self, data):
		self.data = self.data + data
	
	def handle_starttag(self, tag, attrs):
		if tag in ["br"]:
			self.data = self.data + u" "
		self.tags.append(tag)
	
	def handle_endtag(self, tag):
		openTag = self.tags.pop()
		while tag != openTag and openTag in ["br", "hr", "img"]:
			openTag = self.tags.pop()
		if tag != openTag:
			raise HTMLParseError("End tag does not match start tag", self.getpos())
		if openTag in ["h1", "h2", "h3", "h4", "h5", "h6"]:
			self.segments.append((SEGMENT_TYPE_HEADING, self.getData()))
		elif openTag == "li":
			self.segments.append((SEGMENT_TYPE_LIST_ITEM, self.getData()))
		elif openTag == "p":
			self.segments.append((SEGMENT_TYPE_PARAGRAPH, self.getData()))
	
	def processInput(self, html):
		self.feed(html)
		self.close()
		return self.segments


def parseHtml(html):
	return VoikkoHTMLParser().processInput(html)
