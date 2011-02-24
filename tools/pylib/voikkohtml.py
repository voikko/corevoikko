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

SEGMENT_TYPE_HEADING = 1
SEGMENT_TYPE_LIST_ITEM = 2
SEGMENT_TYPE_PARAGRAPH = 3

class VoikkoHTMLParser(HTMLParser):
	
	def __init__(self):
		HTMLParser.__init__(self)
		self.tags = []
		self.segments = []
	
	def handle_starttag(self, tag, attrs):
		self.tags.append(tag)
	
	def handle_endtag(self, tag):
		openTag = self.tags.pop()
		if tag != openTag:
			raise HTMLParseError("End tag does not match start tag", self.getpos())
	
	def processInput(self, html):
		self.feed(html)
		self.close()
		return self.segments


def parseHtml(html):
	return VoikkoHTMLParser().processInput(html)
