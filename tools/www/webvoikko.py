# -*- coding: utf-8 -*-

# Copyright 2007 Harri Pitkänen (hatapitk@iki.fi)
# Web interface for Finnish linguistic tools based on Voikko

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

# This file contains tools to find inflection class for a word

from mod_python import apache

import sys
import subprocess
import urllib
import xml.sax.saxutils

# Hyphenator command
HYPHCOMMAND = 'LC_CTYPE="fi_FI.UTF-8" voikkohyphenate no_ugly_hyphenation=0 ignore_dot=1'


def _write(req, text):
	req.write(text.encode('UTF-8'))

# Decodes a string from html form to unicode
def _decode_form_value(string):
	return unicode(urllib.unquote_plus(string), 'UTF-8')

# Converts a string to a form that is suitable for use in html document text
def _escape_html(string):
	return xml.sax.saxutils.escape(string)

def _hyphenate_wordlist(wordlist):
	hyphenator = subprocess.Popen(HYPHCOMMAND, shell = True, stdin = subprocess.PIPE,
	                              stdout = subprocess.PIPE, close_fds = True)
	for word in wordlist:
		if len(word) > 100: hyphenator.stdin.write(u'YLIPITKÄSANA\n'.encode('UTF-8'))
		else: hyphenator.stdin.write(word.encode('UTF-8') + '\n')
	(out, err) = hyphenator.communicate()
	rawlist = out.split('\n')
	hyphenatedlist = []
	for hword in rawlist:
		hyphenatedlist.append(unicode(hword, 'UTF-8'))
	return hyphenatedlist #FIXME: last item is an extra empty string

def _split_words(text):
	words = []
	separators = []
	for line in text.splitlines():
		for word in line.split():
			words.append(word)
			separators.append(u' ')
		words.append(u'')
		separators.append(u'<br />')
	return (words, separators)

def hyphenate(req, hyphstring = None):
	req.content_type = "text/html; charset=UTF-8"
	req.send_http_header()
	_write(req, u'<html><head><title>Voikko-tavuttaja</title></head>\n')
	_write(req, u'<body><h1>Voikko-tavuttaja</h1>\n')
	
	if hyphstring != None and len(hyphstring) > 0:
		(words, separators) = _split_words(_decode_form_value(hyphstring))
		hwords = _hyphenate_wordlist(words)
		_write(req, u'<p>Alla antamasi teksti tavutettuna:</p>\n')
		_write(req, u'<p style="border: 1px solid black">\n')
		for i in range(0, len(separators)):
			_write(req, _escape_html(hwords[i]))
			_write(req, separators[i])
		_write(req, u'</p>\n')
	
	_write(req, u'<form method="post" action="hyphenate">\n')
	_write(req, u'<p>Kirjoita alla olevaan kenttään teksti, jonka haluat tavuttaa, ja\n')
	_write(req, u'paina "Tavuta".</p>\n')
	_write(req, u'<textarea name="hyphstring" rows="30" cols="90"></textarea>\n')
	_write(req, u'<p><input type="submit" value="Tavuta" /></p>\n')
	_write(req, u'</form>\n')
	_write(req, u'</body></html>\n')
