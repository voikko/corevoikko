# -*- coding: utf-8 -*-

# Copyright 2009 Harri Pitkänen (hatapitk@iki.fi)
# Web server that provides Ajax interface for using Voikko.
# Requires Python version 2.5 or newer and Python interface to libvoikko.

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

from BaseHTTPServer import BaseHTTPRequestHandler
from BaseHTTPServer import HTTPServer
from urllib import unquote_plus
from cgi import escape
from libvoikko import Voikko
from libvoikko import Token

_STATIC_PAGE = \
u"""
<html>
<head>
<title>WebVoikko 2.0</title>
<link type="text/css"
 href="http://jqueryui.com/latest/themes/base/ui.all.css"
 rel="stylesheet" />
<script type="text/javascript" src="http://www.google.com/jsapi"></script>
<script>

function wordInfoReceived(html) {
  $(html).dialog().show();
}

function wordClicked(evt) {
  var word = $(this).text()
  console.log(word);
  $.get("/wordinfo", {q: word}, wordInfoReceived, "html");
}

function updateReceived(html) {
  $("#result").html(html);
  $("#result .word").click(wordClicked);
}

function inputChanged() {
  var text = $("#input").val();
  $.get("/spell", {q: text}, updateReceived, "html");
}

google.load("jquery", "1.3.2");
google.load("jqueryui", "1.7.2");
google.setOnLoadCallback(function() { jQuery(function($) {
  $("#input").keyup(inputChanged);
});});
</script>
<style type="text/css">
span.error {
  color: red;
}
span.grammarerror {
  text-decoration: underline;
  color: blue;
}
.word {
  color: black;
  -moz-border-radius: 3px;
}
span.word:hover {
  background-color: #CCCCEE;
  cursor: help;
}
</style>
</head>
<body>
<textarea id="input" cols="80" rows="5"></textarea>
<pre id="result"></pre>
</body>
</html>
"""

_voikko = None

def mergeDots(tokenList):
	newList = []
	i = 0
	l = len(tokenList)
	while i < l:
		if i < l - 1 \
		   and tokenList[i].tokenType == Token.WORD \
		   and tokenList[i+1].tokenText == u".":
			word = tokenList[i].tokenText + u"."
			newList.append(Token(word, Token.WORD))
			i = i + 2
		else:
			newList.append(tokenList[i])
			i = i + 1
	return newList

def nonOverlappingGrammarErrorsByStartPosition(text):
	errors = _voikko.grammarErrors(text)
	nonOverlapping = {}
	lastEnd = 0
	for error in errors:
		if error.startPos < lastEnd:
			continue
		nonOverlapping[error.startPos] = error
		lastEnd = error.startPos + error.errorLen
	return nonOverlapping

def spell(text):
	tokens = mergeDots(_voikko.tokens(text))
	gErrors = nonOverlappingGrammarErrorsByStartPosition(text)
	res = u""
	position = 0
	currentGError = None
	for token in tokens:
		if position in gErrors:
			currentGError = gErrors[position]
			res = res + u"<span class='grammarerror'>"
		if token.tokenType == Token.WORD:
			if not _voikko.spell(token.tokenText):
				res = res + u"<span class='word error'>" \
				      + escape(token.tokenText) \
				      + u"</span>"
			else:
				res = res + u"<span class='word'>" \
				      + escape(token.tokenText) \
				      + u"</span>"
		else:
			res = res + escape(token.tokenText)
		position = position + len(token.tokenText)
		if currentGError != None and \
		   currentGError.startPos + currentGError.errorLen == position:
			currentGError = None
			res = res + u"</span>"
	return res.replace(u"\n", u"<br />")

def escapeAttr(word):
	return escape(word).replace(u"'", u"&#39;").replace(u'"', u"&#34;")

def suggestions(word):
	suggs = _voikko.suggest(word)
	res = u"<ul>"
	for sugg in suggs:
		res = res + u"<li>" + escape(sugg) + u"</li>"
	res = res + "</ul>"
	return res

def getAnalysis(analysis):
	res = u""
	if "CLASS" in analysis:
		res = res + u"Sanaluokka: " + analysis["CLASS"]
	if "SIJAMUOTO" in analysis and analysis["SIJAMUOTO"] != "none":
		res = res + u"<br />Sijamuoto: " + analysis["SIJAMUOTO"]
	return res

def analyzeWord(word):
	analysisList = _voikko.analyze(word)
	if len(analysisList) == 0 and word.endswith(u"."):
		analysisList = _voikko.analyze(word[:-1])
	if len(analysisList) == 0:
		return u""
	if len(analysisList) == 1:
		return getAnalysis(analysisList[0])
	
	res = u"Sanalla on useita merkityksiä: <ol>"
	for analysis in analysisList:
		res = res + u"<li> " \
		      + getAnalysis(analysis) \
		      + u"</li>"
	res = res + u"</ol>"
	return res

def wordInfo(word):
	res = u"<div title='Tietoja sanasta %s'>" % escapeAttr(word)
	if not _voikko.spell(word):
		res = res + u"Sana on tuntematon. Tarkoititiko kenties" \
		      + suggestions(word)
	else:
		res = res + analyzeWord(word)
	res = res + "</div>"
	return res

class VoikkoHandler(BaseHTTPRequestHandler):
	def do_GET(self):
		if self.path == "/":
			self.send_response(200)
			self.send_header("Content-Type", "text/html; charset=UTF-8")
			self.end_headers()
			self.wfile.write(_STATIC_PAGE.encode("UTF-8"))
		elif self.path.startswith("/spell?q="):
			self.send_response(200)
			self.send_header("Content-Type", "text/html; charset=UTF-8")
			self.end_headers()
			query = unicode(unquote_plus(self.path[9:]), "UTF-8")
			self.wfile.write(spell(query).encode("UTF-8"))
		elif self.path.startswith("/wordinfo?q="):
			self.send_response(200)
			self.send_header("Content-Type", "text/html; charset=UTF-8")
			self.end_headers()
			query = unicode(unquote_plus(self.path[12:]), "UTF-8")
			self.wfile.write(wordInfo(query).encode("UTF-8"))
		else:
			self.send_response(404)
			self.end_headers()

def runServer(port):
	try:
		server = HTTPServer(("", port), VoikkoHandler)
		server.serve_forever()
	except KeyboardInterrupt:
		server.socket.close()

if __name__ == '__main__':
	_voikko = Voikko()
	_voikko.init()
	_voikko.setIgnoreDot(True)
	runServer(8080)
	_voikko.terminate()
