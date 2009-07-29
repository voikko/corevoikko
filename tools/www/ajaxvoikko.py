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
<script type="text/javascript" src="http://www.google.com/jsapi"></script>
<script>

function updateReceived(html) {
  $("#result").html(html);
}

function inputChanged() {
  var text = $("#input").val();
  $.get("/spell", {q: text}, updateReceived, "html");
}

google.load("jquery", "1.3.2");
google.setOnLoadCallback(function() { jQuery(function($) {
  $("#input").keyup(inputChanged);
});});
</script>
<style type="text/css">
.error {
  color: red;
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

def spell(text):
	voikko = _voikko
	tokens = mergeDots(voikko.tokens(text))
	res = u""
	for token in tokens:
		if token.tokenType == Token.WORD:
			if not voikko.spell(token.tokenText):
				res = res + u"<span class='error'>"
				res = res + escape(token.tokenText)
				res = res + u"</span>"
				continue
		res = res + escape(token.tokenText)
	return res.replace(u"\n", u"<br />")

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
