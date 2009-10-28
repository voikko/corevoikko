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
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
 "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html>
<head>
<title>WebVoikko 2.0</title>
<link type="text/css"
 href="http://jqueryui.com/latest/themes/base/ui.all.css"
 rel="stylesheet" />
<script type="text/javascript" src="http://www.google.com/jsapi"></script>
<script>

function joukahainen(wid) {
  var options = {
    title: "Joukahainen"
  }
  var frame = "<iframe src='http://joukahainen.puimula.org/word/edit?wid="
              + wid + "'></iframe>";
  $(frame).dialog(options).show();
}

function wordInfoReceived(html) {
  var options = {
    width: 450
  };
  $(html).dialog(options).show();
}

function wordClicked(evt) {
  var word = $(this).text();
  $.get("/wordinfo", {q: word}, wordInfoReceived, "html");
}

function gErrorClicked(evt) {
  var options = {
    width: 450,
    title: "Mahdollinen kielioppivirhe"
  };
  var outerElement = $(this).parent()
  var original = $("<span />").text(outerElement.find(".gErrorInner").text());
  var explanation = outerElement.attr("errortext");
  var html = "<div>... " + original.html() + " ...<br />"
             + explanation + "</div>";
  $(html).dialog(options).show();
}

function buildGrammarError(outerElement) {
  var content = outerElement.html();
  outerElement.html("*<span class='gErrorInner'>" + html + "</span>");
}

function updateReceived(html) {
  $("#result").html(html);
  $("#result .gErrorOuter").wrapInner("<span class='gErrorInner'></span>");
  $("#result .gErrorOuter").prepend("<span class='gErrorHandle'>*</span>");
  $("#result .word").click(wordClicked);
  $("#result .gErrorHandle").click(gErrorClicked);
}

function inputChanged() {
  var text = $("#input").val();
  $.get("/spell", {q: text}, updateReceived, "html");
}

function keyUpInInput(evt) {
  if (evt.keyCode >= 16 && evt.keyCode <= 40) {
    // Modifier keys such as Ctrl
    // Movement keys such as arrow left etc.
    return;
  }
  inputChanged();
}

google.load("jquery", "1.3.2");
google.load("jqueryui", "1.7.2");
google.setOnLoadCallback(function() { jQuery(function($) {
  $("#input").keyup(keyUpInInput);
});});
</script>
<style type="text/css">
#result {
  line-height: 1.6em;
}
span.error {
  color: red;
}
span.gErrorOuter {
  padding-bottom: 0.3em;
  background-color: #FFBBBB;
}
span.gErrorInner {
  background-color: white;
}
.word {
  color: black;
  background-color: white;
  -moz-border-radius: 0.2em;
}
span.word:hover, span.gErrorHandle:hover {
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

SANALUOKAT = {
"asemosana": u"pronomini eli asemosana",
"nimisana": u"substantiivi (yleisnimi)",
"etunimi": u"substantiivi (etunimi)",
"sukunimi": u"substantiivi (sukunimi)",
"paikannimi": u"substantiivi (paikannimi)",
"nimi": u"substantiivi (luokittelematon erisnimi)",
"laatusana": u"adjektiivi eli laatusana",
"nimisana_laatusana": u"substantiivi (yleisnimi) tai adjektiivi eli laatusana",
"lukusana": u"numeraali eli lukusana",
"teonsana": u"verbi eli teonsana",
"seikkasana": u"adverbi eli seikkasana",
"suhdesana": u"partikkeli (suhdesana)",
"huudahdussana": u"partikkeli (huudahdussana)",
"sidesana": u"partikkeli (konjunktio eli sidesana)",
"kieltosana": u"partikkeli (kieltosana)",
"lyhenne": u"lyhenne"
}

SIJAMUODOT = {
"nimento": u"nominatiivi eli nimentö",
"omanto": u"genetiivi eli omanto",
"osanto": u"partitiivi eli osanto",
"olento": u"essiivi eli olento",
"tulento": u"translatiivi eli tulento",
"kohdanto": u"akkusatiivi eli kohdanto",
"sisaolento": u"inessiivi eli sisäolento",
"sisaeronto": u"elatiivi eli sisäeronto",
"sisatulento": u"illatiivi eli sisätulento",
"ulkoolento": u"adessiivi eli ulko-olento",
"ulkoeronto": u"ablatiivi eli ulkoeronto",
"ulkotulento": u"allatiivi eli ulkotulento",
"vajanto": u"abessiivi eli vajanto",
"seuranto": u"komitatiivi eli seuranto",
"keinonto": u"instruktiivi eli keinonto",
"kerrontosti": u"sti-päätteinen kerronto (adverbi)"
}

WORD_INFO_URL = u"http://joukahainen.puimula.org/word/edit?wid="

def fromMapIfPossible(key, valueMap):
	if key in valueMap:
		return valueMap[key]
	else:
		return key

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

def markTrailingDots(tokenList):
	newList = []
	i = 0
	l = len(tokenList)
	while i < l:
		token = tokenList[i]
		if i < l - 1 \
		   and tokenList[i].tokenType == Token.WORD \
		   and tokenList[i+1].tokenText == u".":
			token.dotFollows = True
		else:
			token.dotFollows = False
		newList.append(token)
		i = i + 1
	return newList

def spell(text):
	tokens = markTrailingDots(_voikko.tokens(text))
	gErrors = nonOverlappingGrammarErrorsByStartPosition(text)
	res = u""
	position = 0
	currentGError = None
	for token in tokens:
		if position in gErrors:
			currentGError = gErrors[position]
			errorCode = currentGError.errorCode
			errorText = _voikko.grammarErrorExplanation(errorCode, "fi")
			res = res + u"<span class='gErrorOuter' " \
			      + u"errortext='" + escapeAttr(errorText) + u"'>"
		if token.tokenType == Token.WORD:
			if _voikko.spell(token.tokenText) or \
			   (token.dotFollows and _voikko.spell(token.tokenText + u".")):
				res = res + u"<span class='word'>" \
				      + escape(token.tokenText) \
				      + u"</span>"
			else:
				res = res + u"<span class='word error'>" \
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
	if len(suggs) == 0:
		return None
	res = u"<ul>"
	for sugg in suggs:
		res = res + u"<li>" + escape(sugg) + u"</li>"
	res = res + "</ul>"
	return res

def idPartToAttrMap(idPart):
	parts = idPart.split(u"(")
	res = {"word": parts[0]}
	if len(parts) > 1:
		for attr in parts[1:]:
			res[attr[0]] = attr[1:-1]
	return res

def wordIdsToHtml(wordIds):
	res = u""
	for part in wordIds.split(u"+"):
		attrs = idPartToAttrMap(part)
		if u"w" in attrs:
			res = res + u'+<a href="javascript:joukahainen(' + \
			      attrs['w'] + u')">' + escape(attrs["word"]) + u'</a>'
		else:
			res = res + u'+' + escape(attrs["word"])
	return res[2:]

def getAnalysis(analysis):
	res = u""
	if "BASEFORM" in analysis:
		res = res + u"Perusmuoto: " \
		      + escape(analysis["BASEFORM"])
	if "CLASS" in analysis:
		res = res + u"<br />Sanaluokka: " \
		      + fromMapIfPossible(analysis["CLASS"], SANALUOKAT)
	if "SIJAMUOTO" in analysis and analysis["SIJAMUOTO"] != "none":
		res = res + u"<br />Sijamuoto: " \
		      + fromMapIfPossible(analysis["SIJAMUOTO"], SIJAMUODOT)
	if "WORDIDS" in analysis:
		ids = analysis["WORDIDS"]
		if u"(w" in ids:
			res = res + u"<br />Rakenne: " \
			      + wordIdsToHtml(ids)
	return res

def analyzeWord(word):
	analysisList = _voikko.analyze(word)
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
	isRecognized = _voikko.spell(word)
	if not isRecognized:
		isRecognized = _voikko.spell(word + u".")
		if isRecognized:
			word = word + u"."
	res = u"<div title='Tietoja sanasta %s'>" % escapeAttr(word)
	if not isRecognized:
		res = res + u"Sana on tuntematon."
		suggs = suggestions(word)
		if suggs is not None:
			res = res + u" Tarkoititko kenties" + suggs
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
	_voikko.init(variant='standard+debug')
	_voikko.setIgnoreDot(False)
	_voikko.setAcceptUnfinishedParagraphsInGc(True)
	runServer(8080)
	_voikko.terminate()
