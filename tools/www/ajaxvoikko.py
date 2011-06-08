# -*- coding: utf-8 -*-

# Copyright 2009 - 2011 Harri Pitkänen (hatapitk@iki.fi)
# Web server that provides Ajax interface for using Voikko.
# Requires Python version 2.5 or newer and Python interface to libvoikko.

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# This server implementation is single threaded.

from BaseHTTPServer import BaseHTTPRequestHandler
from BaseHTTPServer import HTTPServer
from urllib import unquote, urlencode
from urllib import urlopen
from cgi import parse_qs
from cgi import escape
from sys import stderr
from signal import signal, SIGHUP
from libvoikko import Voikko
from libvoikko import Token
from voikkostatistics import getStatistics
from voikkohtml import getHtmlSafely, parseHtml, HttpException
from voikkohtml import SEGMENT_TYPE_HEADING, SEGMENT_TYPE_LIST_ITEM, SEGMENT_TYPE_PARAGRAPH, SEGMENT_TYPE_OTHER
from HTMLParser import HTMLParseError
import codecs

_voikko = {}

ALLOWED_DICTS = [u"fi-x-standard+debug", u"fi-x-medicine"]

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

NUMBERS = {
"singular": u"yksikkö",
"plural": u"monikko"
}

PERSONS = {
"1": u"ensimmäinen",
"2": u"toinen",
"3": u"kolmas",
"4": u"passiivi"
}

MOODS = {
"indicative": u"indikatiivi eli tositapa",
"imperative": u"imperatiivi eli käskytapa",
"conditional": u"konditionaali eli ehtotapa",
"potential": u"potentiaali eli mahtotapa",
"A-infinitive": u"A-infinitiivi",
"E-infinitive": u"E-infinitiivi",
"MA-infinitive": u"MA-infinitiivi"
}

TENSES = {
"present_simple": u"preesens",
"past_imperfective": u"imperfekti"
}

PARTICIPLES = {
"present_active": u"nykyaika, aktiivi (-VA)",
"present_passive": u"nykyaika, passiivi (-VA)",
"past_active": u"mennyt aika, aktiivi (-NUT)",
"past_passive": u"mennyt aika, passiivi (-TU)",
"agent": u"agenttipartisiippi (-MA)",
"negation": u"kieltopartisiippi (-TON)"
}

POSSESSIVES = {
"1s": u"yksikön ensimmäinen persoona",
"2s": u"yksikön toinen persoona",
"1p": u"monikon ensimmäinen persoona",
"2p": u"monikon toinen persoona",
"3": u"kolmas persoona"
}

COMPARISONS = {
"positive": u"positiivi",
"comparative": u"komparatiivi",
"superlative": u"superlatiivi"
}

FOCUSES = {
"kin": u"kin",
"kaan": u"kAAn"
}

WORD_INFO_URL = u"http://joukahainen.puimula.org/word/edit?wid="

"""Maximum number of bytes allowed in incoming POST requests"""
MAX_DOCUMENT_BYTES = 50000


def fromMapIfPossible(key, valueMap):
	if key in valueMap:
		return valueMap[key]
	else:
		return key

def nonOverlappingGrammarErrorsByStartPosition(text, v):
	errors = v.grammarErrors(text)
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

def grammarErrorDetails(grammarError, v):
	errorCode = grammarError.errorCode
	errorText = v.grammarErrorExplanation(errorCode, "fi")
	if len(grammarError.suggestions) == 0:
		return errorText
	else:
		if not errorText.endswith(u"."):
			errorText = errorText + u"."
		return errorText + u' Ehkäpä ennemminkin "...' + \
		       u'...", "...'.join(grammarError.suggestions) + u'..."?'

def spell(text, dictionary):
	if dictionary not in _voikko:
		return u""
	v = _voikko[dictionary]
	v.setAcceptUnfinishedParagraphsInGc(True)
	v.setAcceptTitlesInGc(False)
	v.setAcceptBulletedListsInGc(False)
	return doSpell(text, v, True)

def doSpell(text, v, checkGrammar):
	tokens = markTrailingDots(v.tokens(text))
	gErrors = {}
	if checkGrammar:
		gErrors = nonOverlappingGrammarErrorsByStartPosition(text, v)
	res = u""
	position = 0
	currentGError = None
	for token in tokens:
		if position in gErrors:
			currentGError = gErrors[position]
			errorText = grammarErrorDetails(currentGError, v)
			res = res + u"<span class='gErrorOuter' " \
			      + u"errortext='" + escapeAttr(errorText) + u"'>"
		if token.tokenType == Token.WORD:
			if v.spell(token.tokenText) or \
			   (token.dotFollows and v.spell(token.tokenText + u".")):
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

def log(message):
	stderr.write("LOG: " + message + "\n")

def formatNumber(number):
	return (u"%.2f" % number).replace(u".", u",")

def stats(text, dictionary):
	if dictionary not in _voikko:
		return u""
	v = _voikko[dictionary]
	stats = getStatistics(text, v)
	if stats[u"WORDS"] < 50:
		return u""
	return u"<br /><hr />Tekstin luettavuus (Wiion yksinkertaisen luokkatasokaavan mukaan) " + formatNumber(stats[u"WIIO_SIMPLE"]) + u"."

def escapeAttr(word):
	return escape(word).replace(u"'", u"&#39;").replace(u'"', u"&#34;")

def suggestions(word, v):
	suggs = v.suggest(word)
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
	if "NUMBER" in analysis:
		res = res + u"<br />Luku: " \
		      + fromMapIfPossible(analysis["NUMBER"], NUMBERS)
	if "SIJAMUOTO" in analysis and analysis["SIJAMUOTO"] != "none":
		res = res + u"<br />Sijamuoto: " \
		      + fromMapIfPossible(analysis["SIJAMUOTO"], SIJAMUODOT)
	if "PERSON" in analysis:
		res = res + u"<br />Tekijä: " \
		      + fromMapIfPossible(analysis["PERSON"], PERSONS)
	if "MOOD" in analysis:
		res = res + u"<br />Tapaluokka tai nominaalimuoto: " \
		      + fromMapIfPossible(analysis["MOOD"], MOODS)
	if "TENSE" in analysis:
		res = res + u"<br />Aikamuoto: " \
		      + fromMapIfPossible(analysis["TENSE"], TENSES)
	if "PARTICIPLE" in analysis:
		res = res + u"<br />Partisiippi: " \
		      + fromMapIfPossible(analysis["PARTICIPLE"], PARTICIPLES)
	if "POSSESSIVE" in analysis:
		res = res + u"<br />Omistusliite: " \
		      + fromMapIfPossible(analysis["POSSESSIVE"], POSSESSIVES)
	if "COMPARISON" in analysis:
		res = res + u"<br />Vertailumuoto: " \
		      + fromMapIfPossible(analysis["COMPARISON"], COMPARISONS)
	if "KYSYMYSLIITE" in analysis:
		res = res + u"<br />Kysymysliite: kO"
	if "FOCUS" in analysis:
		res = res + u"<br />Fokuspartikkeli: " \
		      + fromMapIfPossible(analysis["FOCUS"], FOCUSES)
	if "WORDIDS" in analysis:
		ids = analysis["WORDIDS"]
		if u"(w" in ids:
			res = res + u"<br />Rakenne: " \
			      + wordIdsToHtml(ids)
	return res

def analyzeWord(word, v):
	analysisList = v.analyze(word.replace(u"\u00AD", u""))
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

def wordInfo(word, dictionary):
	res = u"<div title='Tietoja sanasta %s'>" % escapeAttr(word)
	if dictionary not in _voikko:
		return res + u"Sisäinen virhe: sanasto ei ole käytettävissä.</div>"
	v = _voikko[dictionary]
	isRecognized = v.spell(word)
	if not isRecognized:
		isRecognized = v.spell(word + u".")
		if isRecognized:
			word = word + u"."
	if not isRecognized:
		res = res + u"Sana on tuntematon."
		suggs = suggestions(word, v)
		if suggs is not None:
			res = res + u" Tarkoititko kenties" + suggs
	else:
		res = res + analyzeWord(word, v)
	res = res + "</div>"
	return res

def checkPage(url, dictionary, clientIp, requestHeaders):
	log("checkPage: " + url.encode("UTF-8"))
	if dictionary not in _voikko:
		return u""
	v = _voikko[dictionary]
	try:
		html = getHtmlSafely(url.encode('UTF-8'), clientIp, requestHeaders)
		segments = parseHtml(html)
		res = u"Analyysi sivusta " + escape(url) + u"<br />"
		v.setAcceptUnfinishedParagraphsInGc(True)
		for segment in segments:
			segmentClass = None
			checkGrammar = True
			if segment[0] == SEGMENT_TYPE_HEADING:
				v.setAcceptTitlesInGc(True)
				v.setAcceptBulletedListsInGc(False)
				segmentClass = u"webvoikkoH"
			elif segment[0] == SEGMENT_TYPE_LIST_ITEM:
				v.setAcceptTitlesInGc(False)
				v.setAcceptBulletedListsInGc(True)
				segmentClass = u"webvoikkoLi"
			elif segment[0] == SEGMENT_TYPE_PARAGRAPH:
				v.setAcceptTitlesInGc(False)
				v.setAcceptBulletedListsInGc(False)
				segmentClass = u"webvoikkoP"
			elif segment[0] == SEGMENT_TYPE_OTHER:
				checkGrammar = False
				segmentClass = u"webvoikkoO"
			res = res + u"<p class='" + segmentClass + u"'>" + doSpell(segment[1], v, checkGrammar) + u"</p>"
		return res
	except HttpException, e:
		return u"Sivua %s ei voitu hakea: %s" % (escape(url), e.parameter)
	except HTMLParseError, e:
		res = u"Sivun %s html-koodin tulkinta epäonnistui: %s<br />" % (escape(url), e)
		res = res + u"WebVoikon toiminta edellyttää, että sivun oleellisimmat tekstielementit "
		res = res + u"(otsikot, tekstikappaleet ja luetelmat) on oikein merkitty. WebVoikko osaa "
		res = res + u"vain rajoitetusti tulkita virheellistä html-koodia. "
		res = res + u"<a target='_blank' href='http://validator.w3.org/check?%s'>" % urlencode({"uri": url})
		res = res + u"Tarkista html-koodi W3C:n validaattorilla.</a>"
		return res

def getPortlet():
	html = u"<p>Valitse sanasto: <select id='voikkoDict'>"
	dictMap = {}
	for d in Voikko.listDicts():
		tag = d.language + u"-x-" + d.variant
		if tag in ALLOWED_DICTS:
			dictMap[ALLOWED_DICTS.index(tag)] = u"<option value='" + tag + u"'>" + escape(d.description) + u"</option>"
	for i in range(0, len(ALLOWED_DICTS)):
		if i in dictMap:
			html = html + dictMap[i]
	html = html + u"</select></p>"
	html = html + u"<div id='tabs'>"
	html = html + u"<ul><li><a href='#tabDirect'>Kirjoita teksti</a></li><li><a href='#tabPage'>Oikolue www-sivu <span style='color: red'>(beta)</span></a></li></ul>"
	html = html + u"<div id='tabDirect'>"
	html = html + u"<textarea id='input' rows='5'></textarea><br />"
	html = html + u"<button onclick='clearClicked()'>Tyhjennä</button>"
	html = html + u"</div>"
	html = html + u"<div id='tabPage'>"
	html = html + u"Oikoluettavan sivun osoite: <input type='text' id='pageUrl' size='40' /> <button id='checkPageClicked'>Oikolue</button>"
	html = html + u"</div>"
	html = html + u"</div>"
	html = html + u"<span id='progress' class='updating'>Analysoidaan... <img src='progress.gif' /></span>"
	html = html + u"<p>Lue analyysin tulokset alta. Lisää tietoja sanoista tai virheistä saat hiirellä napsauttamalla.</p>"
	html = html + u"<div id='result'></div>"
	return html

FILES_TO_SERVE = {
	"/": ("ajaxvoikko-index.html", "text/html"),
	"/style.css": ("ajaxvoikko-style.css", "text/css"),
	"/script.js": ("ajaxvoikko-script.js", "text/javascript")
}

def parseQuery(queryString, attrName):
	attrs = parse_qs(queryString)
	if attrName not in attrs:
		return u""
	values = attrs[attrName]
	if len(values) != 1:
		return u""
	return unicode(values[0], "UTF-8")

class VoikkoHandler(BaseHTTPRequestHandler):
	def sendHtmlPage(self, content, contentType):
		self.send_response(200)
		self.send_header("Content-Type", contentType + "; charset=UTF-8")
		self.end_headers()
		self.wfile.write(content.encode("UTF-8"))
	
	def sendBinary(self, content, contentType):
		self.send_response(200)
		self.send_header("Content-Type", contentType)
		self.end_headers()
		self.wfile.write(content)
	
	def serveFiles(self):
		path = self.path.split("?")[0]
		if path not in FILES_TO_SERVE:
			return False
		fileName, contentType = FILES_TO_SERVE[path]
		file = codecs.open(fileName, "r", "UTF-8")
		content = file.read()
		file.close()
		self.sendHtmlPage(content, contentType)
		return True
	
	def serveBinaries(self):
		if self.path != "/progress.gif":
			return False
		f = open("progress.gif", "rb")
		content = f.read()
		f.close()
		self.sendBinary(content, "image/gif")
		return True
	
	def serveUrl(self, url):
		stream = urlopen(url)
		content = stream.read()
		stream.close()
		self.sendHtmlPage(unicode(content, "UTF-8"), "text/html")
		return True
	
	def do_GET(self):
		if self.serveFiles():
			return
		if self.serveBinaries():
			return
		elif self.path.startswith("/portlet"):
			self.sendHtmlPage(getPortlet(), "text/html")
		elif self.path.startswith("/wordinfo?q="):
			query = self.path[10:]
			self.sendHtmlPage(wordInfo(parseQuery(query, "q"), parseQuery(query, "d")), "text/html")
		elif self.path.startswith("/joukahainen?wid="):
			query = unicode(unquote(self.path[17:]), "UTF-8")
			wid = int(query)
			self.serveUrl(WORD_INFO_URL + `wid`)
		else:
			self.send_response(404)
			self.end_headers()
	
	def do_POST(self):
		if self.path.startswith("/spell"):
			contentLength = int(self.headers.getheader('content-length'))
			queryData = self.rfile.read(min(contentLength, MAX_DOCUMENT_BYTES))
			self.sendHtmlPage(spell(parseQuery(queryData, "q"), parseQuery(queryData, "d")) + stats(parseQuery(queryData, "q"), parseQuery(queryData, "d")), "text/html")
		elif self.path.startswith("/checkPage"):
			contentLength = int(self.headers.getheader('content-length'))
			queryData = self.rfile.read(min(contentLength, MAX_DOCUMENT_BYTES))
			clientIp = self.client_address[0]
			headers = self.headers.headers
			self.sendHtmlPage(checkPage(parseQuery(queryData, "url"), parseQuery(queryData, "d"), clientIp, headers), "text/html")
		else:
			self.send_response(404)
			self.end_headers()

def runServer(port):
	try:
		server = HTTPServer(("", port), VoikkoHandler)
		server.serve_forever()
	except KeyboardInterrupt:
		server.socket.close()

def initVoikko():
	global _voikko
	for allowedDict in ALLOWED_DICTS:
		v = Voikko(allowedDict)
		v.setIgnoreDot(False)
		v.setAcceptUnfinishedParagraphsInGc(True)
		_voikko[allowedDict] = v

def uninitVoikko():
	global _voikko
	for v in _voikko.values():
		v.terminate()
	_voikko.clear()

def reinitVoikko():
	uninitVoikko()
	initVoikko()

def sighupHandler(signum, frame):
	print u"Received SIGHUP, reloading vocabulary files."
	reinitVoikko()

if __name__ == '__main__':
	initVoikko()
	signal(SIGHUP, sighupHandler)
	runServer(8080)
	uninitVoikko()
