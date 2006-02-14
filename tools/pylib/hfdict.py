# -*- coding: utf-8 -*-

# Copyright 2005-2006 Harri Pitkänen (hatapitk@cc.jyu.fi)
# Library for reading and writing dictionary data for Hunspell-fi -project.
# This library requires Python version 2.4 or newer.

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

import hfutils
import codecs
import sys
import gzip

# Internal version number
DICTLEVEL=8


# PÄÄOHJELMAAN
# Sanaston vieminen tietokantaan tapahtuu psql-komennolla
# \copy sana(sana, luokka) from 'tiedosto-jossa-uusia-sanoja'

#connection = hfutils.get_db()
#if connection != None:
#	print 'Haetaan sanoja tietokannasta...'
#	tulokset = connection.query('select sana, luokka, parametri, kommentti from sana order by sana, luokka').getresult()
#	#outputfile = codecs.open('src/fi_FI-perus.dic', 'w', 'utf-8')
#	outputfile = gzip.GzipFile('src/fi_FI-perus.dic.gz', 'wb');
#	rivi = u'#HF_DICTLEVEL='+`HF_DICTLEVEL`+'\n'
#	outputfile.write(rivi.encode('utf-8'))
#	for tulos in tulokset:
#		r_sana = tulos[0].strip().decode('utf-8')
#		r_luokka = tulos[1].strip().decode('utf-8')
#		if tulos[2] == None: r_parametri = ''
#		else: r_parametri = tulos[2].strip().decode('utf-8')
#		if tulos[3] == None: r_kommentti = ''
#		else: r_kommentti = '#'+tulos[3].strip().decode('utf-8')
#		rivi = r_sana+'\t'+r_luokka+'\t'+r_parametri+'\t'+r_kommentti+'\n'
#		outputfile.write(rivi.encode('utf-8'))
#	outputfile.close()
#	connection.close()


# Returns a tuple (word_class, inflection_class, gradation_class) from a given
# specification, or None if specification is malformed.
def __get_word_classification(class_spec):
	parts = class_spec.split('-')
	if len(parts) == 3:
		wclass = parts[0]
		iclass = parts[1]
		gclass = parts[2]
	elif len(parts) == 2:
		wclass = parts[0]
		iclass = parts[1]
		gclass = '-'
	else: return None
	if not gclass in ['-', 'av1', 'av2', 'av3', 'av4', 'av5', 'av6']: return None
	if not wclass in ['part', 'subst', 'adj', 'pron', 'numer', 'verbi']: return None
	return (wclass, iclass, gclass)




def testaa_av(sana, tyyppi, astevaihtelut):
	if not astevaihtelutyyppi(tyyppi) in astevaihtelut:
		print 'Astevaihtelutyypin '+astevaihtelutyyppi(tyyppi)+u' käyttö estetty:'
		print 'sana '+sana+', tyyppi '+tyyppi
		sys.exit(1)

def yhdista_affiksit(aff1, aff2):
	yhd_affiksi = aff1
	aff_uusi = True
	for i in range(len(aff2)/2):
		for j in range(len(yhd_affiksi)/2):
			if yhd_affiksi[2*j:2*j+2] == aff2[2*i:2*i+2]:
				aff_uusi = False
		if aff_uusi:
			yhd_affiksi = yhd_affiksi + aff2[2*i:2*i+2]
	return yhd_affiksi

# Lisää sanastoon ryhmän saman sanan taivutusmuotoja, yhdistäen tarvittaessa samanlaiset vartalot
# yhdelle riville. sanat = [(sana, affiksi, onko_sallittu_ilman_affiksia), ...]
def dictlist_append_ryhma(lista, sanat, sluokkaselite):
	lopputulos = {}
	for sana in sanat:
		if lopputulos.has_key(sana[0]):
			aff = yhdista_affiksit(lopputulos[sana[0]][0], sana[1])
			sal_ilm_aff = lopputulos[sana[0]][1] or sana[2]
			lopputulos[sana[0]] = (aff, sal_ilm_aff)
		else:
			lopputulos[sana[0]] = (sana[1], sana[2])
	for vartalo, affiksi in lopputulos.iteritems():
		if affiksi[1]:
			lista.append((vartalo+'/'+affiksi[0], sluokkaselite))
		else:
			t = (vartalo+'/'+affiksi[0]+'F0', sluokkaselite)
			lista.append( t	)

def luo_astevaihtelut(sana, tyyppi, vahva_affiksi, heikko_affiksi):
	avtyyp = astevaihtelutyyppi(tyyppi)
	if avtyyp == '':
		return [ (sana, yhdista_affiksit(heikko_affiksi, vahva_affiksi), True) ]
	if avtyyp in ('av1', 'av2', 'av3', 'av4', 'av5', 'av6'):
		(vahva_v, heikko_v) = astevaihteluvartalo(sana, avtyyp)
		return [ (vahva_v, vahva_affiksi, True), (heikko_v, heikko_affiksi, False) ]
	print 'Virhe: luo_astevaihtelut: tuntematon astevaihtelutyyppi: '+sana+', '+tyyppi
	sys.exit(1)

def dictlist_append(lista, sana, tyyppi, vahva_affiksi, heikko_affiksi):
	sluokka = sanaluokka(tyyppi)
	sluokkaselite = ''
	if sluokka == 'subst':
		sluokkaselite = '[SUBST]'
	if sluokka == 'nimi':
		sluokkaselite = '[NIMI]'
	if sluokka == 'adj':
		sluokkaselite = '[ADJ]'
	if sluokka == 'verbi':
		sluokkaselite = '[VERBI]'
		vahva_affiksi = vahva_affiksi + 'F0'
	if sluokkaselite == '':
		print 'Virhe: dictlist_append: tuntematon sanaluokka: "'+sluokka+'", sana '+sana
		sys.exit(1)
	vartalot = luo_astevaihtelut(sana, tyyppi, vahva_affiksi, heikko_affiksi)
	dictlist_append_ryhma(lista, vartalot, sluokkaselite)

def muodosta_heikko_vartalo(sana, avtyyppi, taivutus):
	lis_lkm = 0
	while lis_lkm < len(taivutus) and taivutus[lis_lkm] == '+':
		lis_lkm = lis_lkm + 1
	taivutus = taivutus.replace('+', '')
	if taivutus != '-':
		lis_lkm = lis_lkm - len(taivutus)
		lisays = taivutus.replace('$', '')
	else:
		lisays = ''
	if avtyyppi != '':
		if lis_lkm == 0: sananrunko = astevaihteluvartalo(sana, avtyyppi)[1]
		else: sananrunko = astevaihteluvartalo(sana, avtyyppi)[1][:lis_lkm]
	else:
		if lis_lkm == 0: sananrunko = sana
		else: sananrunko = sana[:lis_lkm]
	if len(lisays) > 0 and lisays[0] == 'P': return sananrunko + sananrunko[-1] + lisays[1:]
	else: return sananrunko + lisays

def dictlist_append_adj(lista, tyyppi, positiivi, vahva_affiksi, heikko_affiksi, komparatiivi_t, superlatiivi_t,\
	                voktyyppi, adverbi_v = 0, ominaisuus_v = 0):
	avtyyp = astevaihtelutyyppi(tyyppi)
	komparatiivi = muodosta_heikko_vartalo(positiivi, avtyyp, komparatiivi_t)
	superlatiivi = muodosta_heikko_vartalo(positiivi, avtyyp, superlatiivi_t)
	if voktyyppi == hfutils.TAKAVOKAALI:
		komp_aff = 'U0'
		sup_aff = 'U2'
	if voktyyppi == hfutils.ETUVOKAALI:
		komp_aff = 'U1'
		sup_aff = 'U3'
	if voktyyppi == hfutils.MOLEMMAT_VOKAALIT:
		komp_aff = 'U0U1'
		sup_aff = 'U2U3'
	adv_aff = 'U4'
	vartalot = luo_astevaihtelut(positiivi, tyyppi, vahva_affiksi, heikko_affiksi)
	vartalot.append((komparatiivi, komp_aff, False))
	vartalot.append((superlatiivi, sup_aff, False))
	if adverbi_v != 0:
		adverbi = muodosta_heikko_vartalo(positiivi, avtyyp, adverbi_v)
		vartalot.append((adverbi, adv_aff, False))
	if ominaisuus_v != 0:
		if avtyyp in ('av2', 'av4', 'av6'):
			if taivutusluokka(tyyppi) == 'as2' and voktyyppi == hfutils.TAKAVOKAALI:
				ominaisuus = muodosta_heikko_vartalo(positiivi, avtyyp, '+uus')
			elif taivutusluokka(tyyppi) == 'as2' and voktyyppi == hfutils.ETUVOKAALI:
				ominaisuus = muodosta_heikko_vartalo(positiivi, avtyyp, '+yys')
			else: ominaisuus = muodosta_heikko_vartalo(positiivi, avtyyp, ominaisuus_v)
		else:
			ominaisuus = muodosta_heikko_vartalo(positiivi, '', ominaisuus_v)
		if ominaisuus[-2:] == 'us': omin_aff = '/SYSZ'
		if ominaisuus[-2:] == 'ys': omin_aff = '/TYTZ'
		lista.append((ominaisuus+omin_aff, '[SUBST]'))
	dictlist_append_ryhma(lista, vartalot, '[ADJ]')


def sananloppu_sopii(sana, loppu):
	ind = len(sana) - len(loppu)
	for c in range(0, len(loppu)):
		if loppu[c] == 'C' and not hfutils.konsonantti(sana[ind+c]): return False
		if loppu[c] == 'V' and hfutils.konsonantti(sana[ind+c]): return False
		if loppu[c] not in ('C', 'V') and loppu[c] != sana[ind+c]: return False
	return True


class Substluokka:
	def __init__(self):
		self.nimi = ''
		self.avluokat = []
		self.loput = []
		self.voktyyppi = ''
		self.vahva_v = ''
		self.heikko_v = ''
	
	def luokka_sopii(self, sana, taivluokka, avluokka, vtyyppi):
		if taivluokka != self.nimi: return False
		if not avluokka in self.avluokat: return False
		if vtyyppi != self.voktyyppi: return False
		for l in self.loput:
			if sananloppu_sopii(sana, l):
				return True
		return False

class Adjluokka:
	def __init__(self):
		self.nimi = ''
		self.avluokat = []
		self.loput = []
		self.voktyyppi = ''
		self.vahva_v = ''
		self.heikko_v = ''
		self.komparatiivi = ''
		self.superlatiivi = ''
		self.adverbi = ''
		self.ominaisuus = ''
	
	def luokka_sopii(self, sana, taivluokka, avluokka, vtyyppi):
		if taivluokka != self.nimi: return False
		if not avluokka in self.avluokat: return False
		if vtyyppi != self.voktyyppi: return False
		for l in self.loput:
			if sananloppu_sopii(sana, l):
				return True
		return False

class Verbiluokka:
	def __init__(self):
		self.nimi = ''
		self.infpaat = 0
		self.avluokat = []
		self.loput = []
		self.voktyyppi = ''
		self.vahva_v = ''
		self.heikko_v = ''
	
	def luokka_sopii(self, sana, taivluokka, avluokka, vtyyppi):
		if taivluokka != self.nimi: return False
		if not avluokka in self.avluokat: return False
		if vtyyppi != self.voktyyppi: return False
		for l in self.loput:
			if sananloppu_sopii(sana, l):
				return True
		return False

def lue_substluokat():
	inputfile = codecs.open(SUBST_LUOKAT, 'r', 'utf-8')
	tiedJatkuu = True
	substluokat = []
	while tiedJatkuu:
		rivi = inputfile.readline()
		tiedJatkuu = rivi.endswith('\n')
		rivi = hfutils.poistakommentit(rivi).strip().replace('\t\t\t', '\t').replace('\t\t', '\t')
		osat=rivi.split('\t')
		if len(rivi) == 0:
			continue
		if len(osat) != 6:
			print 'Virhe: lue_subst_luokat: virheellinen tiedosto.'
			sys.exit(1)
		sluokka = Substluokka()
		sluokka.nimi = osat[0]
		for avl in osat[1].split(','):
			if avl == '-': sluokka.avluokat.append('')
			else: sluokka.avluokat.append(avl)
		for lop in osat[2].split(','): sluokka.loput.append(lop)
		sluokka.voktyyppi = osat[3]
		sluokka.vahva_v = osat[4]
		if osat[5] == '-': sluokka.heikko_v = ''
		else: sluokka.heikko_v = osat[5]
		substluokat.append(sluokka)
	inputfile.close()
	return substluokat

def lue_adjluokat():
	inputfile = codecs.open(ADJ_LUOKAT, 'r', 'utf-8')
	tiedJatkuu = True
	adjluokat = []
	while tiedJatkuu:
		rivi = inputfile.readline()
		tiedJatkuu = rivi.endswith('\n')
		rivi = hfutils.poistakommentit(rivi).strip().replace('\t\t\t', '\t').replace('\t\t', '\t')
		osat=rivi.split('\t')
		if len(rivi) == 0:
			continue
		if len(osat) != 10:
			print 'Virhe: lue_adj_luokat: virheellinen tiedosto.'
			sys.exit(1)
		sluokka = Adjluokka()
		sluokka.nimi = osat[0]
		for avl in osat[1].split(','):
			if avl == '-': sluokka.avluokat.append('')
			else: sluokka.avluokat.append(avl)
		for lop in osat[2].split(','): sluokka.loput.append(lop)
		sluokka.voktyyppi = osat[3]
		sluokka.vahva_v = osat[4]
		if osat[5] == '-': sluokka.heikko_v = ''
		else: sluokka.heikko_v = osat[5]
		sluokka.komparatiivi = osat[6]
		sluokka.superlatiivi = osat[7]
		sluokka.adverbi = osat[8]
		sluokka.ominaisuus = osat[9]
		adjluokat.append(sluokka)
	inputfile.close()
	return adjluokat

def lue_verbiluokat():
	inputfile = codecs.open(VERBI_LUOKAT, 'r', 'utf-8')
	tiedJatkuu = True
	verbiluokat = []
	while tiedJatkuu:
		rivi = inputfile.readline()
		tiedJatkuu = rivi.endswith('\n')
		rivi = hfutils.poistakommentit(rivi).strip().replace('\t\t\t', '\t').replace('\t\t', '\t')
		osat=rivi.split('\t')
		if len(rivi) == 0:
			continue
		if len(osat) != 7:
			print 'Virhe: lue_verbi_luokat: virheellinen tiedosto.'
			sys.exit(1)
		sluokka = Substluokka()
		sluokka.nimi = osat[0]
		sluokka.infpaat = int(osat[1])
		for avl in osat[2].split(','):
			if avl == '-': sluokka.avluokat.append('')
			else: sluokka.avluokat.append(avl)
		for lop in osat[3].split(','): sluokka.loput.append(lop)
		sluokka.voktyyppi = osat[4]
		sluokka.vahva_v = osat[5]
		if osat[6] == '-': sluokka.heikko_v = ''
		else: sluokka.heikko_v = osat[6]
		verbiluokat.append(sluokka)
	inputfile.close()
	return verbiluokat


def dictoutput(osat, lista, substluokat, adjluokat, verbiluokat):
	sana = osat[0].replace(u'|', '')
	typ = muunna_kotus_luokitus(osat[1])
	vtyyp = hfutils.vokaalityyppi(sana)
	sluokka = sanaluokka(typ)
	taivluokka = taivutusluokka(typ)
	avtyyp = astevaihtelutyyppi(typ)
	lisattylistaan = False
	if sluokka in ('subst', 'nimi'):
		if vtyyp in (hfutils.TAKAVOKAALI, hfutils.MOLEMMAT_VOKAALIT):
			for s in substluokat:
				if s.luokka_sopii(sana, taivluokka, avtyyp, 'T'):
					dictlist_append(lista, sana, typ, s.vahva_v, s.heikko_v)
					lisattylistaan = True
					break
		if vtyyp in (hfutils.ETUVOKAALI, hfutils.MOLEMMAT_VOKAALIT):
			for s in substluokat:
				if s.luokka_sopii(sana, taivluokka, avtyyp, 'E'):
					dictlist_append(lista, sana, typ, s.vahva_v, s.heikko_v)
					lisattylistaan = True
					break
	if lisattylistaan:
		return
	if sanaluokka(typ)=='subst' and taivutusluokka(typ)=='poik':
		if len(osat)==3:
			lista.append((sana+osat[2], "[SUBST]"))
			return
		else:
			lista.append((sana, "[SUBST]"))
			return
	if sanaluokka(typ)=='nimi' and taivutusluokka(typ)=='poik':
		if len(osat)==3:
			lista.append((sana+osat[2], "[NIMI]"))
			return
		else:
			lista.append((sana, "[NIMI]"))
			return
	if sluokka == 'adj':
		if vtyyp in (hfutils.TAKAVOKAALI, hfutils.MOLEMMAT_VOKAALIT):
			for s in adjluokat:
				if s.luokka_sopii(sana, taivluokka, avtyyp, 'T'):
					dictlist_append_adj(lista, typ, sana, s.vahva_v, s.heikko_v, s.komparatiivi, \
					                    s.superlatiivi, hfutils.TAKAVOKAALI, s.adverbi, s.ominaisuus)
					lisattylistaan = True
					break
		if vtyyp in (hfutils.ETUVOKAALI, hfutils.MOLEMMAT_VOKAALIT):
			for s in adjluokat:
				if s.luokka_sopii(sana, taivluokka, avtyyp, 'E'):
					dictlist_append_adj(lista, typ, sana, s.vahva_v, s.heikko_v, s.komparatiivi, \
					                    s.superlatiivi, hfutils.ETUVOKAALI, s.adverbi, s.ominaisuus)
					lisattylistaan = True
					break
	if lisattylistaan:
		return
	if sluokka == 'verbi':
		if vtyyp in (hfutils.TAKAVOKAALI, hfutils.MOLEMMAT_VOKAALIT):
			for s in verbiluokat:
				if s.luokka_sopii(sana, taivluokka, avtyyp, 'T'):
					dictlist_append(lista, sana[:-s.infpaat], typ, s.vahva_v, s.heikko_v)
					lisattylistaan = True
					break
		if vtyyp in (hfutils.ETUVOKAALI, hfutils.MOLEMMAT_VOKAALIT):
			for s in verbiluokat:
				if s.luokka_sopii(sana, taivluokka, avtyyp, 'E'):
					dictlist_append(lista, sana[:-s.infpaat], typ, s.vahva_v, s.heikko_v)
					lisattylistaan = True
					break
	if lisattylistaan:
		return
	if typ=='part-prep':
		if len(osat)==3:
			lista.append((sana+osat[2], "[PREP]"))
			return
		else:
			lista.append((sana, "[PREP]"))
			return
	if typ=='part-erill':
		lista.append((sana, "[PART_ERILLINEN]"))
		return
	if typ=='numer':
		if len(osat)==3:
			lista.append((sana+osat[2], "[NUMER]"))
			return
		else:
			lista.append((sana, "[NUMER]"))
			return
	if typ=='pron':
		if len(osat)==3:
			lista.append((sana+osat[2], "[PRON]"))
			return
		else:
			lista.append((sana, "[PRON]"))
			return
	if typ=='merkkisana':
		return
	if typ in ('subst-luokitt', 'adj-luokitt', 'subst-apua', 'nimi-luokitt', 'nimi-apua', 'adj-apua', 'part-luokitt'):
		lista.append((sana, '[LUOKITTELEMATON]'))
		return
	print u'Virhe: Ei löydy affiksisääntöä sanalle '+sana+', tyyppi '+typ
	sys.exit(1)



# Appends a word represented by parts to list word_list. input_file and
# class lists may be used to read additional information about the word.
def __append_to_wordlist(parts, word_list, input_file, noun_classes):
	class_fields = __get_word_classification(parts[1])
	if class_fields == None:
		print 'Malformed word class: ' + parts[1]
		sys.exit(1)
	word = parts[0]
	if class_fields[0] == 'subst':
		# TODO
		return
	if class_fields[0] == 'part':
		if class_fields[1] == 'erill':
			word_list.append((word, '[PART_ERILLINEN]'))
			return
	if class_fields[0] == 'merkkisana':
		return
	# TODO: other word classes and error message if an incorrect class was given.


# Appends a list of words from input dictionary input_file_name to list
# word_list.
def __list_words(input_file_name, word_list, noun_classes):
	is_compressed = input_file_name.endswith('.gz')
	if is_compressed: input_file = gzip.GzipFile(input_file_name, 'rb')
	else: input_file = codecs.open(input_file_name, 'r', hfutils.INPUT_ENCODING)
	data_left = True
	line_no = 0
	while data_left:
		if is_compressed: line = input_file.readline().decode(hfutils.INPUT_ENCODING)
		else: line = input_file.readline()
		if line_no == 0 and line.startswith('HF_DICTLEVEL='):
			file_dictlevel = int(line[14:])
			if line_dictlevel > DICTLEVEL:
				print 'File ' + input_file_name + ' requires a newer version of ' + \
				      'dictionary utilies.'
				sys.exit(1)
		line_no = line_no + 1
		data_left = line.endswith('\n')
		line = hfutils.remove_comments(line).strip()
		if len(line) == 0: continue
		parts = line.split('\t')
		if len(parts) == 1:
			print 'Error in input dictionary ' + input_file_name + \
			      ' on line ' + `line_no`
			sys.exit(1)
		__append_to_wordlist(parts, word_list, input_file, noun_classes)
	input_file.close()

# Public functions

# Creates a Hunspell dictionary based on given list of input files and
# noun classes.
def write_dictionary(input_file_names, output_file_name, noun_classes):
	word_list = []
	for input_file_name in input_file_names:
		__list_words(input_file_name, word_list, noun_classes)
	word_list.sort()
	# TODO: dictionary compression should be done here
	output_file = codecs.open(output_file_name, 'w', hfutils.OUTPUT_ENCODING)
	output_file.write(`len(word_list)` + '\n')
	for word in word_list:
		output_file.write(word[0] + '\t' + word[1] + '\n')
	output_file.close()
