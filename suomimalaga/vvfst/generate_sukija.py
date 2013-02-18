# -*- coding: utf-8 -*-

# Copyright 2013 Hannu Väisänen (Hannu.Vaisanen@uef.fi)
# Program to generate old spellings and common spelling mistakes for Voikko lexicon.

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


# This program generates old spellings (e.g. symbooli) and
# common spelling errors (e.g. kirjottaa) from file "all.lexc".
#
# An example: from line
# [Ln][Xp]symboli[X]symbol@P.INEN_SALLITTU.ON@:symbol@P.INEN_SALLITTU.ON@ NimisanaPaperi_a ;
#
# generate line
# [Ln][Xp]symboli[X]symbool@P.INEN_SALLITTU.ON@:symbool@P.INEN_SALLITTU.ON@ NimisanaPaperi_a 
#
# Shoud I generate these lines also?
# [Ln][Xp]symbooli[X]symbol@P.INEN_SALLITTU.ON@:symbol@P.INEN_SALLITTU.ON@ NimisanaPaperi_a ;
# [Ln][Xp]symbooli[X]symbool@P.INEN_SALLITTU.ON@:symbool@P.INEN_SALLITTU.ON@ NimisanaPaperi_a 
#
# Note that this automatic generation will generate some old
# spellings that do not exist in real life.


import codecs
import getopt
import re
import sys


try:
    optlist = ["destdir="]
    (opts, args) = getopt.getopt(sys.argv[1:], "", optlist)
except getopt.GetoptError:
    sys.stderr.write("Invalid option list for %s\n" % sys.argv[0])
    sys.exit(1)
options = {"destdir": None}
for (name, value) in opts:
    if (name == "--destdir"):
        options["destdir"] = value


infile = codecs.open (options["destdir"] + u"/all.lexc", "r", "UTF-8")
outfile = codecs.open (options["destdir"] + u"/all-sukija.lexc", 'w', 'UTF-8')

C = u"[qwrtpsšdfghjklzžxcvbnm]"  # Consonants.
V = u"[aeiouüyäö]"               # Vovels.


# Translate characters. For example,
# translate (u"AObcd", u"AO", u"äö") returns u"äöbcd"
#
def translate (s, fr, to):
    p = list (s)
    for i in range(len(s)):
        for j in range(len(fr)):
            if (s[i] == fr[j]):
                p[i] = to[j]
    return ''.join (p)


def makeRePattern (wordClass, word):
    u = u"^\\[%s\\]\\[Xp\\].*%s\\[X\\]" % (wordClass, word)
    u = u.replace ('C', C)
    u = u.replace ('V', V)
    return u


def makeRe (wordClass, word):
    return re.compile (makeRePattern (wordClass, word), re.UNICODE)


def makeRe2 (wordClass, word):
    u = makeRePattern (wordClass, word)
    v = translate (u, u"AO", u"ao")
    w = translate (u, u"AO", u"äö")
    return [re.compile (v, re.UNICODE), re.compile (w, re.UNICODE)]


def replace (s, old, new):
    u = s.replace (old + u":", new + u":")
    u = u.replace (old + u" ", new + u" ")
    u = u.replace (old + u"@", new + u"@")
    return u


def replace_and_write (line, string1, string2):
    s = replace (line, string1, string2)
    outfile.write (s)


def generate4 (line, re, string1, string2):
    if (re.match(line)):
        replace_and_write (line, string1, string2)


def generate5 (line, re, string1, string2, re_x):
    if (re.match(line) and not re_x.match(line)):
        replace_and_write (line, string1, string2)


def zreplace (line, u1, u2, v1, v2):
    replace_and_write (line.replace (v1, v2), u1, u2)


def generate6 (line, re, string1, string2, oldContinuation, newContinuation):
    if (re[0].match(line) and (line.find (oldContinuation) >= 0)):
        zreplace (line, translate(string1,u"AO",u"ao"), translate(string2,u"AO",u"ao"), oldContinuation, newContinuation)
    elif (re[1].match(line) and (line.find (oldContinuation) >= 0)):
        zreplace (line, translate(string1,u"AO",u"äö"), translate(string2,u"AO",u"äö"), oldContinuation, newContinuation)


def generate7 (line, re, string1, string2a, string2b, continue1, continue2):
    if (re.match(line)):
        s = line.replace (continue1, continue2)
        outfile.write (replace (s, string1, string2a))
        outfile.write (replace (s, string1, string2b))


def generate8 (line, re, string1, string2a, string2b, continue1, continue2, re_x):
    if (re.match(line) and not re_x.match(line)):
        s = line.replace (continue1, continue2)
        outfile.write (replace (s, string1, string2a))
        outfile.write (replace (s, string1, string2b))


re_adi = makeRe (u"Ln", u".Cadi")
re_odi = makeRe (u"Ln", u".Codi")
re_ofi = makeRe (u"Ln", u".Cofi")
re_ogi = makeRe (u"Ln", u".Cogi")
re_oli = makeRe (u"Ln", u".Coli")
re_omi = makeRe (u"Ln", u".Comi")
re_oni = makeRe (u"Ln", u".Coni")
re_ori = makeRe (u"Ln", u".Cori")

re_grafia   = makeRe (u"Ln", u"grafia")
re_grafinen = makeRe (u"Ll", u"grafinen")
re_logia    = makeRe (u"Ln", u"logia")
re_loginen  = makeRe (u"Ll", u"loginen")

re_OittAA   = makeRe2 (u"Lt", u".COittAA")
re_OttAA    = makeRe2 (u"Lt", u".COttAA")

re_oitin = makeRe (u"Ln", u".Coitin")
re_aatio = makeRe (u"Ln", u".Caatio")
re_uutio = makeRe (u"Ln", u".Cuutio")
re_uusio = makeRe (u"Ln", u".Cuusio")
re_tio   = makeRe (u"Ln", u"[ik]tio") # Traditio, funktio.

# Words to be excluded.
#
re_adi_x = re.compile (u"\\A\[Ln\]\[Xp\](dekadi|faradi|pikofaradi|stadi)\[X\]")
re_ogi_x = re.compile (u"\\A\[Ln\]\[Xp\](blogi|grogi|judogi)\[X\]")
re_omi_x = re.compile (u"\\A\[Ln\]\[Xp\](binomi|bromi|dibromi|genomi|kromi|trinomi)\[X\]")
re_oni_x = re.compile (u"\\A\[Ln\]\[Xp\](ikoni)\[X\]")
re_ori_x = re.compile (u"\\A\[Ln\]\[Xp\](hevosori|jalostusori|reettori|siitosori)\[X\]")

re_logia_x = re.compile (u"\\A\[Ln\]\[Xp\](genealogia|trilogia)\[X\]")
re_uusio_x = re.compile (u"\\A\[Ln\]\[Xp\](diffuusio)\[X\]")
re_tio_x   = re.compile (u"\\A\[Ln\]\[Xp\](aitio)\[X\]")

Joukahainen_t = re.compile (u"\\AJoukahainen_t.*;")


# Old spellings and common spelling errors of words
# that do not conform to any pattern.
#
spelling_word_list = [
    (u"alkovi",      (u"alkov",      u"alkoov")),
    (u"emali",       (u"emal",       u"emalj")),
    (u"hevonen",     (u"hevo",       u"hevoi")),
    (u"humaaninen",  (u"humaani",    u"humani")),
    (u"kamari",      (u"kamar",      u"kammar")),
    (u"kirjoitelma", (u"kirjoitelm", u"kirjotelm")),
    (u"kraatteri",   (u"kraatter",   u"kraater")),
    (u"kulttuuri",   (u"kulttuur",   u"kultuur")),
    (u"liipaisin",   (u"liipaisi",   u"liipasi")),
    (u"pioni",       (u"pion",       u"pioon")),
    (u"poliisi",     (u"poliis",     u"polis")),
    (u"poliitikko",  (u"poliitik",   u"politik")),
    (u"politiikka",  (u"politiik",   u"politik")),
    (u"preettori",   (u"preettor",   u"preetor")),
    (u"reettori",    (u"reettor",    u"reetor")),
    (u"senaatti",    (u"senaat",     u"senaat", u"NimisanaTatti_a", u"NimisanaRisti_a",
                      u"senaat",     u"senat",  u"NimisanaTatti_a", u"NimisanaRisti_a")),
    (u"serafi",      (u"seraf",      u"seraaf")),
    (u"teatteri",    (u"teatter",    u"teaatter")),
    (u"tällainen",   (u"tällai",     u"tälläi", u"NimiLaatusanaNainen_a", u"NimiLaatusanaNainen_ä"))
]


def convert_to_dictionary (word_list):
    l0 = map (lambda x : x[0], word_list)
    l1 = map (lambda x : x[1], word_list)
    return dict (zip (l0, l1))


sukija_dictionary = convert_to_dictionary (spelling_word_list)


# Extract base form from a line.
#
base_form_re = re.compile (u"\\[Xp\\]([^[]+)\\[X\\]", re.UNICODE)

def generate_word (line, sukija_dictionary):
    r = base_form_re.search (line)
    if r:
        try:
            g = sukija_dictionary[r.group(1)]
            if len (g) == 2:
                replace_and_write (line, g[0], g[1])
            elif len(g) == 4:
                s = line.replace (g[2], g[3])
                outfile.write (replace (s, g[0], g[1]))
            elif len(g) == 8:
                s = line.replace (g[2], g[3])
                outfile.write (replace (s, g[0], g[1]))
                s = line.replace (g[6], g[7])
                outfile.write (replace (s, g[4], g[5]))
            else:
                sys.stderr.write (line)
                sys.stderr.write ("Wrong format in sukija_dictionary.\n")
                sys.exit (1)
        except KeyError:  # It is not an error if a word is not in sukija_dictionary.
            pass


# Copy Voikko vocabulary.
#
while True:
    line = infile.readline()
    if line == u"":
        break;
    if (Joukahainen_t.match(line)):
        outfile.write (line + u"Sukija;\n") # Add "Sukija" to LEXICON Joukahainen.
    else:
        outfile.write (line)
infile.close()


outfile.write (u"LEXICON Sukija\n")
infile = codecs.open (options["destdir"] + u"/all.lexc", "r", "UTF-8")


# Generate LEXICON Sukija, or, old spellings and common spelling errors for indexing.
#
while True:
        line = infile.readline()
        if line == u"":
            break;
        generate5 (line, re_adi, u"ad", u"aad", re_adi_x)  # Serenadi  => senenaadi.
        generate4 (line, re_odi, u"od", u"ood")            # Aplodi    => aploodi.
        generate4 (line, re_ofi, u"of", u"oof")            # Filosofi  => filosoofi.
        generate5 (line, re_ogi, u"og", u"oog", re_ogi_x)  # Arkeologi => arkeoloogi.
        generate4 (line, re_oli, u"ol", u"ool")            # Symboli   => symbooli.
        generate5 (line, re_omi, u"om", u"oom", re_omi_x)  # Atomi     => atoomi.
        generate5 (line, re_oni, u"on", u"oon", re_oni_x)  # Telefoni  => telefooni.
        generate5 (line, re_ori, u"or", u"oor", re_ori_x)  # Pehtori   => pehtoori.

        generate4 (line, re_grafia,   u"grafi",  u"graafi")
        generate4 (line, re_grafinen, u"grafi",  u"graafi")
        generate5 (line, re_logia,    u"logi",   u"loogi", re_logia_x)
        generate4 (line, re_loginen,  u"logi",   u"loogi")

        generate6 (line, re_OittAA, u"O",   u"Ot", u"Kirjoittaa", u"Alittaa")
        generate6 (line, re_OittAA, u"Oit", u"Ot", u"Alittaa",    u"Alittaa")

        generate6 (line, re_OttAA, u"Ot", u"Oit", u"Alittaa",  u"Alittaa")
        generate6 (line, re_OttAA, u"O",  u"Oi",  u"Ammottaa", u"Ammottaa")

        generate4 (line, re_oitin, u"oit", u"ot");  # Kirjoitin => kirjotin (esim. kirjo(i)ttimen).

        generate7 (line, re_aatio, u"aatio", u"atsion", u"atsioon", u"NimisanaAutio_a", u"NimisanaPaperi_a")
        generate7 (line, re_uutio, u"uutio", u"utsion", u"utsioon", u"NimisanaAutio_a", u"NimisanaPaperi_a")
        generate8 (line, re_uusio, u"uusio", u"usion",  u"usioon",  u"NimisanaAutio_a", u"NimisanaPaperi_a", re_uusio_x)
        generate8 (line, re_tio,   u"tio",   u"tsion",  u"tsioon",  u"NimisanaAutio_a", u"NimisanaPaperi_a", re_tio_x)

        generate_word (line, sukija_dictionary)
infile.close()


# Generate old inflected forms.
#
outfile.write (u"[Ln][Xp]ahven[X]ahvenna:ahvenna # ;\n")        # Ahvenena.
outfile.write (u"[Ln][Xp]kappale[X]kappalten:kappalten # ;\n")  # Kappaleiden.
outfile.write (u"[Ln][Xp]maailma[X]maailmoitse:maailmoitse # ;\n")
outfile.write (u"[Ln][Xp]mies[X]miesnä:miesnä # ;\n")           # Miehenä.
outfile.write (u"[Ln][Xp]mies[X]miessä:miessä # ;\n")           # Miehenä.
outfile.write (u"[Ln][Xp]neiti[X]neiden:neiden # ;\n")          # Neidin.
outfile.write (u"[Lnl][Xp]nuori[X]nuorna:nuorna # ;\n")         # Nuorena.
outfile.write (u"[Lnl][Xp]nuori[X]nuorra:nuorra # ;\n")         # Nuorena.
outfile.write (u"[Ln][Xp]pieni[X]piennä:piennä # ;\n")          # Pienenä.
outfile.write (u"[Ln][Xp]sankari[X]sankarten:sankarten # ;\n")  # Sankarien.
outfile.write (u"[Ln][Xp]venäjä[X]venättä:venättä # ;\n")       # Venäjää.
