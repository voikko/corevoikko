# This data is based on Suomi-malaga 0.7 by Hannu Väisänen, and includes
# modifications from Harri Pitkänen, Teemu Likonen and others.
#
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


# Etuliitteitä. Lisätessäsi etuliitteitä
# mieti, minkä sanaluokan sanoille kyseistä etuliitettä todella
# tarvitaan. Tämä parantaa oikoluvun laatua, varsinkin lyhyiden etuliitteiden
# kanssa.

# Kaikissa etuliitteissä perusmuoto ja alku ovat samat, luokka = etuliite ja
# äs = aä.

# == Näitä voi käyttää jatko-kentissä ==
# @eln: etuliite (nimisanat)
# @ell: etuliite (laatusanat)
# @elt: etuliite (teonsanat)
# @eltj: etuliite (teonsanojen nimi- ja laatusanajohdokset)
# Huom! Jatkoa @eltj EI SAA käyttää yhdessä jatkon @eln tai @ell kanssa.
# FIXME: Nimisanojen etuliite kelpaa myös partisiipeille ja laatusanojen
# etuliite verbien -minen-substantiivijohdoksille.

# tiedot-listassa voi käyttää symbolia ei_ysj estämään etuliitteen käyttö
# muualla kuin sanan alussa.

# Etuliitteille, joissa on yhdysviiva lopussa ja jotka ovat sanastossa myös
# omina nimi- tai laatusanoina tai etuliitteinä (esim. "jazz-", "online-")
# kannatta laittaa tiedot-kenttään symboli ei_sukija, koska Sukija-versiossa
# yhdysviiva kelpaa  minkä tahansa etuliitteen jälkeen.

[perusmuoto: "uima", alku: "uima", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "ulko", alku: "ulko", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "ulos", alku: "ulos", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "ulos", alku: "ulos", luokka: etuliite, jatko: @eltj, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "ultra", alku: "ultra", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "umpi", alku: "umpi", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "uppo", alku: "uppo", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "uudelleen", alku: "uudelleen", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "uudelleen", alku: "uudelleen", luokka: etuliite, jatko: @eltj, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "uudis", alku: "uudis", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "uus", alku: "uus", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_ysj>];
[perusmuoto: "uusio", alku: "uusio", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "vaivais", alku: "vaivais", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "vaaleanpuna", alku: "vaaleanpuna", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "vaaleanpurppura", alku: "vaaleanpurppura", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "vaaleansini", alku: "vaaleansini", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "valko", alku: "valko", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "vammais", alku: "vammais", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "vapaa", alku: "vapaa", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "varhais", alku: "varhais", luokka: etuliite, jatko: @eln, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "varo", alku: "varo", luokka: etuliite, jatko: @eln, äs: aä];
[perusmuoto: "vastaan", alku: "vastaan", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "vastakkais", alku: "vastakkais", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "veneeri", alku: "veneeri", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "vertais", alku: "vertais", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "vierekkäis", alku: "vierekkäis", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "viher", alku: "viher", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "vihki", alku: "vihki", luokka: etuliite, jatko: @eln, äs: aä];
[perusmuoto: "vihkimä", alku: "vihkimä", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "viitois", alku: "viitois", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "virpoma", alku: "virpoma", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "virtuaali", alku: "virtuaali", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "vuos", alku: "vuos", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "vähimmäis", alku: "vähimmäis", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "väliaikais", alku: "väliaikais", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>, rakenne: "=pppp=pppppp"];
[perusmuoto: "väärin", alku: "väärin", luokka: etuliite, jatko: @eltj, äs: aä];
[perusmuoto: "yhdys", alku: "yhdys", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "yhteen", alku: "yhteen", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "yhteis", alku: "yhteis", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "yhtä", alku: "yhtä", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "yksin", alku: "yksin", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "yksittäis", alku: "yksittäis", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "yksityis", alku: "yksityis", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "yksöis", alku: "yksöis", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "yleis", alku: "yleis", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "ylen", alku: "ylen", luokka: etuliite, jatko: @eln + @ell + @elt, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "yli", alku: "yli", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "ylitse", alku: "ylitse", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "yltiö", alku: "yltiö", luokka: etuliite, jatko: @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "ylä", alku: "ylä", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "ylön", alku: "ylön", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "ylös", alku: "ylös", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "ympäri", alku: "ympäri", luokka: etuliite, jatko: @eltj, äs: aä];
[perusmuoto: "äkki", alku: "äkki", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "äsken", alku: "äsken", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "ääreis", alku: "ääreis", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "öky", alku: "öky", luokka: etuliite, jatko: @eln + @ell, äs: aä];
