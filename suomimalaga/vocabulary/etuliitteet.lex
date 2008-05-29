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

[perusmuoto: "aero", alku: "aero", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "agro", alku: "agro", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "aikakaus", alku: "aikakaus", luokka: etuliite, jatko: @eln + @ell, äs: aä, rakenne: "=pppp=pppp"];
[perusmuoto: "ainais", alku: "ainais", luokka: etuliite, jatko: @eln, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "ajantasa", alku: "ajantasa", luokka: etuliite, jatko: @eln + @ell, äs: aä, rakenne: "=pppp=pppp"];
[perusmuoto: "alas", alku: "alas", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "ali", alku: "ali", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "alkeis", alku: "alkeis", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "alkuperäis", alku: "alkuperäis", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>, rakenne: "=pppp=pppppp"];
[perusmuoto: "alkuun", alku: "alkuun", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_voikko>]; # Alkuunpanija jne.
[perusmuoto: "alle", alku: "alle", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "amerikan", alku: "amerikan", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "amfibio", alku: "amfibio", luokka: etuliite, jatko: @eln, äs: aä, tiedot: <ei_sukija>];   # On sana: "amfibio lentokone".
[perusmuoto: "ammonium", alku: "ammonium", luokka: etuliite, jatko: @eln, äs: aä, tiedot: <ei_voikko>]; # -karbonaatti, -suola jne.
[perusmuoto: "ampuma", alku: "ampuma", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "anarko", alku: "anarko", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_voikko>]; # -kapitalismi, -kommunismi.
[perusmuoto: "anglo", alku: "anglo", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];   # On sana: "anglo saksi".
[perusmuoto: "anti", alku: "anti", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "approbatur-", alku: "approbatur-", luokka: etuliite, jatko: @eln, äs: aä, rakenne: "=pp=pppppppp-", tiedot: <ei_sukija>];
[perusmuoto: "astraali", alku: "astraali", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "astro", alku: "astro", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "asuin", alku: "asuin", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "auki", alku: "auki", luokka: etuliite, jatko: @eltj, äs: aä, tiedot: <ei_voikko, ei_ysj>];
[perusmuoto: "avo", alku: "avo", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "bi-", alku: "bi-", luokka: etuliite, jatko: @eln + @ell, äs: aä, rakenne: "=pp-"];
[perusmuoto: "bio", alku: "bio", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "blues-", alku: "blues-", luokka: etuliite, jatko: @eln, äs: aä, rakenne: "=ppppp-", tiedot: <ei_sukija>];
[perusmuoto: "digi", alku: "digi", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "digitaali", alku: "digitaali", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "edestakais", alku: "edestakais", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>, rakenne: "=pppp=pppppp"];
[perusmuoto: "ei-", alku: "ei-", luokka: etuliite, jatko: @eln + @ell, äs: aä, rakenne: "=pp-"];
[perusmuoto: "eko", alku: "eko", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <siv>];
[perusmuoto: "elektro", alku: "elektro", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "enimmäis", alku: "enimmäis", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "enkel", alku: "enkel", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "ennen", alku: "ennen", luokka: etuliite, jatko: @eln + @ell + @ell, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "ensi", alku: "ensi", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "ensiö", alku: "ensiö", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "epä", alku: "epä", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "erikois", alku: "erikois", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "erillis", alku: "erillis", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "erityis", alku: "erityis", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "esi", alku: "esi", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "esiin", alku: "esiin", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "etno", alku: "etno", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "etä", alku: "etä", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "fenno", alku: "fenno", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "feodaali", alku: "feodaali", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "ferro", alku: "ferro", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "finanssi", alku: "finanssi", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "finn", alku: "finn", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "geo", alku: "geo", luokka: etuliite, jatko: @eln + @ell, äs: aä]; # Aiheuttaa joidenkin väärien sanojen tunnistumisen, mm. *geograafia.
[perusmuoto: "golf-", alku: "golf-", luokka: etuliite, jatko: @eln, äs: aä, rakenne: "=pppp-", tiedot: <ei_sukija>];
[perusmuoto: "haja-", alku: "haja-", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, rakenne: "=pppp-", tiedot: <ei_voikko>];
[perusmuoto: "haja", alku: "haja", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "haukkuma", alku: "haukkuma", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "hautoma", alku: "hautoma", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "herras", alku: "herras", luokka: etuliite, jatko: @eln, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "hieroma", alku: "hieroma", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "hioma", alku: "hioma", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "huippu", alku: "huippu", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "hydro", alku: "hydro", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "hyper", alku: "hyper", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "hyvin", alku: "hyvin", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "iki", alku: "iki", luokka: etuliite, jatko: @eln + @ell, äs: ä];
[perusmuoto: "imaginaari", alku: "imaginaari", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "immateriaali", alku: "immateriaali", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "improbatur-", alku: "improbatur-", luokka: etuliite, jatko: @eln, äs: aä, rakenne: "=pp=pppppppp-", tiedot: <ei_sukija>];
[perusmuoto: "indo", alku: "indo", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "infra", alku: "infra", luokka: etuliite, jatko: @eln + @ell, äs: aä]; # Lähes turha.
[perusmuoto: "inva", alku: "inva", luokka: etuliite, jatko: @ell, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "inva", alku: "inva", luokka: etuliite, jatko: @eln, äs: aä];
[perusmuoto: "irti", alku: "irti", luokka: etuliite, jatko: @eln + @ell + @elt, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "irto", alku: "irto", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "irvi", alku: "irvi", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "islami", alku: "islami", luokka: etuliite, jatko: @eln, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "istuma", alku: "istuma", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "itseis", alku: "itseis", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "jazz-", alku: "jazz-", luokka: etuliite, jatko: @eln, äs: aä, rakenne: "=pppp-", tiedot: <ei_sukija>];
[perusmuoto: "jouto", alku: "jouto", luokka: etuliite, jatko: @eln, äs: aä];
[perusmuoto: "julki", alku: "julki", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "jumal", alku: "jumal", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "jälleen", alku: "jälleen", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "jälleen", alku: "jälleen", luokka: etuliite, jatko: @eltj, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "kaakkois", alku: "kaakkois", luokka: etuliite, jatko: @eln, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "kaatuma", alku: "kaatuma", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "kaiken", alku: "kaiken", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "kaksin", alku: "kaksin", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "kaksin", alku: "kaksin", luokka: etuliite, jatko: @elt + @eln, äs: aä, tiedot: <ei_voikko>]; # -kamppailla, -kamppailu, -peli
[perusmuoto: "kaksois", alku: "kaksois", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "kanssa", alku: "kanssa", luokka: etuliite, jatko: @eln + @ell, äs: aä]; # Mahdollisesti vain verbin subst.- ja adj.johdoksiin
[perusmuoto: "karjo", alku: "karjo", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "kauko", alku: "kauko", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "kausaali", alku: "kausaali", luokka: etuliite, jatko: @eln, äs: aä];
[perusmuoto: "kautta", alku: "kautta", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "kesken", alku: "kesken", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "keski", alku: "keski", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "kierteis", alku: "kierteis", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "kiinni", alku: "kiinni", luokka: etuliite, jatko: @eltj, äs: aä];
[perusmuoto: "kiinto", alku: "kiinto", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "koe", alku: "koe", luokka: etuliite, jatko: @elt, äs: aä];
[perusmuoto: "koillis", alku: "koillis", luokka: etuliite, jatko: @eln, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "kollegiaali", alku: "kollegiaali", luokka: etuliite, jatko: @eln, äs: aä];
[perusmuoto: "kolmi", alku: "kolmi", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "kolmois", alku: "kolmois", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "kontra", alku: "kontra", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "kosio", alku: "kosio", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "kotiin", alku: "kotiin", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "kukkais", alku: "kukkais", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "kuolin", alku: "kuolin", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "kuorima", alku: "kuorima", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "kutoma", alku: "kutoma", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "kutsuma", alku: "kutsuma", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "kvasi", alku: "kvasi", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "kyynär", alku: "kyynär", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "käsin", alku: "käsin", luokka: etuliite, jatko: @eltj, äs: aä];
[perusmuoto: "käänteis", alku: "käänteis", luokka: etuliite, jatko: @eln, äs: aä];
[perusmuoto: "kääntymä", alku: "kääntymä", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "laudatur-", alku: "laudatur-", luokka: etuliite, jatko: @eln, äs: aä, rakenne: "=pppppppp-", tiedot: <ei_sukija>];
[perusmuoto: "lehmi", alku: "lehmi", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "leivin", alku: "leivin", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "liehu", alku: "liehu", luokka: etuliite, jatko: @eln, äs: aä];
[perusmuoto: "liikkuma", alku: "liikkuma", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "live-", alku: "live-", luokka: etuliite, jatko: @eln, äs: aä, rakenne: "=pppp-", tiedot: <ei_sukija>];
[perusmuoto: "lounais", alku: "lounais", luokka: etuliite, jatko: @eln, äs: aä];
[perusmuoto: "luontais", alku: "luontais", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "luoteis", alku: "luoteis", luokka: etuliite, jatko: @eln, äs: aä];
[perusmuoto: "lyömä", alku: "lyömä", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "lähi", alku: "lähi", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "läsnä", alku: "läsnä", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "maalis", alku: "maalis", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "mentaali", alku: "mentaali", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "mestaris", alku: "mestaris", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "meta", alku: "meta", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "militaari", alku: "militaari", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "morsius", alku: "morsius", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "muinais", alku: "muinais", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "multi", alku: "multi", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "myöhäis", alku: "myöhäis", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "myötä", alku: "myötä", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "naima", alku: "naima", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "nano", alku: "nano", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "nukkuma", alku: "nukkuma", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "nyky", alku: "nyky", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "näennäis", alku: "näennäis", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "ohi", alku: "ohi", luokka: etuliite, jatko: @eltj, äs: a];
[perusmuoto: "oikein", alku: "oikein", luokka: etuliite, jatko: @eltj, äs: a];
[perusmuoto: "oiko", alku: "oiko", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "olympia", alku: "olympia", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "ominais", alku: "ominais", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "online", alku: "online", luokka: etuliite, jatko: @eln + @ell, äs: aä, rakenne: "=pp=pppp"];
[perusmuoto: "online-", alku: "online-", luokka: etuliite, jatko: @eln + @ell, äs: aä, rakenne: "=pp=pppp-", tiedot: <ei_sukija>];
[perusmuoto: "osittais", alku: "osittais", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "osuma", alku: "osuma", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "pappis", alku: "pappis", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "perille", alku: "perille", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "perään", alku: "perään", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "pesimä", alku: "pesimä", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "pienois", alku: "pienois", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "pika", alku: "pika", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "pikku", alku: "pikku", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "pitkittäis", alku: "pitkittäis", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "poikittais", alku: "poikittais", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "poikki", alku: "poikki", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "pois", alku: "pois", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "pois", alku: "pois", luokka: etuliite, jatko: @eltj, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "poissa-", alku: "poissa-", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, rakenne: "=pppppp-", tiedot: <ei_voikko>];
[perusmuoto: "poly", alku: "poly", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "pop-", alku: "pop-", luokka: etuliite, jatko: @eln, äs: aä, rakenne: "=ppp-", tiedot: <ei_sukija>];
[perusmuoto: "porvaris", alku: "porvaris", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "post-", alku: "post-", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, rakenne: "=pppp-", tiedot: <ei_voikko>];
[perusmuoto: "pre-", alku: "pre-", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, rakenne: "=ppp-", tiedot: <ei_voikko>];
[perusmuoto: "pseudo", alku: "pseudo", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "puima", alku: "puima", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "puoli", alku: "puoli", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "puolittais", alku: "puolittais", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "pysyväis", alku: "pysyväis", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "päälle", alku: "päälle", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "raitio", alku: "raitio", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "rationaali", alku: "rationaali", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "refleksiivi", alku: "refleksiivi", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "riippu", alku: "riippu", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "rinnakkais", alku: "rinnakkais", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "ristimä", alku: "ristimä", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "rock-", alku: "rock-", luokka: etuliite, jatko: @eln, äs: aä, rakenne: "=pppp-", tiedot: <ei_sukija>];
[perusmuoto: "rouvas", alku: "rouvas", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "sala", alku: "sala", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "satunnais", alku: "satunnais", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "seisoma", alku: "seisoma", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "seitsen", alku: "seitsen", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "seka", alku: "seka", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "seksuaali", alku: "seksuaali", luokka: etuliite, jatko: @eln + @ell, äs: a];
[perusmuoto: "sijais", alku: "sijais", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "sisällis", alku: "sisällis", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "sisään", alku: "sisään", luokka: etuliite, jatko: @eltj, äs: aä];
[perusmuoto: "sivuttais", alku: "sivuttais", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "sosiaali", alku: "sosiaali", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "sosio", alku: "sosio", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "sovinnais", alku: "sovinnais", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "squash-", alku: "squash-", luokka: etuliite, jatko: @eln, äs: aä, rakenne: "=pppppp-", tiedot: <ei_sukija>];
[perusmuoto: "suhu", alku: "suhu", luokka: etuliite, jatko: @eln, äs: aä];
[perusmuoto: "super", alku: "super", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "supra", alku: "supra", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "suur", alku: "suur", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "synnyin", alku: "synnyin", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "syys", alku: "syys", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "syömä", alku: "syömä", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "taka", alku: "taka", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "takaisin", alku: "takaisin", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "tarttuma", alku: "tarttuma", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "tasa", alku: "tasa", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "tekno", alku: "tekno", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "telemark-", alku: "telemark-", luokka: etuliite, jatko: @eln, äs: aä, rakenne: "=pppp=pppp-", tiedot: <ei_sukija>];
[perusmuoto: "termo", alku: "termo", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "tieteis", alku: "tieteis", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "tois", alku: "tois", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "toivio", alku: "toivio", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "totaali", alku: "totaali", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "tämän-", alku: "tämän-", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_voikko>, rakenne: "=ppppp-"];
[perusmuoto: "täsmä", alku: "täsmä", luokka: etuliite, jatko: @eln + @ell, äs: aä]; 
[perusmuoto: "täys", alku: "täys", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "uima", alku: "uima", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "ulko", alku: "ulko", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "ulos", alku: "ulos", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "ulos", alku: "ulos", luokka: etuliite, jatko: @eltj, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "ultra", alku: "ultra", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "umpi", alku: "umpi", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "uppo", alku: "uppo", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "uudelleen", alku: "uudelleen", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "uudelleen", alku: "uudelleen", luokka: etuliite, jatko: @eltj, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "uudis", alku: "uudis", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "uus", alku: "uus", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_ysj>];
[perusmuoto: "uusio", alku: "uusio", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "vaivais", alku: "vaivais", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
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
[perusmuoto: "yltiö", alku: "yltiö", luokka: etuliite, jatko: @ell, äs: aä];
[perusmuoto: "ylä", alku: "ylä", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "ylön", alku: "ylön", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "ylös", alku: "ylös", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "ympäri", alku: "ympäri", luokka: etuliite, jatko: @eltj, äs: aä];
[perusmuoto: "äkki", alku: "äkki", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "äsken", alku: "äsken", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "ääreis", alku: "ääreis", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "öky", alku: "öky", luokka: etuliite, jatko: @eln + @ell, äs: aä];
