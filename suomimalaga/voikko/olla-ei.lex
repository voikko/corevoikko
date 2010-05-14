# Suomi-malaga, suomen kielen muoto-opin kuvaus.
#
# Tekijänoikeus © 2006-2007 Hannu Väisänen (Etunimi.Sukunimi@joensuu.fi)
#
# Tämä ohjelma on vapaa; tätä ohjelmaa on sallittu levittää
# edelleen ja muuttaa GNU yleisen lisenssin (GPL lisenssin)
# ehtojen mukaan sellaisina kuin Free Software Foundation
# on ne julkaissut; joko Lisenssin version 2, tai (valinnan
# mukaan) minkä tahansa myöhemmän version mukaisesti.
#
# Tätä ohjelmaa levitetään siinä toivossa, että se olisi
# hyödyllinen, mutta ilman mitään takuuta; ilman edes
# hiljaista takuuta kaupallisesti hyväksyttävästä laadusta tai
# soveltuvuudesta tiettyyn tarkoitukseen. Katso GPL
# lisenssistä lisää yksityiskohtia.
#
# Tämän ohjelman mukana pitäisi tulla kopio GPL
# lisenssistä; jos näin ei ole, kirjoita osoitteeseen Free
# Software Foundation Inc., 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA.
#
# Tämän ohjeman linkittäminen staattisesti tai dynaamisesti
# muihin moduuleihin on ohjelmaan perustuvan teoksen
# tekemistä, joka on siis GPL lisenssin ehtojen alainen.
#
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; see the file COPYING.  If not, write to the
# Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.
#
# Linking this program statically or dynamically with other modules is
# making a combined work based on this program.  Thus, the terms and
# conditions of the GNU General Public License cover the whole
# combination.


# Olla-teonsana ja kieltosana.


[alku: "liene", luokka: teonsana, jatko: <lienee>, perusmuoto: "olla", äs: ä,
                tapaluokka: ehtotapa];
[alku: "lie",   luokka: teonsana, jatko: <liitesana, loppu>, perusmuoto: "olla",
                äs: ä, tapaluokka: ehtotapa];
[alku: "liene", luokka: teonsana, jatko: <liitesana, liitesana2, loppu>, perusmuoto: "olla",
                äs: ä, tapaluokka: ehtotapa];

[alku: "ol",  luokka: teonsana, jatko: <olla>, perusmuoto: "olla", äs: a, tiedot: <ei_sukija>];
[alku: "o",   luokka: teonsana, jatko: <olla>, perusmuoto: "olla", äs: a, tiedot: <ei_voikko>];

[alku: "ol",  luokka: teonsana, jatko: <loppu>, perusmuoto: "olla",  # Oli.
              äs: a, tapaluokka: tositapa, aikamuoto: kertoma,
              luku: yksikkö, tekijä: 3, tiedot: <murre>];

# Olla-sanan ehtotavan kestämä on myös tällainen.
#
[alku: "oisin", luokka: teonsana, jatko: <liitesana, loppu>, perusmuoto: "olla",
                äs: a, tapaluokka: ehtotapa, aikamuoto: kestämä,
                luku: yksikkö, tekijä: 1, tiedot: <murre>];
[alku: "oisit", luokka: teonsana, jatko: <liitesana, loppu>, perusmuoto: "olla",
                äs: a, tapaluokka: ehtotapa, aikamuoto: kestämä,
                luku: yksikkö, tekijä: 2, tiedot: <murre>];
[alku: "oisi",  luokka: teonsana, jatko: <liitesana, loppu>, perusmuoto: "olla",
                äs: a, tapaluokka: ehtotapa, aikamuoto: kestämä,
                luku: yksikkö, tekijä: 3, tiedot: <murre>];
[alku: "ois",   luokka: teonsana, jatko: <liitesana, loppu>, perusmuoto: "olla",
                äs: a, tapaluokka: ehtotapa, aikamuoto: kestämä,
                luku: yksikkö, tekijä: 3, tiedot: <murre>];

[alku: "oisimme", luokka: teonsana, jatko: <liitesana, loppu>, perusmuoto: "olla",
                  äs: a, tapaluokka: ehtotapa, aikamuoto: kestämä,
                  luku: monikko, tekijä: 1, tiedot: <murre>];
[alku: "oisitte", luokka: teonsana, jatko: <liitesana, loppu>, perusmuoto: "olla",
                  äs: a, tapaluokka: ehtotapa, aikamuoto: kestämä,
                  luku: monikko, tekijä: 2, tiedot: <murre>];
[alku: "oisivat", luokka: teonsana, jatko: <liitesana, loppu>, perusmuoto: "olla",
                  äs: a, tapaluokka: ehtotapa, aikamuoto: kestämä, 
                  luku: monikko, tekijä: 3, tiedot: <murre>];


# Olla-sanan tositavan kestämä on epäsäännöllinen.
#
[alku: "olen", luokka: teonsana, jatko: <liitesana, loppu>, perusmuoto: "olla",
               äs: a, tapaluokka: tositapa, aikamuoto: kestämä,
               luku: yksikkö, tekijä: 1];
[alku: "olet", luokka: teonsana, jatko: <liitesana, loppu>, perusmuoto: "olla",
               äs: a, tapaluokka: tositapa, aikamuoto: kestämä,
               luku: yksikkö, tekijä: 2];
[alku: "on",   luokka: teonsana, jatko: <liitesana, loppu>, perusmuoto: "olla",
               äs: a, tapaluokka: tositapa, aikamuoto: kestämä,
               luku: yksikkö, tekijä: 3];
[alku: "om",   luokka: teonsana, jatko: <liitesana_pi>, perusmuoto: "olla",
               äs: a, tapaluokka: tositapa, aikamuoto: kestämä,
               luku: yksikkö, tekijä: 3];

[alku: "oo",   luokka: teonsana, jatko: <loppu>, perusmuoto: "olla",    # Ei oo.
               äs: a, tapaluokka: tositapa, aikamuoto: kestämä,
               luku: yksikkö, tekijä: 3, tiedot: <murre>];


[alku: "olemme", luokka: teonsana, jatko: <liitesana, loppu>, perusmuoto: "olla",
                 äs: a, tapaluokka: tositapa, aikamuoto: kestämä,
                 luku: monikko, tekijä: 1];
[alku: "olette", luokka: teonsana, jatko: <liitesana, loppu>, perusmuoto: "olla",
                 äs: a, tapaluokka: tositapa, aikamuoto: kestämä,
                 luku: monikko, tekijä: 2];
[alku: "ovat",   luokka: teonsana, jatko: <liitesana, loppu>, perusmuoto: "olla",
                 äs: a, tapaluokka: tositapa, aikamuoto: kestämä, 
                 luku: monikko, tekijä: 3];

[alku: "ollaan", luokka: teonsana, jatko: <liitesana, loppu>, perusmuoto: "olla",
                 äs: a, tapaluokka: tositapa, aikamuoto: kestämä, tekijä: 4];

[alku: "oltiin", luokka: teonsana, jatko: <liitesana, loppu>, perusmuoto: "olla",
                 äs: a, tapaluokka: tositapa, aikamuoto: kertoma, tekijä: 4];


[alku: "oltane", luokka: teonsana, jatko: <liitesana, loppu>, perusmuoto: "olla",
                 äs: a, tapaluokka: mahtotapa, aikamuoto: kestämä, tekijä: 4];

[alku: "oltaneen", luokka: teonsana, jatko: <liitesana, loppu>, perusmuoto: "olla",
                   äs: a, tapaluokka: mahtotapa, aikamuoto: kestämä, tekijä: 4];

# "Ollos huoleton, poikas valveil' on..." (optatiivi)
#
[alku: "ollos", luokka: teonsana, jatko: <loppu>, perusmuoto: "olla",
                äs: a, tapaluokka: käskytapa, tekijä: 2, luku: yksikkö, tiedot: <murre>];


# Kieltosana.
#
[alku: "en",         luokka: kieltosana, jatko: <kieltosanan_liitesana, loppu>,
                     perusmuoto: "ei", tapaluokka: tositapa,
                     aikamuoto: kestämä, tekijä: 1, luku: yksikkö];
[alku: "et",         luokka: kieltosana, jatko: <kieltosanan_liitesana, loppu>,
                     perusmuoto: "ei", tapaluokka: tositapa,
                     aikamuoto: kestämä, tekijä: 2, luku: yksikkö];
[alku: "ei",         luokka: kieltosana, jatko: <kieltosanan_liitesana, loppu>,
                     perusmuoto: "ei", tapaluokka: tositapa,
                     aikamuoto: kestämä, tekijä: 3, luku: yksikkö];
[alku: "emme",       luokka: kieltosana, jatko: <kieltosanan_liitesana, loppu>,
                     perusmuoto: "ei", tapaluokka: tositapa,
                     aikamuoto: kestämä, tekijä: 1, luku: monikko];
[alku: "ette",       luokka: kieltosana, jatko: <kieltosanan_liitesana, loppu>,
                     perusmuoto: "ei", tapaluokka: tositapa,
                     aikamuoto: kestämä, tekijä: 2, luku: monikko];
[alku: "eivät",      luokka: kieltosana, jatko: <kieltosanan_liitesana, loppu>,
                     perusmuoto: "ei", äs: ä, tapaluokka: tositapa,
                     aikamuoto: kestämä, tekijä: 3, luku: monikko];


[alku: "ehk",  luokka: kieltosanan_etuliite, jatko: <>, perusmuoto: "ehk"];
[alku: "ell",  luokka: kieltosanan_etuliite, jatko: <>, perusmuoto: "ell"];
[alku: "ett",  luokka: kieltosanan_etuliite, jatko: <>, perusmuoto: "ett"];
[alku: "ehk'", luokka: kieltosanan_etuliite, jatko: <>, perusmuoto: "ehk'"];
[alku: "ell'", luokka: kieltosanan_etuliite, jatko: <>, perusmuoto: "ell'"];
[alku: "ett'", luokka: kieltosanan_etuliite, jatko: <>, perusmuoto: "ett'"];

[alku: "joll",  luokka: kieltosanan_etuliite, jatko: <>, perusmuoto: "joll",  äs: a];
[alku: "jott",  luokka: kieltosanan_etuliite, jatko: <>, perusmuoto: "jott",  äs: a];
[alku: "joll'", luokka: kieltosanan_etuliite, jatko: <>, perusmuoto: "joll'", äs: a];
[alku: "jott'", luokka: kieltosanan_etuliite, jatko: <>, perusmuoto: "jott'", äs: a];

[alku: "miks",  luokka: kieltosanan_etuliite, jatko: <>, perusmuoto: "miks"];
[alku: "miks'", luokka: kieltosanan_etuliite, jatko: <>, perusmuoto: "miks'"];

[alku: "vaikk",  luokka: kieltosanan_etuliite, jatko: <>, perusmuoto: "vaikk",  äs: a];
[alku: "vaikk'", luokka: kieltosanan_etuliite, jatko: <>, perusmuoto: "vaikk'", äs: a];

[alku: "kosk'", luokka: kieltosanan_etuliite, jatko: <>, perusmuoto: "kosk'", äs: a];
[alku: "kosk",  luokka: kieltosanan_etuliite, jatko: <>, perusmuoto: "kosk", äs: a];

[alku: "mutt'", luokka: kieltosanan_etuliite, jatko: <>, perusmuoto: "mutt'", äs: a];
[alku: "mutt",  luokka: kieltosanan_etuliite, jatko: <>, perusmuoto: "mutt", äs: a];


[alku: "älä",        luokka: kieltosana, jatko: <kieltosanan_liitesana, loppu>,
                     perusmuoto: "ei", äs: ä, tapaluokka: käskytapa,
                     aikamuoto: kestämä, tekijä: 2, luku: yksikkö];
[alku: "älköön",     luokka: kieltosana, jatko: <kieltosanan_liitesana, loppu>,
                     perusmuoto: "ei", äs: ä, tapaluokka: käskytapa,
                     aikamuoto: kestämä, tekijä: 3, luku: yksikkö];
[alku: "älkäämme",   luokka: kieltosana, jatko: <kieltosanan_liitesana, loppu>,
                     perusmuoto: "ei", äs: ä, tapaluokka: käskytapa,
                     aikamuoto: kestämä, tekijä: 1, luku: monikko];
[alku: "älkää",      luokka: kieltosana, jatko: <kieltosanan_liitesana, loppu>,
                     perusmuoto: "ei", äs: ä, tapaluokka: käskytapa,
                     aikamuoto: kestämä, tekijä: 2, luku: monikko];
[alku: "älkööt",     luokka: kieltosana, jatko: <kieltosanan_liitesana, loppu>,
                     perusmuoto: "ei", äs: ä, tapaluokka: käskytapa,
                     aikamuoto: kestämä, tekijä: 3, luku: monikko];

[alku: "olko", luokka: teonsana, jatko: <loppu>, perusmuoto: "olla", äs: a];  # Älkää olko.


# "Ällös itke, oma kulta..." (optatiivi)
#
[alku: "ällös", luokka: kieltosana, jatko: <loppu>,
                äs: ä, tapaluokka: käskytapa, tekijä: 2, luku: yksikkö, tiedot: <murre>];

[alku: "olem", luokka: nimisana, jatko: <asema>, perusmuoto: "olema", äs: a];
