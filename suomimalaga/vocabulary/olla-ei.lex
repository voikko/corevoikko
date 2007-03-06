# Suomi-malaga, suomen kielen muoto-opin kuvaus.
#
# Tekijänoikeus © 2006 Hannu Väisänen (Etunimi.Sukunimi@joensuu.fi)
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
# Software Foundation Inc., 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.
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
# Free Software Foundation, Inc., 59 Temple Place -  Suite 330, Boston, MA
# 02111-1307 USA.
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

[alku: "ol",  luokka: teonsana, jatko: <olla>, perusmuoto: "olla", äs: a];


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

# "Ollos huoleton, poikas valveil' on..."
#
[alku: "ollos", luokka: teonsana, jatko: <loppu>, perusmuoto: "olla",
                äs: a, tapaluokka: käskytapa, tekijä: 2, luku: yksikkö, tiedot: <murre>];


# Kieltosana.
#
[alku: "en",         luokka: kieltosana, jatko: <liitesana, liitesana_kä, loppu>,
                     perusmuoto: "ei", äs: ä, tapaluokka: käskytapa,
                     aikamuoto: kestämä, tekijä: 1, luku: yksikkö];
[alku: "et",         luokka: kieltosana, jatko: <liitesana, liitesana_kä, loppu>,
                     perusmuoto: "ei", äs: ä, tapaluokka: käskytapa,
                     aikamuoto: kestämä, tekijä: 2, luku: yksikkö];
[alku: "ei",         luokka: kieltosana, jatko: <liitesana, liitesana_kä, loppu>,
                     perusmuoto: "ei", äs: ä, tapaluokka: käskytapa,
                     aikamuoto: kestämä, tekijä: 3, luku: yksikkö];
[alku: "emme",       luokka: kieltosana, jatko: <liitesana, liitesana_kä, loppu>,
                     perusmuoto: "ei", äs: ä, tapaluokka: käskytapa,
                     aikamuoto: kestämä, tekijä: 1, luku: monikko];
[alku: "ette",       luokka: kieltosana, jatko: <liitesana, liitesana_kä, loppu>,
                     perusmuoto: "ei", äs: ä, tapaluokka: käskytapa,
                     aikamuoto: kestämä, tekijä: 2, luku: monikko];
[alku: "eivät",      luokka: kieltosana, jatko: <liitesana, liitesana_kä, loppu>,
                     perusmuoto: "ei", äs: ä, tapaluokka: käskytapa,
                     aikamuoto: kestämä, tekijä: 3, luku: monikko];


[alku: "ehk",  luokka: etuliite, jatko: <kieltosana>, perusmuoto: "ehk",  äs: ä];
[alku: "ell",  luokka: etuliite, jatko: <kieltosana>, perusmuoto: "ell",  äs: ä];
[alku: "ett",  luokka: etuliite, jatko: <kieltosana>, perusmuoto: "ett",  äs: ä];
[alku: "ehk'", luokka: etuliite, jatko: <kieltosana>, perusmuoto: "ehk'", äs: ä];
[alku: "ell'", luokka: etuliite, jatko: <kieltosana>, perusmuoto: "ell'", äs: ä];
[alku: "ett'", luokka: etuliite, jatko: <kieltosana>, perusmuoto: "ett'", äs: ä];

[alku: "joll",  luokka: etuliite, jatko: <kieltosana>, perusmuoto: "joll",  äs: ä];
[alku: "jott",  luokka: etuliite, jatko: <kieltosana>, perusmuoto: "jott",  äs: ä];
[alku: "joll'", luokka: etuliite, jatko: <kieltosana>, perusmuoto: "joll'", äs: ä];
[alku: "jott'", luokka: etuliite, jatko: <kieltosana>, perusmuoto: "jott'", äs: ä];

[alku: "miks",  luokka: etuliite, jatko: <kieltosana>, perusmuoto: "miks",  äs: ä];
[alku: "miks'", luokka: etuliite, jatko: <kieltosana>, perusmuoto: "miks'", äs: ä];

[alku: "vaikk",  luokka: etuliite, jatko: <kieltosana>, perusmuoto: "vaikk",  äs: ä];
[alku: "vaikk'", luokka: etuliite, jatko: <kieltosana>, perusmuoto: "vaikk'", äs: ä];

[alku: "kosk'", luokka: etuliite, jatko: <kieltosana>, perusmuoto: "kosk'", äs: ä];
[alku: "kosk",  luokka: etuliite, jatko: <kieltosana>, perusmuoto: "kosk", äs: ä];

[alku: "mutt'", luokka: etuliite, jatko: <kieltosana>, perusmuoto: "mutt'", äs: ä];
[alku: "mutt",  luokka: etuliite, jatko: <kieltosana>, perusmuoto: "mutt", äs: ä];


[alku: "älä",        luokka: kieltosana, jatko: <liitesana, liitesana_kä, loppu>,
                     perusmuoto: "ei", äs: ä, tapaluokka: käskytapa,
                     aikamuoto: kestämä, tekijä: 2, luku: yksikkö];
[alku: "älköön",     luokka: kieltosana, jatko: <liitesana, liitesana_kä, loppu>,
                     perusmuoto: "ei", äs: ä, tapaluokka: käskytapa,
                     aikamuoto: kestämä, tekijä: 3, luku: yksikkö];
[alku: "älkäämme",   luokka: kieltosana, jatko: <liitesana, liitesana_kä, loppu>,
                     perusmuoto: "ei", äs: ä, tapaluokka: käskytapa,
                     aikamuoto: kestämä, tekijä: 1, luku: monikko];
[alku: "älkää",      luokka: kieltosana, jatko: <liitesana, liitesana_kä, loppu>,
                     perusmuoto: "ei", äs: ä, tapaluokka: käskytapa,
                     aikamuoto: kestämä, tekijä: 2, luku: monikko];
[alku: "älkööt",     luokka: kieltosana, jatko: <liitesana, liitesana_kä, loppu>,
                     perusmuoto: "ei", äs: ä, tapaluokka: käskytapa,
                     aikamuoto: kestämä, tekijä: 3, luku: monikko];

[alku: "olko", luokka: teonsana, jatko: <loppu>, perusmuoto: "olla", äs: a];  # Älkää olko.


# "Ällös itke, oma kulta..."
#
[alku: "ällös", luokka: kieltosana, jatko: <loppu>,
                äs: ä, tapaluokka: käskytapa, tekijä: 2, luku: yksikkö, tiedot: <murre>];

[alku: "olem", luokka: nimisana, jatko: <asema>, perusmuoto: "olema", äs: a];
