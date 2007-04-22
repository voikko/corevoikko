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


# Olla-teonsanan epäsäännölliset muodot ja kieltosana.


[alku: "liene", luokka: teonsana, jatko: <lienee>, perusmuoto: "olla", äs: ä,
                tapaluokka: ehtotapa];
[alku: "lie",   luokka: teonsana, jatko: <liitesana, loppu>, perusmuoto: "olla",
                äs: ä, tapaluokka: ehtotapa, tiedot: <ei_ys>];
[alku: "liene", luokka: teonsana, jatko: <liitesana, liitesana2, loppu>, perusmuoto: "olla",
                äs: ä, tapaluokka: ehtotapa];

[alku: "o",  luokka: teonsana, jatko: <olla>, perusmuoto: "olla", äs: a];


[alku: "ol",  luokka: teonsana, jatko: <loppu>, perusmuoto: "olla",  # Oli.
              äs: a, tapaluokka: tositapa, aikamuoto: kertoma,
              luku: yksikkö, tekijä: 3, tiedot: <murre>];

# Olla-sanan ehtotavan kestämä on myös tällainen.
#
[alku: "oisin", luokka: teonsana, jatko: <liitesana, loppu>, perusmuoto: "olla",
                äs: a, tapaluokka: ehtotapa, aikamuoto: kestämä,
                luku: yksikkö, tekijä: 1, tiedot: <murre, ei_ys>];
[alku: "oisit", luokka: teonsana, jatko: <liitesana, loppu>, perusmuoto: "olla",
                äs: a, tapaluokka: ehtotapa, aikamuoto: kestämä,
                luku: yksikkö, tekijä: 2, tiedot: <murre, ei_ys>];
[alku: "oisi",  luokka: teonsana, jatko: <liitesana, loppu>, perusmuoto: "olla",
                äs: a, tapaluokka: ehtotapa, aikamuoto: kestämä,
                luku: yksikkö, tekijä: 3, tiedot: <murre, ei_ys>];
[alku: "ois",   luokka: teonsana, jatko: <liitesana, loppu>, perusmuoto: "olla",
                äs: a, tapaluokka: ehtotapa, aikamuoto: kestämä,
                luku: yksikkö, tekijä: 3, tiedot: <murre, ei_ys>];

[alku: "oisimme", luokka: teonsana, jatko: <liitesana, loppu>, perusmuoto: "olla",
                  äs: a, tapaluokka: ehtotapa, aikamuoto: kestämä,
                  luku: monikko, tekijä: 1, tiedot: <murre>];
[alku: "oisitte", luokka: teonsana, jatko: <liitesana, loppu>, perusmuoto: "olla",
                  äs: a, tapaluokka: ehtotapa, aikamuoto: kestämä,
                  luku: monikko, tekijä: 2, tiedot: <murre>];
[alku: "oisivat", luokka: teonsana, jatko: <liitesana, loppu>, perusmuoto: "olla",
                  äs: a, tapaluokka: ehtotapa, aikamuoto: kestämä, 
                  luku: monikko, tekijä: 3, tiedot: <murre>];

[perusmuoto: "olla", alku: "olen",   luokka: teonsana, tapaluokka: tositapa,
                                     aikamuoto: kestämä, luku: yksikkö, tekijä: 1,
                                     jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "olla", alku: "oon",    luokka: teonsana, tapaluokka: tositapa,
                                     aikamuoto: kestämä, luku: yksikkö, tekijä: 1,
                                     jatko: <liitesana, loppu>, äs: a, tiedot: <murre, ei_ys>];
[perusmuoto: "olla", alku: "oom",    luokka: teonsana, tapaluokka: tositapa,
                                     aikamuoto: kestämä, luku: yksikkö, tekijä: 1,
                                     jatko: <liitesana>, äs: a, tiedot: <murre, ei_ys>];  # Oompahan.
[perusmuoto: "olla", alku: "olet",   luokka: teonsana, tapaluokka: tositapa,
                                     aikamuoto: kestämä, luku: yksikkö, tekijä: 2,
                                     jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "olla", alku: "on",     luokka: teonsana, tapaluokka: tositapa,
                                     aikamuoto: kestämä, luku: yksikkö, tekijä: 3,
                                     jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "olla", alku: "om",     luokka: teonsana, tapaluokka: tositapa,
                                     aikamuoto: kestämä, luku: yksikkö, tekijä: 3,
                                     jatko: <liitesana>, äs: a, tiedot: <murre, ei_ys>];  # Ompahan.
[perusmuoto: "olla", alku: "ompi",   luokka: teonsana, tapaluokka: tositapa,
                                     aikamuoto: kestämä, luku: yksikkö, tekijä: 3,
                                     jatko: <liitesana, loppu>, äs: a, tiedot: <murre>];
[perusmuoto: "olla", alku: "onpi",   luokka: teonsana, tapaluokka: tositapa,
                                     aikamuoto: kestämä, luku: yksikkö, tekijä: 3,
                                     jatko: <liitesana, loppu>, äs: a, tiedot: <murre>];
[perusmuoto: "olla", alku: "olemme", luokka: teonsana, tapaluokka: tositapa,
                                     aikamuoto: kestämä, luku: monikko, tekijä: 1,
                                     jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "olla", alku: "olette", luokka: teonsana, tapaluokka: tositapa,
                                     aikamuoto: kestämä, luku: monikko, tekijä: 2,
                                     jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "olla", alku: "ovat",   luokka: teonsana, tapaluokka: tositapa,
                                     aikamuoto: kestämä, luku: monikko, tekijä: 3,
                                     jatko: <liitesana, loppu>, äs: a];

[alku: "oo",   luokka: teonsana, jatko: <loppu>, perusmuoto: "olla",    # Ei oo.
               äs: a, tapaluokka: tositapa, aikamuoto: kestämä,
               luku: yksikkö, tekijä: 3, tiedot: <murre, ei_ys>];


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


[alku: "ehk",  luokka: etuliite, jatko: <kieltosana>, perusmuoto: "ehk",  äs: ä, tiedot: <ei_ysj>];
[alku: "ell",  luokka: etuliite, jatko: <kieltosana>, perusmuoto: "ell",  äs: ä, tiedot: <ei_ysj>];
[alku: "ett",  luokka: etuliite, jatko: <kieltosana>, perusmuoto: "ett",  äs: ä, tiedot: <ei_ysj>];
[alku: "ehk'", luokka: etuliite, jatko: <kieltosana>, perusmuoto: "ehk'", äs: ä, tiedot: <ei_ysj>];
[alku: "ell'", luokka: etuliite, jatko: <kieltosana>, perusmuoto: "ell'", äs: ä, tiedot: <ei_ysj>];
[alku: "ett'", luokka: etuliite, jatko: <kieltosana>, perusmuoto: "ett'", äs: ä, tiedot: <ei_ysj>];

[alku: "joll",  luokka: etuliite, jatko: <kieltosana>, perusmuoto: "joll",  äs: ä, tiedot: <ei_ysj>];
[alku: "jott",  luokka: etuliite, jatko: <kieltosana>, perusmuoto: "jott",  äs: ä, tiedot: <ei_ysj>];
[alku: "joll'", luokka: etuliite, jatko: <kieltosana>, perusmuoto: "joll'", äs: ä, tiedot: <ei_ysj>];
[alku: "jott'", luokka: etuliite, jatko: <kieltosana>, perusmuoto: "jott'", äs: ä, tiedot: <ei_ysj>];

[alku: "miks",  luokka: etuliite, jatko: <kieltosana>, perusmuoto: "miks",  äs: ä, tiedot: <ei_ysj>];
[alku: "miks'", luokka: etuliite, jatko: <kieltosana>, perusmuoto: "miks'", äs: ä, tiedot: <ei_ysj>];

[alku: "vaikk",  luokka: etuliite, jatko: <kieltosana>, perusmuoto: "vaikk",  äs: ä, tiedot: <ei_ysj>];
[alku: "vaikk'", luokka: etuliite, jatko: <kieltosana>, perusmuoto: "vaikk'", äs: ä, tiedot: <ei_ysj>];

[alku: "kosk'", luokka: etuliite, jatko: <kieltosana>, perusmuoto: "kosk'", äs: ä, tiedot: <ei_ysj>];
[alku: "kosk",  luokka: etuliite, jatko: <kieltosana>, perusmuoto: "kosk",  äs: ä, tiedot: <ei_ysj>];

[alku: "mutt'", luokka: etuliite, jatko: <kieltosana>, perusmuoto: "mutt'", äs: ä, tiedot: <ei_ysj>];
[alku: "mutt",  luokka: etuliite, jatko: <kieltosana>, perusmuoto: "mutt",  äs: ä, tiedot: <ei_ysj>];


[alku: "älä",        luokka: kieltosana, jatko: <liitesana, liitesana_kä, loppu>,
                     perusmuoto: "ei", äs: ä, tapaluokka: käskytapa,
                     aikamuoto: kestämä, tekijä: 2, luku: yksikkö];
[alku: "älköön",     luokka: kieltosana, jatko: <liitesana, liitesana_kä, loppu>,
                     perusmuoto: "ei", äs: ä, tapaluokka: käskytapa,
                     aikamuoto: kestämä, tekijä: 3, luku: yksikkö];
[alku: "älköhön",    luokka: kieltosana, jatko: <liitesana, liitesana_kä, loppu>,
                     perusmuoto: "ei", äs: ä, tapaluokka: käskytapa,
                     aikamuoto: kestämä, tekijä: 3, luku: yksikkö, tiedot: <murre>];
[alku: "älkäämme",   luokka: kieltosana, jatko: <liitesana, liitesana_kä, loppu>,
                     perusmuoto: "ei", äs: ä, tapaluokka: käskytapa,
                     aikamuoto: kestämä, tekijä: 1, luku: monikko];
[alku: "älkää",      luokka: kieltosana, jatko: <liitesana, liitesana_kä, loppu>,
                     perusmuoto: "ei", äs: ä, tapaluokka: käskytapa,
                     aikamuoto: kestämä, tekijä: 2, luku: monikko];
[alku: "älkään",     luokka: kieltosana, jatko: <liitesana, liitesana_kä, loppu>,
                     perusmuoto: "ei", äs: ä, tapaluokka: käskytapa,
                     aikamuoto: kestämä, tekijä: 2, luku: monikko, tiedot: <murre>];
[alku: "älkääs",     luokka: kieltosana, jatko: <liitesana, liitesana_kä, loppu>,
                     perusmuoto: "ei", äs: ä, tapaluokka: käskytapa,
                     aikamuoto: kestämä, tekijä: 2, luku: monikko, tiedot: <murre>];
[alku: "älkäät",     luokka: kieltosana, jatko: <liitesana, liitesana_kä, loppu>,
                     perusmuoto: "ei", äs: ä, tapaluokka: käskytapa,
                     aikamuoto: kestämä, tekijä: 2, luku: monikko, tiedot: <murre>];
[alku: "älkäätte",   luokka: kieltosana, jatko: <liitesana, liitesana_kä, loppu>,
                     perusmuoto: "ei", äs: ä, tapaluokka: käskytapa,
                     aikamuoto: kestämä, tekijä: 2, luku: monikko, tiedot: <murre>];
[alku: "älkööt",     luokka: kieltosana, jatko: <liitesana, liitesana_kä, loppu>,
                     perusmuoto: "ei", äs: ä, tapaluokka: käskytapa,
                     aikamuoto: kestämä, tekijä: 3, luku: monikko];
[alku: "älköhöt",    luokka: kieltosana, jatko: <liitesana, liitesana_kä, loppu>,
                     perusmuoto: "ei", äs: ä, tapaluokka: käskytapa,
                     aikamuoto: kestämä, tekijä: 3, luku: monikko, tiedot: <murre>];


# »Älkäästä vielä puhuko, minun täytyy mennä katsomaan, seisooko hevoseni,
# se on hyvin irstainen.»
# --- Theodolinda Hahnsson: KOTIKUUSEN KUISKEHIA (Otava 1920; www.lönnrot.net).
#
[alku: "älkäästä", luokka: kieltosana, jatko: <liitesana, liitesana_kä, loppu>,
                   perusmuoto: "ei", äs: ä, tapaluokka: käskytapa,
                   aikamuoto: kestämä, tekijä: 2, luku: monikko, tiedot: <murre>];


# Suku Suomen! Sa maan pyhän, vapaan sait,
# sitä älkösi ahtaaksi aitaa,
# -- Eino Leino: KOOTUT TEOKSET III (Otava, Helsinki, 1926; www.lönnrot.net).

[alku: "älkösi", luokka: kieltosana, jatko: <liitesana, liitesana_kä, loppu>,
                 perusmuoto: "ei", äs: ä, tapaluokka: käskytapa,
                 aikamuoto: kestämä, tekijä: 2, luku: monikko, tiedot: <murre>];


# "Ällös itke, oma kulta..."
#
[alku: "ällös", luokka: kieltosana, jatko: <loppu>,
                äs: ä, tapaluokka: käskytapa, tekijä: 2, luku: yksikkö, tiedot: <murre>];
