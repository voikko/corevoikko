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
#
# This file has been modified by the contributors of Voikko project.
# Last change was on $Date$ by $Author$.

# Lyhenteitä. (Lyhennesanat [esimerkiksi "Nato"] käsitellään Joukahaisessa)
#
# Muista, että lyhenteen voi joskus lukea sekä lyhentämättömässä muodossaan
# (esim. "aktiebolag") että kirjain kerrallaan ("aa-bee"). Tämä saattaa
# tarkoittaa, että etu- ja takavokaalitaivutukset täytyy molemmat hyväksyä.

define @lyhenteen_jatko := <tavuviiva, kaksoispiste, loppu>;

[alku: "1:o", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];  # 1°
[alku: "2:o", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];  # 2° jne
[alku: "3:o", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "4:o", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "5:o", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "6:o", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "7:o", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "8:o", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "9:o", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];

[alku: "A4", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jj"];
[alku: "adj.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # adjektiivi
[alku: "alk.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # alkuaan, alkaen
[alku: "ao.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # asianomainen
[alku: "a.p.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "apulaisj.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "apul.joht.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>]; 
[alku: "arv.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # arvoisa
[alku: "c.s.i.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "dem.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # demokraattinen
[alku: "eaa.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # ennen ajanlaskun alkua
[alku: "ed.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # edellinen; edellä; edustaja
[alku: "eKr.", luokka: lyhenne, jatko: <loppu>, äs: aä, rakenne: "=qjqq"]; # ennen Kristuksen syntymää
[alku: "elok.", luokka: lyhenne, jatko: <loppu>, äs: a]; # elokuu
[alku: "em.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # edellä mainittu
[alku: "emt.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # edellä mainittu teos
[alku: "ent.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # entinen
[alku: "esim", luokka: lyhenne, jatko: <kaksoispiste>, äs: ä, tiedot: <ei_voikko>];
[alku: "esim.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # esimerkki
[alku: "ev.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # eversti
[alku: "harv.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "heinäk.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "helmik.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "hra", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "huhtik.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "huom.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "ilm.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # ilmestynyt; ilmoitus
[alku: "jaa.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # jälkeen ajanlaskun alun
[alku: "j.e.p.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>]; # Ja niin edespäin.
[alku: "Jk.", luokka: lyhenne, jatko: <loppu>, äs: aä, rakenne: "=jqq"]; # jälkikirjoitus
[alku: "jKr.", luokka: lyhenne, jatko: <loppu>, äs: aä, rakenne: "=qjqq"]; # jälkeen Kristuksen syntymän
[alku: "jne.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # ja niin edelleen
[alku: "j.n.e.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "jnep", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>]; # Ja niin edespäin.
[alku: "jouluk.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "kesäk.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "kh.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # koehenkilö; kylpyhuone; kertausharjoitus
[alku: "khra", luokka: lyhenne, jatko: <tavuviiva, loppu>, äs: aä]; # kirkkoherra
[alku: "kirj.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # kirjoittanut; kirjataan
[alku: "kk.", luokka: lyhenne, jatko: <loppu>, äs: a]; # kirkonkylä; keittokomero
[alku: "k:llo", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "klo", luokka: lyhenne, jatko: <tavuviiva, loppu>, äs: aä]; # kello (sisälyhenne, ei taivuteta kaksoispisteen avulla)
[alku: "ko.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # kyseessä oleva
[alku: "kok.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # Kokoomus; kokelas
[alku: "kom.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>]; # Kommunisti(t).
[alku: "ks.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # katso
[alku: "kts", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "lis.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # lisensiaatti
[alku: "lkm.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # lukumäärä
[alku: "lokak.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "lut.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # luterilainen
[alku: "maalisk.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "maanvilj.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "maanv.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "maist.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # maisteri
[alku: "marrask.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "milj.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "mm.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # muun muassa
[alku: "mp3", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a];
[alku: "MP3", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jjj"];
[alku: "mrd.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # miljardia
[alku: "mrk", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>]; # Markkaa.
[alku: "MTV3", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jjjj", tiedot: <ei_voikko>];
[alku: "n.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # noin
[alku: "nimim.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # nimimerkki
[alku: "nk.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # niin kutsuttu
[alku: "n.k.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];  # Niin kutsuttu.
[alku: "n:o.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>]; # Numero.
[alku: "ns.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # niin sanottu
[alku: "n.s.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>]; # Niin sanottu.
[alku: "nuor.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # Nuorsuomalainen.
[alku: "OK", luokka: lyhenne, jatko: <loppu>, äs: a, rakenne: "=jj"];
[alku: "op.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # opettaja
[alku: "o.s.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # omaa sukua
[alku: "os.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # osoite; osasto
[alku: "pj", luokka: lyhenne, jatko: <kaksoispiste, tavuviiva>, äs: aä]; # puheenjohtaja
[alku: "pj.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # puheenjohtaja
[alku: "varapj", luokka: lyhenne, jatko: <kaksoispiste, tavuviiva>, äs: aä, rakenne: "=qqqqqq"]; # varapuheenjohtaja
[alku: "varapj.", luokka: lyhenne, jatko: <loppu>, äs: aä, rakenne: "=qqqqqq"]; # varapuheenjohtaja
[alku: "pl.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # pois lukien
[alku: "pnä", luokka: lyhenne, jatko: <loppu>, äs: aä]; # päivänä
[alku: "po.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # pitää olla; puheena oleva
[alku: "porv.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "prof.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # professori
[alku: "puh.joht.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # puheenjohtaja
[alku: "puh.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # puhelin
[alku: "pvä", luokka: lyhenne, jatko: <loppu>, äs: aä]; # päivä
[alku: "r.l.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "rn:o", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>]; # (Maatilan) rekisterinumero.
[alku: "rva", luokka: lyhenne, jatko: <loppu>, äs: aä]; # rouva
[alku: "r.y.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "s.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # sivu
[alku: "sd.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # sosiaalidemokraatti
[alku: "s.k.d.l", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "SK", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>]; # Suomen Kuvalehti
[alku: "SKOP", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "SKP", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"]; # Suomen Kommunistinen Puolue
[alku: "SKS", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj", tiedot: <ei_voikko>]; # Suomalaisen Kirjallisuuden Seura
[alku: "Smk", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jqq", tiedot: <ei_voikko>]; # Suomen markka
[alku: "SM", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jj"]; # suomenmestaruus, Suomen-mestaruus, Suomen mestaruus
[alku: "SMP", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj", tiedot: <ei_voikko>]; # Suomen Maaseudun Puolue
[alku: "SNDL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjjj", tiedot: <ei_voikko>];
[alku: "SNTL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjjj", tiedot: <ei_voikko>]; # Sosialististen neuvostotasavaltojen liitto
[alku: "snt", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä]; # sentti(ä)
[alku: "so.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # se on
[alku: "soqt", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "sos.dem.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # sosiaalidemokraatti(nen)
[alku: "sos.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # sosiaalinen; sosialisti(nen)
[alku: "SOS", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjj"]; # Save Our Souls
[alku: "SPR", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjj"];
[alku: "ST1", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "STL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj", tiedot: <ei_voikko>]; # Suomen Teollisuustoimihenkilöiden Liitto
[alku: "STT", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"]; # Suomen Tietotoimisto
[alku: "suom.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # suomentanut, suomennos
[alku: "synt.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "SYP", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj", tiedot: <ei_voikko>];
[alku: "syysk.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "tammik.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "TEL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj", tiedot: <ei_voikko>];
[alku: "TKK", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "tlk", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä]; # tölkki(ä)
[alku: "tl", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä]; # teelusikallinen, -sta
[alku: "tmi", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä]; # toiminimi
[alku: "tm.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # tai muu(ta)
[alku: "tms.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # tai muuta sellaista
[alku: "toim.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # toimittanut, toimittaja
[alku: "toukok.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "tov.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "TPSL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjjj", tiedot: <ei_voikko>];
[alku: "tri", luokka: lyhenne, jatko: <loppu>, äs: aä]; # tohtori
[alku: "tsfs", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "tsl", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "ts.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # toisin sanoen
[alku: "tst", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "TUL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj", tiedot: <ei_voikko>]; # Työväen Urheiluliitto
[alku: "TVL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj", tiedot: <ei_voikko>];
[alku: "TV", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jj"]; # televisio, teevee
[alku: "tv", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, tiedot: <ei_sukija>]; # televisio, teevee
[alku: "ty", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "UKK", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"]; # Usein kysyttyjä kysymyksiä; Urho Kaleva Kekkonen
[alku: "UMTS", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjjj"]; # Universal Mobile Telecommunication System
[alku: "umts", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä]; # Universal Mobile Telecommunication System
[alku: "USA", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jjj"]; # United States of America
[alku: "USB", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjj"];
[alku: "UTC", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjj"]; # Universal Coordinated Time
[alku: "v3", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
#alku: "vanh.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "virall.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "VRK", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jjj"]; # Väestörekisterikeskus
[alku: "vrk", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a]; # vuorokausi
[alku: "VR", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jj"]; # VR-konserni
[alku: "vrt.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # vertaa
[alku: "vs.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # viransijainen, vastaan
[alku: "vt.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # virkaa toimittava
[alku: "VTT", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"]; # Valtion teknillinen tutkimuskeskus; valtiotieteiden tohtori
[alku: "wc", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä];
[alku: "WC", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jj"];
[alku: "WHO", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jjj"]; # World Health Organization
[alku: "WSOY", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjjj"];
[alku: "WTO", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jjj"]; # World Trade Organisation
[alku: "WWW", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjj"]; # World Wide Web
[alku: "www", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä]; # World Wide Web
[alku: "yht.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # yhteensä
[alku: "YK", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jj"];
[alku: "ym.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # ynnä muuta
[alku: "YM", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jj"]; # Ympäristöministeriö
[alku: "yms.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # ynnä muuta sellaista
[alku: "YTL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj", tiedot: <ei_voikko>];
[alku: "YYA", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"]; # ystävyys-, yhteistyö- ja avunantosopimus
[alku: "ÄO", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jj"]; # Älykkyysosamäärä

[alku: "a", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a];
[alku: "á", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "à", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "â", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ã", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "b", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "c", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ç", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "d", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ð", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "e", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "é", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "è", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ê", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ë", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "f", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "g", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "h", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "i", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "í", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ì", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "î", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ï", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "j", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "k", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "l", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "m", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "n", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ñ", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "o", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ó", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ò", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ô", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "p", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "q", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "r", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "s", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "š", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ß", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "þ", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "t", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "u", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ú", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ù", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "û", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "v", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "w", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "x", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "y", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ý", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ÿ", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ü", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "z", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ž", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "å", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ä", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "æ", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ö", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "õ", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ø", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];

[alku: "°C", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jj"];
[alku: "°F", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jj"];
[alku: "°K", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jj"];
[alku: "°", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a];
[alku: "€", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a];

#[alku: "", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
