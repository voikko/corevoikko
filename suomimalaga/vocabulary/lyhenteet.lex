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

[alku: "esim", luokka: lyhenne, jatko: <kaksoispiste>, äs: ä, tiedot: <ei_voikko>];
[alku: "khra", luokka: lyhenne, jatko: <tavuviiva, loppu>, äs: aä]; # kirkkoherra
[alku: "k:llo", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "klo", luokka: lyhenne, jatko: <tavuviiva, loppu>, äs: aä]; # kello (sisälyhenne, ei taivuteta kaksoispisteen avulla)
[alku: "n:o.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>]; # Numero.
[alku: "pj", luokka: lyhenne, jatko: <kaksoispiste, tavuviiva>, äs: aä]; # puheenjohtaja
[alku: "varapj", luokka: lyhenne, jatko: <kaksoispiste, tavuviiva>, äs: aä, rakenne: "=qqqqqq"]; # varapuheenjohtaja
[alku: "rn:o", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>]; # (Maatilan) rekisterinumero.

[alku: "°C", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jj"];
[alku: "°F", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jj"];
[alku: "°K", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jj"];
[alku: "°", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a];
[alku: "€", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a];

#[alku: "", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
