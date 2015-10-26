#
# Tekijänoikeus © 2006 Kalle Lampila (Etunimi.Sukunimi@iki.fi.invalid)
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
#
# This file contains some special words and workarounds for known bugs.


# Joissakin sanoissa on käytetty rakenne-kentässä =-merkkiä tavutusvihjeenä,
# vaikka myöhemmin sitä on tarkoitus käyttää vain sanojen ja etuliitteiden
# rajalla.

# Special abbreviations. There are some more of these, but they should preferably be
# included only in special scientific word lists.
 [alku: "m:eissä", luokka: lyhenne, jatko: <loppu>, äs: aä];
 [alku: "s:eissa", luokka: lyhenne, jatko: <loppu>, äs: aä];
 [alku: "h:eissa", luokka: lyhenne, jatko: <loppu>, äs: aä];
 [alku: "m:eineen", luokka: lyhenne, jatko: <loppu>, äs: aä];
 [alku: "s:eineen", luokka: lyhenne, jatko: <loppu>, äs: aä];
 [alku: "h:eineen", luokka: lyhenne, jatko: <loppu>, äs: aä];
 [alku: "m:einä", luokka: lyhenne, jatko: <loppu>, äs: aä];
 [alku: "s:eina", luokka: lyhenne, jatko: <loppu>, äs: aä];
 [alku: "h:eina", luokka: lyhenne, jatko: <loppu>, äs: aä];


# genetiivi + liitepartikkeli + inen-adjektiivi

 [perusmuoto: "kahdenlainen", alku: "kahdenkinlai", luokka: laatusana, jatko: <nainen>, äs: a, tiedot: <ei_vertm>, rakenne: "=ppppppppp=pppppp"];
 [perusmuoto: "kahdenlainen", alku: "kahdenkaanlai", luokka: laatusana, jatko: <nainen>, äs: a, tiedot: <ei_vertm>, rakenne: "=pppppppppp=pppppp"];
 [perusmuoto: "kolmenlainen", alku: "kolmenkinlai", luokka: laatusana, jatko: <nainen>, äs: a, tiedot: <ei_vertm>, rakenne: "=ppppppppp=pppppp"];
 [perusmuoto: "kolmenlainen", alku: "kolmenkaanlai", luokka: laatusana, jatko: <nainen>, äs: a, tiedot: <ei_vertm>, rakenne: "=pppppppppp=pppppp"];
 [perusmuoto: "kummanlainen", alku: "kummankaanlai", luokka: laatusana, jatko: <nainen>, äs: a, tiedot: <ei_vertm>, rakenne: "=pppppppppp=pppppp"];
 [perusmuoto: "kummanlainen", alku: "kummankinlai", luokka: laatusana, jatko: <nainen>, äs: a, tiedot: <ei_vertm>, rakenne: "=ppppppppp=pppppp"];
 [perusmuoto: "minkälainen", alku: "minkähänlai", luokka: laatusana, jatko: <nainen>, äs: a, tiedot: <ei_vertm>, rakenne: "=pppppppp=pppppp"];
 [perusmuoto: "minkälainen", alku: "minkäkinlai", luokka: laatusana, jatko: <nainen>, äs: a, tiedot: <ei_vertm>, rakenne: "=pppppppp=pppppp"];
 [perusmuoto: "minkälainen", alku: "minkäköhänlai", luokka: laatusana, jatko: <nainen>, äs: a, tiedot: <ei_vertm>, rakenne: "=pppppppppp=pppppp"];
 [perusmuoto: "muunlainen", alku: "muunkaanlai", luokka: laatusana, jatko: <nainen>, äs: a, tiedot: <ei_vertm>, rakenne: "=pppppppp=pppppp"];
 [perusmuoto: "muunlainen", alku: "muunkinlai", luokka: laatusana, jatko: <nainen>, äs: a, tiedot: <ei_vertm>, rakenne: "=ppppppp=pppppp"];
 [perusmuoto: "muunmaalainen", alku: "muunkaanmaalai", luokka: nimi_laatusana, jatko: <nainen>, äs: a, tiedot: <ei_vertm>, rakenne: "=pppppppp=ppppppppp"];
 [perusmuoto: "muunmaalainen", alku: "muunkinmaalai", luokka: nimi_laatusana, jatko: <nainen>, äs: a, tiedot: <ei_vertm>, rakenne: "=ppppppp=ppppppppp"];
 [perusmuoto: "neljänlainen", alku: "neljänkinlai", luokka: laatusana, jatko: <nainen>, äs: a, tiedot: <ei_vertm>, rakenne: "=ppppppppp=pppppp"];
 [perusmuoto: "toisenlainen", alku: "toisenkaanlai", luokka: laatusana, jatko: <nainen>, äs: a, tiedot: <ei_vertm>, rakenne: "=pppppppppp=pppppp"];
 [perusmuoto: "toisenlainen", alku: "toisenkinlai", luokka: laatusana, jatko: <nainen>, äs: a, tiedot: <ei_vertm>, rakenne: "=pppppppp=pppppp"];
 [perusmuoto: "tämänkertainen", alku: "tämänkinkertai", luokka: laatusana, jatko: <nainen>, äs: a, tiedot: <ei_vertm>, rakenne: "=pppppppp=ppppppppp"];
 [perusmuoto: "tämänkertainen", alku: "tämänkäänkertai", luokka: laatusana, jatko: <nainen>, äs: a, tiedot: <ei_vertm>, rakenne: "=ppppppppp=ppppppppp"];
 [perusmuoto: "tämänpäiväinen", alku: "tämänkinpäiväi", luokka: laatusana, jatko: <nainen>, äs: ä, tiedot: <ei_vertm>, rakenne: "=pppppppp=ppppppppp"];
 [perusmuoto: "tämänpäiväinen", alku: "tämänkäänpäiväi", luokka: laatusana, jatko: <nainen>, äs: ä, tiedot: <ei_vertm>, rakenne: "=ppppppppp=ppppppppp"];
 [perusmuoto: "tämänviikkoinen", alku: "tämänkinviikkoi", luokka: laatusana, jatko: <nainen>, äs: a, tiedot: <ei_vertm>, rakenne: "=pppppppp=pppppppppp"];
 [perusmuoto: "tämänviikkoinen", alku: "tämänkäänviikkoi", luokka: laatusana, jatko: <nainen>, äs: a, tiedot: <ei_vertm>, rakenne: "=ppppppppp=pppppppppp"];
 [perusmuoto: "tämänvuotinen", alku: "tämänkinvuoti", luokka: laatusana, jatko: <nainen>, äs: a, tiedot: <ei_vertm>, rakenne: "=pppppppp=pppppppp"];
 [perusmuoto: "tämänvuotinen", alku: "tämänkäänvuoti", luokka: laatusana, jatko: <nainen>, äs: a, tiedot: <ei_vertm>, rakenne: "=ppppppppp=pppppppp"];
 [perusmuoto: "yhdenlainen", alku: "yhdenkinlai", luokka: laatusana, jatko: <nainen>, äs: a, tiedot: <ei_vertm>, rakenne: "=pppppppp=pppppp"];
 [perusmuoto: "yhdenlainen", alku: "yhdenkäänlai", luokka: laatusana, jatko: <nainen>, äs: a, tiedot: <ei_vertm>, rakenne: "=ppppppppp=pppppp"];
