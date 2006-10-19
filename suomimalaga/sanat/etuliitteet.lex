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

# Etuliitteitä. Näitä löytyy myös tiedostosta erikoiset.lex, mutta kaikki
# kuuluisi kuitenkin siirtää tähän tiedostoon. Siirtäessäsi etuliitteitä
# tänne mieti, minkä sanaluokan sanoille kyseistä etuliitettä todella
# tarvitaan. Tämä parantaa oikoluvun laatua, varsinkin lyhyiden etuliitteiden
# kanssa.

# Kaikissa etuliitteissä perusmuoto ja alku ovat samat, luokka = etuliite ja
# äs = aä.

# == Näitä voi käyttää jatko-kentissä ==
# Etuliite (nimisanat)
define @eln := <tavuviiva, etuliite, nimisana, nimi_laatusana>;
# Etuliite (laatusanat)
define @ell := <tavuviiva, etuliite, laatusana>;
# Etuliite (teonsanat)
define @elt := <tavuviiva, etuliite, teonsana>;

# Aikaisemmin käytetyt määritelmät jatko-kentissä, älä käytä näitä enää
# define @sana1 := <nimisana, laatusana, nimi_laatusana, tavuviiva, etuliite>;
# define @sana2 := @sana1 + <teonsana>;

[perusmuoto: "aero", alku: "aero", luokka: etuliite, jatko: @eln+@ell, äs: aä];
[perusmuoto: "agro", alku: "agro", luokka: etuliite, jatko: @eln+@ell, äs: aä];
[perusmuoto: "avo", alku: "avo", luokka: etuliite, jatko: @eln, äs: aä];

