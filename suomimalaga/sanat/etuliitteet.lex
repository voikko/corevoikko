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
define @ell := <tavuviiva, etuliite, laatusana, nimi_laatusana>;
# Etuliite (teonsanat)
define @elt := <tavuviiva, etuliite, teonsana>;

# Aikaisemmin käytetyt määritelmät jatko-kentissä, älä käytä näitä enää
# define @sana1 := <nimisana, laatusana, nimi_laatusana, tavuviiva, etuliite>;
# define @sana2 := @sana1 + <teonsana>;


# Etuliite + nimisana/laatusana

[perusmuoto: "agro", alku: "agro", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "aikakaus", alku: "aikakaus", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "ainois", alku: "ainois", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "astro", alku: "astro", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "asuin", alku: "asuin", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "bi-", alku: "bi-", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "digitaali", alku: "digitaali", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "elektro", alku: "elektro", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "enkel", alku: "enkel", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "ennen", alku: "ennen", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "feodaali", alku: "feodaali", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "geo", alku: "geo", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "indo", alku: "indo", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "infra", alku: "infra", luokka: etuliite, jatko: @eln + @ell, äs: aä]; #Ei oo
[perusmuoto: "irvi", alku: "irvi", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "itsestään", alku: "itsestään", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "jumal", alku: "jumal", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "kaiken", alku: "kaiken", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "lehmi", alku: "lehmi", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "mestaris", alku: "mestaris", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "ominais", alku: "ominais", luokka: etuliite, jatko: @eln + @ell, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "pikku", alku: "pikku", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "rationaali", alku: "rationaal", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "refleksiivi", alku: "refleksiiv", luokka: etuliite, jatko: @eln + @ell, äs: aä];
[perusmuoto: "synnyin", alku: "synnyin", luokka: etuliite, jatko: @eln + @ell, äs: aä];


# Etuliite + teonsana

[perusmuoto: "de", alku: "de", luokka: etuliite, jatko: @elt, äs: aä];
[perusmuoto: "re", alku: "re", luokka: etuliite, jatko: @elt, äs: aä, tiedot: <siv>];


# Etuliite + nimisana/laatusana/teonsana

[perusmuoto: "aero", alku: "aero", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "ali", alku: "ali", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "alkuun", alku: "alkuun", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä]; # Alkuunpanija jne.
[perusmuoto: "alle", alku: "alle", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "anti", alku: "anti", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "avo", alku: "avo", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "bio", alku: "bio", luokka: etuliite, jatko: @eln + @ell + @elt, äs: a, tiedot: <ei_sukija>];
[perusmuoto: "digi", alku: "digi", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "ei-", alku: "ei-", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "eko", alku: "eko", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <siv>];
[perusmuoto: "ensi", alku: "ensi", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "ensiö", alku: "ensiö", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "epä", alku: "epä", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "erikois", alku: "erikois", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "erityis", alku: "erityis", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "esi", alku: "esi", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "etno", alku: "etno", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "etä", alku: "etä", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "ferro", alku: "ferro", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "futuro", alku: "futuro", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "fysio", alku: "fysio", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <siv>];
[perusmuoto: "gastro", alku: "gastro", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <siv>];
[perusmuoto: "gyro", alku: "gyro", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "haja", alku: "haja", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "hydro", alku: "hydro", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "hyper", alku: "hyper", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "iki", alku: "iki", luokka: etuliite, jatko: @eln + @ell + @elt, äs: ä];
[perusmuoto: "inter", alku: "inter", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "irti", alku: "irti", luokka: etuliite, jatko: @eln + @ell + @elt, äs: ä];
[perusmuoto: "irto", alku: "irto", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "julki", alku: "julki", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "kaksois", alku: "kaksois", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "kanssa", alku: "kanssa", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "kauko", alku: "kauko", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "kautta", alku: "kautta", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "kesken", alku: "kesken", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "keski", alku: "keski", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "kokonais", alku: "kokonais", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "kolmi", alku: "kolmi", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "kolmois", alku: "kolmois", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "kosio", alku: "kosio", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "kuolin", alku: "kuolin", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "kvasi", alku: "kvasi", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "kyynär", alku: "kyynär", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "lähi", alku: "lähi", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "maalais", alku: "maalais", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "maalis", alku: "maalis", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "mentaali", alku: "mentaali", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "meta", alku: "meta", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "muinais", alku: "muinais", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "multi", alku: "multi", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "myöhäis", alku: "myöhäis", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "nano", alku: "nano", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "nelis", alku: "nelis", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "nyky", alku: "nyky", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "ohi", alku: "ohi", luokka: etuliite, jatko: @eln + @ell + @elt, äs: a];
[perusmuoto: "oikein", alku: "oikein", luokka: etuliite, jatko: @eln + @ell + @elt, äs: a];
[perusmuoto: "oiko", alku: "oiko", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "osittais", alku: "osittais", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "pappis", alku: "pappis", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "perille", alku: "perille", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "pienois", alku: "pienois", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "pika", alku: "pika", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "piko", alku: "piko", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "pitkittäis", alku: "pitkittäis", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "poikittais", alku: "poikittais", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "poly", alku: "poly", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "porvaris", alku: "porvaris", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "post", alku: "post", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "pre", alku: "pre", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "päälle", alku: "päälle", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "riippu", alku: "riippu", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "rinnakkais", alku: "rinnakkais", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "rouvas", alku: "rouvas", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "satunnais", alku: "satunnais", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "seitsen", alku: "seitsen", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "seksuaali", alku: "seksuaali", luokka: etuliite, jatko: @eln + @ell + @elt, äs: a];
[perusmuoto: "sisällis", alku: "sisällis", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "sisään", alku: "sisään", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "sivuttais", alku: "sivuttais", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "sosiaali", alku: "sosiaali", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "sosio", alku: "sosio", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "supra", alku: "supra", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "suur", alku: "suur", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "syys", alku: "syys", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "taka", alku: "taka", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "tekno", alku: "tekno", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "termo", alku: "termo", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "toivio", alku: "toivio", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "täsmä", alku: "täsmä", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä]; 
[perusmuoto: "täys", alku: "täys", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "ulko", alku: "ulko", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "umpi", alku: "umpi", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "uudis", alku: "uudis", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "uus", alku: "uus", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "vapaa", alku: "vapaa", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "vastaan", alku: "vastaan", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "viher", alku: "viher", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "viitois", alku: "viitois", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "yhdys", alku: "yhdys", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "yhteen", alku: "yhteen", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "yhteis", alku: "yhteis", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "yksittäis", alku: "yksittäis", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "yksityis", alku: "yksityis", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "yksöis", alku: "yksöis", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "yleis", alku: "yleis", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_sukija>];
[perusmuoto: "ylen", alku: "ylen", luokka: etuliite, jatko: @eln + @ell + @elt, äs: ä];
[perusmuoto: "yli", alku: "yli", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "yllä", alku: "yllä", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "ylä", alku: "ylä", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "ympäri", alku: "ympäri", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "äkki", alku: "äkki", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "ääreis", alku: "ääreis", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];


# Etuliite + nimisana/laatusana/teonsana (Ei Voikko-versioon)

[perusmuoto: "allo", alku: "allo", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "amfi", alku: "amfi", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "andro", alku: "andro", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <siv>];
[perusmuoto: "anestesio", alku: "anestesio", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <siv>];
[perusmuoto: "antropo", alku: "antropo", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <siv>];
[perusmuoto: "bakterio", alku: "bakterio", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <siv>];
[perusmuoto: "biblio", alku: "biblio", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <siv>];
[perusmuoto: "etymo", alku: "etymo", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "finn", alku: "finn", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "grafo", alku: "grafo", luokka: etuliite, jatko: @eln + @ell + @elt, äs: a];
[perusmuoto: "gyneko", alku: "gyneko", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <siv>];
[perusmuoto: "haja-", alku: "haja-", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "histo", alku: "histo", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "hypo", alku: "hypo", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "iktyo", alku: "iktyo", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "iono", alku: "iono", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "jalko", alku: "jalko", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "kardio", alku: "kardio", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "karjo", alku: "karjo", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "karto", alku: "karto", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "kotiin", alku: "kotiin", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "krono", alku: "krono", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <siv>];
[perusmuoto: "leivin", alku: "leivin", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "leksiko", alku: "leksiko", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "mareo", alku: "mareo", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "meteo", alku: "meteo", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "meteoro", alku: "meteoro", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "morfo", alku: "morfo", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "orto", alku: "orto", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "oseano", alku: "oseano", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "paleonto", alku: "paleonto", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <siv>];
[perusmuoto: "perään", alku: "perään", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "petro", alku: "petro", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "polito", alku: "polito", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "pyro", alku: "pyro", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "seismo", alku: "seismo", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "sekso", alku: "sekso", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "sino", alku: "sino", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <siv>];
[perusmuoto: "tanato", alku: "tanato", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "tomo", alku: "tomo", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "topo", alku: "topo", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "toska", alku: "toska", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];  # Toskakakku, -leivos.
[perusmuoto: "tropo", alku: "tropo", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "tämän", alku: "tämän", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "uro", alku: "uro", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
[perusmuoto: "vuos", alku: "vuos", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä, tiedot: <ei_voikko>];
[perusmuoto: "yltä", alku: "yltä", luokka: etuliite, jatko: @eln + @ell + @elt, äs: aä];
