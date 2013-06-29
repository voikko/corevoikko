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

# Sanatietueita, joita ei käsitellä Joukahaisessa.

[perusmuoto: "af", alku: "af", luokka: nimi, jatko: <loppu>, äs: a, rakenne: "=pp"];
[perusmuoto: "bin", alku: "bin", luokka: nimi, jatko: <loppu>, äs: ä, rakenne: "=ppp"];  # Osama bin Laden.
[perusmuoto: "eri", alku: "eri", luokka: laatusana, jatko: @sana1 + <loppu>, äs: ä];
[perusmuoto: "eräs", alku: "erä", luokka: asemosana, jatko: <vieras>, äs: ä];
[perusmuoto: "halleluja", alku: "halleluj", luokka: nimisana, jatko: <koira>, äs: a, tiedot: <ei_voikko>]; # Taipuu Nykysuomen sanakirjassa.
[perusmuoto: "he", alku: "he", luokka: asemosana, luku: monikko, jatko: <me>, äs: ä];
[perusmuoto: "hunningolla", alku: "hunningo", luokka: nimisana, jatko: @ulkopaikallissijat, äs: a];
[perusmuoto: "huomassa", alku: "huoma", luokka: nimisana, jatko: @sisäpaikallissijat_Vn, äs: a];
[perusmuoto: "hän", alku: "hä", luokka: asemosana, jatko: @ulkopaikallissijat, äs: ä, tiedot: <ei_sukija>]; # Hällä, hälle, hältä.
[perusmuoto: "hän", alku: "hän", luokka: asemosana, jatko: <hän>, äs: ä, tiedot: <ei_sukija>];
[perusmuoto: "hän", alku: "hä", luokka: asemosana, jatko: <hän>, äs: ä, tiedot: <ei_voikko>]; # Myös hällä, hälle, hältä.
[perusmuoto: "ilki", alku: "ilki", luokka: nimisana, jatko: @sana1, äs: ä];
[perusmuoto: "ilmi", alku: "ilmi", luokka: nimi_laatusana, jatko: @sana2 + <liitesana, loppu>, äs: ä];
[perusmuoto: "joka", alku: "jo", luokka: asemosana, jatko: <joka>, äs: a, tiedot: <ei_ys>];
[perusmuoto: "jompi", alku: "jo", luokka: asemosana, jatko: <suurempi>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "kuka", alku: "ku", luokka: asemosana, jatko: <joka>, äs: a];
[perusmuoto: "kuka", alku: "kukas", luokka: asemosana, jatko: <loppu>, äs: a];
[perusmuoto: "kumpainen", alku: "kumpai", luokka: asemosana, jatko: <nainen>, äs: a];
[perusmuoto: "kumpanen", alku: "kumpa", luokka: asemosana, jatko: <nainen>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "kumpi", alku: "kum", luokka: asemosana, jatko: <kumpi>, äs: a, tiedot: <ei_sukija>];
[perusmuoto: "kumpi", alku: "ku", luokka: asemosana, jatko: <suurempi>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "liki", alku: "liki", luokka: laatusana, jatko: <liitesana, loppu> + @sana2, äs: ä];
[perusmuoto: "lyx", alku: "lyx", luokka: lyhenne, jatko: <kalsium>, äs: ä];
[perusmuoto: "me", alku: "me", luokka: asemosana, luku: monikko, jatko: <me>, äs: ä];
[perusmuoto: "meikä", alku: "meikä", luokka: nimisana, jatko: <liitesana, loppu> + @sana1, äs: ä];
[perusmuoto: "melko", alku: "melko", luokka: laatusana, jatko: @sana1 + <loppu>, äs: a];
[perusmuoto: "mi", alku: "mi", luokka: asemosana, jatko: <loppu>, äs: a, tiedot: <ei_voikko, ei_ys>];  # Mikä.
[perusmuoto: "mi", alku: "min", luokka: asemosana, jatko: <loppu>, äs: a, tiedot: <ei_voikko, ei_ys>]; # Minkä.
[perusmuoto: "minä", alku: "min", luokka: asemosana, jatko: <minä>, äs: a, tiedot: <ei_sukija>];
[perusmuoto: "mitä", alku: "mitäh", luokka: asemosana, jatko: <loppu>, äs: ä];
[perusmuoto: "mones", alku: "mone", luokka: asemosana, jatko: <kahdeksas>, äs: a];
[perusmuoto: "moni", alku: "montaa", luokka: asemosana, jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "muu", alku: "mu", luokka: asemosana, jatko: <puu>, äs: a];
[perusmuoto: "muuan", alku: "muuan", luokka: asemosana, jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "muuan", alku: "muuatta", luokka: asemosana, jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "muuan", alku: "muuanna", luokka: asemosana, jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "muuan", alku: "muudan", luokka: asemosana, jatko: <liitesana, loppu>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "muuan", alku: "muudatta", luokka: asemosana, jatko: <liitesana, loppu>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "muuan", alku: "muudanna", luokka: asemosana, jatko: <liitesana, loppu>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "pikku", alku: "pikku", luokka: laatusana, jatko: <loppu>, äs: a];
[perusmuoto: "plus", alku: "plus", luokka: nimisana, jatko: <liitesana, loppu> + @sana1, äs: a];
[perusmuoto: "pro", alku: "pro", luokka: asemosana, jatko: <loppu>, äs: a]; #NS:n taivutus: suo.
[perusmuoto: "päikkäin", alku: "päikkäin", luokka: asemosana, jatko: <loppu>, äs: ä];
[perusmuoto: "satikuti", alku: "satikutia", luokka: nimisana, jatko: <liitesana, loppu>, äs: a, tiedot: <murre>];
[perusmuoto: "silti", alku: "silti", luokka: asemosana, jatko: <liitesana, loppu>, äs: ä];
[perusmuoto: "sinä", alku: "sin", luokka: asemosana, jatko: <minä>, äs: a, tiedot: <ei_sukija>];
[perusmuoto: "sisä", alku: "sisä", luokka: nimisana, jatko: @sisäpaikallissijat_Vn + @ulkopaikallissijat, äs: ä];
[perusmuoto: "tanhuvilla", alku: "tanhuv", luokka: nimisana, jatko: @ulkopaikallissijat_monikko, äs: a];
[perusmuoto: "te", alku: "te", luokka: asemosana, luku: monikko, jatko: <me>, äs: ä];
[perusmuoto: "tex", alku: "tex", luokka: lyhenne, jatko: <kalsium>, äs: ä];
[perusmuoto: "ulko", alku: "ulko", luokka: nimisana, jatko: <loppu>, äs: a, tiedot: <ei_voikko, ei_ys>]; # "ulko" on myös etuliite.
[perusmuoto: "van", alku: "van", luokka: nimi, jatko: <loppu>, äs: a, rakenne: "=ppp"];
[perusmuoto: "veli", alku: "veliseni", luokka: nimisana, jatko: <liitesana, loppu>, äs: ä, tiedot: <ei_ysa>];
[perusmuoto: "viime", alku: "viime", luokka: laatusana, jatko: @sana2 + <liitesana, loppu>, äs: ä];
[perusmuoto: "von", alku: "von", luokka: nimi, jatko: <loppu>, äs: a, rakenne: "=ppp"];
[perusmuoto: "öky", alku: "öky", luokka: laatusana, jatko: <loppu>, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "ökö", alku: "ökö", luokka: laatusana, jatko: @sana1 + <loppu>, äs: ä, tiedot: <ei_voikko>];

# Aluetta tarkentavia etuliitteitä ei hyväksytä normaalisti pienellä alkukirjaimella. Muutamat voivat esiintyä kuitenkin
# ilman yhdysviivaa yleisnimiyhdyssanoissa, esim. "sydäntauti" ja "mannerlaatta". Tästä syystä nämä täytyy hyväksyä
# pienellä alkukirjaimella "sydän- ja verisuonitaudit"-tyyppisissä rakenteissa.
[perusmuoto: "etelä-", alku: "etelä-", luokka: nimisana, jatko: <loppu>, äs: aä, rakenne: "=ppppp-", tiedot: <ei_sukija>];
[perusmuoto: "iso-", alku: "iso-", luokka: nimisana, jatko: <loppu>, äs: aä, rakenne: "=ppp-", tiedot: <ei_sukija>];
[perusmuoto: "itä-", alku: "itä-", luokka: nimisana, jatko: <loppu>, äs: aä, rakenne: "=ppp-", tiedot: <ei_sukija>];
[perusmuoto: "keski-", alku: "keski-", luokka: nimisana, jatko: <loppu>, äs: aä, rakenne: "=ppppp-", tiedot: <ei_sukija>];
[perusmuoto: "länsi-", alku: "länsi-", luokka: nimisana, jatko: <loppu>, äs: aä, rakenne: "=ppppp-", tiedot: <ei_sukija>];
[perusmuoto: "manner-", alku: "manner-", luokka: nimisana, jatko: <loppu>, äs: aä, rakenne: "=pppppp-", tiedot: <ei_sukija>];
[perusmuoto: "pohjois-", alku: "pohjois-", luokka: nimisana, jatko: <loppu>, äs: aä, rakenne: "=ppppppp-", tiedot: <ei_sukija>];
[perusmuoto: "suur-", alku: "suur-", luokka: nimisana, jatko: <loppu>, äs: aä, rakenne: "=ppppp-", tiedot: <ei_sukija>];
[perusmuoto: "sydän-", alku: "sydän-", luokka: nimisana, jatko: <loppu>, äs: aä, rakenne: "=ppppp-", tiedot: <ei_sukija>];
[perusmuoto: "uusi-", alku: "uusi-", luokka: nimisana, jatko: <loppu>, äs: aä, rakenne: "=pppp-", tiedot: <ei_sukija>];
