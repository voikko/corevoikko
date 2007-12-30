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
[perusmuoto: "halleluja", alku: "halleluj", luokka: nimisana, jatko: <koira>, äs: a, tiedot: <ei_voikko>]; # Taipuu Nykysuomen sanakirjassa.
[perusmuoto: "he", alku: "he", luokka: asemosana, jatko: <me>, äs: ä];
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
[perusmuoto: "kukas", alku: "kukas", luokka: asemosana, jatko: <loppu>, äs: a];
[perusmuoto: "kumpainen", alku: "kumpai", luokka: asemosana, jatko: <nainen>, äs: a];
[perusmuoto: "kumpanen", alku: "kumpa", luokka: asemosana, jatko: <nainen>, äs: a];
[perusmuoto: "kumpi", alku: "kum", luokka: asemosana, jatko: <kumpi>, äs: a, tiedot: <ei_sukija>];
[perusmuoto: "kumpi", alku: "ku", luokka: asemosana, jatko: <suurempi>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "liki", alku: "liki", luokka: laatusana, jatko: <liitesana, loppu> + @sana2, äs: ä];
[perusmuoto: "lyx", alku: "lyx", luokka: lyhenne, jatko: <kalsium>, äs: ä];
[perusmuoto: "maksi", alku: "maks", luokka: nimisana, jatko: @sana1 + <risti>, äs: a];
[perusmuoto: "me", alku: "me", luokka: asemosana, jatko: <me>, äs: ä];
[perusmuoto: "meikä", alku: "meikä", luokka: nimisana, jatko: <liitesana, loppu> + @sana1, äs: ä];
[perusmuoto: "melko", alku: "melko", luokka: laatusana, jatko: @sana1 + <loppu>, äs: a];
[perusmuoto: "mi", alku: "mi", luokka: asemosana, jatko: <omanto_n, loppu>, äs: a, tiedot: <ei_voikko, ei_ys>]; # Mikä.
[perusmuoto: "minä", alku: "min", luokka: asemosana, jatko: <minä>, äs: a, tiedot: <ei_sukija>];
[perusmuoto: "mitä", alku: "mitäh", luokka: asemosana, jatko: <loppu>, äs: ä];
[perusmuoto: "mones", alku: "mone", luokka: asemosana, jatko: <kahdeksas>, äs: a];
[perusmuoto: "moni", alku: "montaa", luokka: asemosana, jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "muu", alku: "mu", luokka: asemosana, jatko: <puu>, äs: a];
[perusmuoto: "muuan", alku: "muua", luokka: asemosana, jatko: <osanto_ttA>, äs: a];
[perusmuoto: "muuan", alku: "muuan", luokka: asemosana, jatko: <olento_nA, liitesana, loppu>, äs: a];
[perusmuoto: "muudan", alku: "muuda", luokka: asemosana, jatko: <osanto_ttA>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "muudan", alku: "muudan", luokka: asemosana, jatko: <olento_nA, liitesana, loppu>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "peri", alku: "peri", luokka: nimisana, jatko: @sana1, äs: aä];
[perusmuoto: "pikku", alku: "pikku", luokka: laatusana, jatko: @sana2 + <loppu>, äs: a];
[perusmuoto: "plus", alku: "plus", luokka: nimisana, jatko: <liitesana, loppu> + @sana1, äs: a];
[perusmuoto: "pro", alku: "pro", luokka: asemosana, jatko: <loppu>, äs: a]; #NS:n taivutus: suo.
[perusmuoto: "päikkäin", alku: "päikkäin", luokka: asemosana, jatko: <loppu>, äs: ä];
[perusmuoto: "ruti", alku: "ruti", luokka: liitesana, jatko: <loppu>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "silti", alku: "silti", luokka: asemosana, jatko: <liitesana, loppu>, äs: ä];
[perusmuoto: "sinä", alku: "sin", luokka: asemosana, jatko: <minä>, äs: a, tiedot: <ei_sukija>];
[perusmuoto: "sisä", alku: "sisä", luokka: nimisana, jatko: @sisäpaikallissijat_Vn + @ulkopaikallissijat + @sana2, äs: ä];
[perusmuoto: "tanhuvilla", alku: "tanhuv", luokka: nimisana, jatko: @ulkopaikallissijat_monikko, äs: a];
[perusmuoto: "tarvis", alku: "tarvis", luokka: nimisana, jatko: @sana1 + <osanto_tA>, äs: a];
[perusmuoto: "te", alku: "te", luokka: asemosana, jatko: <me>, äs: ä];
[perusmuoto: "tex", alku: "tex", luokka: lyhenne, jatko: <kalsium>, äs: ä];
[perusmuoto: "toteen", alku: "toteen", luokka: nimisana, jatko: @sana2, äs: aä];
[perusmuoto: "ulko", alku: "ulko", luokka: nimisana, jatko: <loppu>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "van", alku: "van", luokka: nimi, jatko: <loppu>, äs: a, rakenne: "=ppp"];
[perusmuoto: "veli", alku: "veliseni", luokka: nimisana, jatko: <liitesana, loppu>, äs: ä, tiedot: <ei_ysa>];
[perusmuoto: "viime", alku: "viime", luokka: laatusana, jatko: @sana2 + <liitesana, loppu>, äs: ä];
[perusmuoto: "von", alku: "von", luokka: nimi, jatko: <loppu>, äs: a, rakenne: "=ppp"];
[perusmuoto: "öky", alku: "öky", luokka: laatusana, jatko: <loppu>, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "ökö", alku: "ökö", luokka: laatusana, jatko: @sana1 + <loppu>, äs: ä, tiedot: <ei_voikko>];
