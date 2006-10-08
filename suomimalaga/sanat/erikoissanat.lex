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


# Sanoja, joilla on niin erikoinen taivutus, että jokainen
# taivutusmuoto on parasta esittää erikseen.

define @loppu := <liitesana, loppu>;

[perusmuoto: "meri", alku: "mer", luokka: nimisana, jatko: <meri>, äs: ä];
[perusmuoto: "meri", alku: "mer", luokka: nimisana, jatko: <osanto_tA>, äs: a];
[perusmuoto: "veri", alku: "ver", luokka: nimisana, jatko: <meri>, äs: ä];
[perusmuoto: "veri", alku: "ver", luokka: nimisana, jatko: <osanto_tA>, äs: a];


[perusmuoto: "jokin", alku: "jokin", luokka: asemosana, sija: nimentö, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "jonkin", luokka: asemosana, sija: omanto_n, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "jotakin", luokka: asemosana, sija: osanto_tA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "jonakin", luokka: asemosana, sija: olento_nA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "joksikin", luokka: asemosana, sija: tulento_ksi, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "jossakin", luokka: asemosana, sija: sisäolento_ssA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "jostakin", luokka: asemosana, sija: sisäeronto_stA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "johonkin", luokka: asemosana, sija: sisätulento_hVn, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "jollakin", luokka: asemosana, sija: ulko_olento_llA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "joltakin", luokka: asemosana, sija: ulkoeronto_ltA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "jollekin", luokka: asemosana, sija: ulkotulento_lle, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "jottakin", luokka: asemosana, sija: vajanto_ttA, luku: yksikkö, jatko: @loppu, äs: a];

[perusmuoto: "jokin", alku: "jotkin", luokka: asemosana, sija: nimentö_t, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "joittenkin", luokka: asemosana, sija: omanto_iT, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "joidenkin", luokka: asemosana, sija: omanto_iT, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "joitakin", luokka: asemosana, sija: osanto_itA, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "joinakin", luokka: asemosana, sija: olento_inA, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "joiksikin", luokka: asemosana, sija: tulento_iksi, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "joissakin", luokka: asemosana, sija: sisäolento_issA, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "joistakin", luokka: asemosana, sija: sisäeronto_istA, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "joihinkin", luokka: asemosana, sija: sisätulento_ihin, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "joillakin", luokka: asemosana, sija: ulko_olento_illA, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "joiltakin", luokka: asemosana, sija: ulkoeronto_iltA, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "joillekin", luokka: asemosana, sija: ulkotulento_ille, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "joittakin", luokka: asemosana, sija: vajanto_ittA, luku: monikko, jatko: @loppu, äs: a];


[perusmuoto: "jokin", alku: "jotain", luokka: asemosana, sija: osanto_tA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "jonain", luokka: asemosana, sija: olento_nA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "jossain", luokka: asemosana, sija: sisäolento_ssA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "jostain", luokka: asemosana, sija: sisäeronto_stA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "jollain", luokka: asemosana, sija: ulko_olento_llA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "joltain", luokka: asemosana, sija: ulkoeronto_ltA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "jottain", luokka: asemosana, sija: vajanto_ttA, luku: yksikkö, jatko: @loppu, äs: a];

[perusmuoto: "jokin", alku: "joitain", luokka: asemosana, sija: osanto_itA, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "joinain", luokka: asemosana, sija: olento_inA, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "joissain", luokka: asemosana, sija: sisäolento_issA, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "joistain", luokka: asemosana, sija: sisäeronto_istA, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "joillain", luokka: asemosana, sija: ulko_olento_illA, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "joiltain", luokka: asemosana, sija: ulkoeronto_iltA, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "joittain", luokka: asemosana, sija: vajanto_ittA, luku: monikko, jatko: @loppu, äs: a];

######################################################

[perusmuoto: "joku", alku: "joku", luokka: asemosana, sija: nimentö, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "joku", alku: "jonkun", luokka: asemosana, sija: omanto_n, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "joku", alku: "jotakuta", luokka: asemosana, sija: osanto_tA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "joku", alku: "jonakuna", luokka: asemosana, sija: olento_nA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "joku", alku: "joksikuksi", luokka: asemosana, sija: tulento_ksi, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "joku", alku: "jossakussa", luokka: asemosana, sija: sisäolento_ssA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "joku", alku: "jostakusta", luokka: asemosana, sija: sisäeronto_stA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "joku", alku: "johonkuhun", luokka: asemosana, sija: sisätulento_hVn, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "joku", alku: "jollakulla", luokka: asemosana, sija: ulko_olento_llA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "joku", alku: "joltakulta", luokka: asemosana, sija: ulkoeronto_ltA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "joku", alku: "jollekulle", luokka: asemosana, sija: ulkotulento_lle, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "joku", alku: "jottakutta", luokka: asemosana, sija: vajanto_ttA, luku: yksikkö, jatko: @loppu, äs: a];

[perusmuoto: "joku", alku: "jotkut", luokka: asemosana, sija: nimentö_t, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "joku", alku: "joittenkuitten", luokka: asemosana, sija: omanto_iT, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "joku", alku: "joidenkuiden", luokka: asemosana, sija: omanto_iT, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "joku", alku: "joitakuita", luokka: asemosana, sija: osanto_itA, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "joku", alku: "joinakuina", luokka: asemosana, sija: olento_inA, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "joku", alku: "joiksikuiksi", luokka: asemosana, sija: tulento_iksi, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "joku", alku: "joissakuissa", luokka: asemosana, sija: sisäolento_issA, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "joku", alku: "joistakuista", luokka: asemosana, sija: sisäeronto_istA, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "joku", alku: "joihinkuihin", luokka: asemosana, sija: sisätulento_ihin, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "joku", alku: "joillakuilla", luokka: asemosana, sija: ulko_olento_illA, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "joku", alku: "joiltakuilta", luokka: asemosana, sija: ulkoeronto_iltA, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "joku", alku: "joillekuille", luokka: asemosana, sija: ulkotulento_ille, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "joku", alku: "joittakuitta", luokka: asemosana, sija: vajanto_ittA, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "joku", alku: "joinekuine", luokka: asemosana, sija: seuranto_ine, luku: monikko, jatko: <omistusliite, loppu>, äs: a];

######################################################

[perusmuoto: "jompikumpi", alku: "jompikumpi", luokka: asemosana, sija: nimentö, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jompikumpi", alku: "jommankumman", luokka: asemosana, sija: omanto_n, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jompikumpi", alku: "jompaakumpaa", luokka: asemosana, sija: osanto_A, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jompikumpi", alku: "jompanakumpana", luokka: asemosana, sija: olento_nA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jompikumpi", alku: "jommaksikummaksi", luokka: asemosana, sija: tulento_ksi, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jompikumpi", alku: "jommassakummassa", luokka: asemosana, sija: sisäolento_ssA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jompikumpi", alku: "jommastakummasta", luokka: asemosana, sija: sisäeronto_stA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jompikumpi", alku: "jompaankumpaan", luokka: asemosana, sija: sisätulento_Vn, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jompikumpi", alku: "jommallakummalla", luokka: asemosana, sija: ulko_olento_llA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jompikumpi", alku: "jommaltakummalta", luokka: asemosana, sija: ulkoeronto_ltA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jompikumpi", alku: "jommallekummalle", luokka: asemosana, sija: ulkotulento_lle, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jompikumpi", alku: "jommattakummatta", luokka: asemosana, sija: vajanto_ttA, luku: yksikkö, jatko: @loppu, äs: a];

[perusmuoto: "jompikumpi", alku: "jommatkummat", luokka: asemosana, sija: nimentö_t, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jompikumpi", alku: "jompienkumpien", luokka: asemosana, sija: omanto_ien, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jompikumpi", alku: "jompainkumpain", luokka: asemosana, sija: omanto_in, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jompikumpi", alku: "jompiakumpia", luokka: asemosana, sija: osanto_iA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jompikumpi", alku: "jompinakumpina", luokka: asemosana, sija: olento_inA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jompikumpi", alku: "jommiksikummiksi", luokka: asemosana, sija: tulento_iksi, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jompikumpi", alku: "jommissakummissa", luokka: asemosana, sija: sisäolento_issA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jompikumpi", alku: "jommistakummista", luokka: asemosana, sija: sisäeronto_istA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jompikumpi", alku: "jompiinkumpiin", luokka: asemosana, sija: sisätulento_iin, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jompikumpi", alku: "jommillakummilla", luokka: asemosana, sija: ulko_olento_illA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jompikumpi", alku: "jommiltakummilta", luokka: asemosana, sija: ulkoeronto_iltA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jompikumpi", alku: "jommillekummille", luokka: asemosana, sija: ulkotulento_ille, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jompikumpi", alku: "jommittakummitta", luokka: asemosana, sija: vajanto_ittA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jompikumpi", alku: "jompinekumpine", luokka: asemosana, sija: seuranto_ine, luku: monikko, jatko: <omistusliite, loppu>, äs: a];
[perusmuoto: "jompikumpi", alku: "jomminkummin", luokka: asemosana, sija: keinonto_in, luku: monikko, jatko: @loppu, äs: a];

######################################################

# Tämä sana on vain yksikössä.
[perusmuoto: "mikin", alku: "mikin", luokka: asemosana, sija: nimentö, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikin", alku: "minkin", luokka: asemosana, sija: omanto_n, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikin", alku: "mitäkin", luokka: asemosana, sija: osanto_tA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikin", alku: "minäkin", luokka: asemosana, sija: olento_nA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikin", alku: "miksikin", luokka: asemosana, sija: tulento_ksi, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikin", alku: "missäkin", luokka: asemosana, sija: sisäolento_ssA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikin", alku: "mistäkin", luokka: asemosana, sija: sisäeronto_stA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikin", alku: "mihinkin", luokka: asemosana, sija: sisätulento_hVn, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikin", alku: "milläkin", luokka: asemosana, sija: ulko_olento_llA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikin", alku: "miltäkin", luokka: asemosana, sija: ulkoeronto_ltA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikin", alku: "millekin", luokka: asemosana, sija: ulkotulento_lle, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikin", alku: "mittäkin", luokka: asemosana, sija: vajanto_ttA, luku: yksikkö, jatko: @loppu, äs: ä];

######################################################

[perusmuoto: "kukin", alku: "kukin", luokka: asemosana, sija: nimentö, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "kukin", alku: "kunkin", luokka: asemosana, sija: omanto_n, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "kukin", alku: "kutakin", luokka: asemosana, sija: osanto_tA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "kukin", alku: "kunakin", luokka: asemosana, sija: olento_nA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "kukin", alku: "kuksikin", luokka: asemosana, sija: tulento_ksi, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "kukin", alku: "kussakin", luokka: asemosana, sija: sisäolento_ssA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "kukin", alku: "kustakin", luokka: asemosana, sija: sisäeronto_stA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "kukin", alku: "kuhunkin", luokka: asemosana, sija: sisätulento_hVn, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "kukin", alku: "kullakin", luokka: asemosana, sija: ulko_olento_llA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "kukin", alku: "kultakin", luokka: asemosana, sija: ulkoeronto_ltA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "kukin", alku: "kullekin", luokka: asemosana, sija: ulkotulento_lle, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "kukin", alku: "kuttakin", luokka: asemosana, sija: vajanto_ttA, luku: yksikkö, jatko: @loppu, äs: a];

######################################################

[perusmuoto: "itsekukin", alku: "itsekukin", luokka: asemosana, sija: nimentö, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "itsekukin", alku: "itsekunkin", luokka: asemosana, sija: omanto_n, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "itsekukin", alku: "itsekutakin", luokka: asemosana, sija: osanto_tA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "itsekukin", alku: "itsekunakin", luokka: asemosana, sija: olento_nA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "itsekukin", alku: "itsekuksikin", luokka: asemosana, sija: tulento_ksi, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "itsekukin", alku: "itsekussakin", luokka: asemosana, sija: sisäolento_ssA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "itsekukin", alku: "itsekustakin", luokka: asemosana, sija: sisäeronto_stA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "itsekukin", alku: "itsekuhunkin", luokka: asemosana, sija: sisätulento_hVn, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "itsekukin", alku: "itsekullakin", luokka: asemosana, sija: ulko_olento_llA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "itsekukin", alku: "itsekultakin", luokka: asemosana, sija: ulkoeronto_ltA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "itsekukin", alku: "itsekullekin", luokka: asemosana, sija: ulkotulento_lle, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "itsekukin", alku: "itsekuttakin", luokka: asemosana, sija: vajanto_ttA, luku: yksikkö, jatko: @loppu, äs: a];

######################################################

#  Tämä sana on vain yksikössä.
[perusmuoto: "mikään", alku: "mikään", luokka: asemosana, sija: nimentö, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikään", alku: "minkään", luokka: asemosana, sija: omanto_n, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikään", alku: "mitäkään", luokka: asemosana, sija: osanto_tA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikään", alku: "minäkään", luokka: asemosana, sija: olento_nA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikään", alku: "miksikään", luokka: asemosana, sija: tulento_ksi, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikään", alku: "missäkään", luokka: asemosana, sija: sisäolento_ssA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikään", alku: "mistäkään", luokka: asemosana, sija: sisäeronto_stA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikään", alku: "mihinkään", luokka: asemosana, sija: sisätulento_hVn, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikään", alku: "milläkään", luokka: asemosana, sija: ulko_olento_llA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikään", alku: "miltäkään", luokka: asemosana, sija: ulkoeronto_ltA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikään", alku: "millekään", luokka: asemosana, sija: ulkotulento_lle, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikään", alku: "mittäkään", luokka: asemosana, sija: vajanto_ttA, luku: yksikkö, jatko: @loppu, äs: ä];

[perusmuoto: "mikään", alku: "mitään", luokka: asemosana, sija: osanto_tA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikään", alku: "minään", luokka: asemosana, sija: olento_nA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikään", alku: "missään", luokka: asemosana, sija: sisäolento_ssA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikään", alku: "mistään", luokka: asemosana, sija: sisäeronto_stA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikään", alku: "millään", luokka: asemosana, sija: ulko_olento_llA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikään", alku: "miltään", luokka: asemosana, sija: ulkoeronto_ltA, luku: yksikkö, jatko: @loppu, äs: ä];

[perusmuoto: "mikään", alku: "mitänä", luokka: asemosana, sija: olento_nA, luku: yksikkö, jatko: @loppu, äs: ä];

######################################################

# Ka-loppuiset joka-sanan muodot paitsi omanto jonka.
[perusmuoto: "joka", alku: "joksika", luokka: asemosana, sija: tulento_ksi, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "joka", alku: "johonka", luokka: asemosana, sija: sisätulento_hVn, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "joka", alku: "jonneka", luokka: asemosana, sija: sisätulento_nne, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "joka", alku: "jolleka", luokka: asemosana, sija: ulkotulento_lle, luku: yksikkö, jatko: @loppu, äs: a];
 
[perusmuoto: "joka", alku: "joiksika", luokka: asemosana, sija: tulento_iksi, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "joka", alku: "joihinka", luokka: asemosana, sija: sisätulento_ihin, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "joka", alku: "joilleka", luokka: asemosana, sija: ulkotulento_ille, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "joka", alku: "joidenka", luokka: asemosana, sija: omanto_iT, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "joka", alku: "joittenka", luokka: asemosana, sija: omanto_iT, luku: monikko, jatko: @loppu, äs: a];

######################################################

# Ka-loppuiset kuka-sanan muodot.
[perusmuoto: "kuka", alku: "kulleka",  luokka: asemosana, sija: ulkotulento_lle, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "kuka", alku: "kuilleka", luokka: asemosana, sija: ulkotulento_ille, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "kuka", alku: "kuhunka",  luokka: asemosana, sija: sisätulento_hVn, luku: yksikkö, jatko: @loppu, äs: a];


######################################################

[perusmuoto: "ken", alku: "ken",     luokka: asemosana, sija: nimentö, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kenen",   luokka: asemosana, sija: omanto_n, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kenet",   luokka: asemosana, sija: kohdanto_t, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "ketä",    luokka: asemosana, sija: osanto_tA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kenenä",  luokka: asemosana, sija: olento_nA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keneksi", luokka: asemosana, sija: tulento_ksi, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kenessä", luokka: asemosana, sija: sisäolento_ssA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kenestä", luokka: asemosana, sija: sisäeronto_stA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keneen",  luokka: asemosana, sija: sisätulento_Vn, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kenellä", luokka: asemosana, sija: ulko_olento_llA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keneltä", luokka: asemosana, sija: ulkoeronto_ltA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kenelle", luokka: asemosana, sija: ulkotulento_lle, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kenettä", luokka: asemosana, sija: vajanto_ttA, luku: yksikkö, jatko: @loppu, äs: ä];

[perusmuoto: "ken", alku: "kenä",  luokka: asemosana, sija: olento_nA,       luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kessä", luokka: asemosana, sija: sisäolento_ssA,  luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kestä", luokka: asemosana, sija: sisäeronto_stA,  luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kehen", luokka: asemosana, sija: sisätulento_hVn, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kellä", luokka: asemosana, sija: ulko_olento_llA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keltä", luokka: asemosana, sija: ulkoeronto_ltA,  luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kelle", luokka: asemosana, sija: ulkotulento_lle, luku: yksikkö, jatko: @loppu, äs: ä];

[perusmuoto: "ken", alku: "ketkä",   luokka: asemosana, sija: nimentö_tkA, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keiden",  luokka: asemosana, sija: omanto_iT, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keitten", luokka: asemosana, sija: omanto_iT, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keitä",   luokka: asemosana, sija: osanto_itA, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keinä",   luokka: asemosana, sija: olento_inA, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keiksi",  luokka: asemosana, sija: tulento_iksi, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keissä",  luokka: asemosana, sija: sisäolento_issA, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keistä",  luokka: asemosana, sija: sisäeronto_istA, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keihin",  luokka: asemosana, sija: sisätulento_ihin, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keillä",  luokka: asemosana, sija: ulko_olento_illA, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keiltä",  luokka: asemosana, sija: ulkoeronto_iltA, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keille",  luokka: asemosana, sija: ulkotulento_ille, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keittä",  luokka: asemosana, sija: vajanto_ittA, luku: monikko, jatko: @loppu, äs: ä];

# Kä-loppuisen yksikön muodot ken-sanasta.
[perusmuoto: "ken", alku: "kenenkä",   luokka: asemosana, sija: omanto_n, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kenenkäs",  luokka: asemosana, sija: omanto_n, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kenetkä",   luokka: asemosana, sija: kohdanto_t, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kenenäkä",  luokka: asemosana, sija: olento_nA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keneksikä", luokka: asemosana, sija: tulento_ksi, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kenessäkä", luokka: asemosana, sija: sisäolento_ssA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kenestäkä", luokka: asemosana, sija: sisäeronto_stA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keneenkä",  luokka: asemosana, sija: sisätulento_Vn, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kenelläkä", luokka: asemosana, sija: ulko_olento_llA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keneltäkä", luokka: asemosana, sija: ulkoeronto_ltA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kenellekä", luokka: asemosana, sija: ulkotulento_lle, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kenettäkä", luokka: asemosana, sija: vajanto_ttA, luku: yksikkö, jatko: @loppu, äs: ä];

[perusmuoto: "ken", alku: "kellekä",   luokka: asemosana, sija: ulkotulento_lle, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keillekä",  luokka: asemosana, sija: ulkotulento_ille, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kehenkä",   luokka: asemosana, sija: sisätulento_hVn, luku: yksikkö, jatko: <liitesana_s, loppu>, äs: ä];
[perusmuoto: "ken", alku: "keidenkä",  luokka: asemosana, sija: omanto_iT, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keittenkä", luokka: asemosana, sija: omanto_iT, luku: monikko, jatko: @loppu, äs: ä];


##########################################################

# Oikeita kukaan-sanan muotoja
#
[perusmuoto: "kukaan", alku: "kukaan",    luokka: asemosana, sija: nimentö, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "kukaan", alku: "kussaan",   luokka: asemosana, sija: sisäolento_ssA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "kukaan", alku: "kustaan",   luokka: asemosana, sija: sisäeronto_stA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "kukaan", alku: "kuhunkaan", luokka: asemosana, sija: sisätulento_hVn, luku: yksikkö, jatko: @loppu, äs: a];

# Kenkään-sanan muotoja.
#
#[perusmuoto: "kenkään", alku: "kenkään",     luokka: asemosana, sija: nimentö, luku: yksikkö, jatko: @loppu, äs: ä];

[perusmuoto: "kukaan", alku: "kenenkään",   luokka: asemosana, sija: omanto_n, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "kenetkään",   luokka: asemosana, sija: kohdanto_t, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "ketään",      luokka: asemosana, sija: osanto_tA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "kenenäkään",  luokka: asemosana, sija: olento_nA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "keneksikään", luokka: asemosana, sija: tulento_ksi, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "kenessäkään", luokka: asemosana, sija: sisäolento_ssA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "kenestäkään", luokka: asemosana, sija: sisäeronto_stA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "keneenkään",  luokka: asemosana, sija: sisätulento_Vn, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "kenelläkään", luokka: asemosana, sija: ulko_olento_llA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "keneltäkään", luokka: asemosana, sija: ulkoeronto_ltA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "kenellekään", luokka: asemosana, sija: ulkotulento_lle, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "kenettäkään", luokka: asemosana, sija: vajanto_ttA, luku: yksikkö, jatko: @loppu, äs: ä];

[perusmuoto: "kukaan", alku: "kessään",   luokka: asemosana, sija: sisäolento_ssA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "kestään",   luokka: asemosana, sija: sisäeronto_stA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "kehenkään", luokka: asemosana, sija: sisätulento_hVn, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "kellään",   luokka: asemosana, sija: ulko_olento_llA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "keltään",   luokka: asemosana, sija: ulkoeronto_ltA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "kellekään", luokka: asemosana, sija: ulkotulento_lle, luku: yksikkö, jatko: @loppu, äs: ä];

[perusmuoto: "kukaan", alku: "ketkään",     luokka: asemosana, sija: nimentö_tkA, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "keidenkään",  luokka: asemosana, sija: omanto_iT, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "keittenkään", luokka: asemosana, sija: omanto_iT, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "keitään",     luokka: asemosana, sija: osanto_itA, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "keinäkään",   luokka: asemosana, sija: olento_inA, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "keiksikään",  luokka: asemosana, sija: tulento_iksi, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "keissäkään",  luokka: asemosana, sija: sisäolento_issA, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "keistäkään",  luokka: asemosana, sija: sisäeronto_istA, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "keihinkään",  luokka: asemosana, sija: sisätulento_ihin, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "keilläkään",  luokka: asemosana, sija: ulko_olento_illA, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "keiltäkään",  luokka: asemosana, sija: ulkoeronto_iltA, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "keillekään",  luokka: asemosana, sija: ulkotulento_ille, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "keittäkään",  luokka: asemosana, sija: vajanto_ittA, luku: monikko, jatko: @loppu, äs: ä];


########################################


[perusmuoto: "mikä", alku: "mikä",    luokka: asemosana, sija: nimentö, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "mikä", alku: "minkä",   luokka: asemosana, sija: omanto_nkA, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "mikä", alku: "mitä",    luokka: asemosana, sija: osanto_tA, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "mikä", alku: "minä",    luokka: asemosana, sija: olento_nA, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "mikä", alku: "miksi",   luokka: asemosana, sija: tulento_ksi, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "mikä", alku: "miksikä", luokka: asemosana, sija: tulento_ksi, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "mikä", alku: "missä",   luokka: asemosana, sija: sisäolento_ssA, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "mikä", alku: "missään", luokka: asemosana, sija: sisäolento_ssA, jatko: @loppu, äs: ä];
[perusmuoto: "mikä", alku: "mistä",   luokka: asemosana, sija: sisäeronto_stA, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "mikä", alku: "mistään", luokka: asemosana, sija: sisäeronto_stA, jatko: @loppu, äs: ä];
[perusmuoto: "mikä", alku: "mihin",   luokka: asemosana, sija: sisätulento_hVn, jatko: @loppu, äs: ä];
[perusmuoto: "mikä", alku: "mihinkä", luokka: asemosana, sija: sisätulento_hVn, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "mikä", alku: "minne",   luokka: asemosana, sija: sisätulento_nne, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "mikä", alku: "minnekä", luokka: asemosana, sija: sisätulento_nne, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "mikä", alku: "millä",   luokka: asemosana, sija: ulko_olento_llA, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "mikä", alku: "miltä",   luokka: asemosana, sija: ulkoeronto_ltA, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "mikä", alku: "mille",   luokka: asemosana, sija: ulkotulento_lle, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "mikä", alku: "millekä", luokka: asemosana, sija: ulkotulento_lle, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
##[perusmuoto: "mikä", alku: "mittä",   luokka: asemosana, sija: vajanto_ttA, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "mikä", alku: "mitkä",   luokka: asemosana, sija: nimentö_tkA, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "mikä", alku: "mitkään", luokka: asemosana, sija: nimentö_tkA, jatko: @loppu, äs: ä];

[perusmuoto: "miten", alku: "miten", luokka: seikkasana, jatko: @loppu, äs: ä];
[perusmuoto: "miten", alku: "mitenkä", luokka: seikkasana, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "miten", alku: "mites", luokka: seikkasana, jatko: <loppu>, äs: ä];

[perusmuoto: "millänsäkään", alku: "millänikään",  luokka: seikkasana, jatko: @loppu, äs: a];
[perusmuoto: "millänsäkään", alku: "milläsikään",  luokka: seikkasana, jatko: @loppu, äs: a];
[perusmuoto: "millänsäkään", alku: "millänsäkään", luokka: seikkasana, jatko: @loppu, äs: a];
[perusmuoto: "millänsäkään", alku: "millämmekään", luokka: seikkasana, jatko: @loppu, äs: a];
[perusmuoto: "millänsäkään", alku: "millättekään", luokka: seikkasana, jatko: @loppu, äs: a]; # Onko sana?

######################################################

[perusmuoto: "se", alku: "se",     luokka: asemosana, sija: nimentö,         luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "se", alku: "sen",    luokka: asemosana, sija: omanto_n,        luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "se", alku: "sitä",   luokka: asemosana, sija: osanto_tA,       luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "se", alku: "sinä",   luokka: asemosana, sija: olento_nA,       luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "se", alku: "siksi",  luokka: asemosana, sija: tulento_ksi,     luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "se", alku: "siinä",  luokka: asemosana, sija: sisäolento_nA,   luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "se", alku: "siitä",  luokka: asemosana, sija: sisäeronto_tA,   luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "se", alku: "siihen", luokka: asemosana, sija: sisätulento_hVn, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "se", alku: "sillä",  luokka: asemosana, sija: ulko_olento_llA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "se", alku: "siltä",  luokka: asemosana, sija: ulkoeronto_ltA,  luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "se", alku: "sille",  luokka: asemosana, sija: ulkotulento_lle, luku: yksikkö, jatko: @loppu, äs: ä];

[perusmuoto: "siellä", alku: "siellä", luokka: asemosana, sija: ulko_olento_llA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "siellä", alku: "sieltä", luokka: asemosana, sija: ulkoeronto_ltA,  luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "siellä", alku: "sinne",  luokka: asemosana, sija: sisätulento_nne, luku: yksikkö, jatko: @loppu, äs: ä];

######################################################

[perusmuoto: "ne", alku: "ne",      luokka: asemosana, sija: nimentö,          luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ne", alku: "niiden",  luokka: asemosana, sija: omanto_iT,        luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ne", alku: "niitten", luokka: asemosana, sija: omanto_iT,        luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ne", alku: "niitä",   luokka: asemosana, sija: osanto_itA,       luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ne", alku: "niinä",   luokka: asemosana, sija: olento_inA,       luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ne", alku: "niiksi",  luokka: asemosana, sija: tulento_iksi,     luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ne", alku: "niissä",  luokka: asemosana, sija: sisäolento_issA,  luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ne", alku: "niistä",  luokka: asemosana, sija: sisäeronto_istA,  luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ne", alku: "niihin",  luokka: asemosana, sija: sisätulento_ihin, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ne", alku: "niillä",  luokka: asemosana, sija: ulko_olento_illA, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ne", alku: "niiltä",  luokka: asemosana, sija: ulkoeronto_iltA,  luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ne", alku: "niille",  luokka: asemosana, sija: ulkotulento_ille, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ne", alku: "niittä",  luokka: asemosana, sija: vajanto_ittA,     luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ne", alku: "niine",   luokka: asemosana, sija: seuranto_ine,     luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ne", alku: "niin",    luokka: asemosana, sija: keinonto_in,      luku: monikko, jatko: @loppu, äs: ä];

######################################################

[perusmuoto: "tämä", alku: "tämä",   luokka: asemosana, sija: nimentö,         luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "tämä", alku: "tämän",  luokka: asemosana, sija: omanto_n,        luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "tämä", alku: "tätä",   luokka: asemosana, sija: osanto_tA,       luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "tämä", alku: "tänä",   luokka: asemosana, sija: olento_nA,       luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "tämä", alku: "täksi",  luokka: asemosana, sija: tulento_ksi,     luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "tämä", alku: "tässä",  luokka: asemosana, sija: sisäolento_ssA,  luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "tämä", alku: "tästä",  luokka: asemosana, sija: sisäeronto_stA,  luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "tämä", alku: "tähän",  luokka: asemosana, sija: sisätulento_hVn, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "tämä", alku: "tällä",  luokka: asemosana, sija: ulko_olento_llA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "tämä", alku: "tältä",  luokka: asemosana, sija: ulkoeronto_ltA,  luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "tämä", alku: "tälle",  luokka: asemosana, sija: ulkotulento_lle, luku: yksikkö, jatko: @loppu, äs: ä];

# Murteissa.
[perusmuoto: "tämä", alku: "tää",    luokka: asemosana, sija: nimentö,         luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "tämä", alku: "tän",    luokka: asemosana, sija: omanto_n,        luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "tämä", alku: "tään",   luokka: asemosana, sija: omanto_n,        luku: yksikkö, jatko: @loppu, äs: ä];


######################################################


[perusmuoto: "täällä", alku: "täällä", luokka: asemosana, sija: ulko_olento_llA, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "täällä", alku: "täältä", luokka: asemosana, sija: ulkoeronto_ltA,  luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "täällä", alku: "tänne",  luokka: asemosana, sija: sisätulento_nne, luku: yksikkö, jatko: @loppu, äs: ä];


######################################################


[perusmuoto: "nämä", alku: "nämä",    luokka: asemosana, sija: nimentö,          luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "nämä", alku: "näiden",  luokka: asemosana, sija: omanto_iT,        luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "nämä", alku: "näitten", luokka: asemosana, sija: omanto_iT,        luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "nämä", alku: "nämät",   luokka: asemosana, sija: kohdanto_t,       luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "nämä", alku: "näitä",   luokka: asemosana, sija: osanto_itA,       luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "nämä", alku: "näinä",   luokka: asemosana, sija: olento_inA,       luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "nämä", alku: "näiksi",  luokka: asemosana, sija: tulento_iksi,     luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "nämä", alku: "näissä",  luokka: asemosana, sija: sisäolento_issA,  luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "nämä", alku: "näistä",  luokka: asemosana, sija: sisäeronto_istA,  luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "nämä", alku: "näihin",  luokka: asemosana, sija: sisätulento_ihin, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "nämä", alku: "näillä",  luokka: asemosana, sija: ulko_olento_illA, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "nämä", alku: "näiltä",  luokka: asemosana, sija: ulkoeronto_iltA,  luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "nämä", alku: "näille",  luokka: asemosana, sija: ulkotulento_ille, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "nämä", alku: "näittä",  luokka: asemosana, sija: vajanto_ittA,     luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "nämä", alku: "näine",   luokka: asemosana, sija: seuranto_ine,     luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "nämä", alku: "näin",    luokka: asemosana, sija: keinonto_in,      luku: monikko, jatko: @loppu, äs: ä];

# Murteissa.
[perusmuoto: "nämä", alku: "nää",     luokka: asemosana, sija: nimentö,          luku: monikko, jatko: @loppu, äs: ä];

######################################################

[perusmuoto: "tuo", alku: "tuo",    luokka: asemosana, sija: nimentö,         luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "tuo", alku: "tuon",   luokka: asemosana, sija: omanto_n,        luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "tuo", alku: "tuota",  luokka: asemosana, sija: osanto_tA,       luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "tuo", alku: "tuona",  luokka: asemosana, sija: olento_nA,       luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "tuo", alku: "tuoksi", luokka: asemosana, sija: tulento_ksi,     luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "tuo", alku: "tuossa", luokka: asemosana, sija: sisäolento_ssA,  luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "tuo", alku: "tuosta", luokka: asemosana, sija: sisäeronto_stA,  luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "tuo", alku: "tuohon", luokka: asemosana, sija: sisätulento_hVn, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "tuo", alku: "tuolla", luokka: asemosana, sija: ulko_olento_llA, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "tuo", alku: "tuolta", luokka: asemosana, sija: ulkoeronto_ltA,  luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "tuo", alku: "tuolle", luokka: asemosana, sija: ulkotulento_lle, luku: yksikkö, jatko: @loppu, äs: a];

[perusmuoto: "tuo", alku: "tuonne", luokka: asemosana, sija: sisätulento_nne, luku: yksikkö, jatko: @loppu, äs: a];

######################################################

[perusmuoto: "nuo", alku: "nuo",     luokka: asemosana, sija: nimentö,          luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "nuo", alku: "noiden",  luokka: asemosana, sija: omanto_iT,        luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "nuo", alku: "noitten", luokka: asemosana, sija: omanto_iT,        luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "nuo", alku: "noita",   luokka: asemosana, sija: osanto_itA,       luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "nuo", alku: "noina",   luokka: asemosana, sija: olento_inA,       luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "nuo", alku: "noiksi",  luokka: asemosana, sija: tulento_iksi,     luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "nuo", alku: "noissa",  luokka: asemosana, sija: sisäolento_issA,  luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "nuo", alku: "noista",  luokka: asemosana, sija: sisäeronto_istA,  luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "nuo", alku: "noihin",  luokka: asemosana, sija: sisätulento_ihin, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "nuo", alku: "noilla",  luokka: asemosana, sija: ulko_olento_illA, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "nuo", alku: "noilta",  luokka: asemosana, sija: ulkoeronto_iltA,  luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "nuo", alku: "noilla",  luokka: asemosana, sija: ulkotulento_ille, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "nuo", alku: "noitta",  luokka: asemosana, sija: vajanto_ittA,     luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "nuo", alku: "noine",   luokka: asemosana, sija: seuranto_ine,     luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "nuo", alku: "noin",    luokka: asemosana, sija: keinonto_in,      luku: monikko, jatko: @loppu, äs: a];

######################################################


# Nämä taitavat olla ainoat sanat, jotka taipuvat näin, ja
# on helpointa tallentaa ne erikseen kuin tehdä uusi sääntö.

[alku: "kuulteni",  luokka: teonsana, tapaluokka: nimitapa_2, sija: keinonto_n,
 jatko: @loppu, perusmuoto: "kuulla", äs: a];
[alku: "kuultesi",  luokka: teonsana, tapaluokka: nimitapa_2, sija: keinonto_n,
 jatko: @loppu, perusmuoto: "kuulla", äs: a];
[alku: "kuultensa", luokka: teonsana, tapaluokka: nimitapa_2, sija: keinonto_n,
 jatko: @loppu, perusmuoto: "kuulla", äs: a];
[alku: "kuultemme", luokka: teonsana, tapaluokka: nimitapa_2, sija: keinonto_n,
 jatko: @loppu, perusmuoto: "kuulla", äs: a];
[alku: "kuultenne", luokka: teonsana, tapaluokka: nimitapa_2, sija: keinonto_n,
 jatko: @loppu, perusmuoto: "kuulla", äs: a];

[alku: "nähteni",  luokka: teonsana, tapaluokka: nimitapa_2, sija: keinonto_n,
 jatko: @loppu, perusmuoto: "nähdä", äs: ä];
[alku: "nähtesi",  luokka: teonsana, tapaluokka: nimitapa_2, sija: keinonto_n,
 jatko: @loppu, perusmuoto: "nähdä", äs: ä];
[alku: "nähtensä", luokka: teonsana, tapaluokka: nimitapa_2, sija: keinonto_n,
 jatko: @loppu, perusmuoto: "nähdä", äs: ä];
[alku: "nähtemme", luokka: teonsana, tapaluokka: nimitapa_2, sija: keinonto_n,
 jatko: @loppu, perusmuoto: "nähdä", äs: ä];
[alku: "nähtenne", luokka: teonsana, tapaluokka: nimitapa_2, sija: keinonto_n,
 jatko: @loppu, perusmuoto: "nähdä", äs: ä];

[alku: "tieteni",  luokka: teonsana, tapaluokka: nimitapa_2, sija: keinonto_n,
 jatko: @loppu, perusmuoto: "tietää", äs: ä];
[alku: "tietesi",  luokka: teonsana, tapaluokka: nimitapa_2, sija: keinonto_n,
 jatko: @loppu, perusmuoto: "tietää", äs: ä];
[alku: "tietensä", luokka: teonsana, tapaluokka: nimitapa_2, sija: keinonto_n,
 jatko: @loppu, perusmuoto: "tietää", äs: ä];
[alku: "tietemme", luokka: teonsana, tapaluokka: nimitapa_2, sija: keinonto_n,
 jatko: @loppu, perusmuoto: "tietää", äs: ä];
[alku: "tietenne", luokka: teonsana, tapaluokka: nimitapa_2, sija: keinonto_n,
 jatko: @loppu, perusmuoto: "tietää", äs: ä];


[perusmuoto: "kauas", alku: "kaukaa",  luokka: seikkasana, jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "kauas", alku: "kaukana", luokka: seikkasana, jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "kauas", alku: "kauaksi", luokka: seikkasana, jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "kauas", alku: "kauaa",   luokka: seikkasana, jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "kauas", alku: "kauan",   luokka: seikkasana, jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "kauas", alku: "kauas",   luokka: seikkasana, jatko: <liitesana, loppu>, äs: a];

[perusmuoto: "kauas", alku: "kauempaa",   luokka: seikkasana, jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "kauas", alku: "kauempana",  luokka: seikkasana, jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "kauas", alku: "kauemmaksi", luokka: seikkasana, jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "kauas", alku: "kauemmin",   luokka: seikkasana, jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "kauas", alku: "kauemmas",   luokka: seikkasana, jatko: <liitesana, loppu>, äs: a];

[perusmuoto: "kauas", alku: "kauimpaa",   luokka: seikkasana, jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "kauas", alku: "kauimpana",  luokka: seikkasana, jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "kauas", alku: "kauimmaksi", luokka: seikkasana, jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "kauas", alku: "kauimmin",   luokka: seikkasana, jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "kauas", alku: "kauimmas",   luokka: seikkasana, jatko: <liitesana, loppu>, äs: a];

######################################################

# Joku näistä oli valittava perusmuodoksi. (-:

[perusmuoto: "edessä", alku: "etee", luokka: seikkasana, jatko: <omistusliite>, äs: ä];
[perusmuoto: "edessä", alku: "eteen", luokka: seikkasana, jatko: <liitesana, loppu>, äs: ä];
[perusmuoto: "edessä", alku: "ede", luokka: seikkasana, jatko: @heikkoasteiset_paikallissijat, äs: ä];

[perusmuoto: "edelläoleva", alku: "edelläolev", luokka: laatusana, jatko: <asema>, äs: a];
[perusmuoto: "edessäoleva", alku: "edessäolev", luokka: nimisana, jatko: <asema>, äs: a];
[perusmuoto: "edeltäkäsin", alku: "edeltäkäsin", luokka: seikkasana, jatko: <liitesana, loppu>, äs: ä];

[perusmuoto: "eteenpäin", alku: "edeltäpäin", luokka: seikkasana, jatko: <liitesana, loppu>, äs: ä];
[perusmuoto: "eteenpäin", alku: "edessäpäin", luokka: seikkasana, jatko: <liitesana, loppu>, äs: ä];
[perusmuoto: "eteenpäin", alku: "edestäpäin", luokka: seikkasana, jatko: <liitesana, loppu>, äs: ä];
[perusmuoto: "eteenpäin", alku: "eteenpäin",  luokka: seikkasana, jatko: <liitesana, loppu>, äs: ä];

######################################################

# Joku näistä oli valittava perusmuodoksi. (-:

[perusmuoto: "takana", alku: "takaa",  luokka: seikkasana, jatko: <omistusliite, liitesana, loppu>, äs: a];
[perusmuoto: "takana", alku: "takana", luokka: seikkasana, jatko: <omistusliite, liitesana, loppu>, äs: a];
[perusmuoto: "takana", alku: "taakse", luokka: seikkasana, jatko: <omistusliite, liitesana, loppu>, äs: a];

######################################################

[perusmuoto: "poikessa", alku: "poikessa", luokka: seikkasana, jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "poikessa", alku: "poikkeen", luokka: seikkasana, jatko: <liitesana, loppu>, äs: a];

######################################################

# Sivistyssanojen loppuosat.

[perusmuoto: "gamia", alku: "gami", luokka: nimisana, jatko: <karahka>, äs: a, tiedot: <siv, ei_ysa>];
[perusmuoto: "gami", alku: "gam", luokka: nimisana, jatko: <risti>, äs: a, tiedot: <ei_inen, siv, ei_ysa>];
[perusmuoto: "gaminen", alku: "gami", luokka: laatusana, jatko: <nainen>, äs: a, tiedot: <siv, ei_ysa>];

[perusmuoto: "grafia", alku: "grafi", luokka: nimisana, jatko: <karahka>, äs: a, tiedot: <siv, ei_ysa>];
[perusmuoto: "grafi", alku: "graf", luokka: nimisana, jatko: <risti>, äs: a, tiedot: <ei_inen, siv, ei_ysa>];
[perusmuoto: "grafinen", alku: "grafi", luokka: laatusana, jatko: <nainen>, äs: a, tiedot: <siv, ei_ysa>];

[perusmuoto: "logia", alku: "logi", luokka: nimisana, jatko: <karahka>, äs: a, tiedot: <siv, ei_ysa>];
[perusmuoto: "logi", alku: "log", luokka: nimisana, jatko: <risti>, äs: a, tiedot: <ei_inen, siv, ei_ysa>];
[perusmuoto: "loginen", alku: "logi", luokka: laatusana, jatko: <nainen>, äs: a, tiedot: <siv, ei_ysa>];

[perusmuoto: "nomia", alku: "nomi", luokka: nimisana, jatko: <karahka>, äs: a, tiedot: <siv, ei_ysa>];
[perusmuoto: "nomi", alku: "nom", luokka: nimisana, jatko: <risti>, äs: a, tiedot: <ei_inen, siv, ei_ysa>];
[perusmuoto: "nominen", alku: "nomi", luokka: laatusana, jatko: <nainen>, äs: a, tiedot: <siv, ei_ysa>];

[perusmuoto: "pedia", alku: "pedi", luokka: nimisana, jatko: <karahka>, äs: a, tiedot: <siv, ei_ysa>];
[perusmuoto: "pedi", alku: "ped", luokka: nimisana, jatko: <risti>, äs: a, tiedot: <ei_inen, siv, ei_ysa>];
[perusmuoto: "pedinen", alku: "pedi", luokka: laatusana, jatko: <nainen>, äs: a, tiedot: <siv, ei_ysa>];

[perusmuoto: "sofia", alku: "sofi", luokka: nimisana, jatko: <karahka>, äs: a, tiedot: <siv, ei_ysa>];
[perusmuoto: "sofi", alku: "sof", luokka: nimisana, jatko: <risti>, äs: a, tiedot: <ei_inen, siv, ei_ysa>];
[perusmuoto: "sofinen", alku: "sofi", luokka: laatusana, jatko: <nainen>, äs: a, tiedot: <siv, ei_ysa>];

