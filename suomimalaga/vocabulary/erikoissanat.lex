# Suomi-malaga, suomen kielen muoto-opin kuvaus.
#
# Tekijänoikeus © 2006-2008 Hannu Väisänen (Etunimi.Sukunimi@joensuu.fi)
#                 2007-2008 Teemu Likonen <tlikonen@iki.fi>
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


# Sanoja, joilla on niin erikoinen taivutus, että niiden
# taivutusmuodot on parasta esittää erikseen.

define @loppu := <liitesana, loppu>;

# Vain nämä muodot ovat yleisessä käytössä. Tuttu ja tuttava tajutaan eri sanoiksi.
[perusmuoto: "tuta", alku: "tuta", luokka: teonsana, jatko: <loppu>, äs: a, tiedot: <ei_ys>];
[perusmuoto: "tuta", alku: "tutaan", luokka: teonsana, jatko: <loppu>, äs: a, tiedot: <ei_ys>];

# Tosi harvinaiset muodot I infinitiivin pitkä muoto (vrt. puhuakse(ni)) ja II infinitiivin instruktiivi (vrt. puhuen).
[perusmuoto: "tuta", alku: "tutakseni", luokka: teonsana, jatko: <loppu>, äs: a, tiedot: <ei_ys, ei_voikko>];
[perusmuoto: "tuta", alku: "tutaksein", luokka: teonsana, jatko: <loppu>, äs: a, tiedot: <ei_ys, ei_voikko>];  # Tutakseni.
[perusmuoto: "tuta", alku: "tutaksesi", luokka: teonsana, jatko: <loppu>, äs: a, tiedot: <ei_ys, ei_voikko>];
[perusmuoto: "tuta", alku: "tutakseen", luokka: teonsana, jatko: <loppu>, äs: a, tiedot: <ei_ys, ei_voikko>];
[perusmuoto: "tuta", alku: "tutaksensa", luokka: teonsana, jatko: <loppu>, äs: a, tiedot: <ei_ys, ei_voikko>];
[perusmuoto: "tuta", alku: "tutaksemme", luokka: teonsana, jatko: <loppu>, äs: a, tiedot: <ei_ys, ei_voikko>];
[perusmuoto: "tuta", alku: "tutaksenne", luokka: teonsana, jatko: <loppu>, äs: a, tiedot: <ei_ys, ei_voikko>];
[perusmuoto: "tuta", alku: "tuten", luokka: teonsana, jatko: <loppu>, äs: a, tiedot: <ei_ys, ei_voikko>];


[perusmuoto: "jokin", alku: "jokin", luokka: asemosana, sijamuoto: nimentö, luku: yksikkö, jatko: <loppu>, äs: a];
[perusmuoto: "jokin", alku: "jonkin", luokka: asemosana, sijamuoto: omanto, luku: yksikkö, jatko: <loppu>, äs: a];
[perusmuoto: "jokin", alku: "jotakin", luokka: asemosana, sijamuoto: osanto, luku: yksikkö, jatko: <loppu>, äs: a];
[perusmuoto: "jokin", alku: "jonakin", luokka: asemosana, sijamuoto: olento, luku: yksikkö, jatko: <loppu>, äs: a];
[perusmuoto: "jokin", alku: "joksikin", luokka: asemosana, sijamuoto: tulento, luku: yksikkö, jatko: <loppu>, äs: a];
[perusmuoto: "jokin", alku: "jossakin", luokka: asemosana, sijamuoto: sisäolento, luku: yksikkö, jatko: <loppu>, äs: a];
[perusmuoto: "jokin", alku: "jostakin", luokka: asemosana, sijamuoto: sisäeronto, luku: yksikkö, jatko: <loppu>, äs: a];
[perusmuoto: "jokin", alku: "johonkin", luokka: asemosana, sijamuoto: sisätulento, luku: yksikkö, jatko: <loppu>, äs: a];
[perusmuoto: "jokin", alku: "jollakin", luokka: asemosana, sijamuoto: ulko_olento, luku: yksikkö, jatko: <loppu>, äs: a];
[perusmuoto: "jokin", alku: "joltakin", luokka: asemosana, sijamuoto: ulkoeronto, luku: yksikkö, jatko: <loppu>, äs: a];
[perusmuoto: "jokin", alku: "jollekin", luokka: asemosana, sijamuoto: ulkotulento, luku: yksikkö, jatko: <loppu>, äs: a];
[perusmuoto: "jokin", alku: "jottakin", luokka: asemosana, sijamuoto: vajanto, luku: yksikkö, jatko: <loppu>, äs: a];

[perusmuoto: "jokin", alku: "jotkin", luokka: asemosana, sijamuoto: nimentö, luku: monikko, jatko: <loppu>, äs: a];
[perusmuoto: "jokin", alku: "joittenkin", luokka: asemosana, sijamuoto: omanto, luku: monikko, jatko: <loppu>, äs: a];
[perusmuoto: "jokin", alku: "joidenkin", luokka: asemosana, sijamuoto: omanto, luku: monikko, jatko: <loppu>, äs: a];
[perusmuoto: "jokin", alku: "joitakin", luokka: asemosana, sijamuoto: osanto, luku: monikko, jatko: <loppu>, äs: a];
[perusmuoto: "jokin", alku: "joinakin", luokka: asemosana, sijamuoto: olento, luku: monikko, jatko: <loppu>, äs: a];
[perusmuoto: "jokin", alku: "joiksikin", luokka: asemosana, sijamuoto: tulento, luku: monikko, jatko: <loppu>, äs: a];
[perusmuoto: "jokin", alku: "joissakin", luokka: asemosana, sijamuoto: sisäolento, luku: monikko, jatko: <loppu>, äs: a];
[perusmuoto: "jokin", alku: "joistakin", luokka: asemosana, sijamuoto: sisäeronto, luku: monikko, jatko: <loppu>, äs: a];
[perusmuoto: "jokin", alku: "joihinkin", luokka: asemosana, sijamuoto: sisätulento, luku: monikko, jatko: <loppu>, äs: a];
[perusmuoto: "jokin", alku: "joillakin", luokka: asemosana, sijamuoto: ulko_olento, luku: monikko, jatko: <loppu>, äs: a];
[perusmuoto: "jokin", alku: "joiltakin", luokka: asemosana, sijamuoto: ulkoeronto, luku: monikko, jatko: <loppu>, äs: a];
[perusmuoto: "jokin", alku: "joillekin", luokka: asemosana, sijamuoto: ulkotulento, luku: monikko, jatko: <loppu>, äs: a];
[perusmuoto: "jokin", alku: "joittakin", luokka: asemosana, sijamuoto: vajanto, luku: monikko, jatko: <loppu>, äs: a];


[perusmuoto: "jokin", alku: "jotain", luokka: asemosana, sijamuoto: osanto, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "jonain", luokka: asemosana, sijamuoto: olento, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "jossain", luokka: asemosana, sijamuoto: sisäolento, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "jostain", luokka: asemosana, sijamuoto: sisäeronto, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "jollain", luokka: asemosana, sijamuoto: ulko_olento, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "joltain", luokka: asemosana, sijamuoto: ulkoeronto, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "jottain", luokka: asemosana, sijamuoto: vajanto, luku: yksikkö, jatko: @loppu, äs: a];

[perusmuoto: "jokin", alku: "joitain", luokka: asemosana, sijamuoto: osanto, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "joinain", luokka: asemosana, sijamuoto: olento, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "joissain", luokka: asemosana, sijamuoto: sisäolento, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "joistain", luokka: asemosana, sijamuoto: sisäeronto, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "joillain", luokka: asemosana, sijamuoto: ulko_olento, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "joiltain", luokka: asemosana, sijamuoto: ulkoeronto, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "jokin", alku: "joittain", luokka: asemosana, sijamuoto: vajanto, luku: monikko, jatko: @loppu, äs: a];

######################################################

[perusmuoto: "joku", alku: "joku", luokka: asemosana, sijamuoto: nimentö, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "joku", alku: "jonkun", luokka: asemosana, sijamuoto: omanto, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "joku", alku: "jotakuta", luokka: asemosana, sijamuoto: osanto, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=pppp=pppp"];
[perusmuoto: "joku", alku: "jonakuna", luokka: asemosana, sijamuoto: olento, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=pppp=pppp"];
[perusmuoto: "joku", alku: "joksikuksi", luokka: asemosana, sijamuoto: tulento, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=ppppp=ppppp"];
[perusmuoto: "joku", alku: "jossakussa", luokka: asemosana, sijamuoto: sisäolento, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=ppppp=ppppp"];
[perusmuoto: "joku", alku: "jostakusta", luokka: asemosana, sijamuoto: sisäeronto, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=ppppp=ppppp"];
[perusmuoto: "joku", alku: "johonkuhun", luokka: asemosana, sijamuoto: sisätulento, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=ppppp=ppppp"];
[perusmuoto: "joku", alku: "jollakulla", luokka: asemosana, sijamuoto: ulko_olento, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=ppppp=ppppp"];
[perusmuoto: "joku", alku: "joltakulta", luokka: asemosana, sijamuoto: ulkoeronto, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=ppppp=ppppp"];
[perusmuoto: "joku", alku: "jollekulle", luokka: asemosana, sijamuoto: ulkotulento, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=ppppp=ppppp"];
[perusmuoto: "joku", alku: "jottakutta", luokka: asemosana, sijamuoto: vajanto, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=ppppp=ppppp"];

[perusmuoto: "joku", alku: "jotkut", luokka: asemosana, sijamuoto: nimentö, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "joku", alku: "joittenkuitten", luokka: asemosana, sijamuoto: omanto, luku: monikko, jatko: @loppu, äs: a, rakenne: "=ppppppp=ppppppp"];
[perusmuoto: "joku", alku: "joidenkuiden", luokka: asemosana, sijamuoto: omanto, luku: monikko, jatko: @loppu, äs: a, rakenne: "=pppppp=pppppp"];
[perusmuoto: "joku", alku: "joitakuita", luokka: asemosana, sijamuoto: osanto, luku: monikko, jatko: @loppu, äs: a, rakenne: "=ppppp=ppppp"];
[perusmuoto: "joku", alku: "joinakuina", luokka: asemosana, sijamuoto: olento, luku: monikko, jatko: @loppu, äs: a, rakenne: "=ppppp=ppppp"];
[perusmuoto: "joku", alku: "joiksikuiksi", luokka: asemosana, sijamuoto: tulento, luku: monikko, jatko: @loppu, äs: a, rakenne: "=pppppp=pppppp"];
[perusmuoto: "joku", alku: "joissakuissa", luokka: asemosana, sijamuoto: sisäolento, luku: monikko, jatko: @loppu, äs: a, rakenne: "=pppppp=pppppp"];
[perusmuoto: "joku", alku: "joistakuista", luokka: asemosana, sijamuoto: sisäeronto, luku: monikko, jatko: @loppu, äs: a, rakenne: "=pppppp=pppppp"];
[perusmuoto: "joku", alku: "joihinkuihin", luokka: asemosana, sijamuoto: sisätulento, luku: monikko, jatko: @loppu, äs: a, rakenne: "=pppppp=pppppp"];
[perusmuoto: "joku", alku: "joillakuilla", luokka: asemosana, sijamuoto: ulko_olento, luku: monikko, jatko: @loppu, äs: a, rakenne: "=pppppp=pppppp"];
[perusmuoto: "joku", alku: "joiltakuilta", luokka: asemosana, sijamuoto: ulkoeronto, luku: monikko, jatko: @loppu, äs: a, rakenne: "=pppppp=pppppp"];
[perusmuoto: "joku", alku: "joillekuille", luokka: asemosana, sijamuoto: ulkotulento, luku: monikko, jatko: @loppu, äs: a, rakenne: "=pppppp=pppppp"];
[perusmuoto: "joku", alku: "joittakuitta", luokka: asemosana, sijamuoto: vajanto, luku: monikko, jatko: @loppu, äs: a, rakenne: "=pppppp=pppppp"];
[perusmuoto: "joku", alku: "joinekuine", luokka: asemosana, sijamuoto: seuranto, luku: monikko, jatko: <omistusliite, loppu>, äs: a, rakenne: "=ppppp=ppppp"];

######################################################

[perusmuoto: "jompikumpi", alku: "jompikumpi", luokka: asemosana, sijamuoto: nimentö, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=ppppp=ppppp"];
[perusmuoto: "jompikumpi", alku: "jommankumman", luokka: asemosana, sijamuoto: omanto, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=pppppp=pppppp"];
[perusmuoto: "jompikumpi", alku: "jompaakumpaa", luokka: asemosana, sijamuoto: osanto, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=pppppp=pppppp"];
[perusmuoto: "jompikumpi", alku: "jompanakumpana", luokka: asemosana, sijamuoto: olento, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=ppppppp=ppppppp"];
[perusmuoto: "jompikumpi", alku: "jommaksikummaksi", luokka: asemosana, sijamuoto: tulento, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=pppppppp=pppppppp"];
[perusmuoto: "jompikumpi", alku: "jommassakummassa", luokka: asemosana, sijamuoto: sisäolento, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=pppppppp=pppppppp"];
[perusmuoto: "jompikumpi", alku: "jommastakummasta", luokka: asemosana, sijamuoto: sisäeronto, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=pppppppp=pppppppp"];
[perusmuoto: "jompikumpi", alku: "jompaankumpaan", luokka: asemosana, sijamuoto: sisätulento, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=ppppppp=ppppppp"];
[perusmuoto: "jompikumpi", alku: "jommallakummalla", luokka: asemosana, sijamuoto: ulko_olento, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=pppppppp=pppppppp"];
[perusmuoto: "jompikumpi", alku: "jommaltakummalta", luokka: asemosana, sijamuoto: ulkoeronto, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=pppppppp=pppppppp"];
[perusmuoto: "jompikumpi", alku: "jommallekummalle", luokka: asemosana, sijamuoto: ulkotulento, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=pppppppp=pppppppp"];
[perusmuoto: "jompikumpi", alku: "jommattakummatta", luokka: asemosana, sijamuoto: vajanto, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=pppppppp=pppppppp"];

[perusmuoto: "jompikumpi", alku: "jommatkummat", luokka: asemosana, sijamuoto: nimentö, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=pppppp=pppppp"];
[perusmuoto: "jompikumpi", alku: "jompienkumpien", luokka: asemosana, sijamuoto: omanto, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=ppppppp=ppppppp"];
[perusmuoto: "jompikumpi", alku: "jompainkumpain", luokka: asemosana, sijamuoto: omanto, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=ppppppp=ppppppp"];
[perusmuoto: "jompikumpi", alku: "jompiakumpia", luokka: asemosana, sijamuoto: osanto, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=pppppp=pppppp"];
[perusmuoto: "jompikumpi", alku: "jompinakumpina", luokka: asemosana, sijamuoto: olento, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=ppppppp=ppppppp"];
[perusmuoto: "jompikumpi", alku: "jommiksikummiksi", luokka: asemosana, sijamuoto: tulento, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=pppppppp=pppppppp"];
[perusmuoto: "jompikumpi", alku: "jommissakummissa", luokka: asemosana, sijamuoto: sisäolento, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=pppppppp=pppppppp"];
[perusmuoto: "jompikumpi", alku: "jommistakummista", luokka: asemosana, sijamuoto: sisäeronto, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=pppppppp=pppppppp"];
[perusmuoto: "jompikumpi", alku: "jompiinkumpiin", luokka: asemosana, sijamuoto: sisätulento, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=ppppppp=ppppppp"];
[perusmuoto: "jompikumpi", alku: "jommillakummilla", luokka: asemosana, sijamuoto: ulko_olento, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=pppppppp=pppppppp"];
[perusmuoto: "jompikumpi", alku: "jommiltakummilta", luokka: asemosana, sijamuoto: ulkoeronto, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=pppppppp=pppppppp"];
[perusmuoto: "jompikumpi", alku: "jommillekummille", luokka: asemosana, sijamuoto: ulkotulento, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=pppppppp=pppppppp"];
[perusmuoto: "jompikumpi", alku: "jommittakummitta", luokka: asemosana, sijamuoto: vajanto, luku: yksikkö, jatko: @loppu, äs: a, rakenne: "=pppppppp=pppppppp"];
[perusmuoto: "jompikumpi", alku: "jompinekumpine", luokka: asemosana, sijamuoto: seuranto, luku: monikko, jatko: <omistusliite, loppu>, äs: a, rakenne: "=ppppppp=ppppppp"];
[perusmuoto: "jompikumpi", alku: "jomminkummin", luokka: asemosana, sijamuoto: keinonto, luku: monikko, jatko: @loppu, äs: a, rakenne: "=pppppp=pppppp"];

######################################################

# Tämä sana on vain yksikössä.
[perusmuoto: "mikin", alku: "mikin", luokka: asemosana, sijamuoto: nimentö, luku: yksikkö, jatko: <loppu>, äs: ä];
[perusmuoto: "mikin", alku: "minkin", luokka: asemosana, sijamuoto: omanto, luku: yksikkö, jatko: <loppu>, äs: ä];
[perusmuoto: "mikin", alku: "mitäkin", luokka: asemosana, sijamuoto: osanto, luku: yksikkö, jatko: <loppu>, äs: ä];
[perusmuoto: "mikin", alku: "minäkin", luokka: asemosana, sijamuoto: olento, luku: yksikkö, jatko: <loppu>, äs: ä];
[perusmuoto: "mikin", alku: "miksikin", luokka: asemosana, sijamuoto: tulento, luku: yksikkö, jatko: <loppu>, äs: ä];
[perusmuoto: "mikin", alku: "missäkin", luokka: asemosana, sijamuoto: sisäolento, luku: yksikkö, jatko: <loppu>, äs: ä];
[perusmuoto: "mikin", alku: "mistäkin", luokka: asemosana, sijamuoto: sisäeronto, luku: yksikkö, jatko: <loppu>, äs: ä];
[perusmuoto: "mikin", alku: "mihinkin", luokka: asemosana, sijamuoto: sisätulento, luku: yksikkö, jatko: <loppu>, äs: ä];
[perusmuoto: "mikin", alku: "milläkin", luokka: asemosana, sijamuoto: ulko_olento, luku: yksikkö, jatko: <loppu>, äs: ä];
[perusmuoto: "mikin", alku: "miltäkin", luokka: asemosana, sijamuoto: ulkoeronto, luku: yksikkö, jatko: <loppu>, äs: ä];
[perusmuoto: "mikin", alku: "millekin", luokka: asemosana, sijamuoto: ulkotulento, luku: yksikkö, jatko: <loppu>, äs: ä];
[perusmuoto: "mikin", alku: "mittäkin", luokka: asemosana, sijamuoto: vajanto, luku: yksikkö, jatko: <loppu>, äs: ä];

######################################################

[perusmuoto: "kukin", alku: "kukin", luokka: asemosana, sijamuoto: nimentö, luku: yksikkö, jatko: <loppu>, äs: a];
[perusmuoto: "kukin", alku: "kunkin", luokka: asemosana, sijamuoto: omanto, luku: yksikkö, jatko: <loppu>, äs: a];
[perusmuoto: "kukin", alku: "kutakin", luokka: asemosana, sijamuoto: osanto, luku: yksikkö, jatko: <loppu>, äs: a];
[perusmuoto: "kukin", alku: "kunakin", luokka: asemosana, sijamuoto: olento, luku: yksikkö, jatko: <loppu>, äs: a];
[perusmuoto: "kukin", alku: "kuksikin", luokka: asemosana, sijamuoto: tulento, luku: yksikkö, jatko: <loppu>, äs: a];
[perusmuoto: "kukin", alku: "kussakin", luokka: asemosana, sijamuoto: sisäolento, luku: yksikkö, jatko: <loppu>, äs: a];
[perusmuoto: "kukin", alku: "kustakin", luokka: asemosana, sijamuoto: sisäeronto, luku: yksikkö, jatko: <loppu>, äs: a];
[perusmuoto: "kukin", alku: "kuhunkin", luokka: asemosana, sijamuoto: sisätulento, luku: yksikkö, jatko: <loppu>, äs: a];
[perusmuoto: "kukin", alku: "kullakin", luokka: asemosana, sijamuoto: ulko_olento, luku: yksikkö, jatko: <loppu>, äs: a];
[perusmuoto: "kukin", alku: "kultakin", luokka: asemosana, sijamuoto: ulkoeronto, luku: yksikkö, jatko: <loppu>, äs: a];
[perusmuoto: "kukin", alku: "kullekin", luokka: asemosana, sijamuoto: ulkotulento, luku: yksikkö, jatko: <loppu>, äs: a];
[perusmuoto: "kukin", alku: "kuttakin", luokka: asemosana, sijamuoto: vajanto, luku: yksikkö, jatko: <loppu>, äs: a];

######################################################

# Osa näistä tunnistuu, vaikka olisi lippu "ei_voikko".
# itsekukin	= itse + kukka(instruktiivi)
# itsekustakin	= itse + kusi(yks. partitiivi) + kin
# itsekultakin	= itse + kulta(yks. nominatiivi) + kin
[perusmuoto: "itsekukin", alku: "itsekukin", luokka: asemosana, sijamuoto: nimentö, luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>, rakenne: "=pppp=ppppp"];
[perusmuoto: "itsekukin", alku: "itsekunkin", luokka: asemosana, sijamuoto: omanto, luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>, rakenne: "=pppp=pppppp"];
[perusmuoto: "itsekukin", alku: "itsekutakin", luokka: asemosana, sijamuoto: osanto, luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>, rakenne: "=pppp=ppppppp"];
[perusmuoto: "itsekukin", alku: "itsekunakin", luokka: asemosana, sijamuoto: olento, luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>, rakenne: "=pppp=ppppppp"];
[perusmuoto: "itsekukin", alku: "itsekuksikin", luokka: asemosana, sijamuoto: tulento, luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>, rakenne: "=pppp=pppppppp"];
[perusmuoto: "itsekukin", alku: "itsekussakin", luokka: asemosana, sijamuoto: sisäolento, luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>, rakenne: "=pppp=pppppppp"];
[perusmuoto: "itsekukin", alku: "itsekustakin", luokka: asemosana, sijamuoto: sisäeronto, luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>, rakenne: "=pppp=pppppppp"];
[perusmuoto: "itsekukin", alku: "itsekuhunkin", luokka: asemosana, sijamuoto: sisätulento, luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>, rakenne: "=pppp=pppppppp"];
[perusmuoto: "itsekukin", alku: "itsekullakin", luokka: asemosana, sijamuoto: ulko_olento, luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>, rakenne: "=pppp=pppppppp"];
[perusmuoto: "itsekukin", alku: "itsekultakin", luokka: asemosana, sijamuoto: ulkoeronto, luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>, rakenne: "=pppp=pppppppp"];
[perusmuoto: "itsekukin", alku: "itsekullekin", luokka: asemosana, sijamuoto: ulkotulento, luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>, rakenne: "=pppp=pppppppp"];
[perusmuoto: "itsekukin", alku: "itsekuttakin", luokka: asemosana, sijamuoto: vajanto, luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>, rakenne: "=pppp=pppppppp"];

######################################################

#  Tämä sana on vain yksikössä.
[perusmuoto: "mikään", alku: "mikään", luokka: asemosana, sijamuoto: nimentö, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikään", alku: "minkään", luokka: asemosana, sijamuoto: omanto, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikään", alku: "mitäkään", luokka: asemosana, sijamuoto: osanto, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikään", alku: "minäkään", luokka: asemosana, sijamuoto: olento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikään", alku: "miksikään", luokka: asemosana, sijamuoto: tulento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikään", alku: "missäkään", luokka: asemosana, sijamuoto: sisäolento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikään", alku: "mistäkään", luokka: asemosana, sijamuoto: sisäeronto, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikään", alku: "mihinkään", luokka: asemosana, sijamuoto: sisätulento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikään", alku: "milläkään", luokka: asemosana, sijamuoto: ulko_olento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikään", alku: "miltäkään", luokka: asemosana, sijamuoto: ulkoeronto, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikään", alku: "millekään", luokka: asemosana, sijamuoto: ulkotulento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikään", alku: "mittäkään", luokka: asemosana, sijamuoto: vajanto, luku: yksikkö, jatko: @loppu, äs: ä];

[perusmuoto: "mikään", alku: "mitään", luokka: asemosana, sijamuoto: osanto, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikään", alku: "minään", luokka: asemosana, sijamuoto: olento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikään", alku: "missään", luokka: asemosana, sijamuoto: sisäolento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikään", alku: "mistään", luokka: asemosana, sijamuoto: sisäeronto, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikään", alku: "millään", luokka: asemosana, sijamuoto: ulko_olento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "mikään", alku: "miltään", luokka: asemosana, sijamuoto: ulkoeronto, luku: yksikkö, jatko: @loppu, äs: ä];

[perusmuoto: "mikään", alku: "mitänä", luokka: asemosana, sijamuoto: olento, luku: yksikkö, jatko: @loppu, äs: ä];

######################################################

# Ka-loppuiset joka-sanan muodot paitsi omanto jonka.
[perusmuoto: "joka", alku: "joksika", luokka: asemosana, sijamuoto: tulento, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "joka", alku: "johonka", luokka: asemosana, sijamuoto: sisätulento, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "joka", alku: "jonneka", luokka: asemosana, sijamuoto: sisätulento, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "joka", alku: "jolleka", luokka: asemosana, sijamuoto: ulkotulento, luku: yksikkö, jatko: @loppu, äs: a];
 
[perusmuoto: "joka", alku: "joiksika", luokka: asemosana, sijamuoto: tulento, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "joka", alku: "joihinka", luokka: asemosana, sijamuoto: sisätulento, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "joka", alku: "joilleka", luokka: asemosana, sijamuoto: ulkotulento, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "joka", alku: "joidenka", luokka: asemosana, sijamuoto: omanto, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "joka", alku: "joittenka", luokka: asemosana, sijamuoto: omanto, luku: monikko, jatko: @loppu, äs: a];

######################################################

# Ka-loppuiset kuka-sanan muodot.
[perusmuoto: "kuka", alku: "kulleka",  luokka: asemosana, sijamuoto: ulkotulento, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "kuka", alku: "kuilleka", luokka: asemosana, sijamuoto: ulkotulento, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "kuka", alku: "kuhunka",  luokka: asemosana, sijamuoto: sisätulento, luku: yksikkö, jatko: @loppu, äs: a];


######################################################

[perusmuoto: "ken", alku: "ken",     luokka: asemosana, sijamuoto: nimentö, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kenen",   luokka: asemosana, sijamuoto: omanto, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kenet",   luokka: asemosana, sijamuoto: kohdanto, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "ketä",    luokka: asemosana, sijamuoto: osanto, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kenenä",  luokka: asemosana, sijamuoto: olento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keneksi", luokka: asemosana, sijamuoto: tulento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kenessä", luokka: asemosana, sijamuoto: sisäolento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kenestä", luokka: asemosana, sijamuoto: sisäeronto, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keneen",  luokka: asemosana, sijamuoto: sisätulento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kenellä", luokka: asemosana, sijamuoto: ulko_olento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keneltä", luokka: asemosana, sijamuoto: ulkoeronto, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kenelle", luokka: asemosana, sijamuoto: ulkotulento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kenettä", luokka: asemosana, sijamuoto: vajanto, luku: yksikkö, jatko: @loppu, äs: ä];

[perusmuoto: "ken", alku: "kenä",  luokka: asemosana, sijamuoto: olento,       luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kessä", luokka: asemosana, sijamuoto: sisäolento,  luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kestä", luokka: asemosana, sijamuoto: sisäeronto,  luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kehen", luokka: asemosana, sijamuoto: sisätulento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kellä", luokka: asemosana, sijamuoto: ulko_olento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keltä", luokka: asemosana, sijamuoto: ulkoeronto,  luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kelle", luokka: asemosana, sijamuoto: ulkotulento, luku: yksikkö, jatko: @loppu, äs: ä];

[perusmuoto: "ken", alku: "ketkä",   luokka: asemosana, sijamuoto: nimentö, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keiden",  luokka: asemosana, sijamuoto: omanto, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keitten", luokka: asemosana, sijamuoto: omanto, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keitä",   luokka: asemosana, sijamuoto: osanto, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keinä",   luokka: asemosana, sijamuoto: olento, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keiksi",  luokka: asemosana, sijamuoto: tulento, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keissä",  luokka: asemosana, sijamuoto: sisäolento, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keistä",  luokka: asemosana, sijamuoto: sisäeronto, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keihin",  luokka: asemosana, sijamuoto: sisätulento, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keillä",  luokka: asemosana, sijamuoto: ulko_olento, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keiltä",  luokka: asemosana, sijamuoto: ulkoeronto, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keille",  luokka: asemosana, sijamuoto: ulkotulento, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keittä",  luokka: asemosana, sijamuoto: vajanto, luku: monikko, jatko: @loppu, äs: ä];

# Kä-loppuisen yksikön muodot ken-sanasta.
[perusmuoto: "ken", alku: "kenenkä",   luokka: asemosana, sijamuoto: omanto, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kenenkäs",  luokka: asemosana, sijamuoto: omanto, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kenetkä",   luokka: asemosana, sijamuoto: kohdanto, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kenenäkä",  luokka: asemosana, sijamuoto: olento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keneksikä", luokka: asemosana, sijamuoto: tulento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kenessäkä", luokka: asemosana, sijamuoto: sisäolento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kenestäkä", luokka: asemosana, sijamuoto: sisäeronto, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keneenkä",  luokka: asemosana, sijamuoto: sisätulento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kenelläkä", luokka: asemosana, sijamuoto: ulko_olento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keneltäkä", luokka: asemosana, sijamuoto: ulkoeronto, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kenellekä", luokka: asemosana, sijamuoto: ulkotulento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kenettäkä", luokka: asemosana, sijamuoto: vajanto, luku: yksikkö, jatko: @loppu, äs: ä];

[perusmuoto: "ken", alku: "kellekä",   luokka: asemosana, sijamuoto: ulkotulento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keillekä",  luokka: asemosana, sijamuoto: ulkotulento, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kehenkä",   luokka: asemosana, sijamuoto: sisätulento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "kehenkäs",  luokka: asemosana, sijamuoto: sisätulento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keidenkä",  luokka: asemosana, sijamuoto: omanto, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ken", alku: "keittenkä", luokka: asemosana, sijamuoto: omanto, luku: monikko, jatko: @loppu, äs: ä];

# Kenelle-taivutusmuodon murteellisia muotoja.
[perusmuoto: "ken", alku: "kellen",     luokka: asemosana, sijamuoto: ulkotulento, luku: yksikkö, jatko: @loppu, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "ken", alku: "kellenkä",   luokka: asemosana, sijamuoto: ulkotulento, luku: yksikkö, jatko: @loppu, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "ken", alku: "kellenkäkö", luokka: asemosana, sijamuoto: ulkotulento, luku: yksikkö, jatko: @loppu, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "ken", alku: "kellenkänä", luokka: asemosana, sijamuoto: ulkotulento, luku: yksikkö, jatko: @loppu, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "ken", alku: "kellenkäs",  luokka: asemosana, sijamuoto: ulkotulento, luku: yksikkö, jatko: @loppu, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "ken", alku: "kellenkään", luokka: asemosana, sijamuoto: ulkotulento, luku: yksikkö, jatko: @loppu, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "ken", alku: "kellenpä",   luokka: asemosana, sijamuoto: ulkotulento, luku: yksikkö, jatko: @loppu, äs: ä, tiedot: <ei_voikko>];

##########################################################

# Oikeita kukaan-sanan muotoja
#
[perusmuoto: "kukaan", alku: "kukaan",    luokka: asemosana, sijamuoto: nimentö, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "kukaan", alku: "kussaan",   luokka: asemosana, sijamuoto: sisäolento, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "kukaan", alku: "kustaan",   luokka: asemosana, sijamuoto: sisäeronto, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "kukaan", alku: "kuhunkaan", luokka: asemosana, sijamuoto: sisätulento, luku: yksikkö, jatko: @loppu, äs: a];

# Kenkään-sanan muotoja.
#
#[perusmuoto: "kenkään", alku: "kenkään",     luokka: asemosana, sijamuoto: nimentö, luku: yksikkö, jatko: @loppu, äs: ä];

[perusmuoto: "kukaan", alku: "kenenkään",   luokka: asemosana, sijamuoto: omanto, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "kenetkään",   luokka: asemosana, sijamuoto: kohdanto, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "ketään",      luokka: asemosana, sijamuoto: osanto, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "kenenäkään",  luokka: asemosana, sijamuoto: olento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "keneksikään", luokka: asemosana, sijamuoto: tulento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "kenessäkään", luokka: asemosana, sijamuoto: sisäolento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "kenestäkään", luokka: asemosana, sijamuoto: sisäeronto, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "keneenkään",  luokka: asemosana, sijamuoto: sisätulento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "kenelläkään", luokka: asemosana, sijamuoto: ulko_olento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "keneltäkään", luokka: asemosana, sijamuoto: ulkoeronto, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "kenellekään", luokka: asemosana, sijamuoto: ulkotulento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "kenettäkään", luokka: asemosana, sijamuoto: vajanto, luku: yksikkö, jatko: @loppu, äs: ä];

[perusmuoto: "kukaan", alku: "kessään",   luokka: asemosana, sijamuoto: sisäolento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "kestään",   luokka: asemosana, sijamuoto: sisäeronto, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "kehenkään", luokka: asemosana, sijamuoto: sisätulento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "kellään",   luokka: asemosana, sijamuoto: ulko_olento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "keltään",   luokka: asemosana, sijamuoto: ulkoeronto, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "kellekään", luokka: asemosana, sijamuoto: ulkotulento, luku: yksikkö, jatko: @loppu, äs: ä];

[perusmuoto: "kukaan", alku: "ketkään",     luokka: asemosana, sijamuoto: nimentö, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "keidenkään",  luokka: asemosana, sijamuoto: omanto, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "keittenkään", luokka: asemosana, sijamuoto: omanto, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "keitään",     luokka: asemosana, sijamuoto: osanto, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "keinäkään",   luokka: asemosana, sijamuoto: olento, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "keiksikään",  luokka: asemosana, sijamuoto: tulento, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "keissäkään",  luokka: asemosana, sijamuoto: sisäolento, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "keistäkään",  luokka: asemosana, sijamuoto: sisäeronto, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "keihinkään",  luokka: asemosana, sijamuoto: sisätulento, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "keilläkään",  luokka: asemosana, sijamuoto: ulko_olento, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "keiltäkään",  luokka: asemosana, sijamuoto: ulkoeronto, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "keillekään",  luokka: asemosana, sijamuoto: ulkotulento, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "kukaan", alku: "keittäkään",  luokka: asemosana, sijamuoto: vajanto, luku: monikko, jatko: @loppu, äs: ä];


########################################


[perusmuoto: "mikä", alku: "mikä",    luokka: asemosana, sijamuoto: nimentö, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "mikä", alku: "minkä",   luokka: asemosana, sijamuoto: omanto, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "mikä", alku: "mitä",    luokka: asemosana, sijamuoto: osanto, jatko: <liitesana, loppu>, äs: ä];
[perusmuoto: "mikä", alku: "mitäs",   luokka: asemosana, sijamuoto: osanto, jatko: <loppu>, äs: ä];
[perusmuoto: "mikä", alku: "minä",    luokka: asemosana, sijamuoto: olento, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "mikä", alku: "miksi",   luokka: asemosana, sijamuoto: tulento, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "mikä", alku: "miksikä", luokka: asemosana, sijamuoto: tulento, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "mikä", alku: "missä",   luokka: asemosana, sijamuoto: sisäolento, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "mikä", alku: "missään", luokka: asemosana, sijamuoto: sisäolento, jatko: @loppu, äs: ä];
[perusmuoto: "mikä", alku: "mistä",   luokka: asemosana, sijamuoto: sisäeronto, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "mikä", alku: "mistään", luokka: asemosana, sijamuoto: sisäeronto, jatko: @loppu, äs: ä];
[perusmuoto: "mikä", alku: "mihin",   luokka: asemosana, sijamuoto: sisätulento, jatko: @loppu, äs: ä];
[perusmuoto: "mikä", alku: "mihinkä", luokka: asemosana, sijamuoto: sisätulento, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "mikä", alku: "minne",   luokka: asemosana, sijamuoto: sisätulento, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "mikä", alku: "minnekä", luokka: asemosana, sijamuoto: sisätulento, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "mikä", alku: "millä",   luokka: asemosana, sijamuoto: ulko_olento, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "mikä", alku: "miltä",   luokka: asemosana, sijamuoto: ulkoeronto, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "mikä", alku: "mille",   luokka: asemosana, sijamuoto: ulkotulento, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "mikä", alku: "millekä", luokka: asemosana, sijamuoto: ulkotulento, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
#[perusmuoto: "mikä", alku: "mittä",   luokka: asemosana, sijamuoto: vajanto, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "mikä", alku: "mitkä",   luokka: asemosana, sijamuoto: nimentö, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "mikä", alku: "mitkään", luokka: asemosana, sijamuoto: nimentö, jatko: @loppu, äs: ä];

######################################################

[perusmuoto: "se", alku: "se",     luokka: asemosana, sijamuoto: nimentö,         luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "se", alku: "sen",    luokka: asemosana, sijamuoto: omanto,        luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "se", alku: "sitä",   luokka: asemosana, sijamuoto: osanto,       luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "se", alku: "sinä",   luokka: asemosana, sijamuoto: olento,       luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "se", alku: "siksi",  luokka: asemosana, sijamuoto: tulento,     luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "se", alku: "siinä",  luokka: asemosana, sijamuoto: sisäolento,   luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "se", alku: "siitä",  luokka: asemosana, sijamuoto: sisäeronto,   luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "se", alku: "siihen", luokka: asemosana, sijamuoto: sisätulento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "se", alku: "sillä",  luokka: asemosana, sijamuoto: ulko_olento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "se", alku: "siltä",  luokka: asemosana, sijamuoto: ulkoeronto,  luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "se", alku: "sille",  luokka: asemosana, sijamuoto: ulkotulento, luku: yksikkö, jatko: @loppu, äs: ä];

######################################################

[perusmuoto: "ne", alku: "ne",      luokka: asemosana, sijamuoto: nimentö,          luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ne", alku: "niiden",  luokka: asemosana, sijamuoto: omanto,        luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ne", alku: "niitten", luokka: asemosana, sijamuoto: omanto,        luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ne", alku: "niitä",   luokka: asemosana, sijamuoto: osanto,       luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ne", alku: "niinä",   luokka: asemosana, sijamuoto: olento,       luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ne", alku: "niiksi",  luokka: asemosana, sijamuoto: tulento,     luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ne", alku: "niissä",  luokka: asemosana, sijamuoto: sisäolento,  luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ne", alku: "niistä",  luokka: asemosana, sijamuoto: sisäeronto,  luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ne", alku: "niihin",  luokka: asemosana, sijamuoto: sisätulento, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ne", alku: "niillä",  luokka: asemosana, sijamuoto: ulko_olento, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ne", alku: "niiltä",  luokka: asemosana, sijamuoto: ulkoeronto,  luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ne", alku: "niille",  luokka: asemosana, sijamuoto: ulkotulento, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ne", alku: "niittä",  luokka: asemosana, sijamuoto: vajanto,     luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ne", alku: "niine",   luokka: asemosana, sijamuoto: seuranto,     luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "ne", alku: "niin",    luokka: asemosana, sijamuoto: keinonto,      luku: monikko, jatko: @loppu, äs: ä];

######################################################

[perusmuoto: "tämä", alku: "tämä",   luokka: asemosana, sijamuoto: nimentö,         luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "tämä", alku: "tämän",  luokka: asemosana, sijamuoto: omanto,        luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "tämä", alku: "tätä",   luokka: asemosana, sijamuoto: osanto,       luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "tämä", alku: "tänä",   luokka: asemosana, sijamuoto: olento,       luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "tämä", alku: "täksi",  luokka: asemosana, sijamuoto: tulento,     luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "tämä", alku: "tässä",  luokka: asemosana, sijamuoto: sisäolento,  luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "tämä", alku: "tästä",  luokka: asemosana, sijamuoto: sisäeronto,  luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "tämä", alku: "tähän",  luokka: asemosana, sijamuoto: sisätulento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "tämä", alku: "tällä",  luokka: asemosana, sijamuoto: ulko_olento, luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "tämä", alku: "tältä",  luokka: asemosana, sijamuoto: ulkoeronto,  luku: yksikkö, jatko: @loppu, äs: ä];
[perusmuoto: "tämä", alku: "tälle",  luokka: asemosana, sijamuoto: ulkotulento, luku: yksikkö, jatko: @loppu, äs: ä];

# Murteissa.
[perusmuoto: "tämä", alku: "tää",    luokka: asemosana, sijamuoto: nimentö,  luku: yksikkö, jatko: @loppu, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "tämä", alku: "tän",    luokka: asemosana, sijamuoto: omanto, luku: yksikkö, jatko: @loppu, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "tämä", alku: "tään",   luokka: asemosana, sijamuoto: omanto, luku: yksikkö, jatko: @loppu, äs: ä, tiedot: <ei_voikko>];


######################################################


[perusmuoto: "nämä", alku: "nämä",    luokka: asemosana, sijamuoto: nimentö,          luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "nämä", alku: "näiden",  luokka: asemosana, sijamuoto: omanto,        luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "nämä", alku: "näitten", luokka: asemosana, sijamuoto: omanto,        luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "nämä", alku: "nämät",   luokka: asemosana, sijamuoto: kohdanto,       luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "nämä", alku: "näitä",   luokka: asemosana, sijamuoto: osanto,       luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "nämä", alku: "näinä",   luokka: asemosana, sijamuoto: olento,       luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "nämä", alku: "näiksi",  luokka: asemosana, sijamuoto: tulento,     luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "nämä", alku: "näissä",  luokka: asemosana, sijamuoto: sisäolento,  luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "nämä", alku: "näistä",  luokka: asemosana, sijamuoto: sisäeronto,  luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "nämä", alku: "näihin",  luokka: asemosana, sijamuoto: sisätulento, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "nämä", alku: "näillä",  luokka: asemosana, sijamuoto: ulko_olento, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "nämä", alku: "näiltä",  luokka: asemosana, sijamuoto: ulkoeronto,  luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "nämä", alku: "näille",  luokka: asemosana, sijamuoto: ulkotulento, luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "nämä", alku: "näittä",  luokka: asemosana, sijamuoto: vajanto,     luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "nämä", alku: "näine",   luokka: asemosana, sijamuoto: seuranto,     luku: monikko, jatko: @loppu, äs: ä];
[perusmuoto: "nämä", alku: "näin",    luokka: asemosana, sijamuoto: keinonto,      luku: monikko, jatko: @loppu, äs: ä];

# Murteissa.
[perusmuoto: "nämä", alku: "nää",     luokka: asemosana, sijamuoto: nimentö,          luku: monikko, jatko: @loppu, äs: ä, tiedot: <ei_voikko>];

######################################################

[perusmuoto: "tuo", alku: "tuo",    luokka: asemosana, sijamuoto: nimentö,         luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "tuo", alku: "tuon",   luokka: asemosana, sijamuoto: omanto,        luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "tuo", alku: "tuota",  luokka: asemosana, sijamuoto: osanto,       luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "tuo", alku: "tuona",  luokka: asemosana, sijamuoto: olento,       luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "tuo", alku: "tuoksi", luokka: asemosana, sijamuoto: tulento,     luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "tuo", alku: "tuossa", luokka: asemosana, sijamuoto: sisäolento,  luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "tuo", alku: "tuosta", luokka: asemosana, sijamuoto: sisäeronto,  luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "tuo", alku: "tuohon", luokka: asemosana, sijamuoto: sisätulento, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "tuo", alku: "tuolla", luokka: asemosana, sijamuoto: ulko_olento, luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "tuo", alku: "tuolta", luokka: asemosana, sijamuoto: ulkoeronto,  luku: yksikkö, jatko: @loppu, äs: a];
[perusmuoto: "tuo", alku: "tuolle", luokka: asemosana, sijamuoto: ulkotulento, luku: yksikkö, jatko: @loppu, äs: a];

[perusmuoto: "tuo", alku: "tuonne", luokka: asemosana, sijamuoto: sisätulento, luku: yksikkö, jatko: @loppu, äs: a];

######################################################

[perusmuoto: "nuo", alku: "nuo",     luokka: asemosana, sijamuoto: nimentö,          luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "nuo", alku: "noiden",  luokka: asemosana, sijamuoto: omanto,        luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "nuo", alku: "noitten", luokka: asemosana, sijamuoto: omanto,        luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "nuo", alku: "noita",   luokka: asemosana, sijamuoto: osanto,       luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "nuo", alku: "noina",   luokka: asemosana, sijamuoto: olento,       luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "nuo", alku: "noiksi",  luokka: asemosana, sijamuoto: tulento,     luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "nuo", alku: "noissa",  luokka: asemosana, sijamuoto: sisäolento,  luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "nuo", alku: "noista",  luokka: asemosana, sijamuoto: sisäeronto,  luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "nuo", alku: "noihin",  luokka: asemosana, sijamuoto: sisätulento, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "nuo", alku: "noilla",  luokka: asemosana, sijamuoto: ulko_olento, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "nuo", alku: "noilta",  luokka: asemosana, sijamuoto: ulkoeronto,  luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "nuo", alku: "noilla",  luokka: asemosana, sijamuoto: ulkotulento, luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "nuo", alku: "noitta",  luokka: asemosana, sijamuoto: vajanto,     luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "nuo", alku: "noine",   luokka: asemosana, sijamuoto: seuranto,     luku: monikko, jatko: @loppu, äs: a];
[perusmuoto: "nuo", alku: "noin",    luokka: asemosana, sijamuoto: keinonto,      luku: monikko, jatko: @loppu, äs: a];

######################################################

[perusmuoto: "minä", alku: "minä",    luokka: asemosana, sijamuoto: nimentö,         luku: yksikkö, jatko: @loppu, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "minä", alku: "minun",   luokka: asemosana, sijamuoto: omanto,        luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "minä", alku: "minut",   luokka: asemosana, sijamuoto: kohdanto,      luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "minä", alku: "minua",   luokka: asemosana, sijamuoto: osanto,        luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "minä", alku: "minuna",  luokka: asemosana, sijamuoto: olento,       luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "minä", alku: "minuksi", luokka: asemosana, sijamuoto: tulento,     luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "minä", alku: "minussa", luokka: asemosana, sijamuoto: sisäolento,  luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "minä", alku: "minusta", luokka: asemosana, sijamuoto: sisäeronto,  luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "minä", alku: "minuun",  luokka: asemosana, sijamuoto: sisätulento,  luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "minä", alku: "minulla", luokka: asemosana, sijamuoto: ulko_olento, luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "minä", alku: "minulta", luokka: asemosana, sijamuoto: ulkoeronto,  luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "minä", alku: "minulle", luokka: asemosana, sijamuoto: ulkotulento, luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "minä", alku: "minutta", luokka: asemosana, sijamuoto: vajanto,     luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>];

[perusmuoto: "sinä", alku: "sinä",    luokka: asemosana, sijamuoto: nimentö,         luku: yksikkö, jatko: @loppu, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "sinä", alku: "sinun",   luokka: asemosana, sijamuoto: omanto,        luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "sinä", alku: "sinut",   luokka: asemosana, sijamuoto: kohdanto,      luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "sinä", alku: "sinua",   luokka: asemosana, sijamuoto: osanto,        luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "sinä", alku: "sinuna",  luokka: asemosana, sijamuoto: olento,       luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "sinä", alku: "sinuksi", luokka: asemosana, sijamuoto: tulento,     luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "sinä", alku: "sinussa", luokka: asemosana, sijamuoto: sisäolento,  luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "sinä", alku: "sinusta", luokka: asemosana, sijamuoto: sisäeronto,  luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "sinä", alku: "sinuun",  luokka: asemosana, sijamuoto: sisätulento,  luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "sinä", alku: "sinulla", luokka: asemosana, sijamuoto: ulko_olento, luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "sinä", alku: "sinulta", luokka: asemosana, sijamuoto: ulkoeronto,  luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "sinä", alku: "sinulle", luokka: asemosana, sijamuoto: ulkotulento, luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "sinä", alku: "sinutta", luokka: asemosana, sijamuoto: vajanto,     luku: yksikkö, jatko: @loppu, äs: a, tiedot: <ei_voikko>];

# Puhekielisiä ja murteellisia muotoja.

[perusmuoto: "mä", alku: "mä",    luokka: asemosana, sijamuoto: nimentö,         luku: yksikkö, jatko: @loppu, äs: ä, tiedot: <murre>];
[perusmuoto: "mä", alku: "mun",   luokka: asemosana, sijamuoto: omanto,        luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "mä", alku: "mut",   luokka: asemosana, sijamuoto: kohdanto,      luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "mä", alku: "mua",   luokka: asemosana, sijamuoto: osanto,        luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "mä", alku: "muna",  luokka: asemosana, sijamuoto: olento,       luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "mä", alku: "muksi", luokka: asemosana, sijamuoto: tulento,     luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "mä", alku: "mussa", luokka: asemosana, sijamuoto: sisäolento,  luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "mä", alku: "musta", luokka: asemosana, sijamuoto: sisäeronto,  luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "mä", alku: "muhun", luokka: asemosana, sijamuoto: sisätulento, luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "mä", alku: "mulla", luokka: asemosana, sijamuoto: ulko_olento, luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "mä", alku: "multa", luokka: asemosana, sijamuoto: ulkoeronto,  luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "mä", alku: "mulle", luokka: asemosana, sijamuoto: ulkotulento, luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
#[perusmuoto: "mä", alku: "mutta", luokka: asemosana, sijamuoto: vajanto,     luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];

[perusmuoto: "sä", alku: "sä",    luokka: asemosana, sijamuoto: nimentö,         luku: yksikkö, jatko: @loppu, äs: ä, tiedot: <murre>];
[perusmuoto: "sä", alku: "sun",   luokka: asemosana, sijamuoto: omanto,        luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "sä", alku: "sut",   luokka: asemosana, sijamuoto: kohdanto,      luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "sä", alku: "sua",   luokka: asemosana, sijamuoto: osanto,        luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "sä", alku: "suna",  luokka: asemosana, sijamuoto: olento,       luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "sä", alku: "suksi", luokka: asemosana, sijamuoto: tulento,     luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "sä", alku: "sussa", luokka: asemosana, sijamuoto: sisäolento,  luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "sä", alku: "susta", luokka: asemosana, sijamuoto: sisäeronto,  luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "sä", alku: "suhun", luokka: asemosana, sijamuoto: sisätulento, luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "sä", alku: "sulla", luokka: asemosana, sijamuoto: ulko_olento, luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "sä", alku: "sulta", luokka: asemosana, sijamuoto: ulkoeronto,  luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "sä", alku: "sulle", luokka: asemosana, sijamuoto: ulkotulento, luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
#[perusmuoto: "sä", alku: "sutta", luokka: asemosana, sijamuoto: vajanto,      luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];

[perusmuoto: "mie", alku: "mie",    luokka: asemosana, sijamuoto: nimentö,         luku: yksikkö, jatko: @loppu, äs: ä, tiedot: <murre>];
[perusmuoto: "mie", alku: "miun",   luokka: asemosana, sijamuoto: omanto,        luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "mie", alku: "miut",   luokka: asemosana, sijamuoto: kohdanto,      luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "mie", alku: "miuta",  luokka: asemosana, sijamuoto: osanto,       luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "mie", alku: "miuna",  luokka: asemosana, sijamuoto: olento,       luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "mie", alku: "miuksi", luokka: asemosana, sijamuoto: tulento,     luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "mie", alku: "miussa", luokka: asemosana, sijamuoto: sisäolento,  luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "mie", alku: "miusta", luokka: asemosana, sijamuoto: sisäeronto,  luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "mie", alku: "miuhun", luokka: asemosana, sijamuoto: sisätulento, luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "mie", alku: "miulla", luokka: asemosana, sijamuoto: ulko_olento, luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "mie", alku: "miulta", luokka: asemosana, sijamuoto: ulkoeronto,  luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "mie", alku: "miulle", luokka: asemosana, sijamuoto: ulkotulento, luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "mie", alku: "miutta", luokka: asemosana, sijamuoto: vajanto,     luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];

[perusmuoto: "sie", alku: "sie",    luokka: asemosana, sijamuoto: nimentö,         luku: yksikkö, jatko: @loppu, äs: ä, tiedot: <murre>];
[perusmuoto: "sie", alku: "siun",   luokka: asemosana, sijamuoto: omanto,        luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "sie", alku: "siut",   luokka: asemosana, sijamuoto: kohdanto,      luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "sie", alku: "siuta",  luokka: asemosana, sijamuoto: osanto,       luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "sie", alku: "siuna",  luokka: asemosana, sijamuoto: olento,       luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "sie", alku: "siuksi", luokka: asemosana, sijamuoto: tulento,     luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "sie", alku: "siussa", luokka: asemosana, sijamuoto: sisäolento,  luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "sie", alku: "siusta", luokka: asemosana, sijamuoto: sisäeronto,  luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "sie", alku: "siuhun", luokka: asemosana, sijamuoto: sisätulento, luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "sie", alku: "siulla", luokka: asemosana, sijamuoto: ulko_olento, luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "sie", alku: "siulta", luokka: asemosana, sijamuoto: ulkoeronto,  luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "sie", alku: "siulle", luokka: asemosana, sijamuoto: ulkotulento, luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];
[perusmuoto: "sie", alku: "siutta", luokka: asemosana, sijamuoto: vajanto,     luku: yksikkö, jatko: @loppu, äs: a, tiedot: <murre>];

#### Nimisanat ####

[perusmuoto: "meri", alku: "mer", luokka: nimisana, jatko: <meri>, äs: ä];
[perusmuoto: "meri", alku: "mer", luokka: nimisana, jatko: <osanto_tA>, äs: a];
[perusmuoto: "Itämeri", alku: "Itämer", luokka: paikannimi, jatko: <meri>, äs: ä, rakenne: "=ipp=ppp"];
[perusmuoto: "Itämeri", alku: "Itämer", luokka: paikannimi, jatko: <osanto_tA>, äs: a, rakenne: "=ipp=ppp"];
[perusmuoto: "veri", alku: "ver", luokka: nimisana, jatko: <meri>, äs: ä];
[perusmuoto: "veri", alku: "ver", luokka: nimisana, jatko: <osanto_tA>, äs: a];

# Oboe (Kotuksen taivutusluokka 3 "valtio") {{{1
[perusmuoto: "oboe", alku: "oboe", luokka: nimisana, jatko: @sana1 + <liitesana, omistusliite, loppu>, äs: a];
[perusmuoto: "oboe", alku: "oboe", luokka: nimisana, jatko: <omanto_n>, äs: a];
[perusmuoto: "oboe", alku: "oboe", luokka: nimisana, jatko: <osanto_tA>, äs: a];
[perusmuoto: "oboe", alku: "oboe", luokka: nimisana, jatko: <olento_nA>, äs: a];
[perusmuoto: "oboe", alku: "oboe", luokka: nimisana, jatko: <tulento_ksi>, äs: a];
[perusmuoto: "oboe", alku: "oboe", luokka: nimisana, jatko: <sisäolento_ssA>, äs: a];
[perusmuoto: "oboe", alku: "oboe", luokka: nimisana, jatko: <sisäeronto_stA>, äs: a];
[perusmuoto: "oboe", alku: "oboe", luokka: nimisana, jatko: <sisätulento_Vn>, äs: a];
[perusmuoto: "oboe", alku: "oboe", luokka: nimisana, jatko: <sisätulento_hVn>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "oboe", alku: "oboe", luokka: nimisana, jatko: @ulkopaikallissijat, äs: a];
[perusmuoto: "oboe", alku: "oboe", luokka: nimisana, jatko: <vajanto_ttA>, äs: a];

[perusmuoto: "oboe", alku: "oboe", luokka: nimisana, jatko: <nimentö_t>, äs: a];
[perusmuoto: "oboe", alku: "oboe", luokka: nimisana, jatko: <omanto_iT>, äs: a];
[perusmuoto: "oboe", alku: "oboe", luokka: nimisana, jatko: <osanto_itA>, äs: a];
[perusmuoto: "oboe", alku: "oboe", luokka: nimisana, jatko: <olento_inA>, äs: a];
[perusmuoto: "oboe", alku: "oboe", luokka: nimisana, jatko: <tulento_iksi>, äs: a];
[perusmuoto: "oboe", alku: "oboe", luokka: nimisana, jatko: <sisäolento_issA>, äs: a];
[perusmuoto: "oboe", alku: "oboe", luokka: nimisana, jatko: <sisäeronto_istA>, äs: a];
[perusmuoto: "oboe", alku: "oboe", luokka: nimisana, jatko: <sisätulento_ihin>, äs: a];
[perusmuoto: "oboe", alku: "oboe", luokka: nimisana, jatko: <ulko_olento_illA>, äs: a];
[perusmuoto: "oboe", alku: "oboe", luokka: nimisana, jatko: <ulkoeronto_iltA>, äs: a];
[perusmuoto: "oboe", alku: "oboe", luokka: nimisana, jatko: <ulkotulento_ille>, äs: a];
[perusmuoto: "oboe", alku: "oboe", luokka: nimisana, jatko: <vajanto_ittA>, äs: a];
[perusmuoto: "oboe", alku: "oboe", luokka: nimisana, jatko: <seuranto_ine>, äs: a];
[perusmuoto: "oboe", alku: "oboe", luokka: nimisana, jatko: <keinonto_in>, äs: a];

# rokokoo (Kotuksen taivutusluokka 18 "maa") {{{1
[perusmuoto: "rokokoo", alku: "rokokoo", luokka: nimisana, jatko: @sana1 + <liitesana, omistusliite, loppu>, äs: a];
[perusmuoto: "rokokoo", alku: "rokokoo", luokka: nimisana, jatko: <omanto_n>, äs: a];
[perusmuoto: "rokokoo", alku: "rokokoo", luokka: nimisana, jatko: <osanto_tA>, äs: a];
[perusmuoto: "rokokoo", alku: "rokokoo", luokka: nimisana, jatko: <olento_nA>, äs: a];
[perusmuoto: "rokokoo", alku: "rokokoo", luokka: nimisana, jatko: <tulento_ksi>, äs: a];
[perusmuoto: "rokokoo", alku: "rokokoo", luokka: nimisana, jatko: <sisäolento_ssA>, äs: a];
[perusmuoto: "rokokoo", alku: "rokokoo", luokka: nimisana, jatko: <sisäeronto_stA>, äs: a];
[perusmuoto: "rokokoo", alku: "rokokoo", luokka: nimisana, jatko: <sisätulento_hVn>, äs: a];
[perusmuoto: "rokokoo", alku: "rokokoo", luokka: nimisana, jatko: <sisätulento_seen>, tiedot: <ei_voikko>];
[perusmuoto: "rokokoo", alku: "rokokoo", luokka: nimisana, jatko: @ulkopaikallissijat, äs: a];
[perusmuoto: "rokokoo", alku: "rokokoo", luokka: nimisana, jatko: <vajanto_ttA>, äs: a];

[perusmuoto: "rokokoo", alku: "rokokoo", luokka: nimisana, jatko: <nimentö_t>, äs: a];
[perusmuoto: "rokokoo", alku: "rokoko", luokka: nimisana, jatko: <omanto_iT>, äs: a];
[perusmuoto: "rokokoo", alku: "rokoko", luokka: nimisana, jatko: <osanto_itA>, äs: a];
[perusmuoto: "rokokoo", alku: "rokoko", luokka: nimisana, jatko: <olento_inA>, äs: a];
[perusmuoto: "rokokoo", alku: "rokoko", luokka: nimisana, jatko: <tulento_iksi>, äs: a];
[perusmuoto: "rokokoo", alku: "rokoko", luokka: nimisana, jatko: <sisäolento_issA>, äs: a];
[perusmuoto: "rokokoo", alku: "rokoko", luokka: nimisana, jatko: <sisäeronto_istA>, äs: a];
[perusmuoto: "rokokoo", alku: "rokoko", luokka: nimisana, jatko: <sisätulento_ihin>, äs: a];
[perusmuoto: "rokokoo", alku: "rokoko", luokka: nimisana, jatko: <ulko_olento_illA>, äs: a];
[perusmuoto: "rokokoo", alku: "rokoko", luokka: nimisana, jatko: <ulkoeronto_iltA>, äs: a];
[perusmuoto: "rokokoo", alku: "rokoko", luokka: nimisana, jatko: <ulkotulento_ille>, äs: a];
[perusmuoto: "rokokoo", alku: "rokoko", luokka: nimisana, jatko: <vajanto_ittA>, äs: a];
[perusmuoto: "rokokoo", alku: "rokoko", luokka: nimisana, jatko: <seuranto_ine>, äs: a];
[perusmuoto: "rokokoo", alku: "rokoko", luokka: nimisana, jatko: <keinonto_in>, äs: a];

# Vierasperäisiä sanoja {{{1
#
# Näitä ei toistaiseksi voi taivuttaa Voikossa oikein automaattisten
# taivutusluokkien kautta. Tässä listatut sanatkaan eivät välttämättä ole
# ehdottoman tarkasti kielenhuollon suositusten mukaisia vaan on harkittu
# tilannekohtaisesti mahdollisimman luonnollisia taivutusmuotoja.
# (Illatiivin osalta nämä liittyvän bugiin #1829873)

# Brontë [bronti] {{{2
[perusmuoto: "Brontë", alku: "Brontë", luokka: sukunimi, jatko: <liitesana, omistusliite, loppu>, äs: a];
[perusmuoto: "Brontë", alku: "Brontë", luokka: sukunimi, jatko: <tavuviiva>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "Brontë", alku: "Brontë", luokka: sukunimi, jatko: <omanto_n>, äs: a];
[perusmuoto: "Brontë", alku: "Brontë", luokka: sukunimi, jatko: <osanto_A>, äs: a];
[perusmuoto: "Brontë", alku: "Brontë", luokka: sukunimi, jatko: <olento_nA>, äs: a];
[perusmuoto: "Brontë", alku: "Brontë", luokka: sukunimi, jatko: <tulento_ksi>, äs: a];
[perusmuoto: "Brontë", alku: "Brontë", luokka: sukunimi, jatko: <sisäolento_ssA>, äs: a];
[perusmuoto: "Brontë", alku: "Brontë", luokka: sukunimi, jatko: <sisäeronto_stA>, äs: a];
[perusmuoto: "Brontë", alku: "Brontëen", luokka: sukunimi, sija: sisätulento, luku: yksikkö, jatko: <loppu, liitesana>, äs: a];
[perusmuoto: "Brontë", alku: "Brontëën", luokka: sukunimi, sija: sisätulento, luku: yksikkö, jatko: <loppu, liitesana>, äs: a];
[perusmuoto: "Brontë", alku: "Brontë", luokka: sukunimi, jatko: @ulkopaikallissijat, äs: a];
[perusmuoto: "Brontë", alku: "Brontë", luokka: sukunimi, jatko: <vajanto_ttA>, äs: a];

[perusmuoto: "Brontë", alku: "Brontë", luokka: sukunimi, jatko: <nimentö_t>, äs: a];
#[perusmuoto: "Brontë", alku: "Brontë", luokka: sukunimi, jatko: <omanto_jen>, äs: a];
#[perusmuoto: "Brontë", alku: "Brontë", luokka: sukunimi, jatko: <omanto_in>, äs: a];
#[perusmuoto: "Brontë", alku: "Brontë", luokka: sukunimi, jatko: <omanto_iT>, äs: a];
#[perusmuoto: "Brontë", alku: "Brontë", luokka: sukunimi, jatko: <osanto_jA>, äs: a];
#[perusmuoto: "Brontë", alku: "Brontë", luokka: sukunimi, jatko: <osanto_itA>, äs: a];
#[perusmuoto: "Brontë", alku: "Brontë", luokka: sukunimi, jatko: <olento_inA>, äs: a];
#[perusmuoto: "Brontë", alku: "Brontë", luokka: sukunimi, jatko: <tulento_iksi>, äs: a];
#[perusmuoto: "Brontë", alku: "Brontë", luokka: sukunimi, jatko: <sisäolento_issA>, äs: a];
#[perusmuoto: "Brontë", alku: "Brontë", luokka: sukunimi, jatko: <sisäeronto_istA>, äs: a];
#[perusmuoto: "Brontë", alku: "Brontë", luokka: sukunimi, jatko: <sisätulento_ihin>, äs: a];
#[perusmuoto: "Brontë", alku: "Brontë", luokka: sukunimi, jatko: <ulko_olento_illA>, äs: a];
#[perusmuoto: "Brontë", alku: "Brontë", luokka: sukunimi, jatko: <ulkoeronto_iltA>, äs: a];
#[perusmuoto: "Brontë", alku: "Brontë", luokka: sukunimi, jatko: <ulkotulento_ille>, äs: a];
#[perusmuoto: "Brontë", alku: "Brontë", luokka: sukunimi, jatko: <vajanto_ittA>, äs: a];
#[perusmuoto: "Brontë", alku: "Brontë", luokka: sukunimi, jatko: <seuranto_ine>, äs: a];
#[perusmuoto: "Brontë", alku: "Brontë", luokka: sukunimi, jatko: <keinonto_in>, äs: a];

# cowboy [kauboi] {{{2
[perusmuoto: "cowboy", alku: "cowboy", luokka: nimisana, jatko: @sana1 + <liitesana, omistusliite, loppu>, äs: a, rakenne: "=ppp=ppp"];
[perusmuoto: "cowboy", alku: "cowboy", luokka: nimisana, jatko: <omanto_n>, äs: a, rakenne: "=ppp=ppp"];
[perusmuoto: "cowboy", alku: "cowboy", luokka: nimisana, jatko: <osanto_tA>, äs: a, rakenne: "=ppp=ppp"];
[perusmuoto: "cowboy", alku: "cowboy", luokka: nimisana, jatko: <olento_nA>, äs: a, rakenne: "=ppp=ppp"];
[perusmuoto: "cowboy", alku: "cowboy", luokka: nimisana, jatko: <tulento_ksi>, äs: a, rakenne: "=ppp=ppp"];
[perusmuoto: "cowboy", alku: "cowboy", luokka: nimisana, jatko: <sisäolento_ssA>, äs: a, rakenne: "=ppp=ppp"];
[perusmuoto: "cowboy", alku: "cowboy", luokka: nimisana, jatko: <sisäeronto_stA>, äs: a, rakenne: "=ppp=ppp"];
[perusmuoto: "cowboy", alku: "cowboyhin", luokka: nimisana, sija: sisätulento, luku: yksikkö, jatko: <loppu, liitesana>, äs: a, rakenne: "=ppp=ppp"];
[perusmuoto: "cowboy", alku: "cowboyhi", luokka: nimisana, sija: sisätulento, luku: yksikkö, jatko: <omistusliite>, äs: a, rakenne: "=ppp=ppp"];
[perusmuoto: "cowboy", alku: "cowboy", luokka: nimisana, jatko: <sisätulento_hVn>, äs: a, rakenne: "=ppp=ppp", tiedot: <ei_voikko>];
[perusmuoto: "cowboy", alku: "cowboy", luokka: nimisana, jatko: @ulkopaikallissijat, äs: a, rakenne: "=ppp=ppp"];
[perusmuoto: "cowboy", alku: "cowboy", luokka: nimisana, jatko: <vajanto_ttA>, äs: a, rakenne: "=ppp=ppp"];

[perusmuoto: "cowboy", alku: "cowboy", luokka: nimisana, jatko: <nimentö_t>, äs: a, rakenne: "=ppp=ppp"];
[perusmuoto: "cowboy", alku: "cowboy", luokka: nimisana, jatko: <omanto_iT>, äs: a, rakenne: "=ppp=ppp"];
[perusmuoto: "cowboy", alku: "cowboy", luokka: nimisana, jatko: <osanto_itA>, äs: a, rakenne: "=ppp=ppp"];
[perusmuoto: "cowboy", alku: "cowboy", luokka: nimisana, jatko: <olento_inA>, äs: a, rakenne: "=ppp=ppp"];
[perusmuoto: "cowboy", alku: "cowboy", luokka: nimisana, jatko: <tulento_iksi>, äs: a, rakenne: "=ppp=ppp"];
[perusmuoto: "cowboy", alku: "cowboy", luokka: nimisana, jatko: <sisäolento_issA>, äs: a, rakenne: "=ppp=ppp"];
[perusmuoto: "cowboy", alku: "cowboy", luokka: nimisana, jatko: <sisäeronto_istA>, äs: a, rakenne: "=ppp=ppp"];
[perusmuoto: "cowboy", alku: "cowboy", luokka: nimisana, jatko: <sisätulento_ihin>, äs: a, rakenne: "=ppp=ppp"];
[perusmuoto: "cowboy", alku: "cowboy", luokka: nimisana, jatko: <ulko_olento_illA>, äs: a, rakenne: "=ppp=ppp"];
[perusmuoto: "cowboy", alku: "cowboy", luokka: nimisana, jatko: <ulkoeronto_iltA>, äs: a, rakenne: "=ppp=ppp"];
[perusmuoto: "cowboy", alku: "cowboy", luokka: nimisana, jatko: <ulkotulento_ille>, äs: a, rakenne: "=ppp=ppp"];
[perusmuoto: "cowboy", alku: "cowboy", luokka: nimisana, jatko: <vajanto_ittA>, äs: a, rakenne: "=ppp=ppp"];
[perusmuoto: "cowboy", alku: "cowboy", luokka: nimisana, jatko: <seuranto_ine>, äs: a, rakenne: "=ppp=ppp"];
[perusmuoto: "cowboy", alku: "cowboy", luokka: nimisana, jatko: <keinonto_in>, äs: a, rakenne: "=ppp=ppp"];

# Dalí {{{2
[perusmuoto: "Dalí", alku: "Dalí", luokka: sukunimi, jatko: <liitesana, omistusliite, loppu>, äs: a];
[perusmuoto: "Dalí", alku: "Dalí", luokka: sukunimi, jatko: <tavuviiva>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "Dalí", alku: "Dalí", luokka: sukunimi, jatko: <omanto_n>, äs: a];
[perusmuoto: "Dalí", alku: "Dalí", luokka: sukunimi, jatko: <osanto_A>, äs: a];
[perusmuoto: "Dalí", alku: "Dalí", luokka: sukunimi, jatko: <olento_nA>, äs: a];
[perusmuoto: "Dalí", alku: "Dalí", luokka: sukunimi, jatko: <tulento_ksi>, äs: a];
[perusmuoto: "Dalí", alku: "Dalí", luokka: sukunimi, jatko: <sisäolento_ssA>, äs: a];
[perusmuoto: "Dalí", alku: "Dalí", luokka: sukunimi, jatko: <sisäeronto_stA>, äs: a];
[perusmuoto: "Dalí", alku: "Dalíin", luokka: sukunimi, sija: sisätulento, luku: yksikkö, jatko: <loppu, liitesana>, äs: a];
[perusmuoto: "Dalí", alku: "Dalíín", luokka: sukunimi, sija: sisätulento, luku: yksikkö, jatko: <loppu, liitesana>, äs: a];
[perusmuoto: "Dalí", alku: "Dalí", luokka: sukunimi, jatko: @ulkopaikallissijat, äs: a];
[perusmuoto: "Dalí", alku: "Dalí", luokka: sukunimi, jatko: <vajanto_ttA>, äs: a];

[perusmuoto: "Dalí", alku: "Dalí", luokka: sukunimi, jatko: <nimentö_t>, äs: a];
[perusmuoto: "Dalí", alku: "Dalí", luokka: sukunimi, jatko: <omanto_en>, äs: a];

# Luonnollinen taivutus mutta mahdollisesti ei normin mukainen
#[perusmuoto: "Dalí", alku: "Dale", luokka: sukunimi, jatko: <osanto_jA>, äs: a];
#[perusmuoto: "Dalí", alku: "Dale", luokka: sukunimi, jatko: <olento_inA>, äs: a];
#[perusmuoto: "Dalí", alku: "Dale", luokka: sukunimi, jatko: <tulento_iksi>, äs: a];
#[perusmuoto: "Dalí", alku: "Dale", luokka: sukunimi, jatko: <sisäolento_issA>, äs: a];
#[perusmuoto: "Dalí", alku: "Dale", luokka: sukunimi, jatko: <sisäeronto_istA>, äs: a];
#[perusmuoto: "Dalí", alku: "Dale", luokka: sukunimi, jatko: <sisätulento_ihin>, äs: a];
#[perusmuoto: "Dalí", alku: "Dale", luokka: sukunimi, jatko: <ulko_olento_illA>, äs: a];
#[perusmuoto: "Dalí", alku: "Dale", luokka: sukunimi, jatko: <ulkoeronto_iltA>, äs: a];
#[perusmuoto: "Dalí", alku: "Dale", luokka: sukunimi, jatko: <ulkotulento_ille>, äs: a];
#[perusmuoto: "Dalí", alku: "Dale", luokka: sukunimi, jatko: <vajanto_ittA>, äs: a];
#[perusmuoto: "Dalí", alku: "Dale", luokka: sukunimi, jatko: <seuranto_ine>, äs: a];
#[perusmuoto: "Dalí", alku: "Dale", luokka: sukunimi, jatko: <keinonto_in>, äs: a];

# Ilmeisesti normin mukainen taivutus
#[perusmuoto: "Dalí", alku: "Dalí", luokka: sukunimi, jatko: <omanto_in>, äs: a];
#[perusmuoto: "Dalí", alku: "Dalí", luokka: sukunimi, jatko: <omanto_jen>, äs: a];
#[perusmuoto: "Dalí", alku: "Dalí", luokka: sukunimi, jatko: <osanto_itA>, äs: a];
#[perusmuoto: "Dalí", alku: "Dalí", luokka: sukunimi, jatko: <olento_inA>, äs: a];
#[perusmuoto: "Dalí", alku: "Dalí", luokka: sukunimi, jatko: <tulento_iksi>, äs: a];
#[perusmuoto: "Dalí", alku: "Dalí", luokka: sukunimi, jatko: <sisäolento_issA>, äs: a];
#[perusmuoto: "Dalí", alku: "Dalí", luokka: sukunimi, jatko: <sisäeronto_istA>, äs: a];
#[perusmuoto: "Dalí", alku: "Dalí", luokka: sukunimi, jatko: <sisätulento_ihin>, äs: a];
#[perusmuoto: "Dalí", alku: "Dalí", luokka: sukunimi, jatko: <ulko_olento_illA>, äs: a];
#[perusmuoto: "Dalí", alku: "Dalí", luokka: sukunimi, jatko: <ulkoeronto_iltA>, äs: a];
#[perusmuoto: "Dalí", alku: "Dalí", luokka: sukunimi, jatko: <ulkotulento_ille>, äs: a];
#[perusmuoto: "Dalí", alku: "Dalí", luokka: sukunimi, jatko: <vajanto_ittA>, äs: a];
#[perusmuoto: "Dalí", alku: "Dalí", luokka: sukunimi, jatko: <seuranto_ine>, äs: a];
#[perusmuoto: "Dalí", alku: "Dalí", luokka: sukunimi, jatko: <keinonto_in>, äs: a];

# Jersey [dzöösi] (mon. gen. -in/-jen; mon. part. -jA) {{{2
[perusmuoto: "Jersey", alku: "Jersey", luokka: paikannimi, jatko: <liitesana, omistusliite, loppu>, äs: ä];
[perusmuoto: "Jersey", alku: "Jersey", luokka: paikannimi, jatko: <tavuviiva>, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "Jersey", alku: "Jersey", luokka: paikannimi, jatko: <omanto_n>, äs: ä];
[perusmuoto: "Jersey", alku: "Jersey", luokka: paikannimi, jatko: <osanto_A>, äs: ä];
[perusmuoto: "Jersey", alku: "Jersey", luokka: paikannimi, jatko: <osanto_tA>, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "Jersey", alku: "Jersey", luokka: paikannimi, jatko: <olento_nA>, äs: ä];
[perusmuoto: "Jersey", alku: "Jersey", luokka: paikannimi, jatko: <tulento_ksi>, äs: ä];
[perusmuoto: "Jersey", alku: "Jersey", luokka: paikannimi, jatko: <sisäolento_ssA>, äs: ä];
[perusmuoto: "Jersey", alku: "Jersey", luokka: paikannimi, jatko: <sisäeronto_stA>, äs: ä];
[perusmuoto: "Jersey", alku: "Jersey", luokka: paikannimi, jatko: <sisätulento_Vn>, äs: ä];
[perusmuoto: "Jersey", alku: "Jersey", luokka: paikannimi, jatko: <sisätulento_hVn>, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "Jersey", alku: "Jerseyhin", luokka: paikannimi, sija: sisätulento, luku: monikko, jatko: <liitesana, loppu>, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "Jersey", alku: "Jerseyhi", luokka: paikannimi, sija: sisätulento, luku: monikko, jatko: <omistusliite>, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "Jersey", alku: "Jersey", luokka: paikannimi, jatko: @ulkopaikallissijat, äs: ä];
[perusmuoto: "Jersey", alku: "Jersey", luokka: paikannimi, jatko: <vajanto_ttA>, äs: ä];

# Linné [linnee] {{{2
[perusmuoto: "Linné", alku: "Linné", luokka: sukunimi, jatko: <liitesana, omistusliite, loppu>, äs: ä];
[perusmuoto: "Linné", alku: "Linné", luokka: sukunimi, jatko: <tavuviiva>, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "Linné", alku: "Linné", luokka: sukunimi, jatko: <omanto_n>, äs: ä];
[perusmuoto: "Linné", alku: "Linnée", luokka: sukunimi, jatko: <omanto_n>, äs: ä, tiedot: <ei_voikko>]; # "Linnéen luokittelujärjestelmä."
[perusmuoto: "Linné", alku: "Linné", luokka: sukunimi, jatko: <osanto_tA>, äs: ä];
[perusmuoto: "Linné", alku: "Linné", luokka: sukunimi, jatko: <olento_nA>, äs: ä];
[perusmuoto: "Linné", alku: "Linné", luokka: sukunimi, jatko: <tulento_ksi>, äs: ä];
[perusmuoto: "Linné", alku: "Linné", luokka: sukunimi, jatko: <sisäolento_ssA>, äs: ä];
[perusmuoto: "Linné", alku: "Linné", luokka: sukunimi, jatko: <sisäeronto_stA>, äs: ä];
[perusmuoto: "Linné", alku: "Linnéhen", luokka: sukunimi, sija: sisätulento, luku: yksikkö, jatko: <loppu, liitesana>, äs: ä];
[perusmuoto: "Linné", alku: "Linnéhe", luokka: sukunimi, sija: sisätulento, luku: yksikkö, jatko: <omistusliite>, äs: ä];
[perusmuoto: "Linné", alku: "Linné", luokka: sukunimi, jatko: <sisätulento_seen>, äs: ä];
[perusmuoto: "Linné", alku: "Linné", luokka: sukunimi, jatko: @ulkopaikallissijat, äs: ä];
[perusmuoto: "Linné", alku: "Linné", luokka: sukunimi, jatko: <vajanto_ttA>, äs: ä];

[perusmuoto: "Linné", alku: "Linné", luokka: sukunimi, jatko: <nimentö_t>, äs: ä];
#[perusmuoto: "Linné", alku: "Linné", luokka: sukunimi, jatko: <omanto_iT>, äs: ä];
#[perusmuoto: "Linné", alku: "Linné", luokka: sukunimi, jatko: <osanto_itA>, äs: ä];
#[perusmuoto: "Linné", alku: "Linné", luokka: sukunimi, jatko: <olento_inA>, äs: ä];
#[perusmuoto: "Linné", alku: "Linné", luokka: sukunimi, jatko: <tulento_iksi>, äs: ä];
#[perusmuoto: "Linné", alku: "Linné", luokka: sukunimi, jatko: <sisäolento_issA>, äs: ä];
#[perusmuoto: "Linné", alku: "Linné", luokka: sukunimi, jatko: <sisäeronto_istA>, äs: ä];
#[perusmuoto: "Linné", alku: "Linné", luokka: sukunimi, jatko: <sisätulento_ihin>, äs: ä];
#[perusmuoto: "Linné", alku: "Linné", luokka: sukunimi, jatko: <ulko_olento_illA>, äs: ä];
#[perusmuoto: "Linné", alku: "Linné", luokka: sukunimi, jatko: <ulkoeronto_iltA>, äs: ä];
#[perusmuoto: "Linné", alku: "Linné", luokka: sukunimi, jatko: <ulkotulento_ille>, äs: ä];
#[perusmuoto: "Linné", alku: "Linné", luokka: sukunimi, jatko: <vajanto_ittA>, äs: ä];
#[perusmuoto: "Linné", alku: "Linné", luokka: sukunimi, jatko: <seuranto_ine>, äs: ä];
#[perusmuoto: "Linné", alku: "Linné", luokka: sukunimi, jatko: <keinonto_in>, äs: ä];

# Monroe [manrou] {{{2
[perusmuoto: "Monroe", alku: "Monroe", luokka: sukunimi, jatko: <liitesana, omistusliite, loppu>, äs: a];
[perusmuoto: "Monroe", alku: "Monroe", luokka: sukunimi, jatko: <tavuviiva>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "Monroe", alku: "Monroe", luokka: sukunimi, jatko: <omanto_n>, äs: a];
[perusmuoto: "Monroe", alku: "Monroe", luokka: sukunimi, jatko: <osanto_tA>, äs: a];
[perusmuoto: "Monroe", alku: "Monroe", luokka: sukunimi, jatko: <olento_nA>, äs: a];
[perusmuoto: "Monroe", alku: "Monroe", luokka: sukunimi, jatko: <tulento_ksi>, äs: a];
[perusmuoto: "Monroe", alku: "Monroe", luokka: sukunimi, jatko: <sisäolento_ssA>, äs: a];
[perusmuoto: "Monroe", alku: "Monroe", luokka: sukunimi, jatko: <sisäeronto_stA>, äs: a];
[perusmuoto: "Monroe", alku: "Monroehun", luokka: sukunimi, sija: sisätulento, luku: yksikkö, jatko: <loppu, liitesana>, äs: a];
[perusmuoto: "Monroe", alku: "Monroe", luokka: sukunimi, luku: yksikkö, jatko: <sisätulento_Vn>, tiedot: <ei_voikko>];  # Monroeen.
[perusmuoto: "Monroe", alku: "Monroe", luokka: sukunimi, luku: yksikkö, jatko: <sisätulento_hVn>, tiedot: <ei_voikko>]; # Monroehen.
[perusmuoto: "Monroe", alku: "Monroe", luokka: sukunimi, jatko: @ulkopaikallissijat, äs: a];
[perusmuoto: "Monroe", alku: "Monroe", luokka: sukunimi, jatko: <vajanto_ttA>, äs: a];

[perusmuoto: "Monroe", alku: "Monroe", luokka: sukunimi, jatko: <nimentö_t>, äs: a];
#[perusmuoto: "Monroe", alku: "Monroe", luokka: sukunimi, jatko: <omanto_iT>, äs: a];
#[perusmuoto: "Monroe", alku: "Monroe", luokka: sukunimi, jatko: <osanto_itA>, äs: a];
#[perusmuoto: "Monroe", alku: "Monroe", luokka: sukunimi, jatko: <olento_inA>, äs: a];
#[perusmuoto: "Monroe", alku: "Monroe", luokka: sukunimi, jatko: <tulento_iksi>, äs: a];
#[perusmuoto: "Monroe", alku: "Monroe", luokka: sukunimi, jatko: <sisäolento_issA>, äs: a];
#[perusmuoto: "Monroe", alku: "Monroe", luokka: sukunimi, jatko: <sisäeronto_istA>, äs: a];
#[perusmuoto: "Monroe", alku: "Monroe", luokka: sukunimi, jatko: <sisätulento_ihin>, äs: a];
#[perusmuoto: "Monroe", alku: "Monroe", luokka: sukunimi, jatko: <ulko_olento_illA>, äs: a];
#[perusmuoto: "Monroe", alku: "Monroe", luokka: sukunimi, jatko: <ulkoeronto_iltA>, äs: a];
#[perusmuoto: "Monroe", alku: "Monroe", luokka: sukunimi, jatko: <ulkotulento_ille>, äs: a];
#[perusmuoto: "Monroe", alku: "Monroe", luokka: sukunimi, jatko: <vajanto_ittA>, äs: a];
#[perusmuoto: "Monroe", alku: "Monroe", luokka: sukunimi, jatko: <seuranto_ine>, äs: a];
#[perusmuoto: "Monroe", alku: "Monroe", luokka: sukunimi, jatko: <keinonto_in>, äs: a];

# playboy [pleiboi] {{{2
[perusmuoto: "playboy", alku: "playboy", luokka: nimisana, jatko: @sana1 + <liitesana, omistusliite, loppu>, äs: a, rakenne: "=pppp=ppp"];
[perusmuoto: "playboy", alku: "playboy", luokka: nimisana, jatko: <omanto_n>, äs: a, rakenne: "=pppp=ppp"];
[perusmuoto: "playboy", alku: "playboy", luokka: nimisana, jatko: <osanto_tA>, äs: a, rakenne: "=pppp=ppp"];
[perusmuoto: "playboy", alku: "playboy", luokka: nimisana, jatko: <olento_nA>, äs: a, rakenne: "=pppp=ppp"];
[perusmuoto: "playboy", alku: "playboy", luokka: nimisana, jatko: <tulento_ksi>, äs: a, rakenne: "=pppp=ppp"];
[perusmuoto: "playboy", alku: "playboy", luokka: nimisana, jatko: <sisäolento_ssA>, äs: a, rakenne: "=pppp=ppp"];
[perusmuoto: "playboy", alku: "playboy", luokka: nimisana, jatko: <sisäeronto_stA>, äs: a, rakenne: "=pppp=ppp"];
[perusmuoto: "playboy", alku: "playboyhin", luokka: nimisana, sija: sisätulento, luku: yksikkö, jatko: <loppu, liitesana>, äs: a, rakenne: "=pppp=ppp"];
[perusmuoto: "playboy", alku: "playboyhi", luokka: nimisana, sija: sisätulento, luku: yksikkö, jatko: <omistusliite>, äs: a, rakenne: "=pppp=ppp"];
[perusmuoto: "playboy", alku: "playboy", luokka: nimisana, jatko: <sisätulento_hVn>, äs: a, tiedot: <ei_voikko>, rakenne: "=pppp=ppp"];
[perusmuoto: "playboy", alku: "playboy", luokka: nimisana, jatko: @ulkopaikallissijat, äs: a, rakenne: "=pppp=ppp"];
[perusmuoto: "playboy", alku: "playboy", luokka: nimisana, jatko: <vajanto_ttA>, äs: a, rakenne: "=pppp=ppp"];

[perusmuoto: "playboy", alku: "playboy", luokka: nimisana, jatko: <nimentö_t>, äs: a, rakenne: "=pppp=ppp"];
[perusmuoto: "playboy", alku: "playboy", luokka: nimisana, jatko: <omanto_iT>, äs: a, rakenne: "=pppp=ppp"];
[perusmuoto: "playboy", alku: "playboy", luokka: nimisana, jatko: <osanto_itA>, äs: a, rakenne: "=pppp=ppp"];
[perusmuoto: "playboy", alku: "playboy", luokka: nimisana, jatko: <olento_inA>, äs: a, rakenne: "=pppp=ppp"];
[perusmuoto: "playboy", alku: "playboy", luokka: nimisana, jatko: <tulento_iksi>, äs: a, rakenne: "=pppp=ppp"];
[perusmuoto: "playboy", alku: "playboy", luokka: nimisana, jatko: <sisäolento_issA>, äs: a, rakenne: "=pppp=ppp"];
[perusmuoto: "playboy", alku: "playboy", luokka: nimisana, jatko: <sisäeronto_istA>, äs: a, rakenne: "=pppp=ppp"];
[perusmuoto: "playboy", alku: "playboy", luokka: nimisana, jatko: <sisätulento_ihin>, äs: a, rakenne: "=pppp=ppp"];
[perusmuoto: "playboy", alku: "playboy", luokka: nimisana, jatko: <ulko_olento_illA>, äs: a, rakenne: "=pppp=ppp"];
[perusmuoto: "playboy", alku: "playboy", luokka: nimisana, jatko: <ulkoeronto_iltA>, äs: a, rakenne: "=pppp=ppp"];
[perusmuoto: "playboy", alku: "playboy", luokka: nimisana, jatko: <ulkotulento_ille>, äs: a, rakenne: "=pppp=ppp"];
[perusmuoto: "playboy", alku: "playboy", luokka: nimisana, jatko: <vajanto_ittA>, äs: a, rakenne: "=pppp=ppp"];
[perusmuoto: "playboy", alku: "playboy", luokka: nimisana, jatko: <seuranto_ine>, äs: a, rakenne: "=pppp=ppp"];
[perusmuoto: "playboy", alku: "playboy", luokka: nimisana, jatko: <keinonto_in>, äs: a, rakenne: "=pppp=ppp"];

# Poe [pou] {{{2
[perusmuoto: "Poe", alku: "Poe", luokka: sukunimi, jatko: <liitesana, omistusliite, loppu>, äs: a];
[perusmuoto: "Poe", alku: "Poe", luokka: sukunimi, jatko: <tavuviiva>, äs: a, tiedot: <ei_voikko>];  # Poe-vaikutteinen.
[perusmuoto: "Poe", alku: "Poe", luokka: sukunimi, jatko: <omanto_n>, äs: a];
[perusmuoto: "Poe", alku: "Poe", luokka: sukunimi, jatko: <osanto_tA>, äs: a];
[perusmuoto: "Poe", alku: "Poe", luokka: sukunimi, jatko: <olento_nA>, äs: a];
[perusmuoto: "Poe", alku: "Poe", luokka: sukunimi, jatko: <tulento_ksi>, äs: a];
[perusmuoto: "Poe", alku: "Poe", luokka: sukunimi, jatko: <sisäolento_ssA>, äs: a];
[perusmuoto: "Poe", alku: "Poe", luokka: sukunimi, jatko: <sisäeronto_stA>, äs: a];
[perusmuoto: "Poe", alku: "Poehun", luokka: sukunimi, sija: sisätulento, luku: yksikkö, jatko: <loppu, liitesana>, äs: a];
[perusmuoto: "Poe", alku: "Poe", luokka: sukunimi, luku: yksikkö, jatko: <sisätulento_Vn>, tiedot: <ei_voikko>];  # Poeen.
[perusmuoto: "Poe", alku: "Poe", luokka: sukunimi, luku: yksikkö, jatko: <sisätulento_hVn>, tiedot: <ei_voikko>]; # Poehen.
[perusmuoto: "Poe", alku: "Poe", luokka: sukunimi, jatko: @ulkopaikallissijat, äs: a];
[perusmuoto: "Poe", alku: "Poe", luokka: sukunimi, jatko: <vajanto_ttA>, äs: a];

[perusmuoto: "Poe", alku: "Poe", luokka: sukunimi, jatko: <nimentö_t>, äs: a];
#[perusmuoto: "Poe", alku: "Poe", luokka: sukunimi, jatko: <omanto_iT>, äs: a];
#[perusmuoto: "Poe", alku: "Poe", luokka: sukunimi, jatko: <omanto_jen>, äs: a];
#[perusmuoto: "Poe", alku: "Poe", luokka: sukunimi, jatko: <osanto_itA>, äs: a];
#[perusmuoto: "Poe", alku: "Poe", luokka: sukunimi, jatko: <olento_inA>, äs: a];
#[perusmuoto: "Poe", alku: "Poe", luokka: sukunimi, jatko: <tulento_iksi>, äs: a];
#[perusmuoto: "Poe", alku: "Poe", luokka: sukunimi, jatko: <sisäolento_issA>, äs: a];
#[perusmuoto: "Poe", alku: "Poe", luokka: sukunimi, jatko: <sisäeronto_istA>, äs: a];
#[perusmuoto: "Poe", alku: "Poe", luokka: sukunimi, jatko: <sisätulento_ihin>, äs: a];
#[perusmuoto: "Poe", alku: "Poe", luokka: sukunimi, jatko: <ulko_olento_illA>, äs: a];
#[perusmuoto: "Poe", alku: "Poe", luokka: sukunimi, jatko: <ulkoeronto_iltA>, äs: a];
#[perusmuoto: "Poe", alku: "Poe", luokka: sukunimi, jatko: <ulkotulento_ille>, äs: a];
#[perusmuoto: "Poe", alku: "Poe", luokka: sukunimi, jatko: <vajanto_ittA>, äs: a];
#[perusmuoto: "Poe", alku: "Poe", luokka: sukunimi, jatko: <seuranto_ine>, äs: a];
#[perusmuoto: "Poe", alku: "Poe", luokka: sukunimi, jatko: <keinonto_in>, äs: a];

# Sidney [sidni] (mon. gen. -in/-jen; mon. part. -jA) {{{2
[perusmuoto: "Sidney", alku: "Sidney", luokka: paikannimi, jatko: <liitesana, omistusliite, loppu>, äs: ä];
[perusmuoto: "Sidney", alku: "Sidney", luokka: paikannimi, jatko: <tavuviiva>, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "Sidney", alku: "Sidney", luokka: paikannimi, jatko: <omanto_n>, äs: ä];
[perusmuoto: "Sidney", alku: "Sidney", luokka: paikannimi, jatko: <osanto_A>, äs: ä];
[perusmuoto: "Sidney", alku: "Sidney", luokka: paikannimi, jatko: <osanto_tA>, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "Sidney", alku: "Sidney", luokka: paikannimi, jatko: <olento_nA>, äs: ä];
[perusmuoto: "Sidney", alku: "Sidney", luokka: paikannimi, jatko: <tulento_ksi>, äs: ä];
[perusmuoto: "Sidney", alku: "Sidney", luokka: paikannimi, jatko: <sisäolento_ssA>, äs: ä];
[perusmuoto: "Sidney", alku: "Sidney", luokka: paikannimi, jatko: <sisäeronto_stA>, äs: ä];
[perusmuoto: "Sidney", alku: "Sidney", luokka: paikannimi, jatko: <sisätulento_Vn>, äs: ä];
[perusmuoto: "Sidney", alku: "Sidney", luokka: paikannimi, jatko: <sisätulento_hVn>, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "Sidney", alku: "Sidneyhin", luokka: paikannimi, sija: sisätulento, jatko: <liitesana, loppu>, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "Sidney", alku: "Sidneyhi", luokka: paikannimi, sija: sisätulento, jatko: <omistusliite>, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "Sidney", alku: "Sidney", luokka: paikannimi, jatko: @ulkopaikallissijat, äs: ä];
[perusmuoto: "Sidney", alku: "Sidney", luokka: paikannimi, jatko: <vajanto_ttA>, äs: ä];

[perusmuoto: "sidneyläinen", alku: "sidneyläi", luokka: nimi_laatusana, jatko: <nainen>, äs: ä];

# Sydney [sidni] (mon. gen. -in/-jen; mon. part. -jA) {{{2
[perusmuoto: "Sydney", alku: "Sydney", luokka: paikannimi, jatko: <liitesana, omistusliite, loppu>, äs: ä];
[perusmuoto: "Sydney", alku: "Sydney", luokka: paikannimi, jatko: <tavuviiva>, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "Sydney", alku: "Sydney", luokka: paikannimi, jatko: <omanto_n>, äs: ä];
[perusmuoto: "Sydney", alku: "Sydney", luokka: paikannimi, jatko: <osanto_A>, äs: ä];
[perusmuoto: "Sydney", alku: "Sydney", luokka: paikannimi, jatko: <osanto_tA>, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "Sydney", alku: "Sydney", luokka: paikannimi, jatko: <olento_nA>, äs: ä];
[perusmuoto: "Sydney", alku: "Sydney", luokka: paikannimi, jatko: <tulento_ksi>, äs: ä];
[perusmuoto: "Sydney", alku: "Sydney", luokka: paikannimi, jatko: <sisäolento_ssA>, äs: ä];
[perusmuoto: "Sydney", alku: "Sydney", luokka: paikannimi, jatko: <sisäeronto_stA>, äs: ä];
[perusmuoto: "Sydney", alku: "Sydney", luokka: paikannimi, jatko: <sisätulento_Vn>, äs: ä];
[perusmuoto: "Sydney", alku: "Sydney", luokka: paikannimi, jatko: <sisätulento_hVn>, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "Sydney", alku: "Sydneyhin", luokka: paikannimi, sija: sisätulento, jatko: <liitesana, loppu>, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "Sydney", alku: "Sydneyhi", luokka: paikannimi, sija: sisätulento, jatko: <omistusliite>, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "Sydney", alku: "Sydney", luokka: paikannimi, jatko: @ulkopaikallissijat, äs: ä];
[perusmuoto: "Sydney", alku: "Sydney", luokka: paikannimi, jatko: <vajanto_ttA>, äs: ä];

[perusmuoto: "sydneyläinen", alku: "sydneyläi", luokka: nimi_laatusana, jatko: <nainen>, äs: ä];

# vim: nowrap filetype=conf
# vim600: foldmethod=marker
