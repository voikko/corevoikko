# Suomi-malaga, suomen kielen muoto-opin kuvaus.
#
# Tekijänoikeus © 2007-2009	Teemu Likonen <tlikonen@iki.fi>
# 				Hannu Väisänen
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

# Täydennyksiä eräiden sanojen taivutusmuotoihin (sanan perustietue
# on sanastossa muualla).

[perusmuoto: "Bach", alku: "Bach", luokka: sukunimi, jatko: <loppu, liitesana>, äs: a, tiedot: <ei_sukija>]; # Estämään tavutus "Ba-ch[+liitesana]"
[perusmuoto: "Bangladesh", alku: "Bangladesh", luokka: paikannimi, jatko: <loppu, liitesana>, äs: a, tiedot: <ei_sukija>]; # Estämään tavutus "Banglade-sh[+liitesana]"
[perusmuoto: "Friedrich", alku: "Friedrich", luokka: etunimi, jatko: <loppu, liitesana>, äs: ä, tiedot: <ei_sukija>]; # Estämään tavutus "Friedri-ch[+liitesana]"
[perusmuoto: "haku", alku: "ha'u", luokka: nimisana, jatko: <vajanto_ittA, keinonto_in> + @sija_monikko_1 + @ulkopaikallissijat_monikko, äs: a];
[perusmuoto: "Kangasala", alku: "Kangasa", luokka: paikannimi, jatko: @ulkopaikallissijat, äs: a, rakenne: "=ippppp=ppp"]; # Kangasalla
[perusmuoto: "kappale", alku: "kappal", luokka: nimisana, jatko: <omanto_ten>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "Kaspianmeri", alku: "Kaspianmer", luokka: nimisana, jatko: <osanto_tA>, äs: a, rakenne: "=ppppppp=pppp"];
[perusmuoto: "kultanen", alku: "kultasemme", luokka: nimisana, jatko: <loppu, liitesana>, äs: a, tiedot: <ei_sukija>];
[perusmuoto: "kultanen", alku: "kultaseni", luokka: nimisana, jatko: <loppu, liitesana>, äs: a, tiedot: <ei_sukija>];
[perusmuoto: "kultanen", alku: "kultasenne", luokka: nimisana, jatko: <loppu, liitesana>, äs: a, tiedot: <ei_sukija>];
[perusmuoto: "kultanen", alku: "kultasensa", luokka: nimisana, jatko: <loppu, liitesana>, äs: a, tiedot: <ei_sukija>];
[perusmuoto: "kultanen", alku: "kultasesi", luokka: nimisana, jatko: <loppu, liitesana>, äs: a, tiedot: <ei_sukija>];
[perusmuoto: "maailma", alku: "maailmoitse", luokka: nimisana, jatko: <liitesana, loppu>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "muutama", alku: "muutamasti", luokka: nimisana, jatko: <liitesana, loppu>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "nukke", alku: "nukk", luokka: nimisana, jatko: <omanto_ien>, äs: a];
[perusmuoto: "saada", alku: "saas", luokka: teonsana, jatko: <loppu>, äs: a];
[perusmuoto: "sankari", alku: "sankar", luokka: nimisana, jatko: <omanto_ten>, äs: a];
[perusmuoto: "Thaimaa", alku: "Thaimaaseen", luokka: paikannimi, jatko: <liitesana, loppu>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "venäjä", alku: "venättä", luokka: nimisana, sija: osanto_tA, luku: yksikkö, jatko: <liitesana, loppu>, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "vuosi", alku: "vuon", luokka: nimisana, jatko: <olento_nA>, äs: a];
[perusmuoto: "Zürich", alku: "Zürich", luokka: paikannimi, jatko: <loppu, liitesana>, äs: ä, tiedot: <ei_sukija>]; # Estämään tavutus "Züri-ch[+liitesana]"

# Kiva-sanan kive-kantainen komparatiivi etuvokaalisena {{{1
# (Yksikön nominatiivi ilman liitteitä tunnistuu normaalin taivutuksen kautta.)

[perusmuoto: "kiva", alku: "kivempi", luokka: laatusana, jatko: <liitesana>, äs: ä];
[perusmuoto: "kiva", alku: "kivemmä", luokka: laatusana, jatko: <omanto_n>, äs: ä];
[perusmuoto: "kiva", alku: "kivempä", luokka: laatusana, jatko: <osanto_A>, äs: ä];
[perusmuoto: "kiva", alku: "kivempä", luokka: laatusana, jatko: <olento_nA>, äs: ä];
[perusmuoto: "kiva", alku: "kivemmä", luokka: laatusana, jatko: <tulento_ksi>, äs: ä];
[perusmuoto: "kiva", alku: "kivemmä", luokka: laatusana, jatko: <sisäeronto_stA>, äs: ä];
[perusmuoto: "kiva", alku: "kivemmä", luokka: laatusana, jatko: <sisäolento_ssA>, äs: ä];
[perusmuoto: "kiva", alku: "kivempä", luokka: laatusana, jatko: <sisätulento_Vn>, äs: ä];
[perusmuoto: "kiva", alku: "kivemmä", luokka: laatusana, jatko: @ulkopaikallissijat, äs: ä];
[perusmuoto: "kiva", alku: "kivemmä", luokka: laatusana, jatko: <vajanto_ttA>, äs: ä];

[perusmuoto: "kiva", alku: "kivemmä", luokka: laatusana, jatko: <nimentö_t>, äs: ä];
[perusmuoto: "kiva", alku: "kivemp", luokka: laatusana, jatko: <omanto_ien>, äs: ä];
[perusmuoto: "kiva", alku: "kivemp", luokka: laatusana, jatko: <osanto_iA>, äs: ä];
[perusmuoto: "kiva", alku: "kivemp", luokka: laatusana, jatko: <olento_inA>, äs: ä];
[perusmuoto: "kiva", alku: "kivemm", luokka: laatusana, jatko: @sija_monikko_1, äs: ä];
[perusmuoto: "kiva", alku: "kivemp", luokka: laatusana, jatko: <sisätulento_iin>, äs: ä];
[perusmuoto: "kiva", alku: "kivemm", luokka: laatusana, jatko: @ulkopaikallissijat_monikko, äs: ä];
[perusmuoto: "kiva", alku: "kivemm", luokka: laatusana, jatko: <vajanto_ittA>, äs: ä];
[perusmuoto: "kiva", alku: "kivemp", luokka: laatusana, jatko: <seuranto_ine>, äs: ä];
[perusmuoto: "kiva", alku: "kivemm", luokka: laatusana, jatko: <keinonto_in>, äs: ä];

# Kiva-sanan kiv-kantainen superlatiivi etuvokaalisena {{{1
# (Yksikön nominatiivi ilman liitteitä tunnistuu normaalin taivutuksen kautta.)

[perusmuoto: "kiva", alku: "kivin", luokka: laatusana, jatko: <liitesana>, äs: ä];
[perusmuoto: "kiva", alku: "kivimmä", luokka: laatusana, jatko: <omanto_n>, äs: ä];
[perusmuoto: "kiva", alku: "kivin", luokka: laatusana, jatko: <osanto_tA>, äs: ä];
[perusmuoto: "kiva", alku: "kivimpä", luokka: laatusana, jatko: <osanto_A>, äs: ä];
[perusmuoto: "kiva", alku: "kivimpä", luokka: laatusana, jatko: <olento_nA>, äs: ä];
[perusmuoto: "kiva", alku: "kivimmä", luokka: laatusana, jatko: <tulento_ksi>, äs: ä];
[perusmuoto: "kiva", alku: "kivimmä", luokka: laatusana, jatko: <sisäolento_ssA>, äs: ä];
[perusmuoto: "kiva", alku: "kivimmä", luokka: laatusana, jatko: <sisäeronto_stA>, äs: ä];
[perusmuoto: "kiva", alku: "kivimpä", luokka: laatusana, jatko: <sisätulento_Vn>, äs: ä];
[perusmuoto: "kiva", alku: "kivimmä", luokka: laatusana, jatko: @ulkopaikallissijat, äs: ä];
[perusmuoto: "kiva", alku: "kivimmä", luokka: laatusana, jatko: <vajanto_ttA>, äs: ä];

[perusmuoto: "kiva", alku: "kivimmä", luokka: laatusana, jatko: <nimentö_t>, äs: ä];
[perusmuoto: "kiva", alku: "kivimp", luokka: laatusana, jatko: <omanto_ien>, äs: ä];
[perusmuoto: "kiva", alku: "kivimp", luokka: laatusana, jatko: <osanto_iA>, äs: ä];
[perusmuoto: "kiva", alku: "kivimp", luokka: laatusana, jatko: <olento_inA>, äs: ä];
[perusmuoto: "kiva", alku: "kivimm", luokka: laatusana, jatko: @sija_monikko_1, äs: ä];
[perusmuoto: "kiva", alku: "kivimp", luokka: laatusana, jatko: <sisätulento_iin>, äs: ä];
[perusmuoto: "kiva", alku: "kivimm", luokka: laatusana, jatko: @ulkopaikallissijat_monikko, äs: ä];
[perusmuoto: "kiva", alku: "kivimm", luokka: laatusana, jatko: <vajanto_ittA>, äs: ä];
[perusmuoto: "kiva", alku: "kivimp", luokka: laatusana, jatko: <seuranto_ine>, äs: ä];
[perusmuoto: "kiva", alku: "kivimm", luokka: laatusana, jatko: <keinonto_in>, äs: ä];

# Kiva-sanan kivo-kantainen superlatiivi {{{1

[perusmuoto: "kiva", alku: "kivoin", luokka: laatusana, jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "kiva", alku: "kivoimma", luokka: laatusana, jatko: <omanto_n>, äs: a];
[perusmuoto: "kiva", alku: "kivoin", luokka: laatusana, jatko: <osanto_tA>, äs: a];
[perusmuoto: "kiva", alku: "kivoimpa", luokka: laatusana, jatko: <osanto_A>, äs: a];
[perusmuoto: "kiva", alku: "kivoimpa", luokka: laatusana, jatko: <olento_nA>, äs: a];
[perusmuoto: "kiva", alku: "kivoimma", luokka: laatusana, jatko: <tulento_ksi>, äs: a];
[perusmuoto: "kiva", alku: "kivoimma", luokka: laatusana, jatko: <sisäolento_ssA>, äs: a];
[perusmuoto: "kiva", alku: "kivoimma", luokka: laatusana, jatko: <sisäeronto_stA>, äs: a];
[perusmuoto: "kiva", alku: "kivoimpa", luokka: laatusana, jatko: <sisätulento_Vn>, äs: a];
[perusmuoto: "kiva", alku: "kivoimma", luokka: laatusana, jatko: @ulkopaikallissijat, äs: a];
[perusmuoto: "kiva", alku: "kivoimma", luokka: laatusana, jatko: <vajanto_ttA>, äs: a];

[perusmuoto: "kiva", alku: "kivoimma", luokka: laatusana, jatko: <nimentö_t>, äs: a];
[perusmuoto: "kiva", alku: "kivoimp", luokka: laatusana, jatko: <omanto_ien>, äs: a];
[perusmuoto: "kiva", alku: "kivoimp", luokka: laatusana, jatko: <osanto_iA>, äs: a];
[perusmuoto: "kiva", alku: "kivoimp", luokka: laatusana, jatko: <olento_inA>, äs: a];
[perusmuoto: "kiva", alku: "kivoimm", luokka: laatusana, jatko: @sija_monikko_1, äs: a];
[perusmuoto: "kiva", alku: "kivoimp", luokka: laatusana, jatko: <sisätulento_iin>, äs: a];
[perusmuoto: "kiva", alku: "kivoimm", luokka: laatusana, jatko: @ulkopaikallissijat_monikko, äs: a];
[perusmuoto: "kiva", alku: "kivoimm", luokka: laatusana, jatko: <vajanto_ittA>, äs: a];
[perusmuoto: "kiva", alku: "kivoimp", luokka: laatusana, jatko: <seuranto_ine>, äs: a];
[perusmuoto: "kiva", alku: "kivoimm", luokka: laatusana, jatko: <keinonto_in>, äs: a];


# Kuulla, nähdä, tietää {{{1
#
# Nämä taitavat olla ainoat sanat, jotka taipuvat tällä tavalla,
# ja on helpointa tallentaa nämä erikseen kuin tehdä uusi sääntö.

[perusmuoto: "kuulla", alku: "kuulteni",  luokka: teonsana, tapaluokka: nimitapa_2, sija: keinonto_n, jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "kuulla", alku: "kuultesi",  luokka: teonsana, tapaluokka: nimitapa_2, sija: keinonto_n, jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "kuulla", alku: "kuultensa", luokka: teonsana, tapaluokka: nimitapa_2, sija: keinonto_n, jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "kuulla", alku: "kuultemme", luokka: teonsana, tapaluokka: nimitapa_2, sija: keinonto_n, jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "kuulla", alku: "kuultenne", luokka: teonsana, tapaluokka: nimitapa_2, sija: keinonto_n, jatko: <liitesana, loppu>, äs: a];

[perusmuoto: "nähdä", alku: "nähteni",  luokka: teonsana, tapaluokka: nimitapa_2, sija: keinonto_n, jatko: <liitesana, loppu>, äs: ä];
[perusmuoto: "nähdä", alku: "nähtesi",  luokka: teonsana, tapaluokka: nimitapa_2, sija: keinonto_n, jatko: <liitesana, loppu>, äs: ä];
[perusmuoto: "nähdä", alku: "nähtensä", luokka: teonsana, tapaluokka: nimitapa_2, sija: keinonto_n, jatko: <liitesana, loppu>, äs: ä];
[perusmuoto: "nähdä", alku: "nähtemme", luokka: teonsana, tapaluokka: nimitapa_2, sija: keinonto_n, jatko: <liitesana, loppu>, äs: ä];
[perusmuoto: "nähdä", alku: "nähtenne", luokka: teonsana, tapaluokka: nimitapa_2, sija: keinonto_n, jatko: <liitesana, loppu>, äs: ä];

[perusmuoto: "tietää", alku: "tieteni",  luokka: teonsana, tapaluokka: nimitapa_2, sija: keinonto_n, jatko: <liitesana, loppu>, äs: ä];
[perusmuoto: "tietää", alku: "tietesi",  luokka: teonsana, tapaluokka: nimitapa_2, sija: keinonto_n, jatko: <liitesana, loppu>, äs: ä];
[perusmuoto: "tietää", alku: "tietensä", luokka: teonsana, tapaluokka: nimitapa_2, sija: keinonto_n, jatko: <liitesana, loppu>, äs: ä];
[perusmuoto: "tietää", alku: "tietemme", luokka: teonsana, tapaluokka: nimitapa_2, sija: keinonto_n, jatko: <liitesana, loppu>, äs: ä];
[perusmuoto: "tietää", alku: "tietenne", luokka: teonsana, tapaluokka: nimitapa_2, sija: keinonto_n, jatko: <liitesana, loppu>, äs: ä];


# vim: nowrap filetype=conf
# vim600: foldmethod=marker
