# Suomi-malaga, suomen kielen muoto-opin kuvaus.
#
# Tekijänoikeus © 2006 - 2008 Hannu Väisänen (Etunimi.Sukunimi@joensuu.fi)
#                 2006 - 2010 Harri Pitkänen (hatapitk@iki.fi)
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


[perusmuoto: "yksi", alku: "y", luokka: lukusana, lukutyyppi: perusluku, alaluokka: yksiyhdeksän, jatko: <kaksi>, äs: ä];
[perusmuoto: "kaksi", alku: "ka", luokka: lukusana, lukutyyppi: perusluku, alaluokka: yksiyhdeksän, jatko: <kaksi>, äs: a];
[perusmuoto: "kolme", alku: "kolm", luokka: lukusana, lukutyyppi: perusluku, alaluokka: yksiyhdeksän, jatko: <kolme>, äs: a];
[perusmuoto: "neljä", alku: "nelj", luokka: lukusana, lukutyyppi: perusluku, alaluokka: yksiyhdeksän, jatko: <koira>, äs: ä];
[perusmuoto: "viisi", alku: "vii", luokka: lukusana, lukutyyppi: perusluku, alaluokka: yksiyhdeksän, jatko: <susi>, äs: ä];
[perusmuoto: "kuusi", alku: "kuu", luokka: lukusana, lukutyyppi: perusluku, alaluokka: yksiyhdeksän, jatko: <susi>, äs: a];
[perusmuoto: "seitsemän", alku: "seitsem", luokka: lukusana, lukutyyppi: perusluku, alaluokka: yksiyhdeksän, jatko: <seitsemän>, äs: ä, tiedot: <ei_sukija>];
[perusmuoto: "kahdeksan", alku: "kahdeks", luokka: lukusana, lukutyyppi: perusluku, alaluokka: yksiyhdeksän, jatko: <seitsemän>, äs: a, tiedot: <ei_sukija>];
[perusmuoto: "yhdeksän",  alku: "yhdeks",  luokka: lukusana, lukutyyppi: perusluku, alaluokka: yksiyhdeksän, jatko: <seitsemän>, äs: ä, tiedot: <ei_sukija>];
[perusmuoto: "seitsemän", alku: "seitse",  luokka: lukusana, lukutyyppi: perusluku, alaluokka: yksiyhdeksän, jatko: <seitsemän>, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "kahdeksan", alku: "kahdeks", luokka: lukusana, lukutyyppi: perusluku, alaluokka: yksiyhdeksän, jatko: <kahdeksan>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "yhdeksän",  alku: "yhdeks",  luokka: lukusana, lukutyyppi: perusluku, alaluokka: yksiyhdeksän, jatko: <kahdeksan>, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "kymmenen",  alku: "kymmen",  luokka: lukusana, lukutyyppi: perusluku, alaluokka: kymmenen, jatko: <kymmenen>, äs: ä];

# Yksitoista, ..., yhdeksäntoista.
[perusmuoto: "toista", alku: "toista", luokka: lukusana, alaluokka: toista, jatko: <liitesana, loppu>, äs: a];

# Yksikolmatta (21), ..., yhdeksänyhdeksättä (89).
[perusmuoto: "kolmatta",    alku: "kolmatta", luokka: lukusana, alaluokka: toista, jatko: <liitesana, loppu>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "neljättä",    alku: "neljättä", luokka: lukusana, alaluokka: toista, jatko: <liitesana, loppu>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "viidettä",    alku: "viidettä", luokka: lukusana, alaluokka: toista, jatko: <liitesana, loppu>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "kuudetta",    alku: "kuudetta", luokka: lukusana, alaluokka: toista, jatko: <liitesana, loppu>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "seitsemättä", alku: "seitsemättä", luokka: lukusana, alaluokka: toista, jatko: <liitesana, loppu>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "kahdeksatta", alku: "kahdeksatta", luokka: lukusana, alaluokka: toista, jatko: <liitesana, loppu>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "yhdeksättä",  alku: "yhdeksättä", luokka: lukusana, alaluokka: toista, jatko: <liitesana, loppu>, äs: a, tiedot: <ei_voikko>];

[perusmuoto: "puolitoista", alku: "puolitoista", luokka: lukusana, alaluokka: erikoisluku, jatko: <liitesana, loppu>, äs: a, rakenne: "=ppppp=pppppp"];
[perusmuoto: "puolitoista", alku: "puolentoista", luokka: lukusana, alaluokka: erikoisluku, jatko: <liitesana, loppu>, äs: a, rakenne: "=pppppp=pppppp"];
[perusmuoto: "puolitoista", alku: "puoltatoista", luokka: lukusana, alaluokka: erikoisluku, jatko: <liitesana, loppu>, äs: a, rakenne: "=pppppp=pppppp"];
[perusmuoto: "puolitoista", alku: "puolenatoista", luokka: lukusana, alaluokka: erikoisluku, jatko: <liitesana, loppu>, äs: a, rakenne: "=ppppppp=pppppp"];
[perusmuoto: "puolitoista", alku: "puoleksitoista", luokka: lukusana, alaluokka: erikoisluku, jatko: <liitesana, loppu>, äs: a, rakenne: "=pppppppp=pppppp"];
[perusmuoto: "puolitoista", alku: "puolessatoista", luokka: lukusana, alaluokka: erikoisluku, jatko: <liitesana, loppu>, äs: a, rakenne: "=pppppppp=pppppp"];
[perusmuoto: "puolitoista", alku: "puolestatoista", luokka: lukusana, alaluokka: erikoisluku, jatko: <liitesana, loppu>, äs: a, rakenne: "=pppppppp=pppppp"];
[perusmuoto: "puolitoista", alku: "puoleentoista",  luokka: lukusana, alaluokka: erikoisluku, jatko: <liitesana, loppu>, äs: a, rakenne: "=ppppppp=pppppp"];
[perusmuoto: "puolitoista", alku: "puoleltatoista", luokka: lukusana, alaluokka: erikoisluku, jatko: <liitesana, loppu>, äs: a, rakenne: "=pppppppp=pppppp"];
[perusmuoto: "puolitoista", alku: "puolellatoista", luokka: lukusana, alaluokka: erikoisluku, jatko: <liitesana, loppu>, äs: a, rakenne: "=pppppppp=pppppp"];
[perusmuoto: "puolitoista", alku: "puolelletoista", luokka: lukusana, alaluokka: erikoisluku, jatko: <liitesana, loppu>, äs: a, rakenne: "=pppppppp=pppppp"];
[perusmuoto: "puolitoista", alku: "puolettatoista", luokka: lukusana, alaluokka: erikoisluku, jatko: <liitesana, loppu>, äs: a, rakenne: "=pppppppp=pppppp"];
[perusmuoto: "puolitoista", alku: "puolisentoista", luokka: lukusana, alaluokka: erikoisluku, jatko: <liitesana, loppu>, äs: a, rakenne: "=pppppppp=pppppp"];

[perusmuoto: "nelitoista", alku: "nelitoista", luokka: lukusana, jatko: @nimisana + <laatusana, nimi_laatusana, teonsana, etuliite, tavuviiva>, äs: a, tiedot: <ei_voikko>, rakenne: "=pppp=pppppp"];

[perusmuoto: "sata", alku: "sa", luokka: lukusana, lukutyyppi: perusluku, alaluokka: sata, jatko: <pata>, äs: a];
[perusmuoto: "tuhat", alku: "tuha", luokka: lukusana, lukutyyppi: perusluku, alaluokka: tuhat, jatko: <tuhat>, äs: a];
[perusmuoto: "miljoona", alku: "miljoon", luokka: lukusana, lukutyyppi: perusluku, alaluokka: miljoona, jatko: <apaja>, äs: a];
[perusmuoto: "miljaardi", alku: "miljaard", luokka: lukusana, lukutyyppi: perusluku, alaluokka:  miljoona, jatko: <paperi>, äs: a, tiedot: <murre>];
[perusmuoto: "miljardi", alku: "miljard", luokka: lukusana, lukutyyppi: perusluku, alaluokka: miljoona, jatko: <paperi>, äs: a];
[perusmuoto: "miljarti", alku: "miljart", luokka: lukusana, lukutyyppi: perusluku, alaluokka: miljoona, jatko: <paperi>, äs: a, tiedot: <murre>];
[perusmuoto: "biljoona", alku: "biljoon", luokka: lukusana, lukutyyppi: perusluku, alaluokka: miljoona, jatko: <asema>, äs: a];
[perusmuoto: "triljoona", alku: "triljoon", luokka: lukusana, lukutyyppi: perusluku, alaluokka: miljoona, jatko: <asema>, äs: a];
[perusmuoto: "kvadriljoona", alku: "kvadriljoon", luokka: lukusana, lukutyyppi: perusluku, alaluokka: miljoona, jatko: <asema>, äs: a];
[perusmuoto: "kvintiljoona", alku: "kvintiljoon", luokka: lukusana, lukutyyppi: perusluku, alaluokka: miljoona, jatko: <asema>, äs: a];
[perusmuoto: "sekstiljoona", alku: "sekstiljoon", luokka: lukusana, lukutyyppi: perusluku, alaluokka: miljoona, jatko: <asema>, äs: a];
[perusmuoto: "septiljoona", alku: "septiljoon", luokka: lukusana, lukutyyppi: perusluku, alaluokka: miljoona, jatko: <asema>, äs: a];
[perusmuoto: "sentiljoona", alku: "sentiljoon", luokka: lukusana, lukutyyppi: perusluku, alaluokka: miljoona, jatko: <asema>, äs: a];

[perusmuoto: "ensimmäinen", alku: "ensimmäi", luokka: lukusana, lukutyyppi: järjestysluku, alaluokka: yksiyhdeksän, jatko: <nainen>, äs: ä];
[perusmuoto: "ensimäinen", alku: "ensimäi", luokka: lukusana, lukutyyppi: järjestysluku, alaluokka: yksiyhdeksän, jatko: <nainen>, äs: ä, tiedot: <murre>];
[perusmuoto: "yhdes", alku: "yhde", luokka: lukusana, lukutyyppi: järjestysluku, alaluokka: yksiyhdeksän, jatko: <kahdeksas>, äs: ä];
[perusmuoto: "toinen", alku: "toi", luokka: lukusana, lukutyyppi: järjestysluku, alaluokka: yksiyhdeksän, jatko: <nainen>, äs: a];
[perusmuoto: "kahdes", alku: "kahde", luokka: lukusana, lukutyyppi: järjestysluku, alaluokka: yksiyhdeksän, jatko: <kahdeksas>, äs: a];
[perusmuoto: "kolmas", alku: "kolma", luokka: lukusana, lukutyyppi: järjestysluku, alaluokka: yksiyhdeksän, jatko: <kahdeksas>, äs: a];
[perusmuoto: "neljäs", alku: "neljä", luokka: lukusana, lukutyyppi: järjestysluku, alaluokka: yksiyhdeksän, jatko: <kahdeksas>, äs: ä];
[perusmuoto: "viides", alku: "viide", luokka: lukusana, lukutyyppi: järjestysluku, alaluokka: yksiyhdeksän, jatko: <kahdeksas>, äs: ä];
[perusmuoto: "kuudes", alku: "kuude", luokka: lukusana, lukutyyppi: järjestysluku, alaluokka: yksiyhdeksän, jatko: <kahdeksas>, äs: a];
[perusmuoto: "seitsemäs", alku: "seitsemä", luokka: lukusana, lukutyyppi: järjestysluku, alaluokka: yksiyhdeksän, jatko: <kahdeksas>, äs: ä];
[perusmuoto: "kahdeksas", alku: "kahdeksa", luokka: lukusana, lukutyyppi: järjestysluku, alaluokka: yksiyhdeksän, jatko: <kahdeksas>, äs: a];
[perusmuoto: "yhdeksäs", alku: "yhdeksä", luokka: lukusana, lukutyyppi: järjestysluku, alaluokka: yksiyhdeksän, jatko: <kahdeksas>, äs: ä];
[perusmuoto: "kymmenes", alku: "kymmene", luokka: lukusana, lukutyyppi: järjestysluku, alaluokka: kymmenen, jatko: <kahdeksas>, äs: ä];

# Eka ja toka ovat oikeasti lukusanoja, mutta lukusanasäännöt eivät tunnista niitä lukusanoina.
[perusmuoto: "eka", alku: "ek", luokka: nimisana, jatko: <kala>, äs: a, tiedot: <ei_ys, ei_voikko>];
[perusmuoto: "toka", alku: "tok", luokka: nimisana, jatko: <koira>, äs: a, tiedot: <ei_ys, ei_voikko>];

[perusmuoto: "sadas", alku: "sada", luokka: lukusana, lukutyyppi: järjestysluku, alaluokka: sata, jatko: <kahdeksas>, äs: a];
[perusmuoto: "tuhannes", alku: "tuhanne", luokka: lukusana, lukutyyppi: järjestysluku, alaluokka: tuhat, jatko: <kahdeksas>, äs: a];
[perusmuoto: "miljoonas", alku: "miljoona", luokka: lukusana, lukutyyppi: järjestysluku, alaluokka: miljoona, jatko: <kahdeksas>, äs: a];
[perusmuoto: "miljardis", alku: "miljardi", luokka: lukusana, lukutyyppi: järjestysluku, alaluokka: miljoona, jatko: <kahdeksas>, äs: a];


[perusmuoto: "parisen", alku: "parisen", luokka: lukusana, alaluokka: erikoisluku, jatko: <loppu>, äs: a];
[perusmuoto: "kolmisen", alku: "kolmisen", luokka: lukusana, alaluokka: erikoisluku, jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "nelisen", alku: "nelisen", luokka: lukusana, alaluokka: erikoisluku, jatko: <liitesana, loppu>, äs: ä];
[perusmuoto: "viitisen", alku: "viitisen", luokka: lukusana, alaluokka: erikoisluku, jatko: <liitesana, loppu>, äs: ä];
[perusmuoto: "kuutisen", alku: "kuutisen", luokka: lukusana, alaluokka: erikoisluku, jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "seitsemisen", alku: "seitsemisen", luokka: lukusana, alaluokka: erikoisluku, jatko: <liitesana, loppu>, äs: ä];
[perusmuoto: "kahdeksisen", alku: "kahdeksisen", luokka: lukusana, alaluokka: erikoisluku, jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "yhdeksisen", alku: "yhdeksisen", luokka: lukusana, alaluokka: erikoisluku, jatko: <liitesana, loppu>, äs: ä];
[perusmuoto: "kymmenisen", alku: "kymmenisen", luokka: lukusana, alaluokka: erikoisluku, jatko: <liitesana, loppu>, äs: ä];

[perusmuoto: "parisenkymmentä", alku: "parisenkymmentä", luokka: lukusana, alaluokka: erikoisluku, jatko: <loppu>, äs: ä, rakenne: "=ppppppp=pppppppp"];
[perusmuoto: "kolmisenkymmentä", alku: "kolmisenkymmentä", luokka: lukusana, alaluokka: erikoisluku, jatko: <liitesana, loppu>, äs: ä, rakenne: "=pppppppp=pppppppp"];
[perusmuoto: "nelisenkymmentä", alku: "nelisenkymmentä", luokka: lukusana, alaluokka: erikoisluku, jatko: <liitesana, loppu>, äs: ä, rakenne: "=ppppppp=pppppppp"];
[perusmuoto: "viitisenkymmentä", alku: "viitisenkymmentä", luokka: lukusana, alaluokka: erikoisluku, jatko: <liitesana, loppu>, äs: ä, rakenne: "=pppppppp=pppppppp"];
[perusmuoto: "kuutisenkymmentä", alku: "kuutisenkymmentä", luokka: lukusana, alaluokka: erikoisluku, jatko: <liitesana, loppu>, äs: ä, rakenne: "=pppppppp=pppppppp"];
[perusmuoto: "seitsemisenkymmentä", alku: "seitsemisenkymmentä", luokka: lukusana, alaluokka: erikoisluku, jatko: <liitesana, loppu>, äs: ä, rakenne: "=ppppppppppp=pppppppp"];
[perusmuoto: "kahdeksisenkymmentä", alku: "kahdeksisenkymmentä", luokka: lukusana, alaluokka: erikoisluku, jatko: <liitesana, loppu>, äs: ä, rakenne: "=ppppppppppp=pppppppp"];
[perusmuoto: "yhdeksisenkymmentä", alku: "yhdeksisenkymmentä", luokka: lukusana, alaluokka: erikoisluku, jatko: <liitesana, loppu>, äs: ä, rakenne: "=pppppppppp=pppppppp"];
[perusmuoto: "parisensataa", alku: "parisensataa", luokka: lukusana, alaluokka: erikoisluku, jatko: <loppu>, äs: a, rakenne: "=ppppppp=ppppp"];

[perusmuoto: "kolmi", alku: "kolmi", luokka: lukusana, alaluokka: erikoisluku, jatko: <lukusanan_jälkiliite>, äs: ä];
[perusmuoto: "neli", alku: "neli", luokka: lukusana, alaluokka: erikoisluku, jatko: <lukusanan_jälkiliite>, äs: ä];
[perusmuoto: "seitsen", alku: "seitsen", luokka: lukusana, alaluokka: erikoisluku, jatko: <lukusanan_jälkiliite>, äs: ä];
[perusmuoto: "kymmen", alku: "kymmen", luokka: lukusana, alaluokka: erikoisluku, jatko: <lukusanan_jälkiliite>, äs: ä];
[perusmuoto: "kaksikymmen", alku: "kaksikymmen", luokka: lukusana, alaluokka: erikoisluku, jatko: <lukusanan_jälkiliite>, äs: ä, rakenne: "=ppppp=pppppp"];
[perusmuoto: "kolmikymmen", alku: "kolmikymmen", luokka: lukusana, alaluokka: erikoisluku, jatko: <lukusanan_jälkiliite>, äs: ä, rakenne: "=ppppp=pppppp"];
[perusmuoto: "nelikymmen", alku: "nelikymmen", luokka: lukusana, alaluokka: erikoisluku, jatko: <lukusanan_jälkiliite>, äs: ä, rakenne: "=pppp=pppppp"];
[perusmuoto: "viisikymmen", alku: "viisikymmen", luokka: lukusana, alaluokka: erikoisluku, jatko: <lukusanan_jälkiliite>, äs: ä, rakenne: "=ppppp=pppppp"];
[perusmuoto: "kuusikymmen", alku: "kuusikymmen", luokka: lukusana, alaluokka: erikoisluku, jatko: <lukusanan_jälkiliite>, äs: ä, rakenne: "=ppppp=pppppp"];
[perusmuoto: "seitsenkymmen", alku: "seitsenkymmen", luokka: lukusana, alaluokka: erikoisluku, jatko: <lukusanan_jälkiliite>, äs: ä, rakenne: "=ppppppp=pppppp"];
[perusmuoto: "kahdeksankymmen", alku: "kahdeksankymmen", luokka: lukusana, alaluokka: erikoisluku, jatko: <lukusanan_jälkiliite>, äs: ä, rakenne: "=ppppppppp=pppppp"];
[perusmuoto: "yhdeksänkymmen", alku: "yhdeksänkymmen", luokka: lukusana, alaluokka: erikoisluku, jatko: <lukusanan_jälkiliite>, äs: ä, rakenne: "=pppppppp=pppppp"];
[perusmuoto: "monisata", alku: "monisata", luokka: lukusana, alaluokka: erikoisluku, jatko: <lukusanan_jälkiliite>, äs: a, rakenne: "=pppp=pppp"];
[perusmuoto: "monituhat", alku: "monituhat", luokka: lukusana, alaluokka: erikoisluku, jatko: <lukusanan_jälkiliite>, äs: a, rakenne: "=pppp=ppppp"];

[perusmuoto: "toistakymmentä",      alku: "toistakymmentä",      luokka: lukusana, alaluokka: erikoisluku, sijamuoto: osanto, luku: yksikkö, jatko: <liitesana, loppu>, äs: ä, rakenne: "=pppppp=pppppppp"];
[perusmuoto: "kolmattakymmentä",    alku: "kolmattakymmentä",    luokka: lukusana, alaluokka: erikoisluku, sijamuoto: osanto, luku: yksikkö, jatko: <liitesana, loppu>, äs: ä, rakenne: "=pppppppp=pppppppp"];
[perusmuoto: "neljättäkymmentä",    alku: "neljättäkymmentä",    luokka: lukusana, alaluokka: erikoisluku, sijamuoto: osanto, luku: yksikkö, jatko: <liitesana, loppu>, äs: ä, rakenne: "=pppppppp=pppppppp"];
[perusmuoto: "viidettäkymmentä",    alku: "viidettäkymmentä",    luokka: lukusana, alaluokka: erikoisluku, sijamuoto: osanto, luku: yksikkö, jatko: <liitesana, loppu>, äs: ä, rakenne: "=pppppppp=pppppppp"];
[perusmuoto: "kuudettakymmentä",    alku: "kuudettakymmentä",    luokka: lukusana, alaluokka: erikoisluku, sijamuoto: osanto, luku: yksikkö, jatko: <liitesana, loppu>, äs: ä, rakenne: "=pppppppp=pppppppp"];
[perusmuoto: "seitsemättäkymmentä", alku: "seitsemättäkymmentä", luokka: lukusana, alaluokka: erikoisluku, sijamuoto: osanto, luku: yksikkö, jatko: <liitesana, loppu>, äs: ä, rakenne: "=ppppppppppp=pppppppp"];
[perusmuoto: "kahdeksattakymmentä", alku: "kahdeksattakymmentä", luokka: lukusana, alaluokka: erikoisluku, sijamuoto: osanto, luku: yksikkö, jatko: <liitesana, loppu>, äs: ä, rakenne: "=ppppppppppp=pppppppp"];
[perusmuoto: "yhdeksättäkymmentä",  alku: "yhdeksättäkymmentä",  luokka: lukusana, alaluokka: erikoisluku, sijamuoto: osanto, luku: yksikkö, jatko: <liitesana, loppu>, äs: ä, rakenne: "=pppppppppp=pppppppp"];

[perusmuoto: "toistasataa",      alku: "toistasataa",      luokka: lukusana, alaluokka: erikoisluku, sijamuoto: osanto, luku: yksikkö, jatko: <liitesana, loppu>, äs: a, rakenne: "=pppppp=ppppp"];
[perusmuoto: "kolmattasataa",    alku: "kolmattasataa",    luokka: lukusana, alaluokka: erikoisluku, sijamuoto: osanto, luku: yksikkö, jatko: <liitesana, loppu>, äs: a, rakenne: "=pppppppp=ppppp"];
[perusmuoto: "neljättäsataa",    alku: "neljättäsataa",    luokka: lukusana, alaluokka: erikoisluku, sijamuoto: osanto, luku: yksikkö, jatko: <liitesana, loppu>, äs: a, rakenne: "=pppppppp=ppppp"];
[perusmuoto: "viidettäsataa",    alku: "viidettäsataa",    luokka: lukusana, alaluokka: erikoisluku, sijamuoto: osanto, luku: yksikkö, jatko: <liitesana, loppu>, äs: a, rakenne: "=pppppppp=ppppp"];
[perusmuoto: "kuudettasataa",    alku: "kuudettasataa",    luokka: lukusana, alaluokka: erikoisluku, sijamuoto: osanto, luku: yksikkö, jatko: <liitesana, loppu>, äs: a, rakenne: "=pppppppp=ppppp"];
[perusmuoto: "seitsemättäsataa", alku: "seitsemättäsataa", luokka: lukusana, alaluokka: erikoisluku, sijamuoto: osanto, luku: yksikkö, jatko: <liitesana, loppu>, äs: a, rakenne: "=ppppppppppp=ppppp"];
[perusmuoto: "kahdeksattasataa", alku: "kahdeksattasataa", luokka: lukusana, alaluokka: erikoisluku, sijamuoto: osanto, luku: yksikkö, jatko: <liitesana, loppu>, äs: a, rakenne: "=ppppppppppp=ppppp"];
[perusmuoto: "yhdeksättäsataa",  alku: "yhdeksättäsataa",  luokka: lukusana, alaluokka: erikoisluku, sijamuoto: osanto, luku: yksikkö, jatko: <liitesana, loppu>, äs: a, rakenne: "=pppppppppp=ppppp"];

[perusmuoto: "toistatuhatta",      alku: "toistatuhatta",      luokka: lukusana, alaluokka: erikoisluku, sijamuoto: osanto, luku: yksikkö, jatko: <liitesana, loppu>, äs: a, rakenne: "=pppppp=ppppppp"];
[perusmuoto: "kolmattatuhatta",    alku: "kolmattatuhatta",    luokka: lukusana, alaluokka: erikoisluku, sijamuoto: osanto, luku: yksikkö, jatko: <liitesana, loppu>, äs: a, rakenne: "=pppppppp=ppppppp"];
[perusmuoto: "neljättätuhatta",    alku: "neljättätuhatta",    luokka: lukusana, alaluokka: erikoisluku, sijamuoto: osanto, luku: yksikkö, jatko: <liitesana, loppu>, äs: a, rakenne: "=pppppppp=ppppppp"];
[perusmuoto: "viidettätuhatta",    alku: "viidettätuhatta",    luokka: lukusana, alaluokka: erikoisluku, sijamuoto: osanto, luku: yksikkö, jatko: <liitesana, loppu>, äs: a, rakenne: "=pppppppp=ppppppp"];
[perusmuoto: "kuudettatuhatta",    alku: "kuudettatuhatta",    luokka: lukusana, alaluokka: erikoisluku, sijamuoto: osanto, luku: yksikkö, jatko: <liitesana, loppu>, äs: a, rakenne: "=pppppppp=ppppppp"];
[perusmuoto: "seitsemättätuhatta", alku: "seitsemättätuhatta", luokka: lukusana, alaluokka: erikoisluku, sijamuoto: osanto, luku: yksikkö, jatko: <liitesana, loppu>, äs: a, rakenne: "=ppppppppppp=ppppppp"];
[perusmuoto: "kahdeksattatuhatta", alku: "kahdeksattatuhatta", luokka: lukusana, alaluokka: erikoisluku, sijamuoto: osanto, luku: yksikkö, jatko: <liitesana, loppu>, äs: a, rakenne: "=ppppppppppp=ppppppp"];
[perusmuoto: "yhdeksättätuhatta",  alku: "yhdeksättätuhatta",  luokka: lukusana, alaluokka: erikoisluku, sijamuoto: osanto, luku: yksikkö, jatko: <liitesana, loppu>, äs: a, rakenne: "=pppppppppp=ppppppp"];

[alku: "1", luokka: lukusana, alaluokka: numeromerkki, jatko: <>];
[alku: "2", luokka: lukusana, alaluokka: numeromerkki, jatko: <>];
[alku: "3", luokka: lukusana, alaluokka: numeromerkki, jatko: <>];
[alku: "4", luokka: lukusana, alaluokka: numeromerkki, jatko: <>];
[alku: "5", luokka: lukusana, alaluokka: numeromerkki, jatko: <>];
[alku: "6", luokka: lukusana, alaluokka: numeromerkki, jatko: <>];
[alku: "7", luokka: lukusana, alaluokka: numeromerkki, jatko: <>];
[alku: "8", luokka: lukusana, alaluokka: numeromerkki, jatko: <>];
[alku: "9", luokka: lukusana, alaluokka: numeromerkki, jatko: <>];
[alku: "0", luokka: lukusana, alaluokka: numeromerkki, jatko: <>];
[alku: ",", luokka: lukusana, alaluokka: pilkku, jatko: <>];

# Lukusanoihin suoraan liittyviä jälkiliitteitä
#
# HUOMIO! Vokaalilla alkavat jälkiliitteet eivät välttämättä toimi
# oikein, koska yhdysmerkkiä ei hyväksytä. Lisäksi jälkiliitteen
# sanaluokkaa ei määritellä, minkä johdosta jälkiliitteiden
# automaattinen johtaminen ja käyttö yhdyssanan sisäosana on jonkin
# verran tavallista rajoitetumpaa. (Verbit eivät tunnu toimivan.)
[perusmuoto: "karkeinen", alku: "karkei", luokka: lukusanan_jälkiliite, lukutyyppi: perusluku, jatko: <nainen>, äs: a];
[perusmuoto: "kiloinen", alku: "kiloi", luokka: lukusanan_jälkiliite, lukutyyppi: perusluku, jatko: <nainen>, äs: a];
[perusmuoto: "kulmio", alku: "kulmio", luokka: lukusanan_jälkiliite, lukutyyppi: perusluku, jatko: <autio>, äs: a];
[perusmuoto: "luku", alku: "lu", luokka: lukusanan_jälkiliite, lukutyyppi: perusluku, jatko: <luku>, äs: a]; # "kahdeksankymmentäluku"
[perusmuoto: "luokkalainen", alku: "luokkalai", luokka: lukusanan_jälkiliite, lukutyyppi: järjestysluku, jatko: <nainen>, äs: a];
[perusmuoto: "osa", alku: "os", luokka: lukusanan_jälkiliite, lukutyyppi: järjestysluku, jatko: <koira>, äs: a];
[perusmuoto: "tekijäinen", alku: "tekijäi", luokka: lukusanan_jälkiliite, lukutyyppi: perusluku, jatko: <nainen>, äs: ä];
[perusmuoto: "vuotias", alku: "vuotia", luokka: lukusanan_jälkiliite, lukutyyppi: perusluku, jatko: <vieras>, äs: a];


# Roomalaiset numerot 2-4, 6-9, 11-40. I, V ja X jäsennetään kirjaimina.
# Luvut 4, 9, 14, 19, ..., kirjoitetaan kahdella eri tavalla.

define @r_jatko := <tavuviiva, kaksoispiste, loppu>;

[alku: "ii",    luokka: lyhenne, jatko: <loppu>, äs: a, rakenne: "=qq", tiedot: <ei_sukija>];
[alku: "iii",   luokka: lyhenne, jatko: <loppu>, äs: a, rakenne: "=qqq", tiedot: <ei_sukija>];
[alku: "iiii",  luokka: lyhenne, jatko: <loppu>, äs: a, rakenne: "=qqqq", tiedot: <ei_sukija>];
[alku: "iv",    luokka: lyhenne, jatko: <loppu>, äs: a, rakenne: "=qq", tiedot: <ei_sukija>];
[alku: "vi",    luokka: lyhenne, jatko: <loppu>, äs: a, rakenne: "=qq", tiedot: <ei_sukija>];
[alku: "vii",   luokka: lyhenne, jatko: <loppu>, äs: a, rakenne: "=qqq", tiedot: <ei_sukija>];
[alku: "viii",  luokka: lyhenne, jatko: <loppu>, äs: a, rakenne: "=qqqq", tiedot: <ei_sukija>];
[alku: "viiii", luokka: lyhenne, jatko: <loppu>, äs: a, rakenne: "=qqqqq", tiedot: <ei_sukija>];
[alku: "ix",    luokka: lyhenne, jatko: <loppu>, äs: a, rakenne: "=qq", tiedot: <ei_sukija>];

[alku: "II",    luokka: lyhenne, jatko: @r_jatko, äs: a, rakenne: "=jj"];
[alku: "III",   luokka: lyhenne, jatko: @r_jatko, äs: a, rakenne: "=jjj"];
[alku: "IIII",  luokka: lyhenne, jatko: @r_jatko, äs: a, rakenne: "=jjjj"];
[alku: "IV",    luokka: lyhenne, jatko: @r_jatko, äs: a, rakenne: "=jj"];
[alku: "VI",    luokka: lyhenne, jatko: @r_jatko, äs: a, rakenne: "=jj"];
[alku: "VII",   luokka: lyhenne, jatko: @r_jatko, äs: a, rakenne: "=jjj"];
[alku: "VIII",  luokka: lyhenne, jatko: @r_jatko, äs: a, rakenne: "=jjjj"];
[alku: "VIIII", luokka: lyhenne, jatko: @r_jatko, äs: a, rakenne: "=jjjjj"];
[alku: "IX",    luokka: lyhenne, jatko: @r_jatko, äs: a, rakenne: "=jj"];

[alku: "xi",     luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xii",    luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xiii",   luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xiv",    luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xv",     luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xvi",    luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xvii",   luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xviii",  luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xviiii", luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xix",    luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xx",     luokka: lyhenne, jatko: @r_jatko, äs: a];

[alku: "xxi",     luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xxii",    luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xxiii",   luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xxiiii",  luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xxiv",    luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xxv",     luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xxvi",    luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xxvii",   luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xxviii",  luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xxviiii", luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xxix",    luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xxx",     luokka: lyhenne, jatko: @r_jatko, äs: a];

[alku: "xxxi",     luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xxxii",    luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xxxiii",   luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xxxiiii",  luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xxxiv",    luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xxxv",     luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xxxvi",    luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xxxvii",   luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xxxviii",  luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xxxviiii", luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xxxix",    luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xxxx",     luokka: lyhenne, jatko: @r_jatko, äs: a];
[alku: "xl",       luokka: lyhenne, jatko: @r_jatko, äs: a];
