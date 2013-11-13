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

# Seikkasanat.

# Jatko-kentässä sallittuja ovat ainoastaan seuraavat: <loppu>, <liitesana>,
# <omistusliite> ja sijapäätteet (sekä <sivumpana>). Huomaa, että
# sijapäätteiden (esim. sisäolento_ssA) jälkeen hyväksytään automaattisesti
# myös omistusliite tai liitepartikkeli. Tästä syystä sijapäätteet sopivat
# vain sanoille, joita voi käyttää substantiivien tavoin.
#
# Seikkasanojen perusmuodot:
#
# Jos sanalla on useampi kuin yksi taivutusmuoto (esim. "vieraisilla,
# vieraisille, vieraisilta"), perusmuodoksi laitetaan joko ulko_olento_llA
# (jos se on olemassa) tai sisäolento_ssA. Omistusliitteen perusmuoto on
# kolmannen persoonan -Vn.
#
# Substantiiveista johdetuille sti-päätteisille seikkasanoille asetetaan
# perusmuodoksi nominatiivi/nimentö (esim. "hitosti", perusmuoto: "hitto").
# Adjektiiveista johdetut sti-päätteiset seikkasanat tunnistetaan
# automattisesti eli niitä ei tarvitse lisätä sanastoon.
#
# Seikkasanoja ei hyväksytä Voikossa yhdyssanan osina. Niille voidaan
# kuitenkin asettaa lippu ys_perusosa, jolloin sana sallitaan yhdyssanan
# perusosana samoissa tilanteissa, joissa yleisnimen käyttö yhdyssanan
# perusosana on sallittu.

 [perusmuoto: "ainut", alku: "ainut", luokka: seikkasana, jatko: <liitesana, loppu, osanto_tA>, äs: a];
 [perusmuoto: "edessä", alku: "ede", luokka: seikkasana, jatko: @heikkoasteiset_paikallissijat, äs: ä];
 [perusmuoto: "eksyksissä", alku: "eksyksi", luokka: seikkasana, jatko: @sisäpaikallissijat_Vn, äs: ä];
 [perusmuoto: "ensin", alku: "ensin", luokka: seikkasana, jatko: <olento_nA, liitesana, loppu>, äs: ä];
 [perusmuoto: "etäämpänä", alku: "etääm", luokka: seikkasana, jatko: <sivumpana>, äs: ä];
 [perusmuoto: "hallussa", alku: "hallu", luokka: seikkasana, jatko: @sisä_ssA_stA, äs: a];
 [perusmuoto: "hallussa", alku: "haltu", luokka: seikkasana, jatko: <sisätulento_Vn>, äs: a];
[perusmuoto: "henkihieverissä", alku: "henkihieveri", luokka: seikkasana, jatko: @sisäpaikallissijat_Vn, äs: ä, rakenne: "=ppppp=pppppppppp"];
 [perusmuoto: "hereillä", alku: "here", luokka: seikkasana, jatko: @ulkopaikallissijat_monikko, äs: ä];
[perusmuoto: "housusilla", alku: "housusilla", luokka: seikkasana, jatko: <omistusliite>, äs: a, tiedot: <ys_perusosa>]; # Myös uimahoususillaan
[perusmuoto: "housusilla", alku: "housusille", luokka: seikkasana, jatko: <omistusliite>, äs: a, tiedot: <ys_perusosa>]; # Myös uimahoususilleen
[perusmuoto: "housusilla", alku: "housus", luokka: seikkasana, jatko: @ulkopaikallissijat_monikko, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "hujakassa", alku: "hujaka", luokka: seikkasana, jatko: <sisäolento_ssA>, äs: a];
[perusmuoto: "huosta", alku: "huosta", luokka: seikkasana, jatko: @sisäpaikallissijat_Vn + <loppu>, äs: a];
 [perusmuoto: "hörö", alku: "hörö", luokka: seikkasana, jatko: @sisäpaikallissijat_Vn + @ulkopaikallissijat, äs: ä]; # Vain -Vn-omistusliite käy
 [perusmuoto: "jalkeilla", alku: "jalke", luokka: seikkasana, jatko: @ulkopaikallissijat_monikko, äs: a];
 [perusmuoto: "juovuksissa", alku: "juovuksi", luokka: seikkasana, jatko: @sisäpaikallissijat_Vn, äs: a];
[perusmuoto: "karku", alku: "karku", luokka: seikkasana, jatko: <osanto_A, sisätulento_Vn>, äs: a]; # Omistusliitteet hyvin harvinaisia.
[perusmuoto: "karku", alku: "karu", luokka: seikkasana, jatko: <sisäolento_ssA>, äs: a]; # Omistusliitteet hyvin harvinaisia.
 [perusmuoto: "keskessä", alku: "keske", luokka: seikkasana, jatko: @sisäpaikallissijat_Vn, äs: ä];
[perusmuoto: "keväämmällä", alku: "kevääm", luokka: seikkasana, jatko: <keväämmällä>, äs: ä];
 [perusmuoto: "kilpasilla", alku: "kilpas", luokka: seikkasana, jatko: @ulkopaikallissijat_monikko, äs: a];
[perusmuoto: "kohme", alku: "kohmee", luokka: seikkasana, jatko: <sisäolento_ssA, sisätulento_seen>, äs: a];
[perusmuoto: "loitolla", alku: "loito", luokka: seikkasana, jatko: <loitolla>, äs: a]; # Hyväksytään on myös tulosija "loitos"
[perusmuoto: "miten", alku: "mitenkä", luokka: seikkasana, jatko: <liitesana, liitesana_s, loppu>, äs: ä];
[perusmuoto: "nonstop", alku: "nonstop", luokka: seikkasana, jatko: <kalsium>, äs: a, rakenne: "=ppp=pppp"];
 [perusmuoto: "painuksissa", alku: "painuksi", luokka: seikkasana, jatko: @sisäpaikallissijat_Vn, äs: a];
[perusmuoto: "paitasilla", alku: "paitasilla", luokka: seikkasana, jatko: <omistusliite>, äs: a, tiedot: <ys_perusosa>];
[perusmuoto: "paitasilla", alku: "paitasille", luokka: seikkasana, jatko: <omistusliite>, äs: a, tiedot: <ys_perusosa>];
 [perusmuoto: "parissa", alku: "pari", luokka: seikkasana, jatko: @sisäpaikallissijat_Vn, äs: a, tiedot: <ei_sukija>];
 [perusmuoto: "perempänä", alku: "perem", luokka: seikkasana, jatko: <sivumpana>, äs: ä];
 [perusmuoto: "pinnempana", alku: "pinnem", luokka: seikkasana, jatko: <sivumpana>, äs: a];
 [perusmuoto: "pohjempana", alku: "pohjem", luokka: seikkasana, jatko: <sivumpana>, äs: a];
 [perusmuoto: "pohjimpana", alku: "pohjim", luokka: seikkasana, jatko: <sivumpana>, äs: a];
[perusmuoto: "posti", alku: "postitse", luokka: seikkasana, jatko: <liitesana, loppu>, äs: a, tiedot: <ys_perusosa>];
[perusmuoto: "puolitse", alku: "puolitse", luokka: seikkasana, jatko: <liitesana, loppu>, äs: a, tiedot: <ys_perusosa>];
[perusmuoto: "päivemmällä", alku: "päivem", luokka: seikkasana, jatko: <keväämmällä>, äs: ä];
 [perusmuoto: "pökerryksissä", alku: "pökerryksi", luokka: seikkasana, jatko: @sisäpaikallissijat_Vn, äs: ä];
 [perusmuoto: "rannempana", alku: "rannem", luokka: seikkasana, jatko: <sivumpana>, äs: a];
 [perusmuoto: "rannimpana", alku: "rannim", luokka: seikkasana, jatko: <sivumpana>, äs: a];
 [perusmuoto: "reunempana", alku: "reunem", luokka: seikkasana, jatko: <sivumpana>, äs: a];
 [perusmuoto: "reunimpana", alku: "reunim", luokka: seikkasana, jatko: <sivumpana>, äs: a];
 [perusmuoto: "seassa", alku: "sea", luokka: seikkasana, jatko: @sisä_ssA_stA, äs: a];
 [perusmuoto: "seassa", alku: "seka", luokka: seikkasana, jatko: <sisätulento_Vn>, äs: a];
 [perusmuoto: "seinempänä", alku: "seinem", luokka: seikkasana, jatko: <sivumpana>, äs: ä];
 [perusmuoto: "sinnempänä", alku: "sinnem", luokka: seikkasana, jatko: <sivumpana>, äs: ä];
[perusmuoto: "sivumennen", alku: "sivumen", luokka: seikkasana, jatko: <nainen>, äs: a, rakenne: "=pppp=pppppp"];
 [perusmuoto: "sivumpana", alku: "sivum", luokka: seikkasana, jatko: <sivumpana>, äs: a];
[perusmuoto: "soopa", alku: "soopa", luokka: seikkasana, jatko: <liitesana, loppu, osanto_A>, äs: a];
 [perusmuoto: "suppu", alku: "suppu", luokka: seikkasana, jatko: <sisätulento_Vn>, äs: a];
 [perusmuoto: "suppu", alku: "supu", luokka: seikkasana, jatko: @sisä_ssA_stA + @ulkopaikallissijat, äs: a];
 [perusmuoto: "syksympänä", alku: "syksym", luokka: seikkasana, jatko: <sivumpana>, äs: ä];
 [perusmuoto: "syrjempänä", alku: "syrjem", luokka: seikkasana, jatko: <sivumpana>, äs: ä];
 [perusmuoto: "tasa", alku: "tasa", luokka: seikkasana, jatko: @sisäpaikallissijat_Vn + @ulkopaikallissijat, äs: a];
[perusmuoto: "tie", alku: "teitse", luokka: seikkasana, jatko: <liitesana, loppu>, äs: ä, tiedot: <ys_perusosa>];
 [perusmuoto: "tekeillä", alku: "teke", luokka: seikkasana, jatko: @ulkopaikallissijat_monikko, äs: ä];
[perusmuoto: "tenä", alku: "tenä", luokka: seikkasana, jatko: <loppu, nimentö_t, osanto_A, omanto_n, sisäolento_ssA>, äs: ä];
[perusmuoto: "tipotiehen", alku: "tipotie", luokka: seikkasana, jatko: <sisätulento_hVn>, äs: ä, rakenne: "=pppp=pppppppp"];
[perusmuoto: "tukkanuottasilla", alku: "tukkanuottas", luokka: seikkasana, jatko: @ulkopaikallissijat_monikko, äs: a, rakenne: "=ppppp=ppppppppppp"];
 [perusmuoto: "tännempänä", alku: "tännem", luokka: seikkasana, jatko: <sivumpana>, äs: ä];
 [perusmuoto: "väijyksissä", alku: "väijyksi", luokka: seikkasana, jatko: @sisäpaikallissijat_Vn, äs: ä];
[perusmuoto: "vääjäämättä", alku: "vääjäämä", luokka: seikkasana, jatko: <vajanto_ttA>, äs: ä];

# Nykyään "sydännä" on seikkasana, eikä sydän-nimisanan taivutusmuoto, siksi nämä pitää laittaa erikseen.
[perusmuoto: "kesäsydän", alku: "kesäsydännä", luokka: seikkasana, jatko: <liitesana, loppu>, äs: ä, tiedot: <ei_voikko>, rakenne: "=pppp=ppppppp"];
[perusmuoto: "päiväsydän", alku: "päiväsydännä", luokka: seikkasana, jatko: <liitesana, loppu>, äs: ä, tiedot: <ei_voikko>, rakenne: "=ppppp=ppppppp"];
[perusmuoto: "päiväsydän", alku: "päivä-sydännä", luokka: seikkasana, jatko: <liitesana, loppu>, äs: ä, tiedot: <ei_voikko>, rakenne: "=ppppp-=ppppppp"];
[perusmuoto: "suvisydän", alku: "suvisydännä", luokka: seikkasana, jatko: <liitesana, loppu>, äs: ä, tiedot: <ei_voikko>, rakenne: "=pppp=ppppppp"];
[perusmuoto: "suvisydän", alku: "suvi-sydännä", luokka: seikkasana, jatko: <liitesana, loppu>, äs: ä, tiedot: <ei_voikko>, rakenne: "=pppp-=ppppppp"];
[perusmuoto: "talvisydän", alku: "talvisydännä", luokka: seikkasana, jatko: <liitesana, loppu>, äs: ä, tiedot: <ei_voikko>, rakenne: "=ppppp=ppppppp"];
[perusmuoto: "yösydän", alku: "yösydännä", luokka: seikkasana, jatko: <liitesana, loppu>, äs: ä, tiedot: <ei_voikko>, rakenne: "=pp=ppppppp"];
[perusmuoto: "yösydän", alku: "yö-sydännä", luokka: seikkasana, jatko: <liitesana, loppu>, äs: ä, tiedot: <ei_voikko>, rakenne: "=pp-=ppppppp"];
