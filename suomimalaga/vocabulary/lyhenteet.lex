# Suomi-malaga, suomen kielen muoto-opin kuvaus.
#
# Tekijänoikeus © 2006-2007 Hannu Väisänen (Etunimi.Sukunimi@joensuu.fi)
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
#
# This file has been modified by the contributors of Voikko project.
# Last change was on $Date$ by $Author$.

# Lyhenteitä. (Lyhennesanat [esimerkiksi "Nato"] käsitellään Joukahaisessa)
#
# Muista, että lyhenteen voi joskus lukea sekä lyhentämättömässä muodossaan
# (esim. "aktiebolag") että kirjain kerrallaan ("aa-bee"). Tämä saattaa
# tarkoittaa, että etu- ja takavokaalitaivutukset täytyy molemmat hyväksyä.

define @lyhenteen_jatko := <tavuviiva, kaksoispiste, loppu>;

# Mitä nämä tarkoittavat?
[alku: "1:o", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];  # 1°
[alku: "2:o", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];  # 2° jne
[alku: "3:o", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "4:o", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "5:o", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "6:o", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "7:o", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "8:o", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "9:o", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];

[alku: "A4", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jj"];
[alku: "ab", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä]; # aktiebolag, osakeyhtiö
[alku: "ADD", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjj"]; # Attention Deficit Disorder
[alku: "ADHD", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjjj"]; # Attention Deficit (and) Hyperactivity Disorder
[alku: "adj.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # adjektiivi
[alku: "ADSL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjjj"];
[alku: "AIDS", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjjj"]; # Acquired Immune Deficiency Syndrome
[alku: "alk.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # alkuaan, alkaen
[alku: "ao.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # asianomainen
[alku: "a.p.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "apulaisj.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "apul.joht.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>]; 
[alku: "arv.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # arvoisa
[alku: "ASCII", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjjjj", tiedot: <ei_voikko>];
[alku: "ASO", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jjj", tiedot: <ei_voikko>];
[alku: "atk", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä]; # automaattinen tietojen käsittely
[alku: "ATK", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"]; # automaattinen tietojen käsittely
[alku: "ay", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä]; # avoin yhtiö; ammattiyhdistys
[alku: "bar", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a];
[alku: "BBC", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"]; # British Broadcasting Corporation
[alku: "cd", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä]; # cd-levy; kandela
[alku: "CD", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jj"]; # cd-levy
[alku: "cd-rom", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä]; # cd-levy; kandela
[alku: "CD-ROM", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jj-=jjj"]; # cd-levy
[alku: "CIA", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"]; # Central Intelligence Agency
[alku: "cm", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä]; # senttimetri(ä)
[alku: "CNN", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"]; # Cable News Network
[alku: "COSS", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjjj", tiedot: <ei_voikko>];
[alku: "c.s.i.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "csi", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "CVS", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjjj", tiedot: <ei_voikko>];
[alku: "dem.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # demokraattinen
[alku: "dlf", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "dl", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä]; # desilitra(a)
[alku: "dm", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä]; # desimetri(ä)
[alku: "DNA", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jjj"]; # deoksiribonukleiinihappo
[alku: "dvd", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä]; # digital versatile disk
[alku: "DVD", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjj"]; # digital versatile disk
[alku: "dvi", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "EAN", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"]; # European Article Number, EAN-koodi
[alku: "ed.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # edellinen; edellä; edustaja
[alku: "EEC", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj", tiedot: <ei_voikko>];
[alku: "EEST", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjjj"]; # Eastern European Summer Time
[alku: "EET", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjj"]; # Eastern European Time
[alku: "EKG", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj", tiedot: <ei_voikko>];
[alku: "EK", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jj"]; # eduskunta
[alku: "eKr.", luokka: lyhenne, jatko: <loppu>, äs: aä, rakenne: "=qjqq"]; # ennen Kristuksen syntymää
[alku: "elok.", luokka: lyhenne, jatko: <loppu>, äs: a]; # elokuu
[alku: "em.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # edellä mainittu
[alku: "EM", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jj"]; # euroopanmestaruus, Euroopan-mestaruus...
[alku: "emt.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # edellä mainittu teos
[alku: "ent.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # entinen
[alku: "eps", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "esim", luokka: lyhenne, jatko: <kaksoispiste>, äs: ä, tiedot: <ei_voikko>];
[alku: "esim.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # esimerkki
[alku: "esp", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "EU", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jj"]; # Euroopan unioni
[alku: "EUR", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"]; # euro(a)
[alku: "ev.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # eversti
[alku: "FAQ", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jjj"]; # Frequently Asked Questions
[alku: "FBI", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"]; # Federal Bureau of Investigation
[alku: "fft", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "fia", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "FI", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jj"]; # Suomen maatunnus
[alku: "fi", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä]; # suomen kielitunnus
[alku: "FL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jj"]; # filosofian lisensiaatti
[alku: "FM", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jj"]; # filosofian maisteri
[alku: "FT", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jj"]; # filosofian tohtori
[alku: "GMT", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjj"]; # Greenwich Mean Time
[alku: "GPS", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjj"];
[alku: "gsl", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "GSM", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjj"]; # Global System for Mobile Communication
[alku: "ha", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a]; # hehtaari
[alku: "harv.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "heinäk.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "helmik.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "HIV", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjj"]; # human immunodeficiency virus
[alku: "hra", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "HS", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jj"]; # Helsingin Sanomat
[alku: "hth", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "HTML", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjjj"];
[alku: "http", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä];
[alku: "HTTP", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjjj"];
[alku: "huhtik.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "huom.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "Hz", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jq"]; # hertsi(ä)
[alku: "iag", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "IKL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjj", tiedot: <ei_voikko>]; # Isänmaallinen kansanliike
[alku: "ilm.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # ilmestynyt; ilmoitus
[alku: "IMF", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjj"]; # International Monetary Fund
[alku: "ip.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];    # Iltapäivällä
[alku: "IP", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jj"]; # Internet Protocol
[alku: "ISBN", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjjj"]; # International Standard Book Number
[alku: "it", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä]; # information technology; ilmatorjunta
[alku: "IT", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jj"]; # information technology; ilmatorjunta
[alku: "j.e.p.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>]; # Ja niin edespäin.
[alku: "Jk.", luokka: lyhenne, jatko: <loppu>, äs: aä, rakenne: "=jqq"]; # jälkikirjoitus
[alku: "jKr.", luokka: lyhenne, jatko: <loppu>, äs: aä, rakenne: "=qjqq"]; # jälkeen Kristuksen syntymän
[alku: "j.n.e.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "jne.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # ja niin edelleen
[alku: "jnep", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>]; # Ja niin edespäin.
[alku: "jouluk.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "kd", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>]; # Kansandemokraatti.
[alku: "kesäk.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "kg", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä]; # kilogramma(a)
[alku: "kh.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # koehenkilö; kylpyhuone; kertausharjoitus
[alku: "KH", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jj", tiedot: <ei_voikko>];
[alku: "khra", luokka: lyhenne, jatko: <tavuviiva, loppu>, äs: aä]; # kirkkoherra
[alku: "kHz", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=qjq"]; # kilohertsi(ä)
[alku: "kirj.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # kirjoittanut; kirjataan
[alku: "kJ", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=qj"]; # kilojoule
[alku: "kk.", luokka: lyhenne, jatko: <loppu>, äs: a]; # kirkonkylä; keittokomero
[alku: "kk", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a]; # kuukausi
[alku: "k:llo", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "klo", luokka: lyhenne, jatko: <tavuviiva, loppu>, äs: aä]; # kello (sisälyhenne, ei taivuteta kaksoispisteen avulla)
[alku: "km", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä]; # kilometri
[alku: "kok.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # Kokoomus; kokelas
[alku: "kom.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>]; # Kommunisti(t).
[alku: "kpl", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä]; # kappale(tta)
[alku: "kr", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä]; # kruunu(a)
[alku: "krs", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä]; # kerros
[alku: "ks.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # katso
[alku: "kts", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "ldl", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "LED", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjj"];
[alku: "lis.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # lisensiaatti
[alku: "lkm.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # lukumäärä
[alku: "LKP", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj", tiedot: <ei_voikko>]; # Liberaalinen kansanpuolue.
[alku: "lokak.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "lut.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # luterilainen
[alku: "LVI", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"]; # lämmitys-, vesijohto- ja ilmanvaihtotekniikka 
[alku: "maalisk.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "maanvilj.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "maanv.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "maist.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # maisteri
[alku: "marrask.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "MHz", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjq"]; # megahertsi(ä)
[alku: "MikroPC", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "milj.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "mk", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a]; # markka(a)
[alku: "ml", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä]; # millilitra(a)
[alku: "mma", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "mm.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # muun muassa
[alku: "MM", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jj"]; # maailmanmestaruus
[alku: "mm", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä]; # millimetri(ä)
[alku: "mp3", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a];
[alku: "MP3", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jjj"];
[alku: "mrd.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # miljardia
[alku: "mrk", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>]; # Markkaa.
[alku: "MTK", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jjj"];
[alku: "MTV3", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jjjj"];
[alku: "MTV", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "mv", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>]; # Maanviljelijä.
[alku: "nimim.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # nimimerkki
[alku: "n.k.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];  # Niin kutsuttu.
[alku: "nk.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # niin kutsuttu
[alku: "NKP", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj", tiedot: <ei_voikko>]; # Neuvostoliiton kommunistinen puolue.
[alku: "nm", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä]; # nanometri (nm); newtonmetri (Nm)
[alku: "n:o.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>]; # Numero.
[alku: "n.s.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>]; # Niin sanottu.
[alku: "ns.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # niin sanottu
[alku: "ns", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä]; # nanosekunti(a)
[alku: "nuor.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # Nuorsuomalainen.
[alku: "OK", luokka: lyhenne, jatko: <loppu>, äs: a, rakenne: "=jj"];
[alku: "op.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # opettaja
[alku: "OpenBSD", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, tiedot: <ei_voikko>];
[alku: "o.s.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # omaa sukua
[alku: "os.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # osoite; osasto
[alku: "OTK", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj", tiedot: <ei_voikko>]; # Osuustukkukauppa.
[alku: "oyj", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä]; # julkinen osakeyhtiö
[alku: "oy", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä]; # osakeyhtiö
[alku: "pc", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä]; # parsek(ia); pesonal computer
[alku: "PC", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jj"]; # personal computer
[alku: "pj", luokka: lyhenne, jatko: <kaksoispiste, tavuviiva>, äs: aä]; # puheenjohtaja
[alku: "pj.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # puheenjohtaja
[alku: "pm", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "pnä", luokka: lyhenne, jatko: <loppu>, äs: aä]; # päivänä
[alku: "POP", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj", tiedot: <ei_voikko>];
[alku: "porv.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "PR", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jj"]; # public relations
[alku: "prof.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # professori
[alku: "ps", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä]; # pussi(a)
[alku: "PS", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jj"]; # jälkikirjoitus; Perussuomalaiset (puolue)
[alku: "puh.joht.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # puheenjohtaja
[alku: "puh.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # puhelin
[alku: "pvä", luokka: lyhenne, jatko: <loppu>, äs: aä]; # päivä
[alku: "qt", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "rkl", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a]; # ruokalusikallinen, -sta
[alku: "RKP", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"]; # Ruotsalainen Kansanpuolue
[alku: "r.l.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "rn:o", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>]; # (Maatilan) rekisterinumero.
[alku: "rva", luokka: lyhenne, jatko: <loppu>, äs: aä]; # rouva
[alku: "r.y.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "ry", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä]; # rekisteröity yhdistys
[alku: "SAJ", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj", tiedot: <ei_voikko>]; # Suomen Ammattijärjestöjen Keskusjärjestö.
[alku: "SAK", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"]; # Suomen Ammattiliittojen Keskusjärjestö
[alku: "SA", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jj"]; # Suomen Akatemia; Suomen armeija
[alku: "sd.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # sosiaalidemokraatti
[alku: "SDP", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"]; # Suomen Sosialidemokraattinen Puolue
[alku: "sf", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "sfst", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "SI", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jj"]; # kansainvälinen mittayksikköjärjestelmä 
[alku: "s.k.d.l", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "SKDL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjjj"]; # Suomen Kansan Demokraattinen Liitto
[alku: "SKL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"]; # Suomen Kristillinen Liitto; Suomen kuntaliitto
[alku: "SK", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä]; # Suomen Kuvalehti
[alku: "SKOP", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "SKP", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"]; # Suomen Kommunistinen Puolue
[alku: "SKS", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"]; # Suomalaisen Kirjallisuuden Seura
[alku: "Smk", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jqq", tiedot: <ei_voikko>]; # Suomen markka
[alku: "SM", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jj"]; # suomenmestaruus, Suomen-mestaruus, Suomen mestaruus
[alku: "SMP", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"]; # Suomen Maaseudun Puolue
[alku: "SNDL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjjj", tiedot: <ei_voikko>];
[alku: "SNTL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjjj"]; # Sosialististen neuvostotasavaltojen liitto
[alku: "snt", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä]; # sentti(ä)
[alku: "so.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # se on
[alku: "soqt", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "sos.dem.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # sosiaalidemokraatti(nen)
[alku: "sos.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # sosiaalinen; sosialisti(nen)
[alku: "SOS", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjj"]; # Save Our Souls
[alku: "SPR", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjj"];
[alku: "SQL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "ST1", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "STL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"]; # Suomen Teollisuustoimihenkilöiden Liitto
[alku: "STT", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"]; # Suomen Tietotoimisto
[alku: "suom.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # suomentanut, suomennos
[alku: "synt.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "SYP", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj", tiedot: <ei_voikko>];
[alku: "syysk.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "tammik.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "tcl", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "TEL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj", tiedot: <ei_voikko>];
[alku: "TKK", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "tlk", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä]; # tölkki(ä)
[alku: "tl", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä]; # teelusikallinen, -sta
[alku: "tmi", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä]; # toiminimi
[alku: "tm.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # tai muu(ta)
[alku: "tms.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # tai muuta sellaista
[alku: "toim.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # toimittanut, toimittaja
[alku: "toukok.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "tov.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "TPSL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjjj", tiedot: <ei_voikko>];
[alku: "tri", luokka: lyhenne, jatko: <loppu>, äs: aä]; # tohtori
[alku: "tsfs", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "tsl", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "ts.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # toisin sanoen
[alku: "tst", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "TUL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"]; # Työväen Urheiluliitto
[alku: "TVL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj", tiedot: <ei_voikko>];
[alku: "TV", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jj"]; # televisio, teevee
[alku: "tv", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä]; # televisio, teevee
[alku: "ty", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "UKK", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"]; # Usein kysyttyjä kysymyksiä; Urho Kaleva Kekkonen
[alku: "UMTS", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjjj"]; # Universal Mobile Telecommunication System
[alku: "umts", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä]; # Universal Mobile Telecommunication System
[alku: "USA", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jjj"]; # United States of America
[alku: "USB", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjj"];
[alku: "UTC", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjj"]; # Universal Coordinated Time
[alku: "v3", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
#alku: "vanh.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "virall.", luokka: lyhenne, jatko: <loppu>, äs: aä, tiedot: <ei_voikko>];
[alku: "VRK", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jjj"]; # Väestörekisterikeskus
[alku: "vrk", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a]; # vuorokausi
[alku: "VR", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jj"]; # VR-konserni
[alku: "vrt.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # vertaa
[alku: "vt.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # virkaa toimittava
[alku: "VTT", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"]; # Valtion teknillinen tutkimuskeskus; valtiotieteiden tohtori
[alku: "wc", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä];
[alku: "WC", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jj"];
[alku: "WHO", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jjj"]; # World Health Organization
[alku: "wlan", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <ei_voikko>];
[alku: "WSOY", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjjj"];
[alku: "WTO", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jjj"]; # World Trade Organisation
[alku: "WWW", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjj"]; # World Wide Web
[alku: "www", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä]; # World Wide Web
[alku: "yht.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # yhteensä
[alku: "YK", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jj"];
[alku: "ym.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # ynnä muuta
[alku: "YM", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jj"]; # Ympäristöministeriö
[alku: "yms.", luokka: lyhenne, jatko: <loppu>, äs: aä]; # ynnä muuta sellaista
[alku: "YTL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj", tiedot: <ei_voikko>];
[alku: "YYA", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"]; # ystävyys-, yhteistyö- ja avunantosopimus

[alku: "a", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a];
[alku: "á", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "à", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "â", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ã", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "b", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "c", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ç", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "d", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ð", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "e", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "é", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "è", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ê", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ë", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "f", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "g", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "h", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "i", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "í", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ì", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "î", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ï", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "j", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "k", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "l", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "m", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "n", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ñ", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "o", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ó", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ò", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ô", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "p", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "q", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "r", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "s", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "š", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ß", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "þ", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "t", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "u", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ú", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ù", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "û", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "v", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "w", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "x", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "y", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ý", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ÿ", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ü", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "z", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ž", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "å", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ä", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "æ", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ö", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "õ", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ø", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];

[alku: "°C", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jj"];
[alku: "°F", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jj"];
[alku: "°K", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jj"];
[alku: "°", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a];
[alku: "€", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a];

#[alku: "", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
