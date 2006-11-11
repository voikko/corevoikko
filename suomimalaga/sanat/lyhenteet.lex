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
#
# This file has been modified by the contributors of Hunspell-fi project.
# Last change was on $Date$ by $Author$.

# Lyhenteitä.

define @lyhenteen_jatko := <tavuviiva, kaksoispiste, loppu>;

# Mitä nämä tarkoittavat?
[alku: "1:o", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "2:o", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "3:o", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "4:o", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "5:o", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "6:o", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "7:o", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "8:o", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "9:o", luokka: lyhenne, jatko: <loppu>, äs: aä];

[alku: "a", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
#[alku: "a.p.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "A4", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jj"];
[alku: "ab", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "adj.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "ADSL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjjj"];
[alku: "alk.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "ao.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "apul.joht.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "apulaisj.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "arv.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "ASCII", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>, rakenne: "=jjjjj"];
#[alku: "ASO", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jjj"];
[alku: "atk", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ay", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "b", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "BBC", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "c", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
#[alku: "c.s.i.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "cd", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä]; # 'kandela'
[alku: "CIA", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "cm", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "CNN", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "COSS", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjjj"];
#[alku: "csi", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
#[alku: "csv", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ctrl", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>];
[alku: "d", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "dem.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "dl", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
#[alku: "dlf", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "dm", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "DNA", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "DVD", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "dvd", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
#[alku: "dvi", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "e", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ed.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "EEC", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "EK", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jj"];
[alku: "EKG", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "eKr", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=qjq"];
[alku: "elok.", luokka: lyhenne, jatko: <loppu>, äs: a];
[alku: "ent.", luokka: lyhenne, jatko: <loppu>, äs: aä];
#[alku: "eps", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
#[alku: "esim", luokka: lyhenne, jatko: <kaksoispiste>, äs: ä];
[alku: "esim.", luokka: lyhenne, jatko: <loppu>, äs: aä];
#[alku: "esp", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "EU", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jj"];
[alku: "ev.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "f", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "FAQ", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "FBI", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
#[alku: "fft", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
#[alku: "fia", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "FL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jj"];
[alku: "FM", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jj"];
[alku: "FT", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jj"];
[alku: "ftp", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>];
[alku: "fvwm", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>];
[alku: "g", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "gcc", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>];
[alku: "glibc", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>];
[alku: "GMT", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "GPL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>, rakenne: "=jjj"];
[alku: "GPS", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
#[alku: "gsl", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "GSM", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "h", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ha", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "harv.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "heinäk.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "helmik.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "HIV", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "hra", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "HS", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jj"];
#[alku: "hth", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "html", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>];
[alku: "http", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>];
[alku: "huhtik.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "huom.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "Hz", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jq"];
[alku: "i", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
#[alku: "iag", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "icewm", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>];
[alku: "ide", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>];
[alku: "IKL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "ilm.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "imap", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>];
[alku: "IMF", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "IP", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>, rakenne: "=jj"];
#[alku: "ip.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "IRC", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>, rakenne: "=jjj"];
[alku: "ISBN", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjjj"];
[alku: "IT", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jj"];
[alku: "j", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
#[alku: "j.e.p.", luokka: lyhenne, jatko: <loppu>, äs: aä];
#[alku: "j.n.e.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "Jk.", luokka: lyhenne, jatko: <loppu>, äs: aä, rakenne: "=jqq"];
[alku: "jKr", luokka: lyhenne, jatko: <loppu>, äs: aä, rakenne: "=qjq"];
[alku: "jne.", luokka: lyhenne, jatko: <loppu>, äs: aä];
#[alku: "jnep", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "jouluk.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "jpg", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>];
[alku: "k", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "k.", luokka: lyhenne, jatko: <loppu>, äs: aä];
#[alku: "k:llo", luokka: lyhenne, jatko: <loppu>, äs: aä];
#[alku: "kd", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "KDE", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>, rakenne: "=jjj"];
[alku: "kesäk.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "kg", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "KH", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jj"];
[alku: "kh.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "khra", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "kirj.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "kJ", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=qj"];
[alku: "kk", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "klo", luokka: lyhenne, jatko: <tavuviiva, loppu>, äs: aä];
[alku: "km", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "kok.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "kom.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "kpl", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "kr", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "krs", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ks.", luokka: lyhenne, jatko: <loppu>, äs: aä];
#[alku: "kts", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "l", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
#[alku: "ldl", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "LGPL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>, rakenne: "=jjjj"];
[alku: "libstdc", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>];
[alku: "lis.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "lkm.", luokka: lyhenne, jatko: <loppu>, äs: aä];
#[alku: "lkp", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "lokak.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "lut.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "LVI", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "m", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "maalisk.", luokka: lyhenne, jatko: <loppu>, äs: aä];
#[alku: "maanv.", luokka: lyhenne, jatko: <loppu>, äs: aä];
#[alku: "maanvilj.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "maist.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "marrask.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "mHz", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=qjq"];
#[alku: "mikropc", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "milj.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "mk", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ml", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "MM", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jj"];
[alku: "mm", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "mm.", luokka: lyhenne, jatko: <loppu>, äs: aä];
#[alku: "mma", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "mp3", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "mrd.", luokka: lyhenne, jatko: <loppu>, äs: aä];
#[alku: "mrk", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "MTK", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "MTV", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "MTV3", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjjj"];
#[alku: "mv", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "n", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
#[alku: "n.k.", luokka: lyhenne, jatko: <loppu>, äs: aä];
#[alku: "n.s.", luokka: lyhenne, jatko: <loppu>, äs: aä];
#[alku: "n:o.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "nimim.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "nk.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "NKP", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "nm", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "NNTP", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>, rakenne: "=jjjj"];
[alku: "ns", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ns.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "NTFS", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>, rakenne: "=jjjj"];
[alku: "NTP", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>, rakenne: "=jjj"];
[alku: "nuor.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "o", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "o.s.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "OK", luokka: lyhenne, jatko: <loppu>, äs: aä, rakenne: "=jj"];
[alku: "op.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "opengl", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>];
[alku: "os.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "OTK", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "oy", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "p", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "p.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "PC", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jj"];
[alku: "pc", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "PCI", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>, rakenne: "=jjj"];
[alku: "PDF", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>, rakenne: "=jjj"];
[alku: "pj", luokka: lyhenne, jatko: <kaksoispiste, tavuviiva>, äs: aä];
[alku: "pj.", luokka: lyhenne, jatko: <loppu>, äs: aä];
#[alku: "pm", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "pnä", luokka: lyhenne, jatko: <loppu>, äs: aä];
#[alku: "POP", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
#[alku: "porv.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "PR", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jj"];
[alku: "prof.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "PS", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jj"];
[alku: "ps", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "puh.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "puh.joht.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "pvä", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "q", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
#[alku: "qt", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "r", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
#[alku: "r.l.", luokka: lyhenne, jatko: <loppu>, äs: aä];
#[alku: "r.y.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "RFC", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>, rakenne: "=jjj"];
[alku: "rkl", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "RKP", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
#[alku: "rn:o", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "RPM", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>, rakenne: "=jjj"];
[alku: "rva", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "ry", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä];
[alku: "s", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
#[alku: "s.k.d.l", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "SA", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jj"];
#[alku: "saj", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "SAK", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "SCSI", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>, rakenne: "=jjjj"];
[alku: "sd.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "SDP", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
#[alku: "sf", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
#[alku: "sfst", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "sgml", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>];
[alku: "SI", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jj"];
[alku: "SK", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "SKDL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjjj"];
[alku: "SKL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
#[alku: "skop", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "SKP", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "SKS", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "Smk", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jqq"];
[alku: "SMP", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
#[alku: "sndl", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "snt", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä];
[alku: "so.", luokka: lyhenne, jatko: <loppu>, äs: aä];
#[alku: "soqt", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "SOS", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "sos.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "sos.dem.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "SPR", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "SPSS", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>, rakenne: "=jjjj"];
[alku: "ssh", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>];
#[alku: "st1", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "STL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "STT", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "suom.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "synt.", luokka: lyhenne, jatko: <loppu>, äs: aä];
#[alku: "SYP", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "syysk.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "t", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "tammik.", luokka: lyhenne, jatko: <loppu>, äs: aä];
#[alku: "tel", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "TKK", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "tl", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "tlk", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "tm.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "tmi", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "tms.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "toim.", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "toukok.", luokka: lyhenne, jatko: <loppu>, äs: aä];
#[alku: "tov.", luokka: lyhenne, jatko: <loppu>, äs: aä];
#[alku: "tpsl", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "tri", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "ts.", luokka: lyhenne, jatko: <loppu>, äs: aä];
#[alku: "tsfs", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
#[alku: "tsl", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
#[alku: "tst", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "TUL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "TV", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jj"];
[alku: "tv", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
#[alku: "tvl", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ty", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "u", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "UKK", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jjj"];
[alku: "UMTS", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjjj"];
[alku: "umts", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "USA", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jjj"];
[alku: "USB", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>, rakenne: "=jjj"];
[alku: "UTF", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>, rakenne: "=jjj"];
[alku: "v", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
#[alku: "v3", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "vanh.", luokka: lyhenne, jatko: <loppu>, äs: aä];
#[alku: "virall.", luokka: lyhenne, jatko: <loppu>, äs: aä];
#[alku: "vis5d", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "VR", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jj"];
[alku: "VRK", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "vrk", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
#[alku: "vrml", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "vrt.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "vt", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "VTT", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "w", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "WC", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jj"];
[alku: "wc", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "WHO", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jjj"];
[alku: "WSOY", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjjj"];
[alku: "WTO", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "WWW", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "www", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "wxgtk", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>];
[alku: "wxx11", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>];
[alku: "x", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "xml", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>];
[alku: "xmms", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>];
[alku: "y", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "yht.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "YK", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jj"];
[alku: "YM", luokka: lyhenne, jatko: <loppu>, äs: aä, rakenne: "=jj"];
[alku: "ym.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "ym.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "yms.", luokka: lyhenne, jatko: <loppu>, äs: aä];
[alku: "YTL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "YYA", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "z", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ß", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "à", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "á", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "â", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ã", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ä", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "å", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "æ", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ç", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "è", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "é", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ê", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ë", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ì", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "í", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "î", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ï", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ð", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ñ", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ò", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ó", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ô", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "õ", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ö", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ø", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ù", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ú", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "û", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ü", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ý", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "þ", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "ÿ", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];


#[alku: "", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
