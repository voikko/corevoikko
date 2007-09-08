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
# This file has been modified by the contributors of Voikko project.
# Last change was on $Date: 2006-11-13 13:44:31 +0200 (ma, 13 marras 2006) $ by $Author: tlikonen $.

# Lyhenteitä. (Lyhennesanat [esimerkiksi "Nato"] käsitellään Joukahaisessa)

[alku: "ASCII", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjjjj"];
[alku: "ctrl", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "CVS", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>];
[alku: "ftp", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä]; # file transfer protocol
[alku: "FTP", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä]; # file transfer protocol
[alku: "fvwm", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "gcc", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "glibc", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "GPL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjj"];
[alku: "Gt", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jq"];
[alku: "icewm", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "IDE", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjj"];
[alku: "IMAP", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjjj"];
[alku: "IP", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jj"]; # Internet Protocol
[alku: "IRC", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjj"];
[alku: "jpg", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä];
[alku: "KDE", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjj"];
[alku: "kt", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=qq"];
[alku: "LGPL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjjj"];
[alku: "libstdc", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "MS", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jj"];
[alku: "MS-DOS", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jj-=jjj"];
[alku: "Mt", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: a, rakenne: "=jq"];
[alku: "NNTP", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjjj"];
[alku: "NTFS", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjjj"];
[alku: "NTP", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjj"];
[alku: "OpenBSD", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, tiedot: <atk>];
[alku: "opengl", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä];
[alku: "PCI", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "PDF", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "RFC", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjj"];
[alku: "RPM", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "SCSI", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjjj"];
[alku: "SGML", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjjj"];
[alku: "SPSS", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjjj"]; # SPSS-ohjelma
[alku: "SQL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>];
[alku: "SSH", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjj"];
[alku: "ssh", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "tcl", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>];
[alku: "URL", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>, rakenne: "=jjj"];
[alku: "UTF", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjj"];
[alku: "vis5d", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "VRML", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, rakenne: "=jjjj"];
[alku: "vrml", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "wlan", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä, tiedot: <atk>];
[alku: "wxgtk", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "wxx11", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];
[alku: "XML", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä, rakenne: "=jjj"];
[alku: "xml", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä];
[alku: "xmms", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: ä];


#[alku: "", luokka: lyhenne, jatko: @lyhenteen_jatko, äs: aä];

