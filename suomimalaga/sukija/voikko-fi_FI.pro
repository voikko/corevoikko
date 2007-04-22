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

# Tiedosto suomi.pro Voikkoa varten.


sym: suomi.sym

all: suomi.all suomi.inc suomi.pro voikko-fi_FI.pro

lex: suomi.lex suomi.inc suomi.pro voikko-fi_FI.pro

lex: sanat/erikoisalat/atk.lex
lex: sanat/erikoisalat/laaketiede.lex
lex: sanat/erikoiset.lex
lex: sanat/erikoissanat.lex
lex: sanat/etuliitteet.lex
lex: sanat/jokinen.lex
lex: sanat/joukahainen.lex
lex: sanat/lainen.lex
lex: sanat/latex.lex
lex: sanat/lukusanat.lex
lex: sanat/lyhenteet.lex
lex: sanat/olla-ei.lex
lex: sanat/omat.lex
lex: sanat/poikkeavat.lex
lex: sanat/yhdyssanat.lex
lex: sanat/11-19.lex


mor: suomi.mor suomi.inc

mallex: set transmit-line "./transmit"
malaga: set transmit-line "./transmit"

#mallex: set hidden +jatko
#malaga: set hidden +jatko

malaga: set display-line "malshow"
mallex: set display-line "malshow"

mallex: set use-display yes
malaga: set use-display yes

info: Voikko-Dictionary-Format: 1


# Lipuilla malli ja tulostus voidaan säätää suomi-malagan toimintaa.
# Lippujen arvot on selitetty tiedostossa suomi-sukija.pro.

malaga: set switch tulostus merkitse_yhdyssanat
malaga: set switch malli voikko
mallex: set switch malli voikko
