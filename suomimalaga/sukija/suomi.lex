# Suomi-malaga, suomen kielen muoto-opin kuvaus.
#
# Tekijänoikeus © 2006-2009 Hannu Väisänen (Etunimi.Sukunimi@joensuu.fi)
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


include "suomi.inc";

define @sana1 := <yhdyssana, nimisana, laatusana, nimi_laatusana, tavuviiva, etuliite>;
define @sana2 := @sana1 + <teonsana>;

define @eln := <yhdyssana, tavuviiva, etuliite, nimisana, nimi_laatusana>;
define @ell := <yhdyssana, tavuviiva, etuliite, laatusana, nimi_laatusana>;
define @elt := <yhdyssana, tavuviiva, etuliite, teonsana>;
define @eltj := <yhdyssana, tavuviiva, etuliite, nimisana, nimi_laatusana, laatusana>;

include "voikonsanat/atk.lex";
include "voikonsanat/atk-lyhenteet.lex";
include "voikonsanat/erikoiset.lex";
include "voikonsanat/erikoissanat.lex";
include "voikonsanat/etuliitteet.lex";
include "voikonsanat/huudahdussanat.lex";
include "voikonsanat/joukahainen.lex";
include "voikonsanat/kasvatustiede.lex";
include "voikonsanat/laaketiede.lex";
include "voikonsanat/lainen.lex";
include "voikonsanat/linux-distributions.lex";
include "voikonsanat/lukusanat.lex";
include "voikonsanat/lyhenteet.lex";
include "voikonsanat/matluonnontiede.lex";
include "voikonsanat/poikkeavat.lex";
include "voikonsanat/seikkasanat.lex";
include "voikonsanat/sidesanat.lex";
include "voikonsanat/suhdesanat.lex";
include "voikonsanat/taivutustäydennykset.lex";
include "voikonsanat/yhdyssanat.lex";
include "voikonsanat/vieraskieliset.lex";
#include "sanat/11-19.lex";
####include "sanat/jokinen.lex";
include "sanat/latex.lex";
##include "sanat/luvut.lex";
include "sanat/olla-ei.lex";
include "sanat/omat.lex";

# Karlsson 1983:
# Fred Karlsson: Suomen kielen äänne- ja muotorakenne.
# WSOY, Juva 1983
# ISBN 951-0-11633-5

define @sijan_jatko := <tavuviiva, liitesana, loppu>;
define @sijan_jatko_ol := @sijan_jatko + <omistusliite>;


[alku: "t",    luokka: sijapääte, sija: nimentö_t,
               luku: monikko, äs: aä, jatko: @sijan_jatko];

[alku: "tka",  luokka: sijapääte, sija: nimentö_tkA,
               luku: monikko, äs: a, jatko: @sijan_jatko];
[alku: "tkä",  luokka: sijapääte, sija: nimentö_tkA,
               luku: monikko, äs: ä, jatko: @sijan_jatko];

[alku: "n",    luokka: sijapääte, sija: omanto_n,
               luku: yksikkö, äs: aä,
               jatko: @sijan_jatko + @yhdyssana];

[alku: "nka",  luokka: sijapääte, sija: omanto_nkA,
               luku: yksikkö, äs: a, jatko: @sijan_jatko];
[alku: "nkä",  luokka: sijapääte, sija: omanto_nkA,
               luku: yksikkö, äs: ä, jatko: @sijan_jatko];



[alku: "en",    luokka: sijapääte, sija: omanto_en,
                luku: monikko, äs: aä, jatko: @sijan_jatko + @yhdyssana];
[alku: "e",     luokka: sijapääte, sija: omanto_en,
                luku: monikko, äs: aä, jatko: <omistusliite>];

[alku: "ien",   luokka: sijapääte, sija: omanto_ien,
                luku: monikko, äs: aä, jatko: @sijan_jatko + @yhdyssana];
[alku: "ie",    luokka: sijapääte, sija: omanto_ien,
                luku: monikko, äs: aä, jatko: <omistusliite>];

[alku: "jen",   luokka: sijapääte, sija: omanto_jen,
                luku: monikko, äs: aä, jatko: @sijan_jatko + @yhdyssana];
[alku: "je",    luokka: sijapääte, sija: omanto_jen,
                luku: monikko, äs: aä, jatko: <omistusliite>];

[alku: "in",    luokka: sijapääte, sija: omanto_in,
                luku: monikko, äs: aä, jatko: @sijan_jatko + @yhdyssana];

# Pitäisikö tämä hyväksyä vai ei? Kyllä: Esi-isäinsä.
[alku: "i",     luokka: sijapääte, sija: omanto_in,
                luku: monikko, äs: aä, jatko: <omistusliite>, tiedot: <murre>];

[alku: "ten",   luokka: sijapääte, sija: omanto_ten,
                luku: monikko, äs: aä, jatko: @sijan_jatko + @yhdyssana];
[alku: "te",    luokka: sijapääte, sija: omanto_ten,
                luku: monikko, äs: aä, jatko: <omistusliite>];

[alku: "iden",  luokka: sijapääte, sija: omanto_iT,
                luku: monikko, äs: aä, jatko: @sijan_jatko + @yhdyssana];
[alku: "ide",   luokka: sijapääte, sija: omanto_iT,
                luku: monikko, äs: aä, jatko: <omistusliite>];
[alku: "itten", luokka: sijapääte, sija: omanto_iT,
                luku: monikko, äs: aä, jatko: @sijan_jatko + @yhdyssana];
[alku: "itte",  luokka: sijapääte, sija: omanto_iT,
                luku: monikko, äs: aä, jatko: <omistusliite>];


[alku: "hitten", luokka: sijapääte, sija: omanto_hitten,
                 luku: monikko, äs: aä, jatko: @sijan_jatko + @yhdyssana, tiedot: <ei_voikko>];  # Korkehitten.
[alku: "hitte",  luokka: sijapääte, sija: omanto_hitten,
                 luku: monikko, äs: aä, jatko: <omistusliite>, tiedot: <ei_voikko>];

[alku: "hien", luokka: sijapääte, sija: omanto_hien,
               luku: monikko, äs: aä, jatko: @sijan_jatko + @yhdyssana, tiedot: <ei_voikko>];  # Korkehien.
[alku: "hie",  luokka: sijapääte, sija: omanto_hien,
               luku: monikko, äs: aä, jatko: <omistusliite>, tiedot: <ei_voikko>];


[alku: "idän",  luokka: sijapääte, sija: omanto_idän,
                luku: monikko, äs: ä, jatko: @sijan_jatko];


[alku: "idät",  luokka: sijapääte, sija: kohdanto_idät,
                luku: monikko, äs: ä, jatko: @sijan_jatko];

[alku: "t",     luokka: sijapääte, sija: kohdanto_t,
                luku: monikko, äs: aä, jatko: @sijan_jatko];


[alku: "a",     luokka: sijapääte, sija: osanto_A,
                luku: yksikkö, äs: a, jatko: @sijan_jatko_ol];
[alku: "ä",     luokka: sijapääte, sija: osanto_A,
                luku: yksikkö, äs: ä, jatko: @sijan_jatko_ol];

[alku: "aa",    luokka: sijapääte, sija: osanto_AA,
                luku: yksikkö, äs: a, jatko: @sijan_jatko_ol];
[alku: "ää",    luokka: sijapääte, sija: osanto_AA,
                luku: yksikkö, äs: ä, jatko: @sijan_jatko_ol];

[alku: "ta",    luokka: sijapääte, sija: osanto_tA,
                luku: yksikkö, äs: a, jatko: @sijan_jatko_ol];
[alku: "tä",    luokka: sijapääte, sija: osanto_tA,
                luku: yksikkö, äs: ä, jatko: @sijan_jatko_ol];

[alku: "tta",   luokka: sijapääte, sija: osanto_ttA,
                luku: yksikkö, äs: a, jatko: @sijan_jatko_ol];
[alku: "ttä",   luokka: sijapääte, sija: osanto_ttA,
                luku: yksikkö, äs: ä, jatko: @sijan_jatko_ol];

[alku: "ita",   luokka: sijapääte, sija: osanto_itA,
                luku: monikko, äs: a, jatko: @sijan_jatko_ol];
[alku: "itä",   luokka: sijapääte, sija: osanto_itA,
                luku: monikko, äs: ä, jatko: @sijan_jatko_ol];

[alku: "ia",    luokka: sijapääte, sija: osanto_iA,
                luku: monikko, äs: a, jatko: @sijan_jatko_ol];
[alku: "iä",    luokka: sijapääte, sija: osanto_iA,
                luku: monikko, äs: ä, jatko: @sijan_jatko_ol];

[alku: "ja",    luokka: sijapääte, sija: osanto_jA,
                luku: monikko, äs: a, jatko: @sijan_jatko_ol];
[alku: "jä",    luokka: sijapääte, sija: osanto_jA,
                luku: monikko, äs: ä, jatko: @sijan_jatko_ol];


[alku: "na",    luokka: sijapääte, sija: olento_nA,
                luku: yksikkö, äs: a, jatko: @sijan_jatko_ol];
[alku: "nä",    luokka: sijapääte, sija: olento_nA,
                luku: yksikkö, äs: ä, jatko: @sijan_jatko_ol];

[alku: "ra",    luokka: sijapääte, sija: olento_rA,
                luku: yksikkö, äs: a, jatko: @sijan_jatko_ol, tiedot: <ei_voikko>];
[alku: "rä",    luokka: sijapääte, sija: olento_rA,
                luku: yksikkö, äs: ä, jatko: @sijan_jatko_ol, tiedot: <ei_voikko>];

[alku: "sa",    luokka: sijapääte, sija: olento_sA,
                luku: yksikkö, äs: a, jatko: @sijan_jatko_ol, tiedot: <ei_voikko>];
[alku: "sä",    luokka: sijapääte, sija: olento_sA,
                luku: yksikkö, äs: ä, jatko: @sijan_jatko_ol, tiedot: <ei_voikko>];


[alku: "ina",   luokka: sijapääte, sija: olento_inA,
                luku: monikko, äs: a, jatko: @sijan_jatko_ol];
[alku: "inä",   luokka: sijapääte, sija: olento_inA,
                luku: monikko, äs: ä, jatko: @sijan_jatko_ol];


[alku: "ksi",   luokka: sijapääte, sija: tulento_ksi,
                luku: yksikkö, äs: aä, jatko: @sijan_jatko];
[alku: "kse",   luokka: sijapääte, sija: tulento_ksi,
                luku: yksikkö, äs: aä, jatko: <omistusliite>];
[alku: "ks",    luokka: sijapääte, sija: tulento_ksi,
                luku: yksikkö, äs: aä, jatko: <loppu>, tiedot: <ei_voikko>, perusmuoto: "ksi"];

[alku: "iksi",  luokka: sijapääte, sija: tulento_iksi,
                luku: monikko, äs: aä, jatko: @sijan_jatko];
[alku: "ikse",  luokka: sijapääte, sija: tulento_iksi,
                luku: monikko, äs: aä, jatko: <omistusliite>];
[alku: "iks",   luokka: sijapääte, sija: tulento_iksi,
                luku: monikko, äs: aä, jatko: <loppu>, tiedot: <ei_voikko>, perusmuoto: "iksi"];


[alku: "ssa",   luokka: sijapääte, sija: sisäolento_ssA,
                luku: yksikkö, äs: a, jatko: @sijan_jatko_ol];
[alku: "ssä",   luokka: sijapääte, sija: sisäolento_ssA,
                luku: yksikkö, äs: ä, jatko: @sijan_jatko_ol];
[alku: "ss",    luokka: sijapääte, sija: sisäolento_ssA,
                luku: yksikkö, äs: a, jatko: <loppu>, tiedot: <ei_voikko>, perusmuoto: "ssa"];
[alku: "ss",    luokka: sijapääte, sija: sisäolento_ssA,
                luku: yksikkö, äs: ä, jatko: <loppu>, tiedot: <ei_voikko>, perusmuoto: "ssä"];

[alku: "issa",  luokka: sijapääte, sija: sisäolento_issA,
                luku: monikko, äs: a, jatko: @sijan_jatko_ol];
[alku: "issä",  luokka: sijapääte, sija: sisäolento_issA,
                luku: monikko, äs: ä, jatko: @sijan_jatko_ol];
[alku: "iss",   luokka: sijapääte, sija: sisäolento_issA,
                luku: monikko, äs: a, jatko: <loppu>, tiedot: <ei_voikko>, perusmuoto: "issa"];
[alku: "iss",   luokka: sijapääte, sija: sisäolento_issA,
                luku: monikko, äs: ä, jatko: <loppu>, tiedot: <ei_voikko>, perusmuoto: "issä"];


[alku: "sta",   luokka: sijapääte, sija: sisäeronto_stA,
                luku: yksikkö, äs: a, jatko: @sijan_jatko_ol];
[alku: "stä",   luokka: sijapääte, sija: sisäeronto_stA,
                luku: yksikkö, äs: ä, jatko: @sijan_jatko_ol];
[alku: "st",    luokka: sijapääte, sija: sisäeronto_stA,
                luku: yksikkö, äs: a, jatko: <loppu>, tiedot: <ei_voikko>, perusmuoto: "sta"];
[alku: "st",    luokka: sijapääte, sija: sisäeronto_stA,
                luku: yksikkö, äs: ä, jatko: <loppu>, tiedot: <ei_voikko>, perusmuoto: "stä"];

[alku: "ista",  luokka: sijapääte, sija: sisäeronto_istA,
                luku: monikko, äs: a, jatko: @sijan_jatko_ol];
[alku: "istä",  luokka: sijapääte, sija: sisäeronto_istA,
                luku: monikko, äs: ä, jatko: @sijan_jatko_ol];
[alku: "ist",   luokka: sijapääte, sija: sisäeronto_istA,
                luku: monikko, äs: a, jatko: <loppu>, tiedot: <ei_voikko>, perusmuoto: "ista"];
[alku: "ist",   luokka: sijapääte, sija: sisäeronto_istA,
                luku: monikko, äs: ä, jatko: <loppu>, tiedot: <ei_voikko>, perusmuoto: "istä"];


[alku: "an",   luokka: sijapääte, sija: sisätulento_Vn,
               luku: yksikkö, äs: a, jatko: @sijan_jatko];
[alku: "a",    luokka: sijapääte, sija: sisätulento_Vn,
               luku: yksikkö, äs: a, jatko: <omistusliite>];
[alku: "en",   luokka: sijapääte, sija: sisätulento_Vn,
               luku: yksikkö, äs: aä, jatko: @sijan_jatko];
[alku: "e",    luokka: sijapääte, sija: sisätulento_Vn,
               luku: yksikkö, äs: aä, jatko: <omistusliite>];
[alku: "in",   luokka: sijapääte, sija: sisätulento_Vn,
               luku: yksikkö, äs: aä, jatko: @sijan_jatko];
[alku: "i",    luokka: sijapääte, sija: sisätulento_Vn,
               luku: yksikkö, äs: aä, jatko: <omistusliite>];
[alku: "on",   luokka: sijapääte, sija: sisätulento_Vn,
               luku: yksikkö, äs: a, jatko: @sijan_jatko];
[alku: "o",    luokka: sijapääte, sija: sisätulento_Vn,
               luku: yksikkö, äs: a, jatko: <omistusliite>];
[alku: "un",   luokka: sijapääte, sija: sisätulento_Vn,
               luku: yksikkö, äs: a, jatko: @sijan_jatko];
[alku: "u",    luokka: sijapääte, sija: sisätulento_Vn,
               luku: yksikkö, äs: a, jatko: <omistusliite>];
[alku: "yn",   luokka: sijapääte, sija: sisätulento_Vn,
               luku: yksikkö, äs: ä, jatko: @sijan_jatko];
[alku: "y",    luokka: sijapääte, sija: sisätulento_Vn,
               luku: yksikkö, äs: ä, jatko: <omistusliite>];
[alku: "än",   luokka: sijapääte, sija: sisätulento_Vn,
               luku: yksikkö, äs: ä, jatko: @sijan_jatko];
[alku: "ä",    luokka: sijapääte, sija: sisätulento_Vn,
               luku: yksikkö, äs: ä, jatko: <omistusliite>];
[alku: "ön",   luokka: sijapääte, sija: sisätulento_Vn,
               luku: yksikkö, äs: ä, jatko: @sijan_jatko];
[alku: "ö",    luokka: sijapääte, sija: sisätulento_Vn,
               luku: yksikkö, äs: ä, jatko: <omistusliite>];

[alku: "aan",   luokka: sijapääte, sija: sisätulento_VVn,
                luku: yksikkö, äs: a, jatko: @sijan_jatko];
[alku: "een",   luokka: sijapääte, sija: sisätulento_VVn,
                luku: yksikkö, äs: aä, jatko: @sijan_jatko];
[alku: "iin",   luokka: sijapääte, sija: sisätulento_VVn,
                luku: yksikkö, äs: aä, jatko: @sijan_jatko];
[alku: "oon",   luokka: sijapääte, sija: sisätulento_VVn,
                luku: yksikkö, äs: a, jatko: @sijan_jatko];
[alku: "uun",   luokka: sijapääte, sija: sisätulento_VVn,
                luku: yksikkö, äs: a, jatko: @sijan_jatko];
[alku: "yyn",   luokka: sijapääte, sija: sisätulento_VVn,
                luku: yksikkö, äs: ä, jatko: @sijan_jatko];
[alku: "ään",   luokka: sijapääte, sija: sisätulento_VVn,
                luku: yksikkö, äs: ä, jatko: @sijan_jatko];
[alku: "öön",   luokka: sijapääte, sija: sisätulento_VVn,
                luku: yksikkö, äs: ä, jatko: @sijan_jatko];


[alku: "han",   luokka: sijapääte, sija: sisätulento_hVn,
                luku: yksikkö, äs: a, jatko: @sijan_jatko + @yhdyssana];
[alku: "ha",    luokka: sijapääte, sija: sisätulento_hVn,
                luku: yksikkö, äs: a, jatko: <omistusliite>];
[alku: "hen",   luokka: sijapääte, sija: sisätulento_hVn,
                luku: yksikkö, äs: aä, jatko: @sijan_jatko + @yhdyssana];
[alku: "he",    luokka: sijapääte, sija: sisätulento_hVn,
                luku: yksikkö, äs: aä, jatko: <omistusliite>];
[alku: "hin",   luokka: sijapääte, sija: sisätulento_hVn,
                luku: yksikkö, äs: aä, jatko: @sijan_jatko + @yhdyssana];
[alku: "hi",    luokka: sijapääte, sija: sisätulento_hVn,
                luku: yksikkö, äs: aä, jatko: <omistusliite>];
[alku: "hon",   luokka: sijapääte, sija: sisätulento_hVn,
                luku: yksikkö, äs: a, jatko: @sijan_jatko + @yhdyssana];
[alku: "ho",    luokka: sijapääte, sija: sisätulento_hVn,
                luku: yksikkö, äs: a, jatko: <omistusliite>];
[alku: "hun",   luokka: sijapääte, sija: sisätulento_hVn,
                luku: yksikkö, äs: a, jatko: @sijan_jatko + @yhdyssana];
[alku: "hu",    luokka: sijapääte, sija: sisätulento_hVn,
                luku: yksikkö, äs: a, jatko: <omistusliite>];
[alku: "hyn",   luokka: sijapääte, sija: sisätulento_hVn,
                luku: yksikkö, äs: ä, jatko: @sijan_jatko + @yhdyssana];
[alku: "hy",    luokka: sijapääte, sija: sisätulento_hVn,
                luku: yksikkö, äs: ä, jatko: <omistusliite>];
[alku: "hän",   luokka: sijapääte, sija: sisätulento_hVn,
                luku: yksikkö, äs: ä, jatko: @sijan_jatko + @yhdyssana];
[alku: "hä",    luokka: sijapääte, sija: sisätulento_hVn,
                luku: yksikkö, äs: ä, jatko: <omistusliite>];
[alku: "hön",   luokka: sijapääte, sija: sisätulento_hVn,
                luku: yksikkö, äs: ä, jatko: @sijan_jatko + @yhdyssana];
[alku: "hö",    luokka: sijapääte, sija: sisätulento_hVn,
                luku: yksikkö, äs: ä, jatko: <omistusliite>];

[alku: "iin",  luokka: sijapääte, sija: sisätulento_iin,
               luku: monikko, äs: aä, jatko: @sijan_jatko];
[alku: "ii",   luokka: sijapääte, sija: sisätulento_iin,
               luku: monikko, äs: aä, jatko: <omistusliite>];

[alku: "ihin",  luokka: sijapääte, sija: sisätulento_ihin,
                luku: monikko, äs: aä, jatko: @sijan_jatko];
[alku: "ihi",   luokka: sijapääte, sija: sisätulento_ihin,
                luku: monikko, äs: aä, jatko: <omistusliite>];

[alku: "seen",  luokka: sijapääte, sija: sisätulento_seen,
                luku: yksikkö, äs: aä, jatko: @sijan_jatko];
[alku: "see",   luokka: sijapääte, sija: sisätulento_seen,
                luku: yksikkö, äs: aä, jatko: <omistusliite>];

[alku: "sehen",  luokka: sijapääte, sija: sisätulento_seen,
                 luku: yksikkö, äs: aä, jatko: @sijan_jatko, tiedot: <ei_voikko>];
[alku: "sehe",   luokka: sijapääte, sija: sisätulento_seen,
                 luku: yksikkö, äs: aä, jatko: <omistusliite>, tiedot: <ei_voikko>];

[alku: "isiin", luokka: sijapääte, sija: sisätulento_isiin,
                luku: monikko, äs: aä, jatko: @sijan_jatko];
[alku: "isii",  luokka: sijapääte, sija: sisätulento_isiin,
                luku: monikko, äs: aä, jatko: <omistusliite>];

[alku: "sen",   luokka: sijapääte, sija: sisätulento_sen,
                luku: yksikkö, äs: aä, jatko: @sijan_jatko];
[alku: "se",    luokka: sijapääte, sija: sisätulento_sen,
                luku: yksikkö, äs: aä, jatko: <omistusliite>];

[alku: "isin",  luokka: sijapääte, sija: sisätulento_isin,
                luku: monikko, äs: aä, jatko: @sijan_jatko];
[alku: "isi",   luokka: sijapääte, sija: sisätulento_isin,
                luku: monikko, äs: aä, jatko: <omistusliite>];

[alku: "nne",   luokka: sijapääte, sija: sisätulento_nne,
                luku: yksikkö, äs: aä, jatko: <liitesana, loppu>];

[alku: "lla",   luokka: sijapääte, sija: ulko_olento_llA,
                luku: yksikkö, äs: a, jatko: @sijan_jatko_ol];
[alku: "llä",   luokka: sijapääte, sija: ulko_olento_llA,
                luku: yksikkö, äs: ä, jatko: @sijan_jatko_ol];

[alku: "illa",  luokka: sijapääte, sija: ulko_olento_illA,
                luku: monikko, äs: a, jatko: @sijan_jatko_ol];
[alku: "illä",  luokka: sijapääte, sija: ulko_olento_illA,
                luku: monikko, äs: ä, jatko: @sijan_jatko_ol];


[alku: "lta",   luokka: sijapääte, sija: ulkoeronto_ltA,
                luku: yksikkö, äs: a, jatko: @sijan_jatko_ol];
[alku: "ltä",   luokka: sijapääte, sija: ulkoeronto_ltA,
                luku: yksikkö, äs: ä, jatko: @sijan_jatko_ol];
[alku: "lt",    luokka: sijapääte, sija: ulkoeronto_ltA,
                luku: yksikkö, äs: a, jatko: <loppu>, tiedot: <ei_voikko>, perusmuoto: "lta"];
[alku: "lt",    luokka: sijapääte, sija: ulkoeronto_ltA,
                luku: yksikkö, äs: ä, jatko: <loppu>, tiedot: <ei_voikko>, perusmuoto: "lta"];

[alku: "ilta",  luokka: sijapääte, sija: ulkoeronto_iltA,
                luku: monikko, äs: a, jatko: @sijan_jatko_ol];
[alku: "iltä",  luokka: sijapääte, sija: ulkoeronto_iltA,
                luku: monikko, äs: ä, jatko: @sijan_jatko_ol];
[alku: "ilt",   luokka: sijapääte, sija: ulkoeronto_iltA,
                luku: monikko, äs: a, jatko: <loppu>, tiedot: <ei_voikko>, perusmuoto: "ilta"];
[alku: "ilt",   luokka: sijapääte, sija: ulkoeronto_iltA,
                luku: monikko, äs: ä, jatko: <loppu>, tiedot: <ei_voikko>, perusmuoto: "lta"];


[alku: "lle",   luokka: sijapääte, sija: ulkotulento_lle,
                luku: yksikkö, äs: aä, jatko: @sijan_jatko_ol];

[alku: "ille",  luokka: sijapääte, sija: ulkotulento_ille,
                luku: monikko, äs: aä, jatko: @sijan_jatko_ol];

[alku: "llen",  luokka: sijapääte, sija: ulkotulento_lle,                           # Lehmä+llen.
                luku: yksikkö, äs: aä, jatko: <liitesana, loppu>, tiedot: <murre>, perusmuoto: "lle"];

[alku: "illen", luokka: sijapääte, sija: ulkotulento_ille,
                luku: monikko, äs: aä, jatko: <liitesana, loppu>, tiedot: <murre>, perusmuoto: "ille"];

[alku: "ll",    luokka: sijapääte, sija: ulkotulento_lle,    # Perusmuoto voisi olla myös llA.
                luku: yksikkö, äs: aä, jatko: <liitesana, loppu>, tiedot: <murre>, perusmuoto: "lle"];

[alku: "ill",   luokka: sijapääte, sija: ulkotulento_ille,   # Perusmuoto voisi olla myös illA.
                luku: monikko, äs: aä, jatko: <liitesana, loppu>, tiedot: <murre>, perusmuoto: "ille"];


[alku: "tta",   luokka: sijapääte, sija: vajanto_ttA,
                luku: yksikkö, äs: a, jatko: @sijan_jatko_ol];
[alku: "ttä",   luokka: sijapääte, sija: vajanto_ttA,
                luku: yksikkö, äs: ä, jatko: @sijan_jatko_ol];

[alku: "itta",  luokka: sijapääte, sija: vajanto_ittA,
                luku: monikko, äs: a, jatko: @sijan_jatko_ol];
[alku: "ittä",  luokka: sijapääte, sija: vajanto_ittA,
                luku: monikko, äs: ä, jatko: @sijan_jatko_ol];


[alku: "ine",   luokka: sijapääte, sija: seuranto_ine,
                luku: monikko, äs: aä, jatko: <omistusliite, loppu>];

[alku: "n",     luokka: sijapääte, sija: keinonto_n,
                luku: yksikkö, äs: aä, jatko: @sijan_jatko];
[alku: "in",    luokka: sijapääte, sija: keinonto_in,
                luku: monikko, äs: aä, jatko: @sijan_jatko];

[alku: "sti",   luokka: sijapääte, sija: kerronto_sti,
                luku: yksikkö, äs: aä, jatko: <liitesana, loppu>];

[alku: "s",     luokka: sijapääte, sija: tulento_s,
                luku: yksikkö, äs: aä, jatko: <liitesana, loppu>];


[alku: "ni",  luokka: omistusliite, äs: aä, tekijä: 1, jatko: <liitesana, loppu>];                  # Matkalle+ni.
[alku: "in",  luokka: omistusliite, äs: aä, tekijä: 1, jatko: <liitesana, loppu>, tiedot: <murre>]; # Matkalle+in.
[alku: "si",  luokka: omistusliite, äs: aä, tekijä: 2, jatko: <liitesana, loppu>];
[alku: "s",   luokka: omistusliite, äs: aä, tekijä: 2, jatko: <liitesana, loppu>, tiedot: <murre>];
[alku: "nsa", luokka: omistusliite, äs: a,  tekijä: 3, jatko: <liitesana, loppu>];
[alku: "nsä", luokka: omistusliite, äs: ä,  tekijä: 3, jatko: <liitesana, loppu>];
[alku: "ns",  luokka: omistusliite, äs: aä, tekijä: 3, jatko: <loppu>, tiedot: <murre>];
[alku: "an",  luokka: omistusliite, äs: a,  tekijä: 3, jatko: <liitesana, loppu>];
[alku: "en",  luokka: omistusliite, äs: aä, tekijä: 3, jatko: <liitesana, loppu>];
[alku: "hen", luokka: omistusliite, äs: aä, tekijä: 3, jatko: <liitesana, loppu>, tiedot: <murre>]; # Esim. verkoille(h)en.
[alku: "in",  luokka: omistusliite, äs: aä, tekijä: 3, jatko: <liitesana, loppu>];
[alku: "on",  luokka: omistusliite, äs: a,  tekijä: 3, jatko: <liitesana, loppu>];
[alku: "un",  luokka: omistusliite, äs: a,  tekijä: 3, jatko: <liitesana, loppu>];
[alku: "yn",  luokka: omistusliite, äs: ä,  tekijä: 3, jatko: <liitesana, loppu>];
[alku: "än",  luokka: omistusliite, äs: ä,  tekijä: 3, jatko: <liitesana, loppu>];
[alku: "ön",  luokka: omistusliite, äs: ä,  tekijä: 3, jatko: <liitesana, loppu>];
[alku: "mme", luokka: omistusliite, äs: aä, tekijä: 1, jatko: <liitesana, loppu>];
[alku: "nne", luokka: omistusliite, äs: aä, tekijä: 1, jatko: <liitesana, loppu>];


# Karlsson 1983, s. 234,
#
[alku: "kin",       luokka: liitesana, äs: aä, jatko: <loppu>];

[alku: "kaan",      luokka: liitesana, äs: a, jatko: <loppu>];
[alku: "kään",      luokka: liitesana, äs: ä, jatko: <loppu>];
[alku: "kaanhan",   luokka: liitesana, äs: a, jatko: <loppu>];
[alku: "käänhän",   luokka: liitesana, äs: ä, jatko: <loppu>];
[alku: "ko",        luokka: liitesana, äs: a, jatko: <loppu>];
[alku: "kö",        luokka: liitesana, äs: ä, jatko: <loppu>];
[alku: "kokaan",    luokka: liitesana, äs: a, jatko: <loppu>];
[alku: "kökään",    luokka: liitesana, äs: ä, jatko: <loppu>];
[alku: "pa",        luokka: liitesana, äs: a, jatko: <loppu>];
[alku: "pä",        luokka: liitesana, äs: ä, jatko: <loppu>];
[alku: "han",       luokka: liitesana, äs: a, jatko: <loppu>];
[alku: "hän",       luokka: liitesana, äs: ä, jatko: <loppu>];
[alku: "kohan",     luokka: liitesana, äs: a, jatko: <loppu>];
[alku: "köhän",     luokka: liitesana, äs: ä, jatko: <loppu>];
[alku: "pahan",     luokka: liitesana, äs: a, jatko: <loppu>];
[alku: "pähän",     luokka: liitesana, äs: ä, jatko: <loppu>];
[alku: "kos",       luokka: liitesana, äs: a, jatko: <loppu>];
[alku: "kös",       luokka: liitesana, äs: ä, jatko: <loppu>];
[alku: "pas",       luokka: liitesana, äs: a, jatko: <loppu>];
[alku: "päs",       luokka: liitesana, äs: ä, jatko: <loppu>];
[alku: "kinko",     luokka: liitesana, äs: a, jatko: <loppu>];
[alku: "kinkö",     luokka: liitesana, äs: ä, jatko: <loppu>];
[alku: "kaanko",    luokka: liitesana, äs: a, jatko: <loppu>];
[alku: "käänkö",    luokka: liitesana, äs: ä, jatko: <loppu>];
[alku: "kinhan",    luokka: liitesana, äs: a, jatko: <loppu>];
[alku: "kinhän",    luokka: liitesana, äs: ä, jatko: <loppu>];
[alku: "kinkohan",  luokka: liitesana, äs: a, jatko: <loppu>];
[alku: "kinköhän",  luokka: liitesana, äs: ä, jatko: <loppu>];
[alku: "kinkos",    luokka: liitesana, äs: a, jatko: <loppu>];
[alku: "kinkös",    luokka: liitesana, äs: ä, jatko: <loppu>];
[alku: "kaankohan", luokka: liitesana, äs: a, jatko: <loppu>];
[alku: "käänköhän", luokka: liitesana, äs: ä, jatko: <loppu>];
[alku: "kaankos",   luokka: liitesana, äs: a, jatko: <loppu>];
[alku: "käänkös",   luokka: liitesana, äs: ä, jatko: <loppu>];


[alku: "kkaan",     luokka: liitesana2, äs: a, jatko: <loppu>, tiedot: <murre>]; # Murteissa.
[alku: "kkään",     luokka: liitesana2, äs: ä, jatko: <loppu>, tiedot: <murre>];
[alku: "ppa",       luokka: liitesana2, äs: a, jatko: <loppu>, tiedot: <murre>];
[alku: "ppä",       luokka: liitesana2, äs: ä, jatko: <loppu>, tiedot: <murre>];
[alku: "ppas",      luokka: liitesana2, äs: a, jatko: <loppu>, tiedot: <murre>];
[alku: "ppäs",      luokka: liitesana2, äs: ä, jatko: <loppu>, tiedot: <murre>];

[alku: "s",         luokka: liitesana_s, äs: aä, jatko: <loppu>, tiedot: <murre>];


# Teonsanojen tositavan kestämän tekijäpäätteet.
#
[alku: "n",   luokka: kestämän_tekijäpääte_y1, luku: yksikkö, tekijä: 1,
              tapaluokka: tositapa, aikamuoto: kestämä, äs: aä, jatko: <liitesana, loppu>];
[alku: "t",   luokka: kestämän_tekijäpääte_y2, luku: yksikkö, tekijä: 2,
              tapaluokka: tositapa, aikamuoto: kestämä, äs: aä, jatko: <liitesana, loppu>];


define @jatko_y3 := <liitesana, loppu>;

[alku: "a",   luokka: kestämän_tekijäpääte_y3, luku: yksikkö, tekijä: 3,
              tapaluokka: tositapa, aikamuoto: kestämä, äs: a, jatko: @jatko_y3];
[alku: "e",   luokka: kestämän_tekijäpääte_y3, luku: yksikkö, tekijä: 3,
              tapaluokka: tositapa, aikamuoto: kestämä, äs: aä, jatko: @jatko_y3];
[alku: "i",   luokka: kestämän_tekijäpääte_y3, luku: yksikkö, tekijä: 3,
              tapaluokka: tositapa, aikamuoto: kestämä, äs: aä, jatko: @jatko_y3];
[alku: "o",   luokka: kestämän_tekijäpääte_y3, luku: yksikkö, tekijä: 3,
              tapaluokka: tositapa, aikamuoto: kestämä, äs: a, jatko: @jatko_y3];
[alku: "u",   luokka: kestämän_tekijäpääte_y3, luku: yksikkö, tekijä: 3,
              tapaluokka: tositapa, aikamuoto: kestämä, äs: a, jatko: @jatko_y3];
[alku: "y",   luokka: kestämän_tekijäpääte_y3, luku: yksikkö, tekijä: 3,
              tapaluokka: tositapa, aikamuoto: kestämä, äs: ä, jatko: @jatko_y3];
[alku: "ä",   luokka: kestämän_tekijäpääte_y3, luku: yksikkö, tekijä: 3,
              tapaluokka: tositapa, aikamuoto: kestämä, äs: ä, jatko: @jatko_y3];
[alku: "ö",   luokka: kestämän_tekijäpääte_y3, luku: yksikkö, tekijä: 3,
              tapaluokka: tositapa, aikamuoto: kestämä, äs: ä, jatko: @jatko_y3];

# Murteellisia tai vanhoja muotoja.
[alku: "api", luokka: kestämän_tekijäpääte_y3_Vpi, luku: yksikkö, tekijä: 3,
              tapaluokka: tositapa, aikamuoto: kestämä, äs: a, jatko: @jatko_y3,
              tiedot: <murre>];
[alku: "epi", luokka: kestämän_tekijäpääte_y3_Vpi, luku: yksikkö, tekijä: 3,
              tapaluokka: tositapa, aikamuoto: kestämä, äs: aä, jatko: @jatko_y3,
              tiedot: <murre>];
[alku: "ipi", luokka: kestämän_tekijäpääte_y3_Vpi, luku: yksikkö, tekijä: 3,
              tapaluokka: tositapa, aikamuoto: kestämä, äs: aä, jatko: @jatko_y3,
              tiedot: <murre>];
[alku: "opi", luokka: kestämän_tekijäpääte_y3_Vpi, luku: yksikkö, tekijä: 3,
              tapaluokka: tositapa, aikamuoto: kestämä, äs: a, jatko: @jatko_y3,
              tiedot: <murre>];
[alku: "upi", luokka: kestämän_tekijäpääte_y3_Vpi, luku: yksikkö, tekijä: 3,
              tapaluokka: tositapa, aikamuoto: kestämä, äs: a, jatko: @jatko_y3,
              tiedot: <murre>];
[alku: "ypi", luokka: kestämän_tekijäpääte_y3_Vpi, luku: yksikkö, tekijä: 3,
              tapaluokka: tositapa, aikamuoto: kestämä, äs: ä, jatko: @jatko_y3,
              tiedot: <murre>];
[alku: "äpi", luokka: kestämän_tekijäpääte_y3_Vpi, luku: yksikkö, tekijä: 3,
              tapaluokka: tositapa, aikamuoto: kestämä, äs: ä, jatko: @jatko_y3,
              tiedot: <murre>];
[alku: "öpi", luokka: kestämän_tekijäpääte_y3_Vpi, luku: yksikkö, tekijä: 3,
              tapaluokka: tositapa, aikamuoto: kestämä, äs: ä, jatko: @jatko_y3,
              tiedot: <murre>];

[alku: "avi", luokka: kestämän_tekijäpääte_y3_Vvi, luku: yksikkö, tekijä: 3,
              tapaluokka: tositapa, aikamuoto: kestämä, äs: a, jatko: @jatko_y3,
              tiedot: <murre>];
[alku: "evi", luokka: kestämän_tekijäpääte_y3_Vvi, luku: yksikkö, tekijä: 3,
              tapaluokka: tositapa, aikamuoto: kestämä, äs: aä, jatko: @jatko_y3,
              tiedot: <murre>];
[alku: "ivi", luokka: kestämän_tekijäpääte_y3_Vvi, luku: yksikkö, tekijä: 3,
              tapaluokka: tositapa, aikamuoto: kestämä, äs: aä, jatko: @jatko_y3,
              tiedot: <murre>];
[alku: "ovi", luokka: kestämän_tekijäpääte_y3_Vvi, luku: yksikkö, tekijä: 3,
              tapaluokka: tositapa, aikamuoto: kestämä, äs: a, jatko: @jatko_y3,
              tiedot: <murre>];
[alku: "uvi", luokka: kestämän_tekijäpääte_y3_Vvi, luku: yksikkö, tekijä: 3,
              tapaluokka: tositapa, aikamuoto: kestämä, äs: a, jatko: @jatko_y3,
              tiedot: <murre>];
[alku: "yvi", luokka: kestämän_tekijäpääte_y3_Vvi, luku: yksikkö, tekijä: 3,
              tapaluokka: tositapa, aikamuoto: kestämä, äs: ä, jatko: @jatko_y3,
              tiedot: <murre>];
[alku: "ävi", luokka: kestämän_tekijäpääte_y3_Vvi, luku: yksikkö, tekijä: 3,
              tapaluokka: tositapa, aikamuoto: kestämä, äs: ä, jatko: @jatko_y3,
              tiedot: <murre>];
[alku: "övi", luokka: kestämän_tekijäpääte_y3_Vvi, luku: yksikkö, tekijä: 3,
              tapaluokka: tositapa, aikamuoto: kestämä, äs: ä, jatko: @jatko_y3,
              tiedot: <murre>];

[alku: "pi", luokka: kestämän_tekijäpääte_y3_pi, luku: yksikkö, tekijä: 3,
             tapaluokka: tositapa, aikamuoto: kestämä, äs: aä, jatko: @jatko_y3,
             tiedot: <murre>];
[alku: "vi", luokka: kestämän_tekijäpääte_y3_vi, luku: yksikkö, tekijä: 3,
             tapaluokka: tositapa, aikamuoto: kestämä, äs: aä, jatko: <liitesana, loppu>,
             tiedot: <murre>];


[alku: "mme", luokka: kestämän_tekijäpääte_m1, luku: monikko, tekijä: 1, äs: aä,
              tapaluokka: tositapa, aikamuoto: kestämä, jatko: <liitesana, loppu>];
[alku: "mma", luokka: kestämän_tekijäpääte_m1, luku: monikko, tekijä: 1, äs: a,
              tapaluokka: tositapa, aikamuoto: kestämä, jatko: <liitesana, loppu>, tiedot: <murre>];
[alku: "mmä", luokka: kestämän_tekijäpääte_m1, luku: monikko, tekijä: 1, äs: ä,
              tapaluokka: tositapa, aikamuoto: kestämä, jatko: <liitesana, loppu>, tiedot: <murre>];
[alku: "tte", luokka: kestämän_tekijäpääte_m2, luku: monikko, tekijä: 2, äs: aä,
              tapaluokka: tositapa, aikamuoto: kestämä, jatko: <liitesana, loppu>];
[alku: "vat", luokka: kestämän_tekijäpääte_m3, luku: monikko, tekijä: 3, äs: a,
              tapaluokka: tositapa, aikamuoto: kestämä, jatko: <liitesana, loppu>];
[alku: "vät", luokka: kestämän_tekijäpääte_m3, luku: monikko, tekijä: 3, äs: ä,
              tapaluokka: tositapa, aikamuoto: kestämä, jatko: <liitesana, loppu>];


# Teonsanojen tositavan kertoman tunnus (i) ja tekijäpääte:
# sano+in, sano+it, sano+i, sano+imme, sano+itte, sano+ivat.
#
# Yhdistin kertoman tunnuksen ja tekijäpäätteen astevaihtelun vuoksi:
# asetu+in, asetu+it, asettu+i, asetu+imme, asetu+itte, asettu+ivat.
#
[alku: "in",   luokka: kertoman_tekijäpääte_y1, luku: yksikkö, tekijä: 1, äs: aä,
               tapaluokka: tositapa, aikamuoto: kertoma, jatko: <liitesana, loppu>];
[alku: "it",   luokka: kertoman_tekijäpääte_y2, luku: yksikkö, tekijä: 2, äs: aä,
               tapaluokka: tositapa, aikamuoto: kertoma, jatko: <liitesana, loppu>];
[alku: "i",    luokka: kertoman_tekijäpääte_y3, luku: yksikkö, tekijä: 3, äs: aä,
               tapaluokka: tositapa, aikamuoto: kertoma, jatko: <liitesana, loppu>];

# Oikeastaan itsekohtainen taivutus "opettelihe".
[alku: "ihe",  luokka: kertoman_tekijäpääte_y3, luku: yksikkö, tekijä: 3, äs: aä,
               tapaluokka: tositapa, aikamuoto: kertoma, jatko: <liitesana, loppu>, tiedot: <murre>];

[alku: "imme", luokka: kertoman_tekijäpääte_m1, luku: monikko, tekijä: 1, äs: aä,
               tapaluokka: tositapa, aikamuoto: kertoma, jatko: <liitesana, loppu>];
[alku: "itte", luokka: kertoman_tekijäpääte_m2, luku: monikko, tekijä: 2, äs: aä,
               tapaluokka: tositapa, aikamuoto: kertoma, jatko: <liitesana, loppu>];
[alku: "ivat", luokka: kertoman_tekijäpääte_m3, luku: monikko, tekijä: 3, äs: a,
               tapaluokka: tositapa, aikamuoto: kertoma, jatko: <liitesana, loppu>];
[alku: "ivät", luokka: kertoman_tekijäpääte_m3, luku: monikko, tekijä: 3, äs: ä,
               tapaluokka: tositapa, aikamuoto: kertoma, jatko: <liitesana, loppu>];


[alku: "da", luokka: tositavan_kestämä_dAAn_kielto, tekijä: 4, äs: a,  # Ei voi+da.
             jatko: <liitesana, loppu>,
             tapaluokka: tositapa, aikamuoto: kestämä];
[alku: "dä", luokka: tositavan_kestämä_dAAn_kielto, tekijä: 4, äs: ä,
             jatko: <liitesana, loppu>,
             tapaluokka: tositapa, aikamuoto: kestämä];

[alku: "daan", luokka: tositavan_kestämä_dAAn, tekijä: 4, äs: a,  # Voi+daan.
               jatko: <liitesana, loppu>,
               tapaluokka: tositapa, aikamuoto: kestämä];
[alku: "dään", luokka: tositavan_kestämä_dAAn, tekijä: 4, äs: ä,
               jatko: <liitesana, loppu>,
               tapaluokka: tositapa, aikamuoto: kestämä];

[alku: "la",   luokka: tositavan_kestämä_lAAn_kielto, tekijä: 4, äs: a,
               jatko: <liitesana, loppu>,
               tapaluokka: tositapa, aikamuoto: kestämä];
[alku: "lä",   luokka: tositavan_kestämä_lAAn_kielto, tekijä: 4, äs: ä,
               jatko: <liitesana, loppu>,
               tapaluokka: tositapa, aikamuoto: kestämä];

[alku: "laan", luokka: tositavan_kestämä_lAAn, tekijä: 4, äs: a,
               jatko: <liitesana, loppu>,
               tapaluokka: tositapa, aikamuoto: kestämä];
[alku: "lään", luokka: tositavan_kestämä_lAAn, tekijä: 4, äs: ä,
               jatko: <liitesana, loppu>,
               tapaluokka: tositapa, aikamuoto: kestämä];

[alku: "na",   luokka: tositavan_kestämä_nAAn_kielto, tekijä: 4, äs: a,
               jatko: <liitesana, loppu>,
               tapaluokka: tositapa, aikamuoto: kestämä];
[alku: "nä",   luokka: tositavan_kestämä_nAAn_kielto, tekijä: 4, äs: ä,
               jatko: <liitesana, loppu>,
               tapaluokka: tositapa, aikamuoto: kestämä];

[alku: "naan", luokka: tositavan_kestämä_nAAn, tekijä: 4, äs: a,
               jatko: <liitesana, loppu>,
               tapaluokka: tositapa, aikamuoto: kestämä];
[alku: "nään", luokka: tositavan_kestämä_nAAn, tekijä: 4, äs: ä,
               jatko: <liitesana, loppu>,
               tapaluokka: tositapa, aikamuoto: kestämä];

[alku: "ra",   luokka: tositavan_kestämä_rAAn_kielto, tekijä: 4, äs: a,
               jatko: <liitesana, loppu>,
               tapaluokka: tositapa, aikamuoto: kestämä];
[alku: "rä",   luokka: tositavan_kestämä_rAAn_kielto, tekijä: 4, äs: ä,
               jatko: <liitesana, loppu>,
               tapaluokka: tositapa, aikamuoto: kestämä];

[alku: "raan", luokka: tositavan_kestämä_rAAn, tekijä: 4, äs: a,
               jatko: <liitesana, loppu>,
               tapaluokka: tositapa, aikamuoto: kestämä];
[alku: "rään", luokka: tositavan_kestämä_rAAn, tekijä: 4, äs: ä,
               jatko: <liitesana, loppu>,
               tapaluokka: tositapa, aikamuoto: kestämä];

[alku: "ta",   luokka: tositavan_kestämä_tAAn_kielto, tekijä: 4, äs: a,
               jatko: <liitesana, loppu>,
               tapaluokka: tositapa, aikamuoto: kestämä];
[alku: "tä",   luokka: tositavan_kestämä_tAAn_kielto, tekijä: 4, äs: ä,
               jatko: <liitesana, loppu>,
               tapaluokka: tositapa, aikamuoto: kestämä];

[alku: "taan", luokka: tositavan_kestämä_tAAn, tekijä: 4, äs: a,
               jatko: <liitesana, loppu>,
               tapaluokka: tositapa, aikamuoto: kestämä];
[alku: "tään", luokka: tositavan_kestämä_tAAn, tekijä: 4, äs: ä,
               jatko: <liitesana, loppu>,
               tapaluokka: tositapa, aikamuoto: kestämä];


[alku: "ttiin", luokka: tositavan_kertoma_ttiin, tekijä: 4, äs: aä,
                jatko: <liitesana, loppu>,
                tapaluokka: tositapa, aikamuoto: kertoma];

[alku: "tiin", luokka: tositavan_kertoma_tiin, tekijä: 4, äs: aä,
               jatko: <liitesana, loppu>,
               tapaluokka: tositapa, aikamuoto: kertoma];


# Vanhoja h:llisia muotoja.

[alku: "tahan", luokka: tositavan_kestämä_tAAn, tekijä: 4, äs: a,
                jatko: <liitesana, loppu>,
                tapaluokka: tositapa, aikamuoto: kestämä, tiedot: <vanha_h>];
[alku: "tähän", luokka: tositavan_kestämä_tAAn, tekijä: 4, äs: ä,
                jatko: <liitesana, loppu>,
                tapaluokka: tositapa, aikamuoto: kestämä, tiedot: <vanha_h>];

[alku: "ttihin", luokka: tositavan_kertoma_ttiin, tekijä: 4, äs: aä,
                 jatko: <liitesana, loppu>,
                 tapaluokka: tositapa, aikamuoto: kertoma, tiedot: <vanha_h>];

[alku: "tihin", luokka: tositavan_kertoma_tiin, tekijä: 4, äs: aä,
                jatko: <liitesana, loppu>,
                tapaluokka: tositapa, aikamuoto: kertoma, tiedot: <vanha_h>];


# Ehtotapa. Olen yhdistänyt ehtotavan tunnuksen (isi) ja tekijäpäätteen.
#
[alku: "isin",   luokka: ehtotapa, tapaluokka: ehtotapa, aikamuoto: kestämä,
                 äs: aä, luku: yksikkö, tekijä: 1, jatko: <liitesana, loppu>];
[alku: "isit",   luokka: ehtotapa, tapaluokka: ehtotapa, aikamuoto: kestämä,
                 äs: aä, luku: yksikkö, tekijä: 2, jatko: <liitesana, loppu>];
[alku: "isi",    luokka: ehtotapa, tapaluokka: ehtotapa, aikamuoto: kestämä,
                 äs: aä, luku: yksikkö, tekijä: 3, jatko: <liitesana, loppu>];
[alku: "is",     luokka: ehtotapa, tapaluokka: ehtotapa, aikamuoto: kestämä,
                 äs: aä, luku: yksikkö, tekijä: 3, jatko: <liitesana, loppu>, tiedot: <murre>];
[alku: "isimme", luokka: ehtotapa, tapaluokka: ehtotapa, aikamuoto: kestämä,
                 äs: aä, luku: monikko, tekijä: 1, jatko: <liitesana, loppu>];
[alku: "isitte", luokka: ehtotapa, tapaluokka: ehtotapa, aikamuoto: kestämä,
                 äs: aä, luku: monikko, tekijä: 2, jatko: <liitesana, loppu>];
[alku: "isivat", luokka: ehtotapa, tapaluokka: ehtotapa, aikamuoto: kestämä,
                 äs: a, luku: monikko, tekijä: 3, jatko: <liitesana, loppu>];
[alku: "isivät", luokka: ehtotapa, tapaluokka: ehtotapa, aikamuoto: kestämä,
                 äs: ä, luku: monikko, tekijä: 3, jatko: <liitesana, loppu>];

[alku: "ttaisi",   luokka: ehtotapa_ttA, tapaluokka: ehtotapa, aikamuoto: kestämä,
                   äs: a, tekijä: 4, jatko: <liitesana, loppu>];
[alku: "ttäisi",   luokka: ehtotapa_ttA, tapaluokka: ehtotapa, aikamuoto: kestämä,
                   äs: ä, tekijä: 4, jatko: <liitesana, loppu>];
[alku: "ttaisiin", luokka: ehtotapa_ttA, tapaluokka: ehtotapa, aikamuoto: kestämä,
                   äs: a, tekijä: 4, jatko: <liitesana, loppu>];
[alku: "ttäisiin", luokka: ehtotapa_ttA, tapaluokka: ehtotapa, aikamuoto: kestämä,
                   äs: ä, tekijä: 4, jatko: <liitesana, loppu>];
[alku: "taisi",    luokka: ehtotapa_tA,  tapaluokka: ehtotapa, aikamuoto: kestämä,
                   äs: a, tekijä: 4, jatko: <liitesana, loppu>];
[alku: "täisi",    luokka: ehtotapa_tA,  tapaluokka: ehtotapa, aikamuoto: kestämä,
                   äs: ä, tekijä: 4, jatko: <liitesana, loppu>];
[alku: "taisiin",  luokka: ehtotapa_tA,  tapaluokka: ehtotapa, aikamuoto: kestämä,
                   äs: a, tekijä: 4, jatko: <liitesana, loppu>];
[alku: "täisiin",  luokka: ehtotapa_tA,  tapaluokka: ehtotapa, aikamuoto: kestämä,
                   äs: ä, tekijä: 4, jatko: <liitesana, loppu>];


# Mahtotapa.
#
[alku: "len",    luokka: mahtotapa_le, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: aä, luku: yksikkö, tekijä: 1, jatko: <liitesana, loppu>];
[alku: "let",    luokka: mahtotapa_le, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: aä, luku: yksikkö, tekijä: 2, jatko: <liitesana, loppu>];
[alku: "lee",    luokka: mahtotapa_le, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: aä, luku: yksikkö, tekijä: 3, jatko: <liitesana, loppu>];
[alku: "lehe",   luokka: mahtotapa_le, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: aä, luku: yksikkö, tekijä: 3, jatko: <liitesana, loppu>, tiedot: <murre>];
[alku: "lemme",  luokka: mahtotapa_le, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: aä, luku: monikko, tekijä: 1, jatko: <liitesana, loppu>];
[alku: "lette",  luokka: mahtotapa_le, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: aä, luku: monikko, tekijä: 2, jatko: <liitesana, loppu>];
[alku: "levat",  luokka: mahtotapa_le, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: a,    luku: monikko, tekijä: 3, jatko: <liitesana, loppu>];
[alku: "levät",  luokka: mahtotapa_le, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: ä,    luku: monikko, tekijä: 3, jatko: <liitesana, loppu>];

[alku: "le",     luokka: mahtotapa_le_kielto, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: aä, jatko: <liitesana, loppu>];

[alku: "nen",    luokka: mahtotapa_ne, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: aä, luku: yksikkö, tekijä: 1, jatko: <liitesana, loppu>];
[alku: "net",    luokka: mahtotapa_ne, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: aä, luku: yksikkö, tekijä: 2, jatko: <liitesana, loppu>];
[alku: "nee",    luokka: mahtotapa_ne, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: aä, luku: yksikkö, tekijä: 3, jatko: <liitesana, loppu>];
[alku: "nehe",   luokka: mahtotapa_ne, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: aä, luku: yksikkö, tekijä: 3, jatko: <liitesana, loppu>, tiedot: <murre>];
[alku: "nemme",  luokka: mahtotapa_ne, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: aä, luku: monikko, tekijä: 1, jatko: <liitesana, loppu>];
[alku: "nette",  luokka: mahtotapa_ne, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: aä, luku: monikko, tekijä: 2, jatko: <liitesana, loppu>];
[alku: "nevat",  luokka: mahtotapa_ne, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: a,    luku: monikko, tekijä: 3, jatko: <liitesana, loppu>];
[alku: "nevät",  luokka: mahtotapa_ne, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: ä,    luku: monikko, tekijä: 3, jatko: <liitesana, loppu>];

[alku: "ne",     luokka: mahtotapa_ne_kielto, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: aä, jatko: <liitesana, loppu>];

[alku: "ren",    luokka: mahtotapa_re, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: aä, luku: yksikkö, tekijä: 1, jatko: <liitesana, loppu>];
[alku: "ret",    luokka: mahtotapa_re, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: aä, luku: yksikkö, tekijä: 2, jatko: <liitesana, loppu>];
[alku: "ree",    luokka: mahtotapa_re, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: aä, luku: yksikkö, tekijä: 3, jatko: <liitesana, loppu>];
[alku: "rehe",   luokka: mahtotapa_re, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: aä, luku: yksikkö, tekijä: 3, jatko: <liitesana, loppu>, tiedot: <murre>];
[alku: "remme",  luokka: mahtotapa_re, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: aä, luku: monikko, tekijä: 1, jatko: <liitesana, loppu>];
[alku: "rette",  luokka: mahtotapa_re, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: aä, luku: monikko, tekijä: 2, jatko: <liitesana, loppu>];
[alku: "revat",  luokka: mahtotapa_re, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: a,    luku: monikko, tekijä: 3, jatko: <liitesana, loppu>];
[alku: "revät",  luokka: mahtotapa_re, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: ä,  jatko: <liitesana, loppu>];

[alku: "re",     luokka: mahtotapa_re_kielto, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: aä, jatko: <liitesana, loppu>];

[alku: "sen",    luokka: mahtotapa_se, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: aä, luku: yksikkö, tekijä: 1, jatko: <liitesana, loppu>];
[alku: "set",    luokka: mahtotapa_se, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: aä, luku: yksikkö, tekijä: 2, jatko: <liitesana, loppu>];
[alku: "see",    luokka: mahtotapa_se, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: aä, luku: yksikkö, tekijä: 3, jatko: <liitesana, loppu>];
[alku: "sehe",   luokka: mahtotapa_se, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: aä, luku: yksikkö, tekijä: 3, jatko: <liitesana, loppu>, tiedot: <murre>];
[alku: "semme",  luokka: mahtotapa_se, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: aä, luku: monikko, tekijä: 1, jatko: <liitesana, loppu>];
[alku: "sette",  luokka: mahtotapa_se, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: aä, luku: monikko, tekijä: 2, jatko: <liitesana, loppu>];
[alku: "sevat",  luokka: mahtotapa_se, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: a,  luku: monikko, tekijä: 3, jatko: <liitesana, loppu>];
[alku: "sevät",  luokka: mahtotapa_se, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: ä,  luku: monikko, tekijä: 3, jatko: <liitesana, loppu>];

[alku: "se",     luokka: mahtotapa_se_kielto, tapaluokka: mahtotapa, aikamuoto: kestämä,
                 äs: aä, jatko: <liitesana, loppu>];


# Murteellisia muotoja. Esim. huutanevi (huutanee).
[alku: "levi",    luokka: mahtotapa_le, tapaluokka: mahtotapa, aikamuoto: kestämä,
                  äs: aä, luku: yksikkö, tekijä: 3, jatko: <liitesana, loppu>, tiedot: <murre>];
[alku: "nevi",    luokka: mahtotapa_ne, tapaluokka: mahtotapa, aikamuoto: kestämä,
                  äs: aä, luku: yksikkö, tekijä: 3, jatko: <liitesana, loppu>, tiedot: <murre>];
[alku: "revi",    luokka: mahtotapa_re, tapaluokka: mahtotapa, aikamuoto: kestämä,
                  äs: aä, luku: yksikkö, tekijä: 3, jatko: <liitesana, loppu>, tiedot: <murre>];
[alku: "sevi",    luokka: mahtotapa_se, tapaluokka: mahtotapa, aikamuoto: kestämä,
                  äs: aä, luku: yksikkö, tekijä: 3, jatko: <liitesana, loppu>, tiedot: <murre>];


[alku: "ttane",   luokka: mahtotapa_ttA, tapaluokka: mahtotapa, aikamuoto: kestämä,
                  äs: a, tekijä: 4, jatko: <liitesana, loppu>];
[alku: "ttäne",   luokka: mahtotapa_ttA, tapaluokka: mahtotapa, aikamuoto: kestämä,
                  äs: ä, tekijä: 4, jatko: <liitesana, loppu>];
[alku: "ttaneen", luokka: mahtotapa_ttA, tapaluokka: mahtotapa, aikamuoto: kestämä,
                  äs: a, tekijä: 4, jatko: <liitesana, loppu>];
[alku: "ttäneen", luokka: mahtotapa_ttA, tapaluokka: mahtotapa, aikamuoto: kestämä,
                  äs: ä, tekijä: 4, jatko: <liitesana, loppu>];
[alku: "tane",    luokka: mahtotapa_tA,  tapaluokka: mahtotapa, aikamuoto: kestämä,
                  äs: a, tekijä: 4, jatko: <liitesana, loppu>];
[alku: "täne",    luokka: mahtotapa_tA,  tapaluokka: mahtotapa, aikamuoto: kestämä,
                  äs: ä, tekijä: 4, jatko: <liitesana, loppu>];
[alku: "taneen",  luokka: mahtotapa_tA,  tapaluokka: mahtotapa, aikamuoto: kestämä,
                  äs: a, tekijä: 4, jatko: <liitesana, loppu>];
[alku: "täneen",  luokka: mahtotapa_tA,  tapaluokka: mahtotapa, aikamuoto: kestämä,
                  äs: ä, tekijä: 4, jatko: <liitesana, loppu>];

# Vanhoja h:llisia muotoja.
#
[alku: "ttanehen", luokka: mahtotapa_ttA, tapaluokka: mahtotapa, aikamuoto: kestämä,
                   äs: a, tekijä: 4, jatko: <liitesana, loppu>, tiedot: <murre>];
[alku: "ttänehen", luokka: mahtotapa_ttA, tapaluokka: mahtotapa, aikamuoto: kestämä,
                   äs: ä, tekijä: 4, jatko: <liitesana, loppu>, tiedot: <murre>];
[alku: "tanehen",  luokka: mahtotapa_tA,  tapaluokka: mahtotapa, aikamuoto: kestämä,
                   äs: a, tekijä: 4, jatko: <liitesana, loppu>, tiedot: <murre>];
[alku: "tänehen",  luokka: mahtotapa_tA,  tapaluokka: mahtotapa, aikamuoto: kestämä,
                   äs: ä, tekijä: 4, jatko: <liitesana, loppu>, tiedot: <murre>];
[alku: "ttanehe",  luokka: mahtotapa_ttA, tapaluokka: mahtotapa, aikamuoto: kestämä,
                   äs: a, tekijä: 4, jatko: <liitesana, loppu>, tiedot: <murre>];
[alku: "ttänehe",  luokka: mahtotapa_ttA, tapaluokka: mahtotapa, aikamuoto: kestämä,
                   äs: ä, tekijä: 4, jatko: <liitesana, loppu>, tiedot: <murre>];
[alku: "tanehe",   luokka: mahtotapa_tA,  tapaluokka: mahtotapa, aikamuoto: kestämä,
                   äs: a, tekijä: 4, jatko: <liitesana, loppu>, tiedot: <murre>];
[alku: "tänehe",   luokka: mahtotapa_tA,  tapaluokka: mahtotapa, aikamuoto: kestämä,
                   äs: ä, tekijä: 4, jatko: <liitesana, loppu>, tiedot: <murre>];


# Teonsanojen käskytavan (punokoon) henkilöpäätteet.
#
[alku: "koon",   luokka: käskytapa, luku: yksikkö, tekijä: 3, äs: a,
                 jatko: <liitesana, loppu>, tapaluokka: käskytapa, aikamuoto: kestämä];
[alku: "köön",   luokka: käskytapa, luku: yksikkö, tekijä: 3, äs: ä,
                 jatko: <liitesana, loppu>, tapaluokka: käskytapa, aikamuoto: kestämä];
[alku: "kaamme", luokka: käskytapa, luku: monikko, tekijä: 1, äs: a,
                 jatko: <liitesana, loppu>, tapaluokka: käskytapa, aikamuoto: kestämä];
[alku: "käämme", luokka: käskytapa, luku: monikko, tekijä: 1, äs: ä,
                 jatko: <liitesana, loppu>, tapaluokka: käskytapa, aikamuoto: kestämä];
[alku: "kaa",    luokka: käskytapa, luku: monikko, tekijä: 2, äs: a,
                 jatko: <liitesana, liitesana_s, loppu>, tapaluokka: käskytapa, aikamuoto: kestämä];
[alku: "kaatte", luokka: käskytapa, luku: monikko, tekijä: 2, äs: a,
                 jatko: <liitesana, loppu>, tapaluokka: käskytapa, aikamuoto: kestämä];
[alku: "kää",    luokka: käskytapa, luku: monikko, tekijä: 2, äs: ä,
                 jatko: <liitesana, liitesana_s, loppu>, tapaluokka: käskytapa, aikamuoto: kestämä];
[alku: "käätte", luokka: käskytapa, luku: monikko, tekijä: 2, äs: ä,
                 jatko: <liitesana, loppu>, tapaluokka: käskytapa, aikamuoto: kestämä];
[alku: "koot",   luokka: käskytapa, luku: monikko, tekijä: 3, äs: a,
                 jatko: <liitesana, loppu>, tapaluokka: käskytapa, aikamuoto: kestämä];
[alku: "kööt",   luokka: käskytapa, luku: monikko, tekijä: 3, äs: ä,
                 jatko: <liitesana, loppu>, tapaluokka: käskytapa, aikamuoto: kestämä];


# Aleksis Kivi: Canzio; Selman juonet (1916; www.lonnrotnet):
# "Uskokaat minua ja tehkäät niinkuin sanon, te ystäväni molemmat. Juokaat
# vettä, naikaat ja olkaat ilosia."
#
[alku: "kaat", luokka: käskytapa, luku: monikko, tekijä: 2, äs: a,
               jatko: <liitesana, loppu>, tapaluokka: käskytapa, aikamuoto: kestämä, tiedot: <murre>];
[alku: "käät", luokka: käskytapa, luku: monikko, tekijä: 2, äs: ä,
               jatko: <liitesana, loppu>, tapaluokka: käskytapa, aikamuoto: kestämä, tiedot: <murre>];


# Vanhoja muotoja.
#
[alku: "kohon",  luokka: käskytapa, luku: yksikkö, tekijä: 3, äs: a,
                 jatko: <liitesana, loppu>, tapaluokka: käskytapa, aikamuoto: kestämä, tiedot: <vanha_h>];
[alku: "köhön",  luokka: käskytapa, luku: yksikkö, tekijä: 3, äs: ä,
                 jatko: <liitesana, loppu>, tapaluokka: käskytapa, aikamuoto: kestämä, tiedot: <vanha_h>];
[alku: "kohot",  luokka: käskytapa, luku: monikko, tekijä: 3, äs: a,
                 jatko: <liitesana, loppu>, tapaluokka: käskytapa, aikamuoto: kestämä, tiedot: <vanha_h>];
[alku: "köhöt",  luokka: käskytapa, luku: monikko, tekijä: 3, äs: ä,
                 jatko: <liitesana, loppu>, tapaluokka: käskytapa, aikamuoto: kestämä, tiedot: <vanha_h>];


# Toinen käskytapa: puhuos, ottaos.
#[alku: "os", luokka: käskytapa, luku: yksikkö, tekijä: 2, äs: a,
#             jatko: <liitesana, loppu>, tapaluokka: käskytapa, aikamuoto: kestämä];
#[alku: "ös", luokka: käskytapa, luku: yksikkö, tekijä: 2, äs: ä,
#             jatko: <liitesana, loppu>, tapaluokka: käskytapa, aikamuoto: kestämä];

[alku: "ttakoon", luokka: käskytapa_ttA, tekijä: 4, äs: a,
                  jatko: <liitesana, loppu>, tapaluokka: käskytapa, aikamuoto: kestämä];
[alku: "ttäköön", luokka: käskytapa_ttA, tekijä: 4, äs: ä,
                  jatko: <liitesana, loppu>, tapaluokka: käskytapa, aikamuoto: kestämä];

[alku: "ttako",   luokka: käskytapa_ttA, tekijä: 4, äs: a,        # Punoa, älköön puno+ttako.
                  jatko: <liitesana, loppu>, tapaluokka: käskytapa, aikamuoto: kestämä];
[alku: "ttäkö",   luokka: käskytapa_ttA, tekijä: 4, äs: ä,
                  jatko: <liitesana, loppu>, tapaluokka: käskytapa, aikamuoto: kestämä];

[alku: "takoon",  luokka: käskytapa_tA, tekijä: 4, äs: a,
                  jatko: <liitesana, loppu>, tapaluokka: käskytapa, aikamuoto: kestämä];
[alku: "täköön",  luokka: käskytapa_tA, tekijä: 4, äs: ä,
                  jatko: <liitesana, loppu>, tapaluokka: käskytapa, aikamuoto: kestämä];

[alku: "tako",    luokka: käskytapa_tA, tekijä: 4, äs: a,         # Juosta, älköön juos+tako.
                  jatko: <liitesana, loppu>, tapaluokka: käskytapa, aikamuoto: kestämä];
[alku: "täkö",    luokka: käskytapa_tA, tekijä: 4, äs: ä,
                  jatko: <liitesana, loppu>, tapaluokka: käskytapa, aikamuoto: kestämä];

# Vanhoja h:llisia muotoja.
[alku: "ttakohon", luokka: käskytapa_ttA, tekijä: 4, äs: a,
                   jatko: <liitesana, loppu>, tapaluokka: käskytapa, aikamuoto: kestämä, tiedot: <murre>];
[alku: "ttäköhön", luokka: käskytapa_ttA, tekijä: 4, äs: ä,
                   jatko: <liitesana, loppu>, tapaluokka: käskytapa, aikamuoto: kestämä, tiedot: <murre>];
[alku: "takohon",  luokka: käskytapa_tA, tekijä: 4, äs: a,
                   jatko: <liitesana, loppu>, tapaluokka: käskytapa, aikamuoto: kestämä, tiedot: <murre>];
[alku: "täköhön",  luokka: käskytapa_tA, tekijä: 4, äs: ä,
                   jatko: <liitesana, loppu>, tapaluokka: käskytapa, aikamuoto: kestämä, tiedot: <murre>];


[alku: "ko", luokka: käskytapa_kielto, äs: a, jatko: <loppu>, tapaluokka: käskytapa, aikamuoto: kestämä];
[alku: "kö", luokka: käskytapa_kielto, äs: ä, jatko: <loppu>, tapaluokka: käskytapa, aikamuoto: kestämä];

# "Älkää puhukokaan sellaista." "Älkää tehkökään." Tämä on oikeastaan kieltomuoto + kAAn-liitesana, mutta
# koska ko-kieltomuodon jälkeen ei voi olla muuta liitesanaa kuin kAAn, oli helpompaa laittaa se näin
# kuin tehdä morfologiatiedostoon uusi sääntö, jossa käskytavan kieltomuodon jälkene voi tulla vain
# kAAn-liitesana eikä muita (*Puhuko+pa ei käy minun kielikorvan mukaan.)
#
[alku: "kokaan", luokka: käskytapa_kielto, äs: a, jatko: <loppu>, tapaluokka: käskytapa, aikamuoto: kestämä];
[alku: "kökään", luokka: käskytapa_kielto, äs: ä, jatko: <loppu>, tapaluokka: käskytapa, aikamuoto: kestämä];


[alku: "kä", luokka: liitesana_kä, äs: ä, jatko: <liitesana, loppu>]; # Ei+kä, älä+kä.


# Ensimmäinen nimitapa.
#
define @nimitapa_1_jatko := <nimitapa_1_pitkä, liitesana, liitesana2, loppu>;

[alku: "a",  luokka: nimitapa_1_A,  tapaluokka: nimitapa_1, äs: a, jatko: @nimitapa_1_jatko];
[alku: "ä",  luokka: nimitapa_1_A,  tapaluokka: nimitapa_1, äs: ä, jatko: @nimitapa_1_jatko];
[alku: "da", luokka: nimitapa_1_dA, tapaluokka: nimitapa_1, äs: a, jatko: @nimitapa_1_jatko];
[alku: "dä", luokka: nimitapa_1_dA, tapaluokka: nimitapa_1, äs: ä, jatko: @nimitapa_1_jatko];
[alku: "la", luokka: nimitapa_1_lA, tapaluokka: nimitapa_1, äs: a, jatko: @nimitapa_1_jatko];
[alku: "lä", luokka: nimitapa_1_lA, tapaluokka: nimitapa_1, äs: ä, jatko: @nimitapa_1_jatko];
[alku: "na", luokka: nimitapa_1_nA, tapaluokka: nimitapa_1, äs: a, jatko: @nimitapa_1_jatko];
[alku: "nä", luokka: nimitapa_1_nA, tapaluokka: nimitapa_1, äs: ä, jatko: @nimitapa_1_jatko];
[alku: "ra", luokka: nimitapa_1_rA, tapaluokka: nimitapa_1, äs: a, jatko: @nimitapa_1_jatko];
[alku: "rä", luokka: nimitapa_1_rA, tapaluokka: nimitapa_1, äs: ä, jatko: @nimitapa_1_jatko];
[alku: "ta", luokka: nimitapa_1_tA, tapaluokka: nimitapa_1, äs: a, jatko: @nimitapa_1_jatko];
[alku: "tä", luokka: nimitapa_1_tA, tapaluokka: nimitapa_1, äs: ä, jatko: @nimitapa_1_jatko];


# Punoa+kse+ni
[alku: "kse", luokka: nimitapa_1_pitkä, tapaluokka: nimitapa_1, äs: aä, jatko: <omistusliite>];


# Toinen nimitapa: puno+essa, puno+en, puno+ttaessa.
#
[alku: "essa", luokka: nimitapa_2, tapaluokka: nimitapa_2, sija: sisäolento_ssA,
               äs: a, jatko: <omistusliite, liitesana, loppu>];
[alku: "essä", luokka: nimitapa_2, tapaluokka: nimitapa_2, sija: sisäolento_ssA,
               äs: ä, jatko: <omistusliite, liitesana, loppu>];
[alku: "en",   luokka: nimitapa_2, tapaluokka: nimitapa_2, sija: keinonto_n,
               äs: aä, jatko: <liitesana, loppu>];

[alku: "ttaessa", luokka: nimitapa_2_ttA, tapaluokka: nimitapa_2, äs: a, jatko: <liitesana, loppu>];
[alku: "ttäessä", luokka: nimitapa_2_ttA, tapaluokka: nimitapa_2, äs: ä, jatko: <liitesana, loppu>];
[alku: "taessa",  luokka: nimitapa_2_tA,  tapaluokka: nimitapa_2, äs: a, jatko: <liitesana, loppu>];
[alku: "täessä",  luokka: nimitapa_2_tA,  tapaluokka: nimitapa_2, äs: ä, jatko: <liitesana, loppu>];

# Murteissa.
[alku: "issa", luokka: nimitapa_2, tapaluokka: nimitapa_2, sija: sisäolento_ssA,
               äs: a, jatko: <omistusliite, liitesana, loppu>, tiedot: <murre>];
[alku: "issä", luokka: nimitapa_2, tapaluokka: nimitapa_2, sija: sisäolento_ssA,
               äs: ä, jatko: <omistusliite, liitesana, loppu>, tiedot: <murre>];

[alku: "ttaissa", luokka: nimitapa_2_ttA, tapaluokka: nimitapa_2, äs: a, jatko: <liitesana, loppu>, tiedot: <murre>];
[alku: "ttäissä", luokka: nimitapa_2_ttA, tapaluokka: nimitapa_2, äs: ä, jatko: <liitesana, loppu>, tiedot: <murre>];
[alku: "taissa",  luokka: nimitapa_2_tA,  tapaluokka: nimitapa_2, äs: a, jatko: <liitesana, loppu>, tiedot: <murre>];
[alku: "täissä",  luokka: nimitapa_2_tA,  tapaluokka: nimitapa_2, äs: ä, jatko: <liitesana, loppu>, tiedot: <murre>];


# Kolmas nimitapa.
#
[alku: "ma",    luokka: nimitapa_3, tapaluokka: nimitapa_3, sija: nimentö,
                luku: yksikkö, äs: a, jatko: <omistusliite, liitesana, loppu>];
[alku: "mä",    luokka: nimitapa_3, tapaluokka: nimitapa_3, sija: nimentö,
                luku: yksikkö, äs: ä, jatko: <omistusliite, liitesana, loppu>];

[alku: "massa", luokka: nimitapa_3, tapaluokka: nimitapa_3, sija: sisäolento_ssA,
                luku: yksikkö, äs: a, jatko: <omistusliite, liitesana, loppu>];
[alku: "mässä", luokka: nimitapa_3, tapaluokka: nimitapa_3, sija: sisäolento_ssA,
                luku: yksikkö, äs: ä, jatko: <omistusliite, liitesana, loppu>];

[alku: "masta", luokka: nimitapa_3, tapaluokka: nimitapa_3, sija: sisäeronto_stA,
                luku: yksikkö, äs: a, jatko: <omistusliite, liitesana, loppu>];
[alku: "mästä", luokka: nimitapa_3, tapaluokka: nimitapa_3, sija: sisäeronto_stA,
                luku: yksikkö, äs: ä, jatko: <omistusliite, liitesana, loppu>];

[alku: "maan",  luokka: nimitapa_3, tapaluokka: nimitapa_3, sija: sisätulento_Vn,
                luku: yksikkö, äs: a, jatko: <liitesana, loppu>];
[alku: "mään",  luokka: nimitapa_3, tapaluokka: nimitapa_3, sija: sisätulento_Vn,
                luku: yksikkö, äs: ä, jatko: <liitesana, loppu>];

[alku: "maa",   luokka: nimitapa_3, tapaluokka: nimitapa_3, sija: sisätulento_Vn,
                luku: yksikkö, äs: a, jatko: <omistusliite>];
[alku: "mää",   luokka: nimitapa_3, tapaluokka: nimitapa_3, sija: sisätulento_Vn,
                luku: yksikkö, äs: ä, jatko: <omistusliite>];

[alku: "malla", luokka: nimitapa_3, tapaluokka: nimitapa_3, sija: ulko_olento_llA,
                luku: yksikkö, äs: a, jatko: <omistusliite, liitesana, loppu>];
[alku: "mällä", luokka: nimitapa_3, tapaluokka: nimitapa_3, sija: ulko_olento_llA,
                luku: yksikkö, äs: ä, jatko: <omistusliite, liitesana, loppu>];

[alku: "matta", luokka: nimitapa_3, tapaluokka: nimitapa_3, sija: vajanto_ttA,
                luku: yksikkö, äs: a, jatko: <omistusliite, liitesana, loppu>];
[alku: "mättä", luokka: nimitapa_3, tapaluokka: nimitapa_3, sija: vajanto_ttA,
                luku: yksikkö, äs: ä, jatko: <omistusliite, liitesana, loppu>];

[alku: "man",   luokka: nimitapa_3, tapaluokka: nimitapa_3, sija: keinonto_n,
                luku: yksikkö, äs: a, jatko: <omistusliite, liitesana, loppu>];
[alku: "män",   luokka: nimitapa_3, tapaluokka: nimitapa_3, sija: keinonto_n,
                luku: yksikkö, äs: ä, jatko: <omistusliite, liitesana, loppu>];


[alku: "ma",    luokka: nimitapa_3_saama, tapaluokka: nimitapa_3, sija: nimentö,
                luku: yksikkö, äs: a, jatko: <omistusliite, liitesana, loppu>];
[alku: "mä",    luokka: nimitapa_3_saama, tapaluokka: nimitapa_3, sija: nimentö,
                luku: yksikkö, äs: ä, jatko: <omistusliite, liitesana, loppu>];

[alku: "massa", luokka: nimitapa_3_saama, tapaluokka: nimitapa_3, sija: sisäolento_ssA,
                luku: yksikkö, äs: a, jatko: <omistusliite, liitesana, loppu>];
[alku: "mässä", luokka: nimitapa_3_saama, tapaluokka: nimitapa_3, sija: sisäolento_ssA,
                luku: yksikkö, äs: ä, jatko: <omistusliite, liitesana, loppu>];

[alku: "masta", luokka: nimitapa_3_saama, tapaluokka: nimitapa_3, sija: sisäeronto_stA,
                luku: yksikkö, äs: a, jatko: <omistusliite, liitesana, loppu>];
[alku: "mästä", luokka: nimitapa_3_saama, tapaluokka: nimitapa_3, sija: sisäeronto_stA,
                luku: yksikkö, äs: ä, jatko: <omistusliite, liitesana, loppu>];

[alku: "maan",  luokka: nimitapa_3_saama, tapaluokka: nimitapa_3, sija: sisätulento_Vn,
                luku: yksikkö, äs: a, jatko: <liitesana, loppu>];
[alku: "mään",  luokka: nimitapa_3_saama, tapaluokka: nimitapa_3, sija: sisätulento_Vn,
                luku: yksikkö, äs: ä, jatko: <liitesana, loppu>];

[alku: "maa",   luokka: nimitapa_3_saama, tapaluokka: nimitapa_3, sija: sisätulento_Vn,
                luku: yksikkö, äs: a, jatko: <omistusliite>];
[alku: "mää",   luokka: nimitapa_3_saama, tapaluokka: nimitapa_3, sija: sisätulento_Vn,
                luku: yksikkö, äs: ä, jatko: <omistusliite>];

[alku: "malla", luokka: nimitapa_3_saama, tapaluokka: nimitapa_3, sija: ulko_olento_llA,
                luku: yksikkö, äs: a, jatko: <omistusliite, liitesana, loppu>];
[alku: "mällä", luokka: nimitapa_3_saama, tapaluokka: nimitapa_3, sija: ulko_olento_llA,
                luku: yksikkö, äs: ä, jatko: <omistusliite, liitesana, loppu>];

[alku: "matta", luokka: nimitapa_3_saama, tapaluokka: nimitapa_3, sija: vajanto_ttA,
                luku: yksikkö, äs: a, jatko: <omistusliite, liitesana, loppu>];
[alku: "mättä", luokka: nimitapa_3_saama, tapaluokka: nimitapa_3, sija: vajanto_ttA,
                luku: yksikkö, äs: ä, jatko: <omistusliite, liitesana, loppu>];

[alku: "man",   luokka: nimitapa_3_saama, tapaluokka: nimitapa_3, sija: keinonto_n,
                luku: yksikkö, äs: a, jatko: <omistusliite, liitesana, loppu>];
[alku: "män",   luokka: nimitapa_3_saama, tapaluokka: nimitapa_3, sija: keinonto_n,
                luku: yksikkö, äs: ä, jatko: <omistusliite, liitesana, loppu>];


[alku: "ttaman", luokka: nimitapa_3_ttA, tapaluokka: nimitapa_3, luku: yksikkö, sija: keinonto_n,
                 äs: a, jatko: <liitesana, loppu>];
[alku: "ttämän", luokka: nimitapa_3_ttA, tapaluokka: nimitapa_3, luku: yksikkö, sija: keinonto_n,
                 äs: ä, jatko: <liitesana, loppu>];
[alku: "taman",  luokka: nimitapa_3_tA,  tapaluokka: nimitapa_3, luku: yksikkö, sija: keinonto_n,
                 äs: a, jatko: <liitesana, loppu>];
[alku: "tämän",  luokka: nimitapa_3_tA,  tapaluokka: nimitapa_3, luku: yksikkö, sija: keinonto_n,
                 äs: ä, jatko: <liitesana, loppu>];


###xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


# Laatusanojen sekä teonsanojen laatutapojen voittoaste ja yliaste.
#
# Suuri  => suurempi,  suurin.
# Puhuva => puhuvampi, puhuvin.
#
[perusmuoto: "mpi", alku: "", luokka: voittoaste, jatko: <suurempi>, äs: a];
[perusmuoto: "mpi", alku: "", luokka: voittoaste, jatko: <suurempi>, äs: ä];

[perusmuoto: "in", alku: "", luokka: yliaste, jatko: <pahin>, äs: a];
[perusmuoto: "in", alku: "", luokka: yliaste, jatko: <pahin>, äs: ä];




# Neljäs nimitapa: puno+minen.
#
[perusmuoto: "minen", alku: "mi", luokka: nimitapa_4, jatko: <nainen>, äs: a];
[perusmuoto: "minen", alku: "mi", luokka: nimitapa_4, jatko: <nainen>, äs: ä];


# Viides nimitapa: puhu+maisilla(an), teke+mäisillä(än).
#
[perusmuoto: "maisilla", alku: "maisilla", luokka: nimitapa_5, jatko: <omistusliite>, äs: a];
[perusmuoto: "mäisillä", alku: "mäisillä", luokka: nimitapa_5, jatko: <omistusliite>, äs: ä];

# Viides nimitapa: murteellinen puhu+maisilla(h)an, teke+mäisillä(h)än.
#
[perusmuoto: "maisilla", alku: "maisillahan", luokka: nimitapa_5, jatko: <loppu>, äs: a, tiedot: <murre>];
[perusmuoto: "mäisillä", alku: "mäisillähän", luokka: nimitapa_5, jatko: <loppu>, äs: ä, tiedot: <murre>];



# Ensimmäinen laatutapa: puhuva, tekevä.
#
[perusmuoto: "va", alku: "v", luokka: laatutapa_1_vA, jatko: <asema>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "vä", alku: "v", luokka: laatutapa_1_vA, jatko: <asema>, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "va", alku: "v", luokka: laatutapa_1_vE, jatko: <koira>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "vä", alku: "v", luokka: laatutapa_1_vE, jatko: <koira>, äs: ä, tiedot: <ei_voikko>];


[perusmuoto: "va", alku: "v", luokka: johdin_vA, jatko: <asema>, äs: a];
[perusmuoto: "vä", alku: "v", luokka: johdin_vA, jatko: <asema>, äs: ä];
[perusmuoto: "va", alku: "v", luokka: johdin_vE, jatko: <koira>, äs: a];
[perusmuoto: "vä", alku: "v", luokka: johdin_vE, jatko: <koira>, äs: ä];


[perusmuoto: "ttava", alku: "ttav", luokka: laatutapa_1_ttA, tapaluokka: laatutapa_1, jatko: <asema>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "ttävä", alku: "ttäv", luokka: laatutapa_1_ttA, tapaluokka: laatutapa_1, jatko: <asema>, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "tava",  alku: "tav",  luokka: laatutapa_1_tA,  tapaluokka: laatutapa_1, jatko: <asema>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "tävä",  alku: "täv",  luokka: laatutapa_1_tA,  tapaluokka: laatutapa_1, jatko: <asema>, äs: ä, tiedot: <ei_voikko>];

[perusmuoto: "ttava", alku: "ttav", luokka: johdin_ttAvA, jatko: <asema>, äs: a];
[perusmuoto: "ttävä", alku: "ttäv", luokka: johdin_ttAvA, jatko: <asema>, äs: ä];
[perusmuoto: "tava",  alku: "tav",  luokka: johdin_tAvA,  jatko: <asema>, äs: a];
[perusmuoto: "tävä",  alku: "täv",  luokka: johdin_tAvA,  jatko: <asema>, äs: ä];


[perusmuoto: "vainen", alku: "vai", luokka: johdin_vAinen, jatko: <nainen>, äs: a];
[perusmuoto: "väinen", alku: "väi", luokka: johdin_vAinen, jatko: <nainen>, äs: ä];

[perusmuoto: "ttavainen", alku: "ttavai", luokka: johdin_ttAvAinen, jatko: <nainen>, äs: a];
[perusmuoto: "ttäväinen", alku: "ttäväi", luokka: johdin_ttAvAinen, jatko: <nainen>, äs: ä];
[perusmuoto: "tavainen",  alku: "tavai",  luokka: johdin_tAvAinen,  jatko: <nainen>, äs: a];
[perusmuoto: "täväinen",  alku: "täväi",  luokka: johdin_tAvAinen,  jatko: <nainen>, äs: ä];



# Toinen laatutapa: puhu+nut, tehnyt.


[perusmuoto: "lut", alku: "", luokka: laatutapa_2_lUt, tapaluokka: laatutapa_2, jatko: <kuollut>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "lyt", alku: "", luokka: laatutapa_2_lUt, tapaluokka: laatutapa_2, jatko: <kuollut>, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "nut", alku: "", luokka: laatutapa_2_nUt, tapaluokka: laatutapa_2, jatko: <punonut>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "nyt", alku: "", luokka: laatutapa_2_nUt, tapaluokka: laatutapa_2, jatko: <punonut>, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "rut", alku: "", luokka: laatutapa_2_rUt, tapaluokka: laatutapa_2, jatko: <purrut>,  äs: a, tiedot: <ei_voikko>];
[perusmuoto: "ryt", alku: "", luokka: laatutapa_2_rUt, tapaluokka: laatutapa_2, jatko: <purrut>,  äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "sut", alku: "", luokka: laatutapa_2_sUt, tapaluokka: laatutapa_2, jatko: <juossut>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "syt", alku: "", luokka: laatutapa_2_sUt, tapaluokka: laatutapa_2, jatko: <juossut>, äs: ä, tiedot: <ei_voikko>];

[perusmuoto: "lut", alku: "", luokka: johdin_lUt, jatko: <kuollut>, äs: a];
[perusmuoto: "lyt", alku: "", luokka: johdin_lUt, jatko: <kuollut>, äs: ä];
[perusmuoto: "nut", alku: "", luokka: johdin_nUt, jatko: <punonut>, äs: a];
[perusmuoto: "nyt", alku: "", luokka: johdin_nUt, jatko: <punonut>, äs: ä];
[perusmuoto: "rut", alku: "", luokka: johdin_rUt, jatko: <purrut>,  äs: a];
[perusmuoto: "ryt", alku: "", luokka: johdin_rUt, jatko: <purrut>,  äs: ä];
[perusmuoto: "sut", alku: "", luokka: johdin_sUt, jatko: <juossut>, äs: a];
[perusmuoto: "syt", alku: "", luokka: johdin_sUt, jatko: <juossut>, äs: ä];

# Kalevala:
# "kuss' oli piian pillannunna,
# emon tuoman turmellunna."
#
[perusmuoto: "lut", alku: "lunna", luokka: laatutapa_2_lUt, tapaluokka: laatutapa_2, jatko: <loppu>, äs: a, tiedot: <ei_voikko, murre>];
[perusmuoto: "lyt", alku: "lynnä", luokka: laatutapa_2_lUt, tapaluokka: laatutapa_2, jatko: <loppu>, äs: ä, tiedot: <ei_voikko, murre>];
[perusmuoto: "nut", alku: "nunna", luokka: laatutapa_2_nUt, tapaluokka: laatutapa_2, jatko: <loppu>, äs: a, tiedot: <ei_voikko, murre>];
[perusmuoto: "nyt", alku: "nynnä", luokka: laatutapa_2_nUt, tapaluokka: laatutapa_2, jatko: <loppu>, äs: ä, tiedot: <ei_voikko, murre>];
[perusmuoto: "rut", alku: "runna", luokka: laatutapa_2_rUt, tapaluokka: laatutapa_2, jatko: <loppu>, äs: a, tiedot: <ei_voikko, murre>];
[perusmuoto: "ryt", alku: "rynnä", luokka: laatutapa_2_rUt, tapaluokka: laatutapa_2, jatko: <loppu>, äs: ä, tiedot: <ei_voikko, murre>];
[perusmuoto: "sut", alku: "sunna", luokka: laatutapa_2_sUt, tapaluokka: laatutapa_2, jatko: <loppu>, äs: a, tiedot: <ei_voikko, murre>];
[perusmuoto: "syt", alku: "synnä", luokka: laatutapa_2_sUt, tapaluokka: laatutapa_2, jatko: <loppu>, äs: ä, tiedot: <ei_voikko, murre>];

[perusmuoto: "lut", alku: "lunna", luokka: johdin_lUt, jatko: <loppu>, äs: a, tiedot: <ei_voikko, murre>];
[perusmuoto: "lyt", alku: "lynnä", luokka: johdin_lUt, jatko: <loppu>, äs: ä, tiedot: <ei_voikko, murre>];
[perusmuoto: "nut", alku: "nunna", luokka: johdin_nUt, jatko: <loppu>, äs: a, tiedot: <ei_voikko, murre>];
[perusmuoto: "nyt", alku: "nynnä", luokka: johdin_nUt, jatko: <loppu>, äs: ä, tiedot: <ei_voikko, murre>];
[perusmuoto: "rut", alku: "runna", luokka: johdin_rUt, jatko: <loppu>, äs: a, tiedot: <ei_voikko, murre>];
[perusmuoto: "ryt", alku: "rynnä", luokka: johdin_rUt, jatko: <loppu>, äs: ä, tiedot: <ei_voikko, murre>];
[perusmuoto: "sut", alku: "sunna", luokka: johdin_sUt, jatko: <loppu>, äs: a, tiedot: <ei_voikko, murre>];
[perusmuoto: "syt", alku: "synnä", luokka: johdin_sUt, jatko: <loppu>, äs: ä, tiedot: <ei_voikko, murre>];

# Esim. aikonna (aikonut).
[perusmuoto: "nut", alku: "nna", luokka: laatutapa_2_nUt, tapaluokka: laatutapa_2, jatko: <loppu>, äs: a, tiedot: <ei_voikko, murre>];
[perusmuoto: "nyt", alku: "nnä", luokka: laatutapa_2_nUt, tapaluokka: laatutapa_2, jatko: <loppu>, äs: ä, tiedot: <ei_voikko, murre>];

[perusmuoto: "leisuus", alku: "leisuu", luokka: johdin_lUt, luku: monikko, jatko: <kalleus>, äs: a];
[perusmuoto: "leisyys", alku: "leisyy", luokka: johdin_lUt, luku: monikko, jatko: <kalleus>, äs: ä];
[perusmuoto: "neisuus", alku: "neisuu", luokka: johdin_nUt, luku: monikko, jatko: <kalleus>, äs: a];
[perusmuoto: "neisyys", alku: "neisyy", luokka: johdin_nUt, luku: monikko, jatko: <kalleus>, äs: ä];
[perusmuoto: "reisuus", alku: "reisuu", luokka: johdin_rUt, luku: monikko, jatko: <kalleus>, äs: a];
[perusmuoto: "reisyys", alku: "reisyy", luokka: johdin_rUt, luku: monikko, jatko: <kalleus>, äs: ä];
[perusmuoto: "seisuus", alku: "seisuu", luokka: johdin_sUt, luku: monikko, jatko: <kalleus>, äs: a];
[perusmuoto: "seisyys", alku: "seisyy", luokka: johdin_sUt, luku: monikko, jatko: <kalleus>, äs: ä];


# Puhu+ttu, men+ty, juos+tu.
#
[perusmuoto: "ttu", alku: "", luokka: laatutapa_2_ttU, tapaluokka: laatutapa_2, jatko: <puhuttu>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "tty", alku: "", luokka: laatutapa_2_ttU, tapaluokka: laatutapa_2, jatko: <puhuttu>, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "ttu", alku: "", luokka: johdin_ttU, jatko: <puhuttu>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "tty", alku: "", luokka: johdin_ttU, jatko: <puhuttu>, äs: ä, tiedot: <ei_voikko>];


[perusmuoto: "tu", alku: "", luokka: laatutapa_2_tU_dU,          tapaluokka: laatutapa_2, jatko: <laatu>,    äs: a, tiedot: <ei_voikko>];
[perusmuoto: "ty", alku: "", luokka: laatutapa_2_tU_dU,          tapaluokka: laatutapa_2, jatko: <laatu>,    äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "tu", alku: "", luokka: laatutapa_2_tU_lU_katseltu, tapaluokka: laatutapa_2, jatko: <katseltu>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "ty", alku: "", luokka: laatutapa_2_tU_lU_katseltu, tapaluokka: laatutapa_2, jatko: <katseltu>, äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "tu", alku: "", luokka: laatutapa_2_tU_lU_oltu,     tapaluokka: laatutapa_2, jatko: <oltu>,     äs: a, tiedot: <ei_voikko>];
[perusmuoto: "ty", alku: "", luokka: laatutapa_2_tU_lU_oltu,     tapaluokka: laatutapa_2, jatko: <oltu>,     äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "tu", alku: "", luokka: laatutapa_2_tU_nU,          tapaluokka: laatutapa_2, jatko: <lintu>,    äs: a, tiedot: <ei_voikko>];
[perusmuoto: "ty", alku: "", luokka: laatutapa_2_tU_nU,          tapaluokka: laatutapa_2, jatko: <lintu>,    äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "tu", alku: "", luokka: laatutapa_2_tU_rU,          tapaluokka: laatutapa_2, jatko: <purtu>,    äs: a, tiedot: <ei_voikko>];
[perusmuoto: "ty", alku: "", luokka: laatutapa_2_tU_rU,          tapaluokka: laatutapa_2, jatko: <purtu>,    äs: ä, tiedot: <ei_voikko>];

[perusmuoto: "tu", alku: "", luokka: johdin_tU_dU,          jatko: <laatu>,    äs: a];
[perusmuoto: "ty", alku: "", luokka: johdin_tU_dU,          jatko: <laatu>,    äs: ä];
[perusmuoto: "tu", alku: "", luokka: johdin_tU_lU_katseltu, jatko: <katseltu>, äs: a];
[perusmuoto: "ty", alku: "", luokka: johdin_tU_lU_katseltu, jatko: <katseltu>, äs: ä];
[perusmuoto: "tu", alku: "", luokka: johdin_tU_lU_oltu,     jatko: <oltu>,     äs: a];
[perusmuoto: "ty", alku: "", luokka: johdin_tU_lU_oltu,     jatko: <oltu>,     äs: ä];
[perusmuoto: "tu", alku: "", luokka: johdin_tU_nU,          jatko: <lintu>,    äs: a];
[perusmuoto: "ty", alku: "", luokka: johdin_tU_nU,          jatko: <lintu>,    äs: ä];
[perusmuoto: "tu", alku: "", luokka: johdin_tU_rU,          jatko: <purtu>,    äs: a];
[perusmuoto: "ty", alku: "", luokka: johdin_tU_rU,          jatko: <purtu>,    äs: ä];


[perusmuoto: "tu", alku: "tu", luokka: laatutapa_2_stU_juostu,    tapaluokka: laatutapa_2, jatko: <valo>,      äs: a, tiedot: <ei_voikko>];
[perusmuoto: "ty", alku: "ty", luokka: laatutapa_2_stU_juostu,    tapaluokka: laatutapa_2, jatko: <valo>,      äs: ä, tiedot: <ei_voikko>];
[perusmuoto: "tu", alku: "",   luokka: laatutapa_2_stU_nuolaistu, tapaluokka: laatutapa_2, jatko: <nuolaistu>, äs: a, tiedot: <ei_voikko>];
[perusmuoto: "ty", alku: "",   luokka: laatutapa_2_stU_nuolaistu, tapaluokka: laatutapa_2, jatko: <nuolaistu>, äs: ä, tiedot: <ei_voikko>];

[perusmuoto: "tu", alku: "tu", luokka: johdin_stU_juostu,    jatko: <valo>,      äs: a];
[perusmuoto: "ty", alku: "ty", luokka: johdin_stU_juostu,    jatko: <valo>,      äs: ä];
[perusmuoto: "tu", alku: "",   luokka: johdin_stU_nuolaistu, jatko: <nuolaistu>, äs: a];
[perusmuoto: "ty", alku: "",   luokka: johdin_stU_nuolaistu, jatko: <nuolaistu>, äs: ä];



# Puno+ma, teke+mä, saa+ma.
[perusmuoto: "ma", alku: "m", luokka: johdin_mA, jatko: <asema>, äs: a];
[perusmuoto: "mä", alku: "m", luokka: johdin_mA, jatko: <asema>, äs: ä];
[perusmuoto: "ma", alku: "m", luokka: johdin_mA_saama, jatko: <koira>, äs: a];
[perusmuoto: "mä", alku: "m", luokka: johdin_mA_saama, jatko: <koira>, äs: ä];



# Maa+ton, pää+tön.
[perusmuoto: "ton", alku: "", luokka: johdin_tOn, jatko: <onneton>, äs: a];
[perusmuoto: "tön", alku: "", luokka: johdin_tOn, jatko: <onneton>, äs: ä];



[perusmuoto: "-", alku: "-", luokka: tavuviiva, äs: aä, jatko: @yhdyssana + <loppu>];  # Linja-auto.

[alku: ":", luokka: kaksoispiste, äs: aä, jatko: <sijapääte>];


# Kallis => kalleus, kalleuden.
[perusmuoto: "us", alku: "u", luokka: johdin_Us, jatko: <kalleus>, äs: a];
[perusmuoto: "ys", alku: "y", luokka: johdin_Us, jatko: <kalleus>, äs: ä];


# Matala => mataluus, mataluuden.
[perusmuoto: "uus", alku: "uu", luokka: johdin_UUs, jatko: <kalleus>, äs: a];
[perusmuoto: "yys", alku: "yy", luokka: johdin_UUs, jatko: <kalleus>, äs: ä];


# Vastata => vastaus, vastauksen.
[perusmuoto: "us", alku: "u", luokka: johdin_Us_ksen, jatko: <vastaus>, äs: a];
[perusmuoto: "ys", alku: "y", luokka: johdin_Us_ksen, jatko: <vastaus>, äs: ä];


# Juhla => juhlallinen.
[perusmuoto: "llinen", alku: "lli", luokka: johdin_llinen, jatko: <nainen>, äs: a];
[perusmuoto: "llinen", alku: "lli", luokka: johdin_llinen, jatko: <nainen>, äs: ä];


# Juhla => juhlainen.
[perusmuoto: "inen", alku: "i", luokka: johdin_inen, jatko: <nainen>, äs: a];
[perusmuoto: "inen", alku: "i", luokka: johdin_inen, jatko: <nainen>, äs: ä];
#[perusmuoto: "nen", alku: "", luokka: johdin_inen, jatko: <nainen>, äs: a, tiedot: <ei_voikko>]; # Puna+(i)nen yms.
#[perusmuoto: "nen", alku: "", luokka: johdin_inen, jatko: <nainen>, äs: ä, tiedot: <ei_voikko>];


# Juhla => juhlittain.
[alku: "ittain", luokka: johdin_ittAin, äs: a, perusmuoto: "ittain", jatko: <liitesana, loppu>];
[alku: "ittäin", luokka: johdin_ittAin, äs: ä, perusmuoto: "ittäin", jatko: <liitesana, loppu>];


# Juhla => juhla+lainen. Parempi esimerikki: kaupunki+lainen.
[perusmuoto: "lainen", alku: "lai", luokka: johdin_lAinen, jatko: <nainen>, äs: a];
[perusmuoto: "läinen", alku: "läi", luokka: johdin_lAinen, jatko: <nainen>, äs: ä];


# Juhla => juhla+mainen.
[perusmuoto: "mainen", alku: "mai", luokka: johdin_mAinen, jatko: <nainen>, äs: a];
[perusmuoto: "mäinen", alku: "mäi", luokka: johdin_mAinen, jatko: <nainen>, äs: ä];


# Hyvän+lainen, paha+nlainen. Ä:llistä muotoa ei ole.
[perusmuoto: "nlainen", alku: "nlai", luokka: johdin_nlainen, jatko: <nainen>, äs: a];


#[perusmuoto: "tar", alku: "t", luokka: johdin_tAr, jatko: <tytär>, äs: a];
#[perusmuoto: "tär", alku: "t", luokka: johdin_tAr, jatko: <tytär>, äs: ä];


[perusmuoto: "ja", alku: "",   luokka: johdin_jA_myyjä, jatko: <myyjä>,      äs: a];
[perusmuoto: "jä", alku: "",   luokka: johdin_jA_myyjä, jatko: <myyjä>,      äs: ä];
[perusmuoto: "jatar", alku: "jat", luokka: johdin_jA_myyjä, jatko: <tytär>, äs: a];
[perusmuoto: "jätär", alku: "jät", luokka: johdin_jA_myyjä, jatko: <tytär>, äs: ä];
[perusmuoto: "juus",  alku: "juu", luokka: johdin_jA_myyjä, jatko: <kalleus>, äs: a];
[perusmuoto: "jyys",  alku: "jyy", luokka: johdin_jA_myyjä, jatko: <kalleus>, äs: ä];
[perusmuoto: "jattaruus", alku: "jattaruu", luokka: johdin_jA_myyjä, jatko: <kalleus>, äs: a];
[perusmuoto: "jättäryys", alku: "jättäryy", luokka: johdin_jA_myyjä, jatko: <kalleus>, äs: ä];
#[perusmuoto: "jittain",   alku: "jittain",  luokka: johdin_jA_myyjä, jatko: <liitesana, loppu>, äs: a];
#[perusmuoto: "jittäin",   alku: "jittäin",  luokka: johdin_jA_myyjä, jatko: <liitesana, loppu>, äs: ä]; # Myyjittäin.

[perusmuoto: "ja", alku: "",   luokka: johdin_jA_kulkija, jatko: <kulkija>,    äs: a];
[perusmuoto: "jä", alku: "",   luokka: johdin_jA_kulkija, jatko: <kulkija>,    äs: ä];
[perusmuoto: "jatar", alku: "jat", luokka: johdin_jA_kulkija, jatko: <tytär>, äs: a];
[perusmuoto: "jätär", alku: "jät", luokka: johdin_jA_kulkija, jatko: <tytär>, äs: ä];
[perusmuoto: "juus",  alku: "juu", luokka: johdin_jA_kulkija, jatko: <kalleus>, äs: a];
[perusmuoto: "jyys",  alku: "jyy", luokka: johdin_jA_kulkija, jatko: <kalleus>, äs: ä];
[perusmuoto: "jattaruus", alku: "jattaruu", luokka: johdin_jA_kulkija, jatko: <kalleus>, äs: a];
[perusmuoto: "jättäryys", alku: "jättäryy", luokka: johdin_jA_kulkija, jatko: <kalleus>, äs: ä];
#[perusmuoto: "joittain",  alku: "joittain",  luokka: johdin_jA_kulkija, jatko: <liitesana, loppu>, äs: a]; # Kulkijoittain.
#[perusmuoto: "jöittäin",  alku: "jöittäin",  luokka: johdin_jA_kulkija, jatko: <liitesana, loppu>, äs: ä];

[perusmuoto: "ja", alku: "",   luokka: johdin_jA_kantaja, jatko: <kantaja>,    äs: a];
[perusmuoto: "jä", alku: "",   luokka: johdin_jA_kantaja, jatko: <kantaja>,    äs: ä];
[perusmuoto: "jatar", alku: "jat", luokka: johdin_jA_kantaja, jatko: <tytär>, äs: a];
[perusmuoto: "jätär", alku: "jät", luokka: johdin_jA_kantaja, jatko: <tytär>, äs: ä];
[perusmuoto: "juus",  alku: "juu", luokka: johdin_jA_kantaja, jatko: <kalleus>, äs: a];
[perusmuoto: "jyys",  alku: "jyy", luokka: johdin_jA_kantaja, jatko: <kalleus>, äs: ä];
[perusmuoto: "jattaruus", alku: "jattaruu", luokka: johdin_jA_kantaja, jatko: <kalleus>, äs: a];
[perusmuoto: "jättäryys", alku: "jättäryy", luokka: johdin_jA_kantaja, jatko: <kalleus>, äs: ä];
#[perusmuoto: "jittain",  alku: "jittain",   luokka: johdin_jA_kantaja, jatko: <liitesana, loppu>, äs: a]; # Kantaj(o)ittain.
#[perusmuoto: "jittäin",  alku: "jittäin",   luokka: johdin_jA_kantaja, jatko: <liitesana, loppu>, äs: ä];
#[perusmuoto: "jittain",  alku: "joittain",  luokka: johdin_jA_kantaja, jatko: <liitesana, loppu>, äs: a];
#[perusmuoto: "jittäin",  alku: "jöittäin",  luokka: johdin_jA_kantaja, jatko: <liitesana, loppu>, äs: ä];


[perusmuoto: "o", alku: "o", luokka: johdin_O , jatko: <valo>, äs: a];
[perusmuoto: "ö", alku: "ö", luokka: johdin_O , jatko: <valo>, äs: ä];

[perusmuoto: "to", alku: "", luokka: johdin_tO_leuto, jatko: <leuto>, äs: a];
[perusmuoto: "tö", alku: "", luokka: johdin_tO_leuto, jatko: <leuto>, äs: ä];

[perusmuoto: "to", alku: "", luokka: johdin_tO_liitto, jatko: <liitto>, äs: a];
[perusmuoto: "tö", alku: "", luokka: johdin_tO_liitto, jatko: <liitto>, äs: ä];

[perusmuoto: "u", alku: "u", luokka: johdin_U_arvelu, jatko: <arvelu>, äs: a];
[perusmuoto: "y", alku: "y", luokka: johdin_U_arvelu, jatko: <arvelu>, äs: ä];

[perusmuoto: "na", alku: "n", luokka: johdin_nA, jatko: <apila>, äs: a];
[perusmuoto: "nä", alku: "n", luokka: johdin_nA, jatko: <apila>, äs: ä];

[perusmuoto: "nti", alku: "n", luokka: johdin_nti, jatko: <tunti>, äs: aä, tiedot: <inen>];

[perusmuoto: "nta", alku: "n", luokka: johdin_ntA, jatko: <kanta>, äs: a, tiedot: <inen>];
[perusmuoto: "ntä", alku: "n", luokka: johdin_ntA, jatko: <kanta>, äs: ä, tiedot: <inen>];

[perusmuoto: "os", alku: "o", luokka: johdin_Os, jatko: <vastaus>, äs: a, tiedot: <inen>];
[perusmuoto: "ös", alku: "ö", luokka: johdin_Os, jatko: <vastaus>, äs: ä, tiedot: <inen>];


# johdin_lAinen + johdin_ittAin
[perusmuoto: "laisittain", alku: "laisittain", luokka: johdin_lAinen, jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "läisittäin", alku: "läisittäin", luokka: johdin_lAinen, jatko: <liitesana, loppu>, äs: ä];

# johdin_llinen + johdin_ittAin
[perusmuoto: "llisittain", alku: "llisittain", luokka: johdin_llinen, jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "llisittäin", alku: "llisittäin", luokka: johdin_llinen, jatko: <liitesana, loppu>, äs: ä];

# johdin_mAinen + johdin_ittAin
[perusmuoto: "maisittain", alku: "maisittain", luokka: johdin_mAinen, jatko: <liitesana, loppu>, äs: a];
[perusmuoto: "mäisittäin", alku: "mäisittäin", luokka: johdin_mAinen, jatko: <liitesana, loppu>, äs: ä];

# johdin_lAinen + johdin_mAinen
[perusmuoto: "laismainen", alku: "laismai", luokka: johdin_lAinen, jatko: <nainen>, äs: a];
[perusmuoto: "läismäinen", alku: "läismäi", luokka: johdin_lAinen, jatko: <nainen>, äs: ä];

# johdin_mAinen + johdin_llinen
[perusmuoto: "maisellinen", alku: "maiselli", luokka: johdin_mAinen, jatko: <nainen>, äs: a];
[perusmuoto: "mäisellinen", alku: "mäiselli", luokka: johdin_mAinen, jatko: <nainen>, äs: ä];

# Muistaa => muistella.
[perusmuoto: "ella", alku: "el", luokka: johdin_ellA, jatko: <katsella>, äs: a];
[perusmuoto: "ellä", alku: "el", luokka: johdin_ellA, jatko: <katsella>, äs: ä];

[perusmuoto: "ella", alku: "", luokka: johdin_tellA, jatko: <aatella>, äs: a];
[perusmuoto: "ellä", alku: "", luokka: johdin_tellA, jatko: <aatella>, äs: ä];

[perusmuoto: "della", alku: "", luokka: johdin_dellA, jatko: <kohdella>, äs: a];
[perusmuoto: "dellä", alku: "", luokka: johdin_dellA, jatko: <kohdella>, äs: ä];

[perusmuoto: "hdella", alku: "h", luokka: johdin_hdellA, jatko: <kohdella>, äs: a];
[perusmuoto: "hdellä", alku: "h", luokka: johdin_hdellA, jatko: <kohdella>, äs: ä];

#[perusmuoto: "ella", alku: "", luokka: johdin_kellA, jatko: <nakella>, äs: a];
#[perusmuoto: "ellä", alku: "", luokka: johdin_kellA, jatko: <nakella>, äs: ä];

[perusmuoto: "lella", alku: "", luokka: johdin_lellA, jatko: <takellella>, äs: a];
[perusmuoto: "lellä", alku: "", luokka: johdin_lellA, jatko: <takellella>, äs: ä];

[perusmuoto: "nella", alku: "", luokka: johdin_nellA, jatko: <pienennellä>, äs: a];
[perusmuoto: "nellä", alku: "", luokka: johdin_nellA, jatko: <pienennellä>, äs: ä];

[perusmuoto: "nnella", alku: "n", luokka: johdin_nnellA, jatko: <pienennellä>, äs: a];
[perusmuoto: "nnellä", alku: "n", luokka: johdin_nnellA, jatko: <pienennellä>, äs: ä];

[perusmuoto: "rella", alku: "", luokka: johdin_rellA, jatko: <askarrella>, äs: a];
[perusmuoto: "rellä", alku: "", luokka: johdin_rellA, jatko: <askarrella>, äs: ä];

# Laskea => laskeskella.
[perusmuoto: "eskella", alku: "eskel", luokka: johdin_eskellA, jatko: <katsella>, äs: a];
[perusmuoto: "eskellä", alku: "eskel", luokka: johdin_eskellA, jatko: <katsella>, äs: ä];

# Kirjo(i)ttaa => kirjo(i)tella.
[perusmuoto: "ella", alku: "", luokka: johdin_oitellA, jatko: <aatella>, äs: a];
[perusmuoto: "ellä", alku: "", luokka: johdin_oitellA, jatko: <aatella>, äs: ä];

# Ammo(i)ttaa => ammo(i)tella.
[perusmuoto: "ella", alku: "", luokka: johdin_otellA, jatko: <aatella>, äs: a];
[perusmuoto: "ellä", alku: "", luokka: johdin_otellA, jatko: <aatella>, äs: ä];
