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


define @a := "[aàáâã]";
define @e := "[eèéêëæ]";
define @i := "[iìíîï]";
define @o := "[oòóôå]";  # Myös ruåtsalainen oo.
define @u := "[uùúû]";
define @y := "[yýÿü]";
define @ä := "[ä]";
define @ö := "[öøõ]";

define @aä := "[aàáâãä]";
define @uy := "[uùúûyýÿü]";
define @oö := "[oòóôåöøõ]";  # Myös ruåtsalainen oo.

define @ääntiö := "[aàáâãeèéêëœæiìíîïoòóôuùúûyýÿüåäöøõ]";
define @kerake := "[bcdfghjklmnpqrstvwxzšžçðñþß]";

define @uao := "[uùúûaàáâãoòóôå]";
define @yäö := "[yýÿüäöøõ]";

### ABCDEFGHIJKLMNOPQRSTUVWXYZÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞ
### abcdefghijklmnopqrstuvwxyzàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþßÿ


define @erisnimi := <etunimi, sukunimi, paikannimi, nimi>;
define @nimisana := <nimisana, nimi_laatusana> + @erisnimi;

define @laatusana := <laatusana, nimi_laatusana>;

define @kestämän_tekijäpääte := <kestämän_tekijäpääte_y1,
                                 kestämän_tekijäpääte_y2,
                                 kestämän_tekijäpääte_y3,
                                 kestämän_tekijäpääte_m1,
                                 kestämän_tekijäpääte_m2,
                                 kestämän_tekijäpääte_m3>;

define @kestämän_tekijäpääte_heikko_aste := <kestämän_tekijäpääte_y1, kestämän_tekijäpääte_y2,
                                             kestämän_tekijäpääte_m1, kestämän_tekijäpääte_m2>;

define @kestämän_tekijäpääte_vahva_aste := <kestämän_tekijäpääte_y3, kestämän_tekijäpääte_m3>;

define @tositavan_tekijäpääte_4 := <tositavan_kestämä_dAAn,
                                    tositavan_kestämä_lAAn,
                                    tositavan_kestämä_nAAn,
                                    tositavan_kestämä_rAAn,
                                    tositavan_kestämä_tAAn,
                                    tositavan_kestämä_dAAn_kielto,
                                    tositavan_kestämä_lAAn_kielto,
                                    tositavan_kestämä_nAAn_kielto,
                                    tositavan_kestämä_rAAn_kielto,
                                    tositavan_kestämä_tAAn_kielto,
                                    tositavan_kertoma_tiin,
                                    tositavan_kertoma_ttiin>;


define @kertoman_tekijäpääte := <kertoman_tekijäpääte_y1,
                                 kertoman_tekijäpääte_y2,
                                 kertoman_tekijäpääte_y3,
                                 kertoman_tekijäpääte_m1,
                                 kertoman_tekijäpääte_m2,
                                 kertoman_tekijäpääte_m3>;

define @kertoman_tekijäpääte_heikko_aste := <kertoman_tekijäpääte_y1,
                                             kertoman_tekijäpääte_y2,
                                             kertoman_tekijäpääte_m1,
                                             kertoman_tekijäpääte_m2>;
                                             

define @kertoman_tekijäpääte_vahva_aste := <kertoman_tekijäpääte_y3,
                                            kertoman_tekijäpääte_m3>;


define @sisäpaikallissijat_Vn  := <sisäolento_ssA,  sisäeronto_stA, sisätulento_Vn>;
define @sisäpaikallissijat_hVn := <sisäolento_ssA,  sisäeronto_stA, sisätulento_hVn>;

define @ulkopaikallissijat     := <ulkopaikallissija_llA>;
define @ulkopaikallissijat_monikko := <ulkopaikallissija_illA>;
define @sija_monikko_1 := <sija_monikko_1>;

define @heikkoasteiset_paikallissijat := <sisäolento_ssA,  sisäeronto_stA> + @ulkopaikallissijat;

define @yhdyssana := @nimisana + <laatusana, nimi_laatusana, teonsana,
                                  etuliite, tavuviiva>;

define @johdin_jA := <johdin_jA_myyjä, johdin_jA_kulkija, johdin_jA_kantaja>;
define @johdin_nUt := <johdin_lUt, johdin_nUt, johdin_rUt, johdin_sUt>;
