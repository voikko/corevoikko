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

define @yäö := "[yýÿüäöøõ]";

### ABCDEFGHIJKLMNOPQRSTUVWXYZÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞ
### abcdefghijklmnopqrstuvwxyzàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþßÿ


define @erisnimi := <etunimi, sukunimi, paikannimi, nimi>;
define @nimisana := <nimisana, nimi_laatusana> + @erisnimi;

define @laatusana := <laatusana, nimi_laatusana>;

define @kestämän_tekijäpääte := <kestämän_tekijäpääte_y1,
                                 kestämän_tekijäpääte_y2,
                                 kestämän_tekijäpääte_y3,
                                 kestämän_tekijäpääte_y3_Vpi,
                                 kestämän_tekijäpääte_y3_Vvi,
                                 kestämän_tekijäpääte_y3_pi,
                                 kestämän_tekijäpääte_y3_vi,
                                 kestämän_tekijäpääte_m1,
                                 kestämän_tekijäpääte_m2,
                                 kestämän_tekijäpääte_m3>;

define @kestämän_tekijäpääte_heikko_aste := <kestämän_tekijäpääte_y1, kestämän_tekijäpääte_y2,
                                             kestämän_tekijäpääte_m1, kestämän_tekijäpääte_m2>;

define @kestämän_tekijäpääte_vahva_aste := <kestämän_tekijäpääte_y3,
                                            kestämän_tekijäpääte_y3_Vpi,
                                            kestämän_tekijäpääte_y3_Vvi,
                                            kestämän_tekijäpääte_y3_pi,
                                            kestämän_tekijäpääte_y3_vi,
                                            kestämän_tekijäpääte_m3>;

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

define @laatutapa_2_stU := <laatutapa_2_stU_juostu, laatutapa_2_stU_nuolaistu>;

define @laatutapa_2_tU := <laatutapa_2_tU_dU,
                           laatutapa_2_tU_lU_katseltu,
                           laatutapa_2_tU_lU_oltu,
                           laatutapa_2_tU_nU,
                           laatutapa_2_tU_rU>;

define @laatutapa_1 := <laatutapa_1_vA, laatutapa_1_vE, laatutapa_1_ttA, laatutapa_1_tA>;

define @laatutapa_2 := <laatutapa_2_lUt, laatutapa_2_nUt, laatutapa_2_rUt, laatutapa_2_sUt,
                        laatutapa_2_ttU> +
                       @laatutapa_2_tU +
                       @laatutapa_2_stU;

define @tositapa_4_d  := <tositavan_kestämä_dAAn,
                          tositavan_kestämä_dAAn_kielto,
                          tositavan_kertoma_tiin>;
define @tositapa_4_l  := <tositavan_kestämä_lAAn,
                          tositavan_kestämä_lAAn_kielto,
                          tositavan_kertoma_tiin>;
define @tositapa_4_n  := <tositavan_kestämä_nAAn,
                          tositavan_kestämä_nAAn_kielto,
                          tositavan_kertoma_tiin>;
define @tositapa_4_r  := <tositavan_kestämä_rAAn,
                          tositavan_kestämä_rAAn_kielto,
                          tositavan_kertoma_tiin>;
define @tositapa_4_t  := <tositavan_kestämä_tAAn,
                          tositavan_kestämä_tAAn_kielto,
                          tositavan_kertoma_tiin>;
define @tositapa_4_tt := <tositavan_kestämä_tAAn,
                          tositavan_kestämä_tAAn_kielto,
                          tositavan_kertoma_ttiin>;

define @tositapa := @kestämän_tekijäpääte + @kertoman_tekijäpääte
                    + <tositavan_kertoma_tiin, tositavan_kertoma_ttiin,
                       tositavan_kestämä_dAAn, tositavan_kestämä_lAAn,
                       tositavan_kestämä_nAAn, tositavan_kestämä_rAAn,
                       tositavan_kestämä_tAAn,
                       tositavan_kestämä_dAAn_kielto, tositavan_kestämä_lAAn_kielto,
                       tositavan_kestämä_nAAn_kielto, tositavan_kestämä_rAAn_kielto,
                       tositavan_kestämä_tAAn_kielto>;


define @sisäpaikallissijat_Vn  := <sisäolento_ssA,  sisäeronto_stA, sisätulento_Vn>;
define @sisäpaikallissijat_hVn := <sisäolento_ssA,  sisäeronto_stA, sisätulento_hVn>;
define @ulkopaikallissijat     := <ulko_olento_llA, ulkoeronto_ltA, ulkotulento_lle>;
define @ulkopaikallissijat_monikko := <ulko_olento_illA, ulkoeronto_iltA, ulkotulento_ille>;

define @heikkoasteiset_paikallissijat := <sisäolento_ssA,  sisäeronto_stA> + @ulkopaikallissijat;

define @lukusana := <lukusana>;

define @yhdyssana := @nimisana + <laatusana, nimi_laatusana, teonsana,
                                  etuliite, tavuviiva>;

#define @lukusana1_jatko := <lukusana3, omistusliite, liitesana, loppu> + @yhdyssana;
#define @lukusana2_jatko := <omistusliite, liitesana, loppu>;
#define @lukusana3_jatko := <lukusana1, omistusliite, liitesana, loppu> + @yhdyssana;

define @johdin_stU := <johdin_stU_juostu, johdin_stU_nuolaistu>;

define @johdin_jA := <johdin_jA_myyjä, johdin_jA_kulkija, johdin_jA_kantaja>;
define @johdin_mA := <johdin_mA, johdin_mA_saama>;
define @johdin_nUt := <johdin_lUt, johdin_nUt, johdin_rUt, johdin_sUt>;

define @johdin_tU := <johdin_tU_dU,
                      johdin_tU_lU_katseltu,
                      johdin_tU_lU_oltu,
                      johdin_tU_nU,
                      johdin_tU_rU>;

define @johdin_laatutapa := @johdin_nUt + @johdin_tU + @johdin_stU +
                            <johdin_ttU, johdin_vA, johdin_vE>;

define @johdin_tAvA := <johdin_tAvA, johdin_ttAvA,
                        johdin_tAvAinen, johdin_ttAvAinen>;

define @teonsanasta_johdettu_nimi_tai_laatusana :=
       <johdin_jA_kantaja, johdin_jA_kulkija, johdin_jA_myyjä, johdin_lUt,
        johdin_mA, johdin_mA_saama, johdin_nA, johdin_ntA, johdin_nti,
        johdin_nUt, johdin_O, johdin_Os, johdin_rUt, johdin_stU_juostu,
        johdin_stU_nuolaistu, johdin_sUt, johdin_tAvA, johdin_tAvAinen,
        johdin_ttAvA, johdin_ttAvAinen, johdin_ttU, johdin_tU_dU,
        johdin_tU_lU_katseltu, johdin_tU_lU_oltu, johdin_tU_nU, johdin_tU_rU,
        johdin_U_arvelu, johdin_Us_ksen, johdin_vA, johdin_vAinen, johdin_vE,
        nimitapa_4>;

define @teonsanasta_johdettu_nimisana :=
       <johdin_jA_kantaja, johdin_jA_kulkija, johdin_jA_myyjä,
        johdin_mA, johdin_mA_saama, johdin_nA, johdin_ntA, johdin_nti,
        johdin_O, johdin_Os,
        johdin_U_arvelu, johdin_Us_ksen,
        nimitapa_4>;


define @teonsanasta_johdettu_laatusana :=
       <johdin_lUt,
        johdin_nUt, johdin_rUt, johdin_stU_juostu,
        johdin_stU_nuolaistu, johdin_sUt, johdin_tAvA, johdin_tAvAinen,
        johdin_ttAvA, johdin_ttAvAinen, johdin_ttU, johdin_tU_dU,
        johdin_tU_lU_katseltu, johdin_tU_lU_oltu, johdin_tU_nU, johdin_tU_rU,
        johdin_vA, johdin_vAinen, johdin_vE>;

define @nimisanasta_johdettu_nimisana :=
       <johdin_tAr, johdin_ittAin, johdin_UUs> # Tekijä => tekijyys.
       + @johdin_jA;

define @nimisanasta_johdettu_laatusana := <johdin_inen, johdin_tOn>;

define @laatusanasta_johdettu_nimisana := <johdin_Us, johdin_UUs>;

define @laatusanasta_johdettu_laatusana := <johdin_nlainen>;

define @nimi_laatusanan_johdin :=
       <johdin_lAinen, johdin_mAinen, johdin_llinen>;
