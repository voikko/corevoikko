# This file is part of Suomi-malaga, description of Finnish morphology
# written in malaga.
# Copyright 2006 Harri Pitänen <hatapitk@iki.fi>
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

include "subrule.inc";

# Aputaulukko funktiolle sijamuoto_sijasta
define @sijamuoto_sijasta_muunnostaulu :=
[nimentö:     nimentö,
 nimentö_t:   nimentö,
 nimentö_tkA: nimentö,
 omanto_n:    omanto,
 omanto_nkA:  omanto,
 omanto_ien:  omanto,
 omanto_jen:  omanto,
 omanto_en:   omanto,
 omanto_in:   omanto,
 omanto_ten:  omanto,
 omanto_iT:   omanto,
 omanto_idän: omanto,
 kohdanto_idät: kohdanto,
 kohdanto_t: kohdanto,
 osanto_A:    osanto,
 osanto_AA:   osanto,
 osanto_iA:   osanto,
 osanto_jA:   osanto,
 osanto_ttA:  osanto,
 osanto_itA:  osanto,
 osanto_tA:   osanto,
 olento_nA:   olento,
 olento_inA:  olento,
 tulento_ksi: tulento,
 tulento_iksi: tulento,
 sisäolento_ssA: sisäolento,
 sisäolento_issA: sisäolento,
 sisäolento_nA: sisäolento,
 sisäeronto_stA: sisäeronto,
 sisäeronto_istA: sisäeronto,
 sisäeronto_tA: sisäeronto,
 sisätulento_Vn: sisätulento,
 sisätulento_VVn: sisätulento,
 sisätulento_hVn: sisätulento,
 sisätulento_iin: sisätulento,
 sisätulento_ihin: sisätulento,
 sisätulento_seen: sisätulento,
 sisätulento_isiin: sisätulento,
 sisätulento_sen: sisätulento,
 sisätulento_isin: sisätulento,
 sisätulento_nne: sisätulento,
 ulko_olento_llA: ulko_olento,
 ulko_olento_illA: ulko_olento,
 ulkoeronto_ltA: ulkoeronto,
 ulkoeronto_iltA: ulkoeronto,
 ulkotulento_lle: ulkotulento,
 ulkotulento_ille: ulkotulento,
 vajanto_ttA: vajanto,
 vajanto_ittA: vajanto,
 seuranto_ine: seuranto,
 keinonto_n: keinonto,
 keinonto_in: keinonto,
 tulento_s: tulento
];

# Palauttaa annettua sijasymbolia vastaavan sijamuodon
subrule sijamuoto_sijasta($sija):
  return @sijamuoto_sijasta_muunnostaulu.$sija;
end;


# Merkkijonon ensimmäinen merkki
# Tekijä: Björn Beutel.
subrule first ($string):
  ! $string matches ".": $first, ".*";
  return $first;
end;


# Palauttaa yes, jos annettu merkki on vokaali, muutoin palauttaa no
subrule vokaali($merkki):
  if $merkki in <"a", "e", "i", "o", "u", "y", "ä", "ö", "é"> then
    return yes;
  else
    return no;
  end;
end;


# Palauttaa yes, jos $sana voi liittyä listaan $vasen yhdyssanassa,
# jossa yhdysviiva käyttö on sallittua ja pakollista täsmälleen silloin,
# kun alkuosa päättyy samaan vokaaliin jolla loppuosa alkaa.
# Listan $vasen on sisällettävä vähintään yksi elementti.
subrule vokaaliehdollinen_yhdysviiva_ok($vasen, $sana):
  define $i := length($vasen);
  if ($vasen.$i.luokka = tavuviiva) then
    if ($i = 1) then
      return yes;
    end;
    if (vokaali(first($sana)) and first($sana) = last($vasen.($i-1).alku)) then
      return yes;
    else
      return no;
    end;
  else
    if (vokaali(first($sana)) and first($sana) = last($vasen.$i.alku)) then
      return no;
    else
      return yes;
    end;
  end;
end;
