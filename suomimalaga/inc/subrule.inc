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


# Merkkijonon viimeinen merkki
# Tekijä: Björn Beutel.
subrule last($string):
  ! $string matches ".*", ".": $last;
  return $last;
end;

# Aputaulukko funktiolle luku_sijasta
define @luku_sijasta_muunnostaulu :=
[nimentö:     yksikkö,
 nimentö_t:   monikko,
 nimentö_tkA: monikko,
 omanto_n:    yksikkö,
 omanto_nkA:  yksikkö,
 omanto_ien:  monikko,
 omanto_jen:  monikko,
 omanto_en:   monikko,
 omanto_in:   monikko,
 omanto_ten:  monikko,
 omanto_iT:   monikko,
 omanto_idän: monikko,
 kohdanto_idät: monikko,
 kohdanto_t: monikko,
 osanto_A:    yksikkö,
 osanto_AA:   yksikkö,
 osanto_iA:   monikko,
 osanto_jA:   monikko,
 osanto_ttA:  yksikkö,
 osanto_itA:  monikko,
 osanto_tA:   yksikkö,
 olento_nA:   yksikkö,
 olento_inA:  monikko,
 tulento_ksi: yksikkö,
 tulento_iksi: monikko,
 sisäolento_ssA: yksikkö,
 sisäolento_issA: monikko,
 sisäolento_nA: yksikkö,
 sisäeronto_stA: yksikkö,
 sisäeronto_istA: monikko,
 sisäeronto_tA: yksikkö,
 sisätulento_Vn: yksikkö,
 sisätulento_VVn: yksikkö,
 sisätulento_hVn: yksikkö,
 sisätulento_iin: monikko,
 sisätulento_ihin: monikko,
 sisätulento_seen: yksikkö,
 sisätulento_isiin: monikko,
 sisätulento_sen: yksikkö,
 sisätulento_isin: monikko,
 sisätulento_nne: monikko,
 ulko_olento_llA: yksikkö,
 ulko_olento_illA: monikko,
 ulkoeronto_ltA: yksikkö,
 ulkoeronto_iltA: monikko,
 ulkotulento_lle: yksikkö,
 ulkotulento_ille: monikko,
 vajanto_ttA: yksikkö,
 vajanto_ittA: monikko,
 seuranto_ine: monikko,
 keinonto_n: yksikkö,
 keinonto_in: monikko,
 tulento_s: yksikkö
];

# Palauttaa annettua sijasymbolia vastaavan luvun
subrule luku_sijasta($sija):
  return @luku_sijasta_muunnostaulu.$sija;
end;
