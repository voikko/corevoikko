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



# Merkkijonon ensimmäinen merkki
# Tekijä: Björn Beutel.
subrule first ($string):
  ! $string matches ".": $first, ".*";
  return $first;
end;


# Merkkijonon viimeinen merkki
# Tekijä: Björn Beutel.
subrule last($string):
  ! $string matches ".*", ".": $last;
  return $last;
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

