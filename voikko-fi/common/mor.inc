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
# Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.

include "subrule.inc";

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
