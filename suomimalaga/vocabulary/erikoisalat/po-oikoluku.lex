# Merkkijonoja ja sanoja, jotka voi hyväksyä oikoluettaessa po-tiedostoa
# pofilterin avulla.
# © 2007 Harri Pitkänen <hatapitk@iki.fi>
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

# Kaksoispisteen perässä olevia tavallisia taivutuspäätteitä. Pofilter
# jakaa sanat kaksoispisteen kohdalta.
[perusmuoto: "ssa", alku: "ssa", luokka: sidesana, jatko: <loppu>, äs: a];
[perusmuoto: "ssä", alku: "ssä", luokka: sidesana, jatko: <loppu>, äs: ä];
[perusmuoto: "sta", alku: "sta", luokka: sidesana, jatko: <loppu>, äs: a];
[perusmuoto: "stä", alku: "stä", luokka: sidesana, jatko: <loppu>, äs: ä];
[perusmuoto: "lla", alku: "lla", luokka: sidesana, jatko: <loppu>, äs: a];
[perusmuoto: "llä", alku: "llä", luokka: sidesana, jatko: <loppu>, äs: ä];
[perusmuoto: "lta", alku: "lta", luokka: sidesana, jatko: <loppu>, äs: a];
[perusmuoto: "ltä", alku: "ltä", luokka: sidesana, jatko: <loppu>, äs: ä];
[perusmuoto: "lle", alku: "lle", luokka: sidesana, jatko: <loppu>, äs: aä];

# XML-entiteettejä
[perusmuoto: "gt", alku: "gt", luokka: sidesana, jatko: <loppu>, äs: ä];
[perusmuoto: "lt", alku: "lt", luokka: sidesana, jatko: <loppu>, äs: ä];

# HTML-elementtejä ja muita vastaavia
[perusmuoto: "application", alku: "application", luokka: sidesana, jatko: <loppu>, äs: a];
[perusmuoto: "big", alku: "big", luokka: sidesana, jatko: <loppu>, äs: ä];
[perusmuoto: "emphasis", alku: "emphasis", luokka: sidesana, jatko: <loppu>, äs: a];
[perusmuoto: "phrase", alku: "phrase", luokka: sidesana, jatko: <loppu>, äs: a];
[perusmuoto: "quote", alku: "quote", luokka: sidesana, jatko: <loppu>, äs: ä];
[perusmuoto: "small", alku: "small", luokka: sidesana, jatko: <loppu>, äs: a];
[perusmuoto: "span", alku: "span", luokka: sidesana, jatko: <loppu>, äs: a];

# TLD-päätteitä
[perusmuoto: "com", alku: "com", luokka: sidesana, jatko: <loppu>, äs: a];
[perusmuoto: "org", alku: "org", luokka: sidesana, jatko: <loppu>, äs: a];
[perusmuoto: "net", alku: "net", luokka: sidesana, jatko: <loppu>, äs: a];
