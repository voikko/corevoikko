#!/bin/sh

# Tarkista, että jokaisessa esimerkkisanassa on kaksi +-merkkiä.

fmt -1 [0-9]*txt \
| grep -F '+' \
| gawk '{
  n = 0
  for (i = 1; i <= length($0); i++) {
    if (substr($0,i,1) == "+") n++;
  }
  print n, $0
}'
