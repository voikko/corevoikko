#!/bin/bash

# Tehdään joki-sanaan päättyvistä paikannimistä
# -kelainen ja -kinen -loppuiset nimi-laatusanat.
# Esim. Utsjoki -> utsjokelainen, utsjokinen.

grep 'joki",' paikannimet.lex \
| gawk '{
  print "[perusmuoto:", substr($4,1,length($4)-2) "kelainen\", alku:",
        substr($4,1,length($4)-2) \
        "kelai\", luokka: nimi_laatusana, jatko: <nainen>, äs: a];"
  print "[perusmuoto:", substr($4,1,length($4)-2) "kinen\", alku:",
        substr($4,1,length($4)-2) \
        "ki\", luokka: nimi_laatusana, jatko: <nainen>, äs: a];"
}' >jokinen.lex

