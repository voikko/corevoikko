#!/bin/sh

if test $# -eq 0
then
  echo 'Tiedoston nimi puuttuu.'
  exit
fi


malaga -m suomi.pro < $1 \
  | grep -v unknown \
  | gawk '{print $3, $2}' \
  | sed -e 's/[":]//g' \
  | gawk '{printf "%-15s %s\n", $1, $2}' \
  | sort \
  | uniq
