#!/bin/bash

gawk '{
  n = length($4)-2;
#  print $2, substr($2,1,n) "§" substr($2,n+1), $4, n
  printf "%s %s", $1, substr($2,1,n) "|" substr($2,n+1)
  for (i = 5; i <= NF; i++) {
    printf " %s", $i
  }
  printf "\n"
}' $1

