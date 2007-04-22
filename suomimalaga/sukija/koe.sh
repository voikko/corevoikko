#!/bin/bash

LC_CTYPE=fi_FI.utf8

fmt -1 -s $@ \
| gawk 'length ($0) > 0 {
#  s = $0
  sub (/^[^[:alpha:]]+/, "", $0)
#  t = $0
  sub (/[^[:alpha:]]+$/, "", $0)
#  print s, t, $0
  print $0
}'
