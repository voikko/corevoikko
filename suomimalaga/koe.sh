#!/bin/bash

LC_CTYPE=fi_FI@euro

fmt -1 -s $@ \
| gawk 'length ($0) > 0 {
  where = match ($0, /^([^[:alpha:]]*)([[:alpha:]-]*)([[:blank:][:punct:][:digit:]]*)$/, arr)
#  if (where == 0)
#  printf "A %2d %2d %2d $[%s] 0[%s] 1[%s] 2[%s] 3[%s]\n",
#         where, RSTART, RLENGTH, $0, arr[0], arr[1], arr[2], arr[3]
  print arr[2]
}' \


#| recode latin9..utf8 \
#| malaga -m suomi.pro
