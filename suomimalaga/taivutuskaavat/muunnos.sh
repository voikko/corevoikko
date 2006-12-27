#!/bin/sh

# Muutetaan tiedostot *.txt uuden formaatin mukaisiksi.


function f()
{
  grep -v '#' $1 \
  | gawk 'length($0) > 0' \
  | gawk '{
     if (NR > 1) printf "\n";
     if ($1 ~ /[*]/) {
       star = "*"
       printf "%s\n", substr($1,1,length($1)-1)
     }
     else {
       star = ""
       printf "%s\n", $1
    }
     for (i = 2; i <= NF; i++) {
       printf "          %s%s\n", $i, star
#      printf "                 %s%s\n", $i, star
     }
  }' \
  | sed -e "s/[*]/  ei_voikko/g"
}


for i in `ls [ab]/*.txt`
do
  echo $i
  f $i >$i.tmp
  mv -f $i.tmp $i
done
