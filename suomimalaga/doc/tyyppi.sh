#!/bin/bash

grep 'perusmuoto:' ../sanat/*nimet.lex \
| gawk 'function rfind(s,c) {
    for (i = length(s); i > 0; i--) {
      if (substr(s,i,1) == c) return i
    }
    return 0;
}
function max(a,b) {return ((a > b) ? a : b);}
function max3(a,b,c) {return max(a,max(b,c));}
function vowel_harmony(s) {
  n = split($0,a);
  for (i = 1; i <= n; i++) {
    if (a[i] == "äs:") {
      if (match(a[i+1], "(a|ä|aä)") > 0) {
        return substr(a[i+1],RSTART,RLENGTH)
      }
    }
  }
  return "?"
}

# Harri Pitkäsen vokaalisointualgoritmi.
# VOWEL_BACK = "a", VOWEL_FRONT = "ä", VOWEL_BOTH == "aä"
function harri(w) {
  last_back = max3(rfind(w,"a"),rfind(w,"o"),rfind(w,"u"))
  last_ord_front = max(rfind(w,"ä"), rfind(w,"ö"))
  last_y = rfind(w,"y")
  if (last_back > 0 && max(last_ord_front,last_y) == 0) return "a";
  if (last_back == 0 && max(last_ord_front,last_y) > 0) return "ä";
  if (max3(last_back,last_ord_front,last_y) == 0) return "ä";
  if (last_y < max(last_back,last_ord_front)) {
    if (last_back > last_ord_front)
      return "a"
    else
      return "ä"
  }
  return "aä";
}
{
   w = substr($2,2,length($2)-3)
   v = (length($NF) == 3) ? substr($NF,1,1) : substr($NF,1,2)

   print w, vowel_harmony(w), harri(w)
}' | gawk '$2 != $3'
