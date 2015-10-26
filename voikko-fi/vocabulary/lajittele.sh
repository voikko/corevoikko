#!/bin/bash

# Lajittele sanat ääkkösjärjestykseen.
# Kaikkia tiedostoja ei lajitella.

function f()
{
  echo $1
  mv $1.lex $1.foo
  ../1 <$1.foo >$1.lex
}

f etunimet
f latex
f lyhenteet1
f lyhenteet2
f nimet
f omat
f paikannimet
f sanat
f sukunimet

exit

../1 <etunimet.lex    >etunimet.lex.zzz
../1 <latex.lex       >latex.lex.zzz
../1 <lukusanat.lex   >lukusanat.lex.zzz
../1 <lyhenteet.lex   >lyhenteet.lex.zzz
../1 <lyhenteet1.lex  >lyhenteet1.lex.zzz
../1 <lyhenteet2.lex  >lyhenteet2.lex.zzz
../1 <nimet.lex       >nimet.lex.zzz
../1 <paikannimet.lex >paikannimet.lex.zzz
../1 <sanat.lex       >sanat.lex.zzz
../1 <sukunimet.lex   >sukunimet.lex.zzz
