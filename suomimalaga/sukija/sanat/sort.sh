#!/bin/bash

# Lajittele sanatiedostot aakkosjärjestykseen.
# Kaikkia tiedostoja ei saa lajitella!

sort etunimet.lex >etunimet.lex.tmp; mv etunimet.lex.tmp etunimet.lex
sort latex.lex >latex.lex.tmp; mv latex.lex.tmp latex.lex
sort nimet.lex >nimet.lex.tmp; mv nimet.lex.tmp nimet.lex
sort paikannimet.lex >paikannimet.lex.tmp; mv paikannimet.lex.tmp paikannimet.lex
sort sanat.lex >sanat.lex.tmp; mv sanat.lex.tmp sanat.lex
sort sukunimet.lex >sukunimet.lex.tmp; mv sukunimet.lex.tmp sukunimet.lex
