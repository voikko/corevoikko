info: Voikko-Dictionary-Format: 1

## Begin-Malaga-Configuration
info: Language-Code: fi_FI
info: Language-Variant: standard
info: Description: Default morphology for Voikko
info: Copyright: 2006 Hannu Väisänen, Harri Pitkänen, Teemu Likonen
info: License: GPL version 2 or later
info: Update-URI: http://joukahainen.lokalisointi.org/update/index-1
info: Lex-Version: 1
## End-Malaga-Configuration

## Begin-User-Configuration
lex: voikko-fi_FI.lex suomi.lex
lex: sanat/joukahainen.lex
lex: #sanat/erikoisalat/atk.lex
lex: #sanat/erikoisalat/atk-lyhenteet.lex
lex: #sanat/erikoisalat/kasvatustiede.lex
lex: #sanat/erikoisalat/kasvatustiede-lyhenteet.lex
lex: #sanat/erikoisalat/laaketiede.lex
mallex: set switch vanhahkot_muodot yes
mallex: set switch vanhat_muodot no
## End-User-Configuration

## Begin-Internal-Configuration
sym: voikko-fi_FI.sym suomi.sym
all: voikko-fi_FI.all suomi.all suomi.inc subrule.inc voikko-fi_FI.pro
lex: suomi.inc subrule.inc voikko-fi_FI.pro
lex: sanat/erikoissanat.lex
lex: sanat/etuliitteet.lex
lex: sanat/lukusanat.lex
lex: sanat/lyhenteet.lex
lex: sanat/olla-ei.lex
lex: sanat/11-19.lex
lex: sanat/omat.lex
lex: sanat/yhdyssanat.lex
lex: sanat/erikoiset.lex
lex: sanat/poikkeavat.lex
lex: sanat/lainen.lex
mor: voikko-fi_FI.mor suomi.inc
mallex: set transmit-line "./transmit"
malaga: set transmit-line "./transmit"
malaga: set display-line "malshow"
mallex: set display-line "malshow"
mallex: set use-display yes
malaga: set use-display yes
malaga: set mor-pruning 20
malaga: set switch tulostus merkitse_yhdyssanat
malaga: set switch malli voikko
mallex: set switch malli voikko
malaga: set switch taivutus uusi
mallex: set switch taivutus uusi
## End-Internal-Configuration

