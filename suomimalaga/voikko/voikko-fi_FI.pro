info: Voikko-Dictionary-Format: 1

## Begin-Malaga-Configuration
info: Language-Code: fi_FI
info: Language-Variant: standard
info: Description: Default morphology for Voikko
info: Copyright: 2006 Hannu Väisänen, Harri Pitkänen, Teemu Likonen
info: License: GPL version 2 or later
info: Update-URI: http://joukahainen.lokalisointi.org/update/index-2
info: Lex-Version: 2
## End-Malaga-Configuration

## Begin-User-Configuration
lex: voikko-fi_FI.lex suomi.lex
lex: joukahainen.lex
lex: #atk.lex
lex: #atk-lyhenteet.lex
lex: #kasvatustiede.lex
lex: #kasvatustiede-lyhenteet.lex
lex: #laaketiede.lex
lex: #matluonnontiede.lex
mallex: set switch vanhahkot_muodot yes
mallex: set switch vanhat_muodot no
## End-User-Configuration

## Begin-Internal-Configuration
sym: voikko-fi_FI.sym suomi.sym
all: voikko-fi_FI.all suomi.all ../suomi.inc ../inc/all.inc ../inc/subrule.inc voikko-fi_FI.pro
lex: ../suomi.inc ../inc/subrule.inc voikko-fi_FI.pro
lex: ../vocabulary/erikoissanat.lex
lex: ../vocabulary/etuliitteet.lex
lex: ../vocabulary/seikkasanat.lex
lex: ../vocabulary/suhdesanat.lex
lex: ../vocabulary/lukusanat.lex
lex: ../vocabulary/lyhenteet.lex
lex: ../vocabulary/olla-ei.lex
lex: ../vocabulary/yhdyssanat.lex
lex: ../vocabulary/erikoiset.lex
lex: ../vocabulary/poikkeavat.lex
lex: ../vocabulary/lainen.lex
mor: voikko-fi_FI.mor ../suomi.inc ../inc/mor.inc ../inc/subrule.inc
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
## End-Internal-Configuration

