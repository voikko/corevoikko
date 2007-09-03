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
sym: voikko-fi_FI.sym
all: voikko-fi_FI.all suomi.inc all.inc subrule.inc voikko-fi_FI.pro
lex: suomi.inc subrule.inc voikko-fi_FI.pro
lex: erikoissanat.lex
lex: etuliitteet.lex
lex: seikkasanat.lex
lex: suhdesanat.lex
lex: lukusanat.lex
lex: lyhenteet.lex
lex: olla-ei.lex
lex: yhdyssanat.lex
lex: erikoiset.lex
lex: poikkeavat.lex
lex: lainen.lex
mor: voikko-fi_FI.mor suomi.inc mor.inc subrule.inc
mallex: set transmit-line "./transmit"
malaga: set transmit-line "./transmit"
malaga: set display-line "malshow"
mallex: set display-line "malshow"
mallex: set use-display yes
malaga: set use-display yes
malaga: set mor-pruning 30
mallex: set switch malli voikko
mallex: set switch voikko_debug no
malaga: set mor-incomplete no
## End-Internal-Configuration

