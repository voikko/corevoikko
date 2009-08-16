include "suomi.inc";

define @sana1 := <nimisana, tavuviiva>;
define @sana2 := @sana1 + <teonsana>;

include "lyhenteet.lex";

# Etuliite (nimisanat)
define @eln := <nimisana>;
# Etuliite (laatusanat)
define @ell := <laatusana>;
# Etuliite (teonsanat)
define @elt := <teonsana>;
# Etuliite (teonsanojen nimi- ja laatusanajohdokset)
define @eltj := <teonsanan_johdoksen_etuliite>;
include "etuliitteet.lex";

include "seikkasanat.lex";
include "suhdesanat.lex";
include "erikoiset.lex";
include "poikkeavat.lex";
include "olla-ei.lex";
include "erikoissanat.lex";
include "lukusanat.lex";
include "lainen.lex";
include "taivutustaydennykset.lex";
include "huudahdussanat.lex";
include "sidesanat.lex";

include "suomi.lex";
include "yhdyssanat.lex";

include "main.lex";

