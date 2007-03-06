include "suomi.inc";

define @sana1 := <nimisana, laatusana, nimi_laatusana, tavuviiva, etuliite>;
define @sana2 := @sana1 + <teonsana>;

include "lyhenteet.lex";
include "etuliitteet.lex";
include "seikkasanat.lex";
include "suhdesanat.lex";
include "erikoiset.lex";
include "poikkeavat.lex";
include "olla-ei.lex";
include "erikoissanat.lex";
include "lukusanat.lex";
include "yhdyssanat.lex";
include "lainen.lex";

include "suomi.lex";

include "joukahainen.lex";

