LEXICON = \
sanat/11-19.lex \
sanat/latex.lex \
sanat/jokinen.lex \
sanat/olla-ei.lex \
sanat/omat.lex \
voikonsanat/atk.lex \
voikonsanat/erikoiset.lex \
voikonsanat/erikoissanat.lex \
voikonsanat/etuliitteet.lex \
voikonsanat/huudahdussanat.lex \
voikonsanat/joukahainen.lex \
voikonsanat/kasvatustiede.lex \
voikonsanat/laaketiede.lex \
voikonsanat/lainen.lex \
voikonsanat/lukusanat.lex \
voikonsanat/lyhenteet.lex \
voikonsanat/matluonnontiede.lex \
voikonsanat/poikkeavat.lex \
voikonsanat/seikkasanat.lex \
voikonsanat/sidesanat.lex \
voikonsanat/suhdesanat.lex \
voikonsanat/taivutustäydennykset.lex \
voikonsanat/yhdyssanat.lex

all: suomi.pro $(LEXICON)
	malmake suomi.pro

run: suomi.pro
	malaga suomi.pro

kehitys: transmit suomi.pro
	malmake suomi.pro

generoi:
	./generoi.sh
	malmake suomi.pro
	
clean:
	/bin/rm -f *_l core.* transmit voikonsanat/* f1 s1 foo

transmit: ../common/transmit.cc
	g++ -g ../common/transmit.cc -o transmit


SM=suomimalaga

tar:
	rm -f ${SM}.tar ${SM}.tar.bz2
	gtar cf ${SM}.tar ${SM}
	bzip2 ${SM}.tar

mcopy:
	mdel a:${SM}.tar.bz2; mcopy ${SM}.tar.bz2 a:; mdir

bcopy:
	cp *_l *.pro ~/.sukija

f1: suomi.all_l suomi.lex_l suomi.mor_l suomi.sym_l
	cat ../../fiwiki.txt | malaga -m suomi.pro >f1

s1: suomi.all_l suomi.lex_l suomi.mor_l suomi.sym_l
	cat ../../sanat.txt | malaga -m suomi.pro >s1

diff:
	(diff ../../fiwiki.out f1; diff ../../sanat.out s1) >foo

cp1:
	cp f1 ../../fiwiki.out; cp s1 ../../sanat.out
