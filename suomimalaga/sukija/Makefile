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
	malmake suomi.pro 2>&1 | grep -v "Warning: No allomorphs generated."

run: suomi.pro
	malaga suomi.pro

kehitys: transmit suomi.pro
	malmake suomi.pro 2>&1 | grep -v "Warning: No allomorphs generated."

generoi:
	./generoi.sh
	malmake suomi.pro 2>&1 | grep -v "Warning: No allomorphs generated."
	
clean:
	/bin/rm -f *_l core.* transmit voikonsanat/* k1 k2 foo bar

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

vcopy:
	cp ../voikko/*_l ../voikko/*.pro ~/.voikko

k1: suomi.all_l suomi.lex_l suomi.mor_l suomi.sym_l
	cat ../../koesanat1.txt | malaga -m suomi.pro >k1

k2: suomi.all_l suomi.lex_l suomi.mor_l suomi.sym_l
	cat ../../koesanat2.txt | malaga -m suomi.pro >k2


diff:
	(diff ../../koesanat1.out k1; diff ../../koesanat2.out k2) >foo

cp1:
	cp k1 ../../koesanat1.out; cp k2 ../../koesanat2.out
