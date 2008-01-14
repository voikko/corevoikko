LEXICON = \
sanat/11-19.lex \
sanat/latex.lex \
sanat/jokinen.lex \
sanat/olla-ei.lex \
sanat/omat.lex \
sanat/vieraskieliset.lex \
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

all: suomi.pro
	malmake suomi.pro

run: suomi.pro
	malaga suomi.pro

kehitys: transmit suomi.pro
	malmake suomi.pro

generoi:
	./generoi.sh
	malmake suomi.pro
	
clean:
	/bin/rm -f *_l core.* transmit voikonsanat/*
#	/bin/rm -f *_l core.* transmit [emr][123] voikonsanat/*

dist:
	cp sanat/omat.lex-jakeluversio sanat/omat.lex
	rm sanat/omat2.lex
	touch sanat/omat2.lex
	rm -rf .svn */.svn */*/.svn


transmit: ../common/transmit.cc
	g++ -g ../common/transmit.cc -o transmit


SM=voikko-suomimalaga

tar:
	rm -f ${SM}.tar ${SM}.tar.bz2
	gtar cf ${SM}.tar ${SM}
	bzip2 ${SM}.tar

mcopy:
	mdel a:${SM}.tar.bz2; mcopy ${SM}.tar.bz2 a:; mdir

bcopy:
	cp *_l *.pro ~/.sukija

koea:
	cat ../../sanat.koe | malaga -m suomi.pro >ka

koeb:
	cat ../../sanat.koe | malaga -m suomi.pro >kb


koe1:
	cat ../../ekirjat.txt | malaga -m suomi.pro >e1
	cat ../../m*8 | malaga -m suomi.pro >m1
	cat ../../raam.txt | malaga -m suomi.pro >r1

koe2:
	cat ../../ekirjat.txt | malaga -m suomi.pro >e2
	cat ../../m*8 | malaga -m suomi.pro >m2
	cat ../../raam.txt | malaga -m suomi.pro >r2

koe3:
	cat ../../ekirjat.txt | malaga -m suomi.pro >e3
	cat ../../m*8 | malaga -m suomi.pro >m3
	cat ../../raam.txt | malaga -m suomi.pro >r3

ekirjat:
	cat ../../ekirjat.txt | malaga -m suomi.pro


# Erojen löytyminen tiedostoista ei ole virhe, siksi "-" rivien alussa.
diff:
	-diff m1 m2
	-diff r1 r2
	-diff e1 e2

diff2:
	-diff m2 m3
	-diff r2 r3
	-diff e2 e3

move:
	mv -f e2 e1; mv -f r2 r1; mv -f m2 m1

move2:
	mv -f e3 e2; mv -f r3 r2; mv -f m3 m2
