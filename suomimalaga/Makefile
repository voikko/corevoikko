SM=suomi-malaga-svn

.PHONY: all voikko sukija kehitys run clean dist tar

all: voikko sukija

kehitys: all transmit

voikko:
	@malmake voikko-fi_FI.pro 2>&1 | grep -v "Warning: No allomorphs generated."

sukija:
	malmake suomi-sukija.pro

run: sukija
	malaga suomi.pro

clean:
	/bin/rm -f *_l core.* transmit

dist:
	cp sanat/omat.lex-jakeluversio sanat/omat.lex
	rm sanat/omat2.lex
	touch sanat/omat2.lex

transmit: transmit.cc
	g++ -g transmit.cc -o transmit

tar:
	rm -f ${SM}.tar ${SM}.tar.bz2
	gtar cf ${SM}.tar ${SM}
	bzip2 ${SM}.tar

mcopy:
	mdel a:${SM}.tar.bz2; mcopy ${SM}.tar.bz2 a:; mdir

bcopy:
	cp *_l *.pro ~/.voikko
