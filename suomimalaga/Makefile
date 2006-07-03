all: suomi.pro
	malmake suomi.pro

run: suomi.pro
	malaga suomi.pro

kehitys: transmit suomi.pro
	./shzh.sh
	malmake suomi.pro

voikko:
	malmake suomi-voikko.pro

clean:
	/bin/rm -f *_l core.* transmit

dist:
	cp sanat/omat.lex-jakeluversio sanat/omat.lex
	rm sanat/omat2.lex
	touch sanat/omat2.lex

transmit: transmit.cc
	g++ -g transmit.cc -o transmit


SM=suomi-malaga-0.7

tar:
	rm -f ${SM}.tar ${SM}.tar.bz2
	gtar cf ${SM}.tar ${SM}
	bzip2 ${SM}.tar

mcopy:
	mdel a:${SM}.tar.bz2; mcopy ${SM}.tar.bz2 a:; mdir
