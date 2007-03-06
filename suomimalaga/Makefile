
# Set some variables
VOIKKO_BINDIST=voikko/voikko-fi_FI.pro voikko/voikko-fi_FI.lex_? voikko/voikko-fi_FI.mor_? \
	voikko/voikko-fi_FI.sym_?
VOIKKO_COPY_FROM_COMMON=suomi.lex all.inc mor.inc subrule.inc suomi.inc
VOIKKO_COPY_FROM_VOCABULARY=joukahainen.lex erikoissanat.lex etuliitteet.lex seikkasanat.lex \
	suhdesanat.lex lukusanat.lex lyhenteet.lex olla-ei.lex yhdyssanat.lex erikoiset.lex \
	poikkeavat.lex lainen.lex


SM=suomi-malaga-svn

.PHONY: all clean voikko voikko-install

all: voikko

voikko: $(patsubst %,voikko/%,$(VOIKKO_COPY_FROM_VOCABULARY)) $(patsubst %,voikko/%,$(VOIKKO_COPY_FROM_COMMON))
	@malmake voikko/voikko-fi_FI.pro 2>&1 | grep -v "Warning: No allomorphs generated."

$(patsubst %,voikko/%,$(VOIKKO_COPY_FROM_VOCABULARY)): voikko/%: vocabulary/%
	cp $^ $@

$(patsubst %,voikko/%,$(VOIKKO_COPY_FROM_COMMON)): voikko/%: common/%
	cp $^ $@

clean:
	 $(RM) voikko/*_l transmit

transmit: transmit.cc
	g++ -g transmit.cc -o transmit

voikko-install: voikko
ifndef DESTDIR
	$(error DESTDIR must be specified)
endif
	install --mode=755 -d $(DESTDIR)
	install --mode=644 -t $(DESTDIR) $(VOIKKO_BINDIST)

