
# Files to ship only in the full source package
SRC_ONLY=ChangeLog CONTRIBUTORS LUE.MINUT Makefile README COPYING

VOIKKO_BINDIST=voikko/voikko-fi_FI.pro voikko/voikko-fi_FI.lex_? voikko/voikko-fi_FI.mor_? \
	voikko/voikko-fi_FI.sym_?

# Shared files needed in Voikko
VOIKKO_COPY_FROM_COMMON=suomi.lex all.inc mor.inc subrule.inc suomi.inc
VOIKKO_COPY_FROM_VOCABULARY=joukahainen.lex erikoissanat.lex etuliitteet.lex seikkasanat.lex \
	suhdesanat.lex lukusanat.lex lyhenteet.lex olla-ei.lex yhdyssanat.lex erikoiset.lex \
	poikkeavat.lex lainen.lex

# Files to ship in the source package (duplicates are allowed)
SRCDIST=$(SRC_ONLY) $(patsubst %,common/%,$(VOIKKO_COPY_FROM_COMMON)) \
	$(patsubst %,vocabulary/%,$(VOIKKO_COPY_FROM_VOCABULARY))

SM_VERSION=svn

.PHONY: all clean voikko voikko-install dist-gzip

all: voikko

voikko: $(patsubst %,voikko/%,$(VOIKKO_COPY_FROM_VOCABULARY)) $(patsubst %,voikko/%,$(VOIKKO_COPY_FROM_COMMON))
	@malmake voikko/voikko-fi_FI.pro 2>&1 | grep -v "Warning: No allomorphs generated."

$(patsubst %,voikko/%,$(VOIKKO_COPY_FROM_VOCABULARY)): voikko/%: vocabulary/%
	cp $^ $@

$(patsubst %,voikko/%,$(VOIKKO_COPY_FROM_COMMON)): voikko/%: common/%
	cp $^ $@

clean:
	$(RM) voikko/*_l voikko/transmit


# Rules for creating the source distribution

dist-gzip: suomi-malaga-$(SM_VERSION).tar.gz

suomi-malaga-$(SM_VERSION).tar.gz: $(patsubst %,suomi-malaga-$(SM_VERSION)/%, $(sort $(SRCDIST)))
	tar czf $@ --group 0 --owner 0 suomi-malaga-$(SM_VERSION)

$(patsubst %,suomi-malaga-$(SM_VERSION)/%, $(sort $(SRCDIST))): suomi-malaga-$(SM_VERSION)/%: %
	install --mode=644 -D $^ $@

voikko/transmit: common/transmit.cc
	g++ -g common/transmit.cc -o voikko/transmit

voikko-install: voikko
ifndef DESTDIR
	$(error DESTDIR must be specified)
endif
	install --mode=755 -d $(DESTDIR)
	install --mode=644 -t $(DESTDIR) $(VOIKKO_BINDIST)

