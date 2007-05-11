# Source files for Sukija version.
SUKIJA_SRC=sukija/README sukija/suomi.inc sukija/suomi.all \
           sukija/suomi.lex sukija/Makefile sukija/suomi.mor sukija/suomi.pro \
           sukija/suomi.sym sukija/sukija.copyright sukija/LUE.MINUT \
           sukija/sanat/11-19.lex sukija/sanat/erikoiset.lex \
           sukija/sanat/olla-ei.lex sukija/sanat/jokinen.lex \
           sukija/sanat/omat.lex sukija/sanat/sitaattilainat.txt \
           sukija/sanat/etuliitteet.lex sukija/sanat/poikkeavat.lex \
           sukija/sanat/lukusanat.lex sukija/sanat/joukahainen.lex \
           sukija/sanat/jokinen.sh sukija/sanat/lainen.lex \
           sukija/sanat/erikoissanat.lex sukija/sanat/lyhenteet.lex \
           sukija/sanat/yhdyssanat.lex sukija/sanat/latex.lex \
           sukija/sanat/erikoisalat/laaketiede.lex \
           sukija/sanat/erikoisalat/atk.lex sukija/generoi.sh sukija/inc/all.inc \
           sukija/inc/mor.inc sukija/inc/subrule.inc


# Files to ship only in the full source package
SRC_ONLY=ChangeLog CONTRIBUTORS LUE.MINUT Makefile README COPYING \
         voikko/voikko-fi_FI.all voikko/voikko-fi_FI.sym voikko/voikko-fi_FI.mor \
         voikko/voikko-fi_FI.lex voikko/voikko-fi_FI.pro voikko/voikkoutils.py \
         vocabulary/joukahainen.xml vocabulary/flags.txt voikko/generate_lex.py \
         voikko/hfconv.py \
         $(SUKIJA_SRC)

VOIKKO_BINDIST=voikko/voikko-fi_FI.pro voikko/voikko-fi_FI.lex_? voikko/voikko-fi_FI.mor_? \
	voikko/voikko-fi_FI.sym_?

# Shared files needed in Voikko
VOIKKO_COPY_FROM_COMMON=suomi.lex all.inc mor.inc subrule.inc suomi.inc
VOIKKO_COPY_FROM_VOCABULARY=erikoissanat.lex etuliitteet.lex seikkasanat.lex \
	suhdesanat.lex lukusanat.lex lyhenteet.lex olla-ei.lex yhdyssanat.lex erikoiset.lex \
	poikkeavat.lex lainen.lex

# Lexicon files generated for Voikko from Joukahainen
VOIKKO_LEX_FROM_XML=joukahainen atk laaketiede matluonnontiede kasvatustiede vieraskieliset

# Files to ship in the preprocessed source package
VOIKKO_FI_SRCDIST=$(VOIKKO_COPY_FROM_VOCABULARY) suomi.lex voikko-fi_FI.pro voikko-fi_FI.mor_l \
                  voikko-fi_FI.sym_l voikko-fi_FI.all_l voikko-fi_FI.lex suomi.inc subrule.inc \
                  $(patsubst %,%.lex,$(VOIKKO_LEX_FROM_XML))

# Files to ship in the source package (duplicates are allowed)
SRCDIST=$(SRC_ONLY) $(patsubst %,common/%,$(VOIKKO_COPY_FROM_COMMON)) \
	$(patsubst %,vocabulary/%,$(VOIKKO_COPY_FROM_VOCABULARY))

SM_VERSION=0.7.5

.PHONY: all clean update-vocabulary voikko voikko-install voikko-preprocessed-l-gzip dist-gzip

all: voikko

voikko: $(patsubst %,voikko/%,$(VOIKKO_COPY_FROM_VOCABULARY)) $(patsubst %,voikko/%,$(VOIKKO_COPY_FROM_COMMON)) \
	$(patsubst %,voikko/%.lex,$(VOIKKO_LEX_FROM_XML))
	@malmake voikko/voikko-fi_FI.pro 2>&1 | grep -v "Warning: No allomorphs generated."

$(patsubst %,voikko/%,$(VOIKKO_COPY_FROM_VOCABULARY)): voikko/%: vocabulary/%
	cp $^ $@

$(patsubst %,voikko/%,$(VOIKKO_COPY_FROM_COMMON)): voikko/%: common/%
	cp $^ $@

$(patsubst %,voikko/%.lex,$(VOIKKO_LEX_FROM_XML)): vocabulary/joukahainen.xml vocabulary/flags.txt \
	voikko/generate_lex.py voikko/voikkoutils.py voikko/hfconv.py
	python voikko/generate_lex.py

clean:
	rm -f voikko/*_l voikko/transmit voikko/voikkoutils.pyc voikko/hfconv.pyc
	rm -f "suomi-malaga-$(SM_VERSION).tar.gz" "voikko-fi-lsrc-$(SM_VERSION).tar.gz"
	rm -rf "suomi-malaga-$(SM_VERSION)" "voikko-fi-$(SM_VERSION)"
	rm -f $(patsubst %,voikko/%,$(VOIKKO_COPY_FROM_VOCABULARY))
	rm -f $(patsubst %,voikko/%,$(VOIKKO_COPY_FROM_COMMON))
	rm -f $(patsubst %,voikko/%.lex,$(VOIKKO_LEX_FROM_XML))


# Rules for creating the preprocessed source package

voikko-preprocessed-l-gzip: voikko-fi-lsrc-$(SM_VERSION).tar.gz

voikko-fi-lsrc-$(SM_VERSION).tar.gz: voikko $(patsubst %,voikko-fi-$(SM_VERSION)/%, $(sort $(VOIKKO_FI_SRCDIST)))
	tar c --group 0 --owner 0 voikko-fi-$(SM_VERSION) | gzip -9 > $@

$(patsubst %,voikko-fi-$(SM_VERSION)/%, $(sort $(VOIKKO_FI_SRCDIST))): voikko-fi-$(SM_VERSION)/%: voikko/%
	install --mode=755 -d voikko-fi-$(SM_VERSION)
	install --mode=644 -t voikko-fi-$(SM_VERSION) $^


# Rules for creating the source distribution

dist-gzip: suomi-malaga-$(SM_VERSION).tar.gz

suomi-malaga-$(SM_VERSION).tar.gz: $(patsubst %,suomi-malaga-$(SM_VERSION)/%, $(sort $(SRCDIST)))
	tar c --group 0 --owner 0 suomi-malaga-$(SM_VERSION) | gzip -9 > $@

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

# Vocabulary update
update-vocabulary:
	wget http://joukahainen.lokalisointi.org/sanastot/joukahainen.xml.gz -O - \
	| gunzip > vocabulary/joukahainen.xml


SUKIJA_COPY_FROM_VOCABULARY=\
erikoiset.lex \
erikoissanat.lex \
etuliitteet.lex \
jokinen.lex \
lainen.lex \
latex.lex \
lukusanat.lex \
lyhenteet.lex \
olla-ei.lex \
omat.lex \
poikkeavat.lex \
seikkasanat.lex \
suhdesanat.lex \
yhdyssanat.lex

sukija: $(patsubst %,sukija/voikonsanat/%,$(SUKIJA_COPY_FROM_VOCABULARY)) \
	$(patsubst %,sukija/voikonsanat/%.lex,$(VOIKKO_LEX_FROM_XML))
	@malmake sukija/suomi.pro 2>&1 | grep -v "Warning: No allomorphs generated."

$(patsubst %,sukija/voikonsanat/%,$(SUKIJA_COPY_FROM_VOCABULARY)): sukija/voikonsanat/%: vocabulary/%
	cp $^ $@

$(patsubst %,sukija/voikonsanat/%.lex,$(VOIKKO_LEX_FROM_XML)): vocabulary/joukahainen.xml vocabulary/flags.txt \
	voikko/generate_lex.py voikko/voikkoutils.py voikko/hfconv.py
	python voikko/generate_lex.py -s
	mv $(patsubst %,voikko/%.lex,$(VOIKKO_LEX_FROM_XML)) sukija/voikonsanat
