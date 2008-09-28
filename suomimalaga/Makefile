
# Build configuration. See README for more information.
PYTHON=python
DESTDIR=/usr/lib/voikko
GENLEX_OPTS=
EXTRA_LEX=
VOIKKO_VARIANT=standard
VOIKKO_DESCRIPTION=Default morphology for Voikko
SM_PATCHINFO=
VANHAHKOT_MUODOT=yes
VANHAT_MUODOT=no
VOIKKO_DEBUG=no

# Suomi-malaga version
SM_VERSION=1.2
SM_BUILDDATE=$(shell date -R -u)

# Optional vocabulary files (needed in the source package)
OPTIONAL_LEX=atk-lyhenteet po-oikoluku

# Source files for Sukija version.
#           sukija/sanat/vieraskieliset.lex \
#

SUKIJA_SRC=sukija/generate_lex.py sukija/generoi.sh sukija/LUE.MINUT \
           sukija/Makefile sukija/malaga1.png sukija/malaga2.png \
           sukija/malaga3.png sukija/README sukija/sanat/11-19.lex \
           sukija/sanat/jokinen.lex sukija/sanat/jokinen.sh sukija/ei-sukija.txt \
           sukija/sanat/latex.lex sukija/sanat/olla-ei.lex sukija/sanat/omat.lex \
           sukija/sukija.py sukija/suomi.all \
           sukija/suomi.inc sukija/suomi.lex sukija/suomi.mor sukija/suomi.pro \
           sukija/suomi.sym sukija/suomi.tex


# Files to ship only in the full source package
SRC_ONLY=ChangeLog CONTRIBUTORS README.fi Makefile README COPYING \
         voikko/voikko-fi_FI.all voikko/voikko-fi_FI.sym voikko/voikko-fi_FI.mor \
         voikko/olla-ei.lex \
         voikko/voikko-fi_FI.lex voikko/voikko-fi_FI.pro.in common/voikkoutils.py \
         vocabulary/joukahainen.xml vocabulary/flags.txt voikko/generate_lex.py \
         common/hfconv.py common/generate_lex_common.py \
         $(SUKIJA_SRC) $(patsubst %,vocabulary/erikoisalat/%.lex,$(OPTIONAL_LEX))

VOIKKO_BINDIST=voikko/voikko-fi_FI.pro voikko/voikko-fi_FI.lex_? voikko/voikko-fi_FI.mor_? \
	voikko/voikko-fi_FI.sym_?

# Shared files needed in Voikko
VOIKKO_COPY_FROM_COMMON=suomi.lex all.inc mor.inc subrule.inc suomi.inc
VOIKKO_COPY_FROM_VOCABULARY=erikoissanat.lex etuliitteet.lex seikkasanat.lex \
	suhdesanat.lex lukusanat.lex lyhenteet.lex yhdyssanat.lex erikoiset.lex \
	poikkeavat.lex lainen.lex taivutustäydennykset.lex sidesanat.lex \
	huudahdussanat.lex

# Lexicon files generated for Voikko from Joukahainen
VOIKKO_LEX_FROM_XML=joukahainen atk laaketiede matluonnontiede kasvatustiede

# Files to ship in the preprocessed source package
VOIKKO_FI_SRCDIST=$(VOIKKO_COPY_FROM_VOCABULARY) suomi.lex voikko-fi_FI.pro voikko-fi_FI.mor_l \
                  voikko-fi_FI.sym_l voikko-fi_FI.all_l voikko-fi_FI.lex suomi.inc subrule.inc \
                  $(patsubst %,%.lex,$(VOIKKO_LEX_FROM_XML)) olla-ei.lex

# Files to ship in the source package (duplicates are allowed)
SRCDIST=$(SRC_ONLY) $(patsubst %,common/%,$(VOIKKO_COPY_FROM_COMMON)) \
	$(patsubst %,vocabulary/%,$(VOIKKO_COPY_FROM_VOCABULARY))


.PHONY: all clean update-vocabulary voikko voikko-install voikko-preprocessed-l-gzip dist-gzip

all: voikko

voikko: $(patsubst %,voikko/%,$(VOIKKO_COPY_FROM_VOCABULARY)) $(patsubst %,voikko/%,$(VOIKKO_COPY_FROM_COMMON)) \
	voikko/main.lex voikko/voikko-fi_FI.pro
	@malmake voikko/voikko-fi_FI.pro 2>&1 | grep -v "Warning: No allomorphs generated."

$(patsubst %,voikko/%,$(VOIKKO_COPY_FROM_VOCABULARY)): voikko/%: vocabulary/%
	cp $^ $@

$(patsubst %,voikko/%,$(VOIKKO_COPY_FROM_COMMON)): voikko/%: common/%
	cp $^ $@

ifeq "$(VOIKKO_DEBUG)" "yes"
    GENLEX_OPTS+= --debug
endif

voikko/generate_lex.flag: vocabulary/joukahainen.xml vocabulary/flags.txt voikko/generate_lex.py \
	common/voikkoutils.py common/hfconv.py common/generate_lex_common.py
	@grep -B1 "ERROR: base form missing" vocabulary/joukahainen.xml | \
		sed -ne "s|^.*\"w\([^\"]*\)\".*|Base form missing: http://joukahainen.lokalisointi.org/word/edit?wid=\1|p"
	$(PYTHON) voikko/generate_lex.py $(GENLEX_OPTS)
	touch $@

voikko/main.lex: voikko/generate_lex.flag $(EXTRA_LEX)
	cat voikko/joukahainen.lex $(EXTRA_LEX) > $@


# Rule to generate voikko-fi_FI.pro

VOIKKO_PRO_SEDSCRIPT="s/VANHAHKOT_MUODOT/$(VANHAHKOT_MUODOT)/; \
	s/VANHAT_MUODOT/$(VANHAT_MUODOT)/; \
	s/VOIKKO_DEBUG/$(VOIKKO_DEBUG)/; \
	s/SM_VOIKKO_VARIANT/$(VOIKKO_VARIANT)/; \
	s/SM_VOIKKO_DESCRIPTION/$(VOIKKO_DESCRIPTION)/; \
	s/SM_VERSION/$(SM_VERSION)/; \
	s/SM_PATCHINFO/$(SM_PATCHINFO)/; \
	s/SM_BUILDCONFIG/$(subst /,\\/,GENLEX_OPTS=$(GENLEX_OPTS) EXTRA_LEX=$(EXTRA_LEX))/; \
	s/SM_BUILDDATE/$(SM_BUILDDATE)/"

voikko/voikko-fi_FI.pro: voikko/voikko-fi_FI.pro.in
	sed -e $(VOIKKO_PRO_SEDSCRIPT) < $^ > $@

$(patsubst %,voikko/%.lex,$(VOIKKO_LEX_FROM_XML)): voikko/generate_lex.flag

clean:
	rm -f voikko/*_l voikko/transmit common/voikkoutils.pyc common/hfconv.pyc voikko/generate_lex.flag
	rm -f common/generate_lex_common.pyc sukija/sukija.pyc
	rm -f "suomi-malaga-$(SM_VERSION).tar.gz" "voikko-fi-lsrc-$(SM_VERSION).tar.gz"
	rm -rf "suomi-malaga-$(SM_VERSION)" "voikko-fi-$(SM_VERSION)"
	rm -f $(patsubst %,voikko/%,$(VOIKKO_COPY_FROM_VOCABULARY))
	rm -f $(patsubst %,voikko/%,$(VOIKKO_COPY_FROM_COMMON))
	rm -f $(patsubst %,voikko/%.lex,$(VOIKKO_LEX_FROM_XML)) voikko/main.lex voikko/voikko-fi_FI.pro
	rm -f sukija/*_l sukija/transmit sukija/sukija.pyc sukija/voikonsanat/*

# Rules for creating the preprocessed source package

voikko-preprocessed-l-gzip: voikko-fi-lsrc-$(SM_VERSION).tar.gz

voikko-fi-lsrc-$(SM_VERSION).tar.gz: voikko $(patsubst %,voikko-fi-$(SM_VERSION)/%, $(sort $(VOIKKO_FI_SRCDIST)))
	tar c --group 0 --owner 0 voikko-fi-$(SM_VERSION) | gzip -9 > $@

$(patsubst %,voikko-fi-$(SM_VERSION)/%, $(sort $(VOIKKO_FI_SRCDIST))): voikko-fi-$(SM_VERSION)/%: voikko/%
	install -m 755 -d voikko-fi-$(SM_VERSION)
	install -m 644 $^ voikko-fi-$(SM_VERSION)


# Rules for creating the source distribution

dist-gzip: suomi-malaga-$(SM_VERSION).tar.gz

suomi-malaga-$(SM_VERSION).tar.gz: $(patsubst %,suomi-malaga-$(SM_VERSION)/%, $(sort $(SRCDIST)))
	tar c --group 0 --owner 0 suomi-malaga-$(SM_VERSION) | gzip -9 > $@

dist-bzip2: suomi-malaga-$(SM_VERSION).tar.bz2

suomi-malaga-$(SM_VERSION).tar.bz2: $(patsubst %,suomi-malaga-$(SM_VERSION)/%, $(sort $(SRCDIST)))
	tar c --group 0 --owner 0 suomi-malaga-$(SM_VERSION) | bzip2 -9 > $@

$(patsubst %,suomi-malaga-$(SM_VERSION)/%, $(sort $(SRCDIST))): suomi-malaga-$(SM_VERSION)/%: %
	install -m 644 -D $^ $@

voikko/transmit: common/transmit.cc
	g++ -g common/transmit.cc -o voikko/transmit

voikko-install: voikko
	install -m 755 -d $(DESTDIR)
	install -m 644 $(VOIKKO_BINDIST) $(DESTDIR)

# Vocabulary update
update-vocabulary:
	wget http://joukahainen.lokalisointi.org/sanastot/joukahainen.xml.gz -O - \
	| gunzip > vocabulary/joukahainen.xml


SUKIJA_COPY_FROM_VOCABULARY=\
erikoiset.lex \
erikoissanat.lex \
etuliitteet.lex \
huudahdussanat.lex \
lainen.lex \
lukusanat.lex \
lyhenteet.lex \
poikkeavat.lex \
seikkasanat.lex \
sidesanat.lex \
suhdesanat.lex \
taivutustäydennykset.lex \
yhdyssanat.lex

sukija: $(patsubst %,sukija/voikonsanat/%,$(SUKIJA_COPY_FROM_VOCABULARY)) \
	$(patsubst %,sukija/voikonsanat/%.lex,$(VOIKKO_LEX_FROM_XML))
	@malmake sukija/suomi.pro 2>&1 | grep -v "Warning: No allomorphs generated."

$(patsubst %,sukija/voikonsanat/%,$(SUKIJA_COPY_FROM_VOCABULARY)): sukija/voikonsanat/%: vocabulary/%
	@mkdir -p sukija/voikonsanat
	cp $^ $@

$(patsubst %,sukija/voikonsanat/%.lex,$(VOIKKO_LEX_FROM_XML)): vocabulary/joukahainen.xml vocabulary/flags.txt \
	sukija/generate_lex.py common/voikkoutils.py common/hfconv.py common/generate_lex_common.py sukija/sukija.py \
	sukija/ei-sukija.txt
	@grep -B1 "ERROR: base form missing" vocabulary/joukahainen.xml | \
		sed -ne "s|^.*\"w\([^\"]*\)\".*|Base form missing: http://joukahainen.lokalisointi.org/word/edit?wid=\1|p"
	$(PYTHON) sukija/generate_lex.py
	-@grep "Malaga class not found" sukija/voikonsanat/joukahainen.lex
