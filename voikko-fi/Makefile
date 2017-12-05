
# Build configuration. See README.md for more information.
PYTHON=python3
VVFST_BUILDDIR=vvfst
DESTDIR=/usr/lib/voikko
GENLEX_OPTS=
EXTRA_LEX=
VOIKKO_VARIANT=standard
VOIKKO_DESCRIPTION=suomi (perussanasto)
SM_PATCHINFO=
VANHAHKOT_MUODOT=yes
VANHAT_MUODOT=no
SUKIJAN_MUODOT=no
VOIKKO_DEBUG=no
VVFST_BASEFORMS=yes

# Include user's configuration
-include config.mak

# Version of voikko-fi
SM_VERSION=2.2
SM_BUILDDATE=$(shell date -R -u)

VVFST_LEXC=vvfst/suomi.lexc vvfst/olla-ei.lexc vvfst/poikkeavat.lexc vvfst/lukusanat.lexc \
           vvfst/asemosanat.lexc vvfst/seikkasanat.lexc vvfst/suhdesanat.lexc

# Files to ship only in the full source package
SRC_ONLY=ChangeLog CONTRIBUTORS Makefile README.md COPYING \
         common/voikkoutils.py \
         vocabulary/autocorrect/autocorrect.dtd vocabulary/autocorrect/fi.xml \
         vocabulary/joukahainen.xml vocabulary/flags.txt \
         common/hfconv.py common/generate_lex_common.py \
         vvfst/generate_lex.py vvfst/generate_taivutuskaavat.py vvfst/root.lexc \
         vvfst/taivutuskaavat.lexc.in vvfst/index.txt.in vvfst/filter_lexc.py \
         vvfst/autocorrect_to_lexc.py vvfst/autocorrect.foma.in \
         vvfst/main.foma.in $(VVFST_LEXC) vvfst/generate_sukija.py vvfst/poikkeavat-sukija.lexc

VVFST_BINDIST=$(VVFST_BUILDDIR)/index.txt $(VVFST_BUILDDIR)/mor.vfst $(VVFST_BUILDDIR)/autocorr.vfst

VVFST_GENERATED_LEXC_SUFFIXES=ee em ep es h l n nl t p a s c
VVFST_GENERATED_LEXC=$(patsubst %,$(VVFST_BUILDDIR)/joukahainen-%.lexc,$(VVFST_GENERATED_LEXC_SUFFIXES))

VVFST_TAIVUTUSKAAVAT_OPTS=
ifeq "$(VANHAT_MUODOT)" "yes"
	VVFST_TAIVUTUSKAAVAT_OPTS=--vanhat
endif
ifeq "$(VVFST_BASEFORMS)" "no"
	GENLEX_OPTS+=--no-baseform
endif

SUKIJA_LEX_FROM_XML=joukahainen atk laaketiede matluonnontiede kasvatustiede

# Files to ship in the source package (duplicates are allowed)
SRCDIST=$(SRC_ONLY)

.PHONY: all clean update-vocabulary dist-gzip vvfst vvfst-install vvfst-sukija vvfst-install-sukija

all: vvfst

clean:
	rm -f common/voikkoutils.pyc common/hfconv.pyc
	rm -f common/generate_lex_common.pyc
	rm -f "voikko-fi-$(SM_VERSION).tar.gz"
	rm -rf "voikko-fi-$(SM_VERSION)"
	rm -f $(VVFST_BUILDDIR)/all.att $(VVFST_BUILDDIR)/all.lexc $(VVFST_BUILDDIR)/main.foma $(VVFST_BUILDDIR)/mor.vfst
	rm -f $(VVFST_BUILDDIR)/autocorrect.att $(VVFST_BUILDDIR)/autocorrect.foma $(VVFST_BUILDDIR)/autocorrect.lexc
	rm -f $(VVFST_BUILDDIR)/autocorr.vfst $(VVFST_BUILDDIR)/taivutuskaavat.lexc $(VVFST_BUILDDIR)/index.txt
	rm -f $(VVFST_GENERATED_LEXC) $(VVFST_BUILDDIR)/joukahainen.lexc.stamp
	rm -f $(VVFST_BUILDDIR)/all-sukija.att $(VVFST_BUILDDIR)/all-sukija.lexc $(VVFST_BUILDDIR)/main-sukija.foma*
	rm -f $(VVFST_BUILDDIR)/generate_sukija.py.stamp $(VVFST_BUILDDIR)/sukija.fst $(VVFST_BUILDDIR)/test.out
	rm -f $(VVFST_BUILDDIR)/mor-sukija.vfst

# Rules for creating the source distribution

dist-gzip: voikko-fi-$(SM_VERSION).tar.gz

voikko-fi-$(SM_VERSION).tar.gz: $(patsubst %,voikko-fi-$(SM_VERSION)/%, $(sort $(SRCDIST)))
	tar c --group 0 --owner 0 voikko-fi-$(SM_VERSION) | gzip -9 > $@

dist-bzip2: voikko-fi-$(SM_VERSION).tar.bz2

voikko-fi-$(SM_VERSION).tar.bz2: $(patsubst %,voikko-fi-$(SM_VERSION)/%, $(sort $(SRCDIST)))
	tar c --group 0 --owner 0 voikko-fi-$(SM_VERSION) | bzip2 -9 > $@

$(patsubst %,voikko-fi-$(SM_VERSION)/%, $(sort $(SRCDIST))): voikko-fi-$(SM_VERSION)/%: %
	install -m 644 -D $^ $@

# Vocabulary update
update-vocabulary:
	wget https://joukahainen.puimula.org/sanastot/joukahainen.xml.gz -O - \
	| gunzip > vocabulary/joukahainen.xml


vvfst: $(VVFST_BUILDDIR)/mor.vfst $(VVFST_BUILDDIR)/autocorr.vfst $(VVFST_BUILDDIR)/index.txt

$(VVFST_BUILDDIR)/joukahainen.lexc.stamp: vocabulary/joukahainen.xml vocabulary/flags.txt vvfst/generate_lex.py \
	common/voikkoutils.py common/hfconv.py common/generate_lex_common.py
	@grep -B1 "ERROR: base form missing" vocabulary/joukahainen.xml | \
		sed -ne "s|^.*\"w\([^\"]*\)\".*|Base form missing: https://joukahainen.puimula.org/word/edit?wid=\1|p"
	$(PYTHON) vvfst/generate_lex.py $(GENLEX_OPTS) --destdir=$(VVFST_BUILDDIR)
	touch $@

$(VVFST_BUILDDIR)/taivutuskaavat.lexc: vvfst/taivutuskaavat.lexc.in vvfst/generate_taivutuskaavat.py
	$(PYTHON) vvfst/generate_taivutuskaavat.py $(GENLEX_OPTS) $(VVFST_TAIVUTUSKAAVAT_OPTS) --destdir=$(VVFST_BUILDDIR)

$(VVFST_BUILDDIR)/all.lexc: vvfst/filter_lexc.py vvfst/root.lexc $(VVFST_BUILDDIR)/taivutuskaavat.lexc $(VVFST_BUILDDIR)/joukahainen.lexc.stamp $(VVFST_LEXC)
	$(PYTHON) vvfst/filter_lexc.py $@ $(GENLEX_OPTS) $(VVFST_TAIVUTUSKAAVAT_OPTS) vvfst/root.lexc $(VVFST_BUILDDIR)/taivutuskaavat.lexc $(VVFST_GENERATED_LEXC) $(VVFST_LEXC)

$(VVFST_BUILDDIR)/autocorrect.lexc: vocabulary/autocorrect/fi.xml vvfst/autocorrect_to_lexc.py
	$(PYTHON) vvfst/autocorrect_to_lexc.py vocabulary/autocorrect/fi.xml $@

$(VVFST_BUILDDIR)/main.foma: vvfst/main.foma.in
	sed -e "s,VVFST_BUILDDIR,$(VVFST_BUILDDIR)," < $^ > $@

$(VVFST_BUILDDIR)/autocorrect.foma: vvfst/autocorrect.foma.in
	sed -e "s,VVFST_BUILDDIR,$(VVFST_BUILDDIR)," < $^ > $@

$(VVFST_BUILDDIR)/all.att: $(VVFST_BUILDDIR)/all.lexc $(VVFST_BUILDDIR)/main.foma
	foma -f $(VVFST_BUILDDIR)/main.foma 2>&1 | grep -v "defined but not used"
	! grep ']]' $@

$(VVFST_BUILDDIR)/autocorrect.att: $(VVFST_BUILDDIR)/autocorrect.lexc $(VVFST_BUILDDIR)/autocorrect.foma
	foma -f $(VVFST_BUILDDIR)/autocorrect.foma 2>&1 | grep -v "defined but not used"

$(VVFST_BUILDDIR)/mor.vfst: $(VVFST_BUILDDIR)/all.att
	cat $^ | sort -n | voikkovfstc -o $@

$(VVFST_BUILDDIR)/autocorr.vfst: $(VVFST_BUILDDIR)/autocorrect.att
	cat $^ | sort -n | voikkovfstc -o $@

VVFST_PRO_SEDSCRIPT="s/SM_VOIKKO_VARIANT/$(VOIKKO_VARIANT)/; \
	s/SM_VOIKKO_DESCRIPTION/$(VOIKKO_DESCRIPTION)/; \
	s/SM_VERSION/$(SM_VERSION)/; \
	s/SM_PATCHINFO/$(SM_PATCHINFO)/; \
	s/SM_BUILDCONFIG/$(subst /,\\/,GENLEX_OPTS=$(GENLEX_OPTS) EXTRA_LEX=$(EXTRA_LEX))/; \
	s/SM_BUILDDATE/$(SM_BUILDDATE)/"

$(VVFST_BUILDDIR)/index.txt: vvfst/index.txt.in
	sed -e $(VVFST_PRO_SEDSCRIPT) < $^ > $@


vvfst-install: vvfst
	install -m 755 -d $(DESTDIR)/5/mor-$(VOIKKO_VARIANT)
	install -m 644 $(VVFST_BINDIST) $(DESTDIR)/5/mor-$(VOIKKO_VARIANT)

vvfst-install-js-preload-file: vvfst-install
ifndef EMSCRIPTEN
	$(error EMSCRIPTEN is undefined, please source emsdk_set_env.sh first)
endif
	cd $(DESTDIR) && python $(EMSCRIPTEN)/tools/file_packager.py jsdict-$(VOIKKO_VARIANT).data --preload 5 --js-output=jsdict-$(VOIKKO_VARIANT).js

# Rules for creating vvfst morphology for Sukija indexer. See file
# $(VVFST_BUILDDIR)/generate_sukija.py for details.
#
vvfst-sukija: VOIKKO_VARIANT=sukija
vvfst-sukija: VOIKKO_DESCRIPTION=suomi (perussanasto) Sukija
vvfst-sukija: GENLEX_OPTS=--style=dialect,foreign,foreignloan,inappropriate,incorrect,international,old --min-frequency=10 --extra-usage=education,human,it,medicine,nature,orgname,science --sukija
vvfst-sukija: $(VVFST_BUILDDIR)/mor-sukija.vfst $(VVFST_BUILDDIR)/autocorr.vfst  $(VVFST_BUILDDIR)/index.txt

vvfst-sukija-ys: VOIKKO_VARIANT=sukija
vvfst-sukija-ys: VOIKKO_DESCRIPTION=suomi (perussanasto) Sukija
vvfst-sukija-ys: GENLEX_OPTS=--style=dialect,foreign,foreignloan,inappropriate,incorrect,international,old --min-frequency=10 --extra-usage=education,human,it,medicine,nature,orgname,science --sukija --sukija-ys
vvfst-sukija-ys: $(VVFST_BUILDDIR)/mor-sukija.vfst  $(VVFST_BUILDDIR)/index.txt

$(VVFST_BUILDDIR)/all-sukija.lexc: $(VVFST_BUILDDIR)/all.lexc $(VVFST_BUILDDIR)/generate_sukija.py.stamp $(VVFST_BUILDDIR)/poikkeavat-sukija.lexc
	$(PYTHON) vvfst/generate_sukija.py $(GENLEX_OPTS) --destdir=$(VVFST_BUILDDIR)

$(VVFST_BUILDDIR)/all-sukija.att: $(VVFST_BUILDDIR)/all-sukija.lexc $(VVFST_BUILDDIR)/main-sukija.foma
	foma -f $(VVFST_BUILDDIR)/main-sukija.foma 2>&1 | grep -v "defined but not used"
	! grep ']]' $@

$(VVFST_BUILDDIR)/main-sukija.foma: $(VVFST_BUILDDIR)/main.foma
	sed -e 's,/all,/all-sukija,' < $^ > $@

$(VVFST_BUILDDIR)/mor-sukija.vfst: $(VVFST_BUILDDIR)/all-sukija.att
	cat $^ | sort -n | voikkovfstc -o $@
	cp $@ $(VVFST_BUILDDIR)/mor.vfst

$(VVFST_BUILDDIR)/generate_sukija.py.stamp: $(VVFST_BUILDDIR)/generate_sukija.py
	touch $@

vvfst-install-sukija: VOIKKO_VARIANT=sukija
vvfst-install-sukija: VOIKKO_DESCRIPTION=suomi (perussanasto) Sukija
vvfst-install-sukija: vvfst-sukija
	install -m 755 -d $(DESTDIR)/5/mor-$(VOIKKO_VARIANT)
	install -m 644 $(VVFST_BINDIST) $(DESTDIR)/5/mor-$(VOIKKO_VARIANT)
