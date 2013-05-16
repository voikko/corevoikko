
# Build configuration. See README for more information.
PYTHON=python
VOIKKO_BUILDDIR=voikko
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

# Include user's configuration
-include config.mak

# Suomi-malaga version
SM_VERSION=1.13
SM_BUILDDATE=$(shell date -R -u)

# Optional vocabulary files (needed in the source package)
OPTIONAL_LEX=atk-lyhenteet po-oikoluku linux-distributions science-misc

# Source files for Sukija version.
SUKIJA_SRC=sukija/generate_lex.py sukija/generoi.sh sukija/LUE.MINUT \
           sukija/Makefile sukija/malaga1.png sukija/malaga2.png \
           sukija/malaga3.png sukija/README sukija/sanat/11-19.lex \
           sukija/sanat/jokinen.lex sukija/sanat/jokinen.sh sukija/ei-sukija.txt \
           sukija/sanat/latex.lex sukija/sanat/olla-ei.lex sukija/sanat/omat.lex \
           sukija/sanat/etuliitteet.lex sukija/sanat/lyhenteet.lex sukija/sanat/atk-lyhenteet.lex \
           sukija/sukija.py sukija/suomi.all sukija/suomi.sym sukija/suomi.tex \
           sukija/suomi.inc sukija/suomi.lex sukija/suomi.mor sukija/suomi.pro


# Files to ship only in the full source package
SRC_ONLY=ChangeLog CONTRIBUTORS README.fi Makefile README COPYING \
         voikko/voikko-fi_FI.pro.in common/voikkoutils.py \
         vocabulary/joukahainen.xml vocabulary/flags.txt voikko/generate_lex.py \
         common/hfconv.py common/generate_lex_common.py \
         $(SUKIJA_SRC) $(patsubst %,vocabulary/erikoisalat/%.lex,$(OPTIONAL_LEX)) \
         vvfst/generate_lex.py vvfst/generate_taivutuskaavat.py vvfst/root.lexc \
         vvfst/suomi.lexc vvfst/taivutuskaavat.lexc.in vvfst/voikko-fi_FI.pro.in \
         vvfst/main.foma.in vvfst/olla-ei.lexc vvfst/poikkeavat.lexc vvfst/generate_sukija.py

VOIKKO_BINDIST=$(VOIKKO_BUILDDIR)/voikko-fi_FI.pro $(VOIKKO_BUILDDIR)/voikko-fi_FI.lex_? \
               $(VOIKKO_BUILDDIR)/voikko-fi_FI.mor_? $(VOIKKO_BUILDDIR)/voikko-fi_FI.sym_?

VVFST_BINDIST=$(VVFST_BUILDDIR)/voikko-fi_FI.pro $(VVFST_BUILDDIR)/mor.vfst

VVFST_GENERATED_LEXC_SUFFIXES=ee em ep es h l n nl t p a
VVFST_GENERATED_LEXC=$(patsubst %,$(VVFST_BUILDDIR)/joukahainen-%.lexc,$(VVFST_GENERATED_LEXC_SUFFIXES))

# Shared files needed in Voikko
VOIKKO_COPY_FROM_COMMON=mor.inc subrule.inc
VOIKKO_COPY_FROM_VOCABULARY=erikoissanat.lex seikkasanat.lex \
	suhdesanat.lex lukusanat.lex lyhenteet.lex yhdyssanat.lex erikoiset.lex \
	poikkeavat.lex lainen.lex taivutustaydennykset.lex sidesanat.lex
VOIKKO_COPY_FROM_VOIKKO=voikko-fi_FI.all voikko-fi_FI.sym voikko-fi_FI.mor \
	olla-ei.lex suomi.lex suomi.inc voikko-fi_FI.lex

SUKIJA_LEX_FROM_XML=joukahainen atk laaketiede matluonnontiede kasvatustiede

# Files to ship in the source package (duplicates are allowed)
SRCDIST=$(SRC_ONLY) $(patsubst %,common/%,$(VOIKKO_COPY_FROM_COMMON)) \
	$(patsubst %,vocabulary/%,$(VOIKKO_COPY_FROM_VOCABULARY)) \
	$(patsubst %,voikko/%,$(VOIKKO_COPY_FROM_VOIKKO))

VOIKKO_ACTUALLY_COPY_FROM_VOIKKO=
ifneq "$(VOIKKO_BUILDDIR)" "voikko"
	VOIKKO_ACTUALLY_COPY_FROM_VOIKKO=$(VOIKKO_COPY_FROM_VOIKKO)
endif

.PHONY: all clean update-vocabulary voikko voikko-install dist-gzip vvfst vvfst-install

all: voikko

voikko: $(patsubst %,$(VOIKKO_BUILDDIR)/%,$(VOIKKO_COPY_FROM_VOCABULARY)) $(patsubst %,$(VOIKKO_BUILDDIR)/%,$(VOIKKO_COPY_FROM_COMMON)) \
	$(patsubst %,$(VOIKKO_BUILDDIR)/%,$(VOIKKO_ACTUALLY_COPY_FROM_VOIKKO)) \
	$(VOIKKO_BUILDDIR)/main.lex $(VOIKKO_BUILDDIR)/voikko-fi_FI.pro $(VOIKKO_BUILDDIR)/config.inc
	@malmake $(VOIKKO_BUILDDIR)/voikko-fi_FI.pro 2>&1 | grep -v "Warning: No allomorphs generated."

$(patsubst %,$(VOIKKO_BUILDDIR)/%,$(VOIKKO_COPY_FROM_VOCABULARY)): $(VOIKKO_BUILDDIR)/%: vocabulary/%
	@mkdir -p $(VOIKKO_BUILDDIR)
	cp $^ $@

$(patsubst %,$(VOIKKO_BUILDDIR)/%,$(VOIKKO_COPY_FROM_COMMON)): $(VOIKKO_BUILDDIR)/%: common/%
	@mkdir -p $(VOIKKO_BUILDDIR)
	cp $^ $@

$(patsubst %,$(VOIKKO_BUILDDIR)/%,$(VOIKKO_ACTUALLY_COPY_FROM_VOIKKO)): $(VOIKKO_BUILDDIR)/%: voikko/%
	@mkdir -p $(VOIKKO_BUILDDIR)
	cp $^ $@

$(VOIKKO_BUILDDIR)/joukahainen.lex: vocabulary/joukahainen.xml vocabulary/flags.txt voikko/generate_lex.py \
	common/voikkoutils.py common/hfconv.py common/generate_lex_common.py
	@grep -B1 "ERROR: base form missing" vocabulary/joukahainen.xml | \
		sed -ne "s|^.*\"w\([^\"]*\)\".*|Base form missing: http://joukahainen.puimula.org/word/edit?wid=\1|p"
	$(PYTHON) voikko/generate_lex.py $(GENLEX_OPTS) --destdir=$(VOIKKO_BUILDDIR)

$(VOIKKO_BUILDDIR)/main.lex: $(VOIKKO_BUILDDIR)/joukahainen.lex $(EXTRA_LEX)
	cat $^ > $@


# Rule to generate voikko-fi_FI.pro

VOIKKO_MURRE=no
ifeq "$(findstring dialect,$(GENLEX_OPTS))" "dialect"
	VOIKKO_MURRE=yes
endif

VOIKKO_PRO_SEDSCRIPT="s/VANHAHKOT_MUODOT/$(VANHAHKOT_MUODOT)/; \
	s/VANHAT_MUODOT/$(VANHAT_MUODOT)/; \
	s/VOIKKO_MURRE/$(VOIKKO_MURRE)/; \
	s/SUKIJAN_MUODOT/$(SUKIJAN_MUODOT)/; \
	s/SM_VOIKKO_VARIANT/$(VOIKKO_VARIANT)/; \
	s/SM_VOIKKO_DESCRIPTION/$(VOIKKO_DESCRIPTION)/; \
	s/SM_VERSION/$(SM_VERSION)/; \
	s/SM_PATCHINFO/$(SM_PATCHINFO)/; \
	s/SM_BUILDCONFIG/$(subst /,\\/,GENLEX_OPTS=$(GENLEX_OPTS) EXTRA_LEX=$(EXTRA_LEX))/; \
	s/SM_BUILDDATE/$(SM_BUILDDATE)/"

$(VOIKKO_BUILDDIR)/voikko-fi_FI.pro: voikko/voikko-fi_FI.pro.in
	sed -e $(VOIKKO_PRO_SEDSCRIPT) < $^ > $@

$(VOIKKO_BUILDDIR)/config.inc:
	echo "define @voikko_debug := $(VOIKKO_DEBUG);" > $@

clean:
	rm -f $(VOIKKO_BUILDDIR)/*_l common/voikkoutils.pyc common/hfconv.pyc
	rm -f $(VOIKKO_BUILDDIR)/config.inc
	rm -f common/generate_lex_common.pyc
	rm -f "suomi-malaga-$(SM_VERSION).tar.gz"
	rm -rf "suomi-malaga-$(SM_VERSION)" "voikko-fi-$(SM_VERSION)"
	rm -f $(patsubst %,$(VOIKKO_BUILDDIR)/%,$(VOIKKO_COPY_FROM_VOCABULARY))
	rm -f $(patsubst %,$(VOIKKO_BUILDDIR)/%,$(VOIKKO_COPY_FROM_COMMON))
	rm -f $(patsubst %,$(VOIKKO_BUILDDIR)/%,$(VOIKKO_ACTUALLY_COPY_FROM_VOIKKO))
	rm -f $(VOIKKO_BUILDDIR)/joukahainen.lex $(VOIKKO_BUILDDIR)/main.lex $(VOIKKO_BUILDDIR)/voikko-fi_FI.pro
	rm -rf sukija/*_l sukija/transmit sukija/sukija.pyc sukija/voikonsanat
	rm -f sukija/k1 sukija/k2 sukija/foo sukija/bar
	rm -f $(VVFST_BUILDDIR)/all.att $(VVFST_BUILDDIR)/all.lexc $(VVFST_BUILDDIR)/main.foma $(VVFST_BUILDDIR)/mor.vfst
	rm -f $(VVFST_BUILDDIR)/taivutuskaavat.lexc $(VVFST_BUILDDIR)/voikko-fi_FI.pro
	rm -f $(VVFST_GENERATED_LEXC) $(VVFST_BUILDDIR)/joukahainen.lexc.stamp
	rm -f TAGS
	rm -f $(VVFST_BUILDDIR)/all-sukija.att $(VVFST_BUILDDIR)/all-sukija.lexc $(VVFST_BUILDDIR)/main-sukija.foma*
	rm -f $(VVFST_BUILDDIR)/generate_sukija.py.stamp

# Rules for creating the source distribution

dist-gzip: suomi-malaga-$(SM_VERSION).tar.gz

suomi-malaga-$(SM_VERSION).tar.gz: $(patsubst %,suomi-malaga-$(SM_VERSION)/%, $(sort $(SRCDIST)))
	tar c --group 0 --owner 0 suomi-malaga-$(SM_VERSION) | gzip -9 > $@

dist-bzip2: suomi-malaga-$(SM_VERSION).tar.bz2

suomi-malaga-$(SM_VERSION).tar.bz2: $(patsubst %,suomi-malaga-$(SM_VERSION)/%, $(sort $(SRCDIST)))
	tar c --group 0 --owner 0 suomi-malaga-$(SM_VERSION) | bzip2 -9 > $@

$(patsubst %,suomi-malaga-$(SM_VERSION)/%, $(sort $(SRCDIST))): suomi-malaga-$(SM_VERSION)/%: %
	install -m 644 -D $^ $@

voikko-install: voikko
	install -m 755 -d $(DESTDIR)/2/mor-$(VOIKKO_VARIANT)
	install -m 644 $(VOIKKO_BINDIST) $(DESTDIR)/2/mor-$(VOIKKO_VARIANT)

# Vocabulary update
update-vocabulary:
	wget http://joukahainen.puimula.org/sanastot/joukahainen.xml.gz -O - \
	| gunzip > vocabulary/joukahainen.xml


SUKIJA_COPY_FROM_VOCABULARY=\
erikoisalat/linux-distributions.lex \
erikoiset.lex \
erikoissanat.lex \
lainen.lex \
lukusanat.lex \
poikkeavat.lex \
seikkasanat.lex \
sidesanat.lex \
suhdesanat.lex \
taivutustaydennykset.lex \
yhdyssanat.lex

sukija: $(patsubst %,sukija/voikonsanat/%,$(SUKIJA_COPY_FROM_VOCABULARY)) \
	$(patsubst %,sukija/voikonsanat/%.lex,$(SUKIJA_LEX_FROM_XML))
	@malmake sukija/suomi.pro 2>&1 | grep -v "Warning: No allomorphs generated."

$(patsubst %,sukija/voikonsanat/%,$(SUKIJA_COPY_FROM_VOCABULARY)): sukija/voikonsanat/%: vocabulary/%
	@mkdir -p sukija/voikonsanat/erikoisalat
	cp $^ $@

$(patsubst %,sukija/voikonsanat/%.lex,$(SUKIJA_LEX_FROM_XML)): vocabulary/joukahainen.xml vocabulary/flags.txt \
	common/voikkoutils.py common/hfconv.py common/generate_lex_common.py $(SUKIJA_SRC)
	@grep -B1 "ERROR: base form missing" vocabulary/joukahainen.xml | \
		sed -ne "s|^.*\"w\([^\"]*\)\".*|Base form missing: http://joukahainen.puimula.org/word/edit?wid=\1|p"
	$(PYTHON) sukija/generate_lex.py
	-@grep "Malaga class not found" sukija/voikonsanat/joukahainen.lex



vvfst: $(VVFST_BUILDDIR)/mor.vfst $(VVFST_BUILDDIR)/voikko-fi_FI.pro

$(VVFST_BUILDDIR)/joukahainen.lexc.stamp: vocabulary/joukahainen.xml vocabulary/flags.txt vvfst/generate_lex.py \
	common/voikkoutils.py common/hfconv.py common/generate_lex_common.py
	@grep -B1 "ERROR: base form missing" vocabulary/joukahainen.xml | \
		sed -ne "s|^.*\"w\([^\"]*\)\".*|Base form missing: http://joukahainen.puimula.org/word/edit?wid=\1|p"
	$(PYTHON) vvfst/generate_lex.py $(GENLEX_OPTS) --destdir=$(VVFST_BUILDDIR)
	touch $@

$(VVFST_BUILDDIR)/taivutuskaavat.lexc: vvfst/taivutuskaavat.lexc.in vvfst/generate_taivutuskaavat.py
	$(PYTHON) vvfst/generate_taivutuskaavat.py $(GENLEX_OPTS) --destdir=$(VVFST_BUILDDIR)

$(VVFST_BUILDDIR)/all.lexc: vvfst/root.lexc $(VVFST_BUILDDIR)/taivutuskaavat.lexc $(VVFST_BUILDDIR)/joukahainen.lexc.stamp vvfst/suomi.lexc vvfst/olla-ei.lexc vvfst/poikkeavat.lexc
	cat vvfst/root.lexc $(VVFST_BUILDDIR)/taivutuskaavat.lexc $(VVFST_GENERATED_LEXC) vvfst/suomi.lexc vvfst/olla-ei.lexc vvfst/poikkeavat.lexc > $@

$(VVFST_BUILDDIR)/main.foma: vvfst/main.foma.in
	sed -e "s,VVFST_BUILDDIR,$(VVFST_BUILDDIR)," < $^ > $@

$(VVFST_BUILDDIR)/all.att: $(VVFST_BUILDDIR)/all.lexc $(VVFST_BUILDDIR)/main.foma
	foma -f $(VVFST_BUILDDIR)/main.foma | grep -v "defined but not used" 2>&1

$(VVFST_BUILDDIR)/mor.vfst: $(VVFST_BUILDDIR)/all.att
	cat $^ | sort -n | voikkovfstc -o $@

VVFST_PRO_SEDSCRIPT="s/SM_VOIKKO_VARIANT/$(VOIKKO_VARIANT)/; \
	s/SM_VOIKKO_DESCRIPTION/$(VOIKKO_DESCRIPTION)/; \
	s/SM_VERSION/$(SM_VERSION)/; \
	s/SM_PATCHINFO/$(SM_PATCHINFO)/; \
	s/SM_BUILDCONFIG/$(subst /,\\/,GENLEX_OPTS=$(GENLEX_OPTS) EXTRA_LEX=$(EXTRA_LEX))/; \
	s/SM_BUILDDATE/$(SM_BUILDDATE)/"

$(VVFST_BUILDDIR)/voikko-fi_FI.pro: vvfst/voikko-fi_FI.pro.in
	sed -e $(VVFST_PRO_SEDSCRIPT) < $^ > $@


vvfst-install: vvfst
	install -m 755 -d $(DESTDIR)/2/mor-$(VOIKKO_VARIANT)
	install -m 644 $(VVFST_BINDIST) $(DESTDIR)/2/mor-$(VOIKKO_VARIANT)


TAGS: $(patsubst %,vocabulary/%,$(VOIKKO_COPY_FROM_VOCABULARY)) \
		$(patsubst %,vocabulary/erikoisalat/%.lex,$(OPTIONAL_LEX))
	etags --lang=none --output=$@ \
		--regex='/[ \t]*\[.*\<perusmuoto:[ \t]*"\([^"]+\)"/\1/' \
		-- $^


# Generate a big Voikko dictionary to use in Sukija (according to Harri Pitkänen).
#
voikko-sukija:
	make voikko VANHAT_MUODOT=yes SUKIJAN_MUODOT=yes \
		GENLEX_OPTS='--style=dialect,foreign,incorrect --min-frequency=10 \
		--extra-usage=sukija,it,medicine,science,education,orgname' \
		EXTRA_LEX="vocabulary/erikoisalat/atk-lyhenteet.lex \
		vocabulary/erikoisalat/science-misc.lex" VOIKKO_DEBUG=yes


# Rules for creating vvfst morphology for Sukija indexer. See file
# $(VVFST_BUILDDIR)/generate_sukija.py for details.
#
vvfst-sukija: $(VVFST_BUILDDIR)/all-sukija.att $(VVFST_BUILDDIR)/all.att

$(VVFST_BUILDDIR)/all-sukija.lexc: $(VVFST_BUILDDIR)/all.lexc $(VVFST_BUILDDIR)/generate_sukija.py.stamp
	$(PYTHON) vvfst/generate_sukija.py --destdir=$(VVFST_BUILDDIR)

$(VVFST_BUILDDIR)/all-sukija.att: $(VVFST_BUILDDIR)/all-sukija.lexc $(VVFST_BUILDDIR)/main-sukija.foma
	foma -f $(VVFST_BUILDDIR)/main-sukija.foma | grep -v "defined but not used" 2>&1

$(VVFST_BUILDDIR)/main-sukija.foma: $(VVFST_BUILDDIR)/main.foma
	sed -e 's,/all,/all-sukija,'  < $^ > $@

$(VVFST_BUILDDIR)/generate_sukija.py.stamp: $(VVFST_BUILDDIR)/generate_sukija.py
	touch $@
