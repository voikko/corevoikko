BUILDDIR=.
DESTDIR=inst

.PHONY: all install

all: $(BUILDDIR)/spl.vfst

$(BUILDDIR)/main.foma: main.foma.in
	sed -e "s,BUILDDIR,$(BUILDDIR)," < $^ > $@

$(BUILDDIR)/all.att: all.lexc $(BUILDDIR)/main.foma
	foma -f $(BUILDDIR)/main.foma

$(BUILDDIR)/spl.vfst: $(BUILDDIR)/all.att
	cat $^ | sort -n | voikkovfstc -o $@

install: all
	install -m 755 -d $(DESTDIR)/2/mor-svfst
	install -m 644 $(BUILDDIR)/spl.vfst voikko-fi_FI.pro $(DESTDIR)/2/mor-svfst
