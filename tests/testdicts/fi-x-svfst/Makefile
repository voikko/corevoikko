BUILDDIR=.
DESTDIR=inst

.PHONY: all install

all: $(BUILDDIR)/spl.vfst $(BUILDDIR)/err.vfst

$(BUILDDIR)/main.foma: main.foma.in
	sed -e "s,BUILDDIR,$(BUILDDIR)," < $^ > $@

$(BUILDDIR)/all.att: all.lexc $(BUILDDIR)/main.foma
	foma -f $(BUILDDIR)/main.foma

$(BUILDDIR)/spl.vfst: all.weighted.att
	cat $^ | sort -n | voikkovfstc -w prob -o $@

$(BUILDDIR)/err.vfst: err.weighted.att
	cat $^ | sort -n | voikkovfstc -w prob -o $@

install: all
	install -m 755 -d $(DESTDIR)/5/mor-svfst
	install -m 644 $(BUILDDIR)/spl.vfst index.txt $(DESTDIR)/5/mor-svfst
	install -m 644 $(BUILDDIR)/spl.vfst $(DESTDIR)/5/mor-svfst/mor.vfst
	install -m 644 $(BUILDDIR)/err.vfst $(DESTDIR)/5/mor-svfst/err.vfst
