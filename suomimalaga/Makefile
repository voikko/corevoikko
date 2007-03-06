
# Set some variables
VOIKKO_BINDIST=voikko/voikko-fi_FI.pro voikko/voikko-fi_FI.lex_? voikko/voikko-fi_FI.mor_? \
	voikko/voikko-fi_FI.sym_?
VOIKKO_COPY_FROM_VOCABULARY=suomi.lex joukahainen.lex


SM=suomi-malaga-svn

.PHONY: all clean voikko voikko-install

all: voikko

voikko: $(patsubst %,voikko/%,$(VOIKKO_COPY_FROM_VOCABULARY))
	@malmake voikko/voikko-fi_FI.pro 2>&1 | grep -v "Warning: No allomorphs generated."

$(patsubst %,voikko/%,$(VOIKKO_COPY_FROM_VOCABULARY)): voikko/%: vocabulary/%
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

