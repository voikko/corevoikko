lib_LTLIBRARIES = libvoikko.la
libvoikko_la_SOURCES = voikko_suggest.c voikko_utils.c voikko_setup.c voikko_spell.c \
                       voikko_hyphenate.c voikko_charset.c porting.c \
                       voikko_tokenize.c sentence.c grammar.c gccache.c \
                       gcerror.c gcanalysis.c
libvoikko_la_LDFLAGS = -lmalaga -no-undefined -version-info 9:0:8 @LIBLDFLAGSWIN@
bin_PROGRAMS = voikkospell voikkohyphenate voikkogc
voikkospell_SOURCES = voikkospell.c porting.c
voikkospell_CFLAGS = $(AM_CFLAGS)
voikkospell_LDADD = libvoikko.la
voikkohyphenate_SOURCES = voikkohyphenate.c porting.c
voikkohyphenate_CFLAGS = $(AM_CFLAGS)
voikkohyphenate_LDADD = libvoikko.la
voikkogc_SOURCES = voikkogc.c porting.c
voikkogc_CFLAGS = $(AM_CFLAGS)
voikkogc_LDADD = libvoikko.la


pkginclude_HEADERS = voikko.h
noinst_HEADERS = voikko_utils.h voikko_setup.h voikko_spell.h voikko_charset.h \
                 voikko_hyphenate.h voikko_defs.h porting.h voikko_suggest.h \
                 gccache.h gcerror.h gcanalysis.h

dist_man_MANS = voikkospell.1 voikkohyphenate.1 voikkogc.1

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = libvoikko.pc

