
INCLUDES = -I$(MALAGA_INC)

lib_LTLIBRARIES = libvoikko.la
libvoikko_la_SOURCES = voikko_suggest.c voikko_utils.c voikko_setup.c voikko_spell.c \
                       voikko_hyphenate.c porting.c
libvoikko_la_LDFLAGS = -lmalaga -L$(MALAGA_PATH)/lib -version-info 1:0:0
LDADD = libmalaga.la

bin_PROGRAMS = voikkospell voikkohyphenate
voikkospell_SOURCES = voikkospell.c porting.c
voikkospell_CFLAGS = $(AM_CFLAGS)
voikkospell_LDADD = libvoikko.la
voikkohyphenate_SOURCES = voikkohyphenate.c porting.c
voikkohyphenate_CFLAGS = $(AM_CFLAGS)
voikkohyphenate_LDADD = libvoikko.la

pkginclude_HEADERS = voikko.h
noinst_HEADERS = voikko_utils.h voikko_setup.h voikko_spell.h \
                 voikko_hyphenate.h voikko_defs.h porting.h voikko_suggest.h

dist_man_MANS = voikkospell.1 voikkohyphenate.1

