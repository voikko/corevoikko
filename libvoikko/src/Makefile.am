
INCLUDES = -I$(MALAGA_PATH)/include
AM_CFLAGS = -Wall -pedantic -std=c99

lib_LTLIBRARIES = libvoikko.la
libvoikko_la_SOURCES = voikko_suggest.c voikko_utils.c voikko_setup.c voikko_spell.c \
                       voikko_hyphenate.c
libvoikko_la_LDFLAGS = -lmalaga -L$(MALAGA_PATH)/lib
LDADD = libmalaga.la

bin_PROGRAMS = voikkospell voikkohyphenate
voikkospell_SOURCES = voikkospell.c
voikkospell_LDADD = libvoikko.la
voikkohyphenate_SOURCES = voikkohyphenate.c
voikkohyphenate_LDADD = libvoikko.la

pkginclude_HEADERS = voikko.h
noinst_HEADERS = voikko_utils.h voikko_setup.h voikko_spell.h \
                 voikko_hyphenate.h
