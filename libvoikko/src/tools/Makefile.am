bin_PROGRAMS = voikkospell voikkohyphenate voikkogc

voikkospell_SOURCES = voikkospell.cpp
voikkospell_CFLAGS = $(AM_CFLAGS)
voikkospell_LDADD = ../libvoikko.la

voikkohyphenate_SOURCES = voikkohyphenate.cpp ../porting.c
voikkohyphenate_CFLAGS = $(AM_CFLAGS)
voikkohyphenate_LDADD = ../libvoikko.la

voikkogc_SOURCES = voikkogc.cpp ../porting.c
voikkogc_CFLAGS = $(AM_CFLAGS)
voikkogc_LDADD = ../libvoikko.la

dist_man_MANS = voikkospell.1 voikkohyphenate.1 voikkogc.1

