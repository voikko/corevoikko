lib_LTLIBRARIES = libvoikko.la
libvoikko_la_SOURCES = voikko_suggest.c voikko_utils.c voikko_setup.c voikko_spell.c \
                       hyphenator/hyphenator.cpp voikko_charset.c \
                       voikko_tokenize.c sentence.c grammar/grammar.cpp gccache.c \
                       gcerror.c gcanalysis.c
libvoikko_la_LDFLAGS = -lmalaga -no-undefined -version-info 9:0:8 @LIBLDFLAGSWIN@

pkginclude_HEADERS = voikko.h
noinst_HEADERS = voikko_utils.h voikko_setup.h voikko_spell.h voikko_charset.h \
                 hyphenator/hyphenator.hpp voikko_defs.h porting.h voikko_suggest.h \
                 gccache.h gcerror.h gcanalysis.h

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = libvoikko.pc

