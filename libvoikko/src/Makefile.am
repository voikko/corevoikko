lib_LTLIBRARIES = libvoikko.la
libvoikko_la_SOURCES = spellchecker/spell.cpp spellchecker/suggestions.cpp voikko_utils.c voikko_setup.c \
                       hyphenator/hyphenator.cpp voikko_charset.c \
                       tokenizer/tokenizer.cpp sentence/sentence.cpp \
                       grammar/grammar.cpp gccache.c \
                       gcerror.c gcanalysis.c
libvoikko_la_LDFLAGS = -lmalaga -no-undefined -version-info 9:0:8 @LIBLDFLAGSWIN@

pkginclude_HEADERS = voikko.h
noinst_HEADERS = voikko_utils.h voikko_setup.h voikko_charset.h \
                 hyphenator/hyphenator.hpp voikko_defs.h porting.h \
                 spellchecker/spell.hpp spellchecker/suggestions.hpp \
                 gccache.h gcerror.h gcanalysis.h

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = libvoikko.pc

