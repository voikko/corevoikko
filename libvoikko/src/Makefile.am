lib_LTLIBRARIES = libvoikko.la
libvoikko_la_SOURCES = spellchecker/spell.cpp spellchecker/suggestions.cpp voikko_utils.c voikko_setup.c \
                       hyphenator/hyphenator.cpp character/charset.cpp \
                       tokenizer/tokenizer.cpp sentence/sentence.cpp \
                       grammar/grammar.cpp grammar/cache.cpp grammar/checks.cpp gccache.c \
                       gcerror.c grammar/analysis.cpp
libvoikko_la_LDFLAGS = -lmalaga -no-undefined -version-info 9:0:8 @LIBLDFLAGSWIN@

pkginclude_HEADERS = voikko.h
noinst_HEADERS = voikko_utils.h voikko_setup.h character/charset.hpp \
                 hyphenator/hyphenator.hpp voikko_defs.h porting.h \
                 spellchecker/spell.hpp spellchecker/suggestions.hpp \
                 grammar/cache.hpp grammar/checks.hpp gccache.h gcerror.h \
                 grammar/analysis.hpp

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = libvoikko.pc

