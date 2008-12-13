lib_LTLIBRARIES = libvoikko.la
libvoikko_la_SOURCES = spellchecker/spell.cpp spellchecker/suggestions.cpp \
                       utils/utils.cpp voikko_setup.c \
                       hyphenator/hyphenator.cpp character/charset.cpp \
                       tokenizer/tokenizer.cpp sentence/sentence.cpp \
                       grammar/grammar.cpp grammar/cache.cpp grammar/checks.cpp gccache.c \
                       grammar/error.cpp grammar/analysis.cpp
libvoikko_la_LDFLAGS = -lmalaga -no-undefined -version-info 9:0:8 @LIBLDFLAGSWIN@

pkginclude_HEADERS = voikko.h
noinst_HEADERS = utils/utils.hpp voikko_setup.h character/charset.hpp \
                 hyphenator/hyphenator.hpp voikko_defs.h porting.h \
                 spellchecker/spell.hpp spellchecker/suggestions.hpp \
                 grammar/cache.hpp grammar/checks.hpp gccache.h grammar/error.hpp \
                 grammar/analysis.hpp

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = libvoikko.pc

