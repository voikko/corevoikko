lib_LTLIBRARIES = libvoikko.la
libvoikko_la_SOURCES = spellchecker/spell.cpp spellchecker/suggestions.cpp \
                       utils/utils.cpp utils/StringUtils.cpp \
                       setup/setup.cpp setup/Dictionary.cpp setup/DictionaryLoader.cpp \
                       setup/DictionaryException.cpp setup/interface.cpp \
                       hyphenator/hyphenator.cpp character/charset.cpp \
                       tokenizer/tokenizer.cpp sentence/sentence.cpp \
                       grammar/grammar.cpp grammar/cache.cpp grammar/checks.cpp \
                       grammar/cachesetup.cpp grammar/error.cpp grammar/analysis.cpp \
                       grammar/Sentence.cpp grammar/Paragraph.cpp
libvoikko_la_LDFLAGS = -lmalaga -no-undefined -version-info 9:0:8 @LIBLDFLAGSWIN@

pkginclude_HEADERS = voikko.h
noinst_HEADERS = utils/utils.hpp utils/StringUtils.hpp \
                 setup/setup.hpp setup/Dictionary.hpp setup/DictionaryLoader.hpp \
                 setup/DictionaryException.hpp \
                 character/charset.hpp \
                 hyphenator/hyphenator.hpp voikko_defs.h porting.h \
                 spellchecker/spell.hpp spellchecker/suggestions.hpp \
                 grammar/cache.hpp grammar/checks.hpp grammar/error.hpp \
                 grammar/cachesetup.hpp grammar/analysis.hpp \
                 grammar/Token.hpp grammar/Sentence.hpp grammar/Paragraph.hpp

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = libvoikko.pc

