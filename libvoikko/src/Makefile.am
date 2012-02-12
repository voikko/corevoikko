AUTOMAKE_OPTIONS=subdir-objects

lib_LTLIBRARIES = libvoikko.la
libvoikko_la_SOURCES = \
    fst/Configuration.cpp \
    fst/Transducer.cpp \
    spellchecker/spell.cpp spellchecker/suggestions.cpp \
    spellchecker/Speller.cpp \
    spellchecker/SpellWithPriority.cpp \
    spellchecker/SpellUtils.cpp \
    spellchecker/AnalyzerToSpellerAdapter.cpp \
    spellchecker/FinnishSpellerTweaksWrapper.cpp \
    spellchecker/FixedResultSpeller.cpp \
    spellchecker/SpellerFactory.cpp \
    spellchecker/SpellerCache.cpp \
    spellchecker/suggestion/Suggestion.cpp \
    spellchecker/suggestion/SuggestionStatus.cpp \
    spellchecker/suggestion/SuggestionGeneratorFactory.cpp \
    spellchecker/suggestion/SuggestionGeneratorCaseChange.cpp \
    spellchecker/suggestion/SuggestionGeneratorDeletion.cpp \
    spellchecker/suggestion/SuggestionGeneratorInsertion.cpp \
    spellchecker/suggestion/SuggestionGeneratorInsertSpecial.cpp \
    spellchecker/suggestion/SuggestionGeneratorMultiReplacement.cpp \
    spellchecker/suggestion/SuggestionGeneratorNull.cpp \
    spellchecker/suggestion/SuggestionGeneratorReplacement.cpp \
    spellchecker/suggestion/SuggestionGeneratorReplaceTwo.cpp \
    spellchecker/suggestion/SuggestionGeneratorSoftHyphens.cpp \
    spellchecker/suggestion/SuggestionGeneratorSplitWord.cpp \
    spellchecker/suggestion/SuggestionGeneratorSwap.cpp \
    spellchecker/suggestion/SuggestionGeneratorVowelChange.cpp \
    spellchecker/suggestion/SuggestionStrategy.cpp \
    spellchecker/suggestion/SuggestionStrategyOcr.cpp \
    spellchecker/suggestion/SuggestionStrategyTyping.cpp \
    utils/utils.cpp utils/StringUtils.cpp \
    morphology/interface.cpp \
    morphology/Analysis.cpp \
    morphology/Analyzer.cpp \
    morphology/AnalyzerFactory.cpp \
    morphology/NullAnalyzer.cpp \
    setup/setup.cpp setup/Dictionary.cpp setup/DictionaryLoader.cpp \
    setup/DictionaryException.cpp setup/interface.cpp \
    setup/LanguageTag.cpp \
    hyphenator/interface.cpp \
    hyphenator/AnalyzerToFinnishHyphenatorAdapter.cpp \
    hyphenator/Hyphenator.cpp \
    hyphenator/HyphenatorFactory.cpp \
    character/charset.cpp \
    character/SimpleChar.cpp \
    tokenizer/interface.cpp \
    tokenizer/Tokenizer.cpp \
    sentence/interface.cpp \
    sentence/Sentence.cpp \
    grammar/interface.cpp \
    grammar/cache.cpp grammar/checks.cpp \
    grammar/cachesetup.cpp grammar/error.cpp grammar/analysis.cpp \
    grammar/Sentence.cpp grammar/Paragraph.cpp grammar/CacheEntry.cpp \
    grammar/GcCache.cpp \
    grammar/check/CapitalizationCheck.cpp \
    grammar/check/CompoundVerbCheck.cpp \
    grammar/check/MissingVerbCheck.cpp \
    grammar/check/NegativeVerbCheck.cpp \
    grammar/check/ParagraphCheck.cpp \
    grammar/check/SentenceCheck.cpp \
    grammar/check/SidesanaCheck.cpp \
    compatibility/interface.cpp \
    autocorrect/AutoCorrect.cpp
libvoikko_la_LDFLAGS = -no-undefined -version-info 13:5:12 @LIBLDFLAGSWIN@

pkginclude_HEADERS = voikko.h voikko_enums.h voikko_defines.h voikko_deprecated.h voikko_structs.h
noinst_HEADERS = \
    fst/Configuration.hpp \
    fst/Transducer.hpp \
    fst/Transition.hpp \
    utils/utils.hpp utils/StringUtils.hpp \
    hyphenator/Hyphenator.hpp \
    hyphenator/AnalyzerToFinnishHyphenatorAdapter.hpp \
    hyphenator/HyphenatorFactory.hpp \
    hyphenator/HfstHyphenator.hpp \
    morphology/Analysis.hpp \
    morphology/Analyzer.hpp \
    morphology/AnalyzerFactory.hpp \
    morphology/HfstAnalyzer.hpp \
    morphology/LttoolboxAnalyzer.hpp \
    morphology/NullAnalyzer.hpp \
    morphology/MalagaAnalyzer.hpp \
    morphology/malaga/analysis.hpp \
    morphology/malaga/malaga_files.hpp \
    morphology/malaga/patterns.hpp \
    morphology/malaga/rule_type.hpp \
    morphology/malaga/tries.hpp \
    morphology/malaga/basic.hpp \
    morphology/malaga/lexicon.hpp \
    morphology/malaga/malaga.hpp \
    morphology/malaga/pools.hpp \
    morphology/malaga/files.hpp \
    morphology/malaga/libmalaga.hpp \
    morphology/malaga/malaga_lib.hpp \
    morphology/malaga/rules.hpp \
    morphology/malaga/symbols.hpp \
    morphology/malaga/values.hpp \
    morphology/malaga/MalagaState.hpp \
    setup/setup.hpp setup/Dictionary.hpp setup/DictionaryLoader.hpp \
    setup/DictionaryException.hpp \
    setup/LanguageTag.hpp \
    character/charset.hpp \
    character/SimpleChar.hpp \
    porting.h \
    spellchecker/SpellWithPriority.hpp \
    spellchecker/SpellUtils.hpp \
    spellchecker/Speller.hpp \
    spellchecker/AnalyzerToSpellerAdapter.hpp \
    spellchecker/FinnishSpellerTweaksWrapper.hpp \
    spellchecker/FixedResultSpeller.hpp \
    spellchecker/HfstSpeller.hpp \
    spellchecker/SpellerFactory.hpp \
    spellchecker/SpellerCache.hpp \
    spellchecker/HfstSuggestion.hpp \
    spellchecker/suggestion/Suggestion.hpp \
    spellchecker/suggestion/SuggestionGenerator.hpp \
    spellchecker/suggestion/SuggestionGeneratorFactory.hpp \
    spellchecker/suggestion/SuggestionGeneratorCaseChange.hpp \
    spellchecker/suggestion/SuggestionGeneratorDeletion.hpp \
    spellchecker/suggestion/SuggestionGeneratorInsertion.hpp \
    spellchecker/suggestion/SuggestionGeneratorInsertSpecial.hpp \
    spellchecker/suggestion/SuggestionGeneratorMultiReplacement.hpp \
    spellchecker/suggestion/SuggestionGeneratorNull.hpp \
    spellchecker/suggestion/SuggestionGeneratorReplacement.hpp \
    spellchecker/suggestion/SuggestionGeneratorReplaceTwo.hpp \
    spellchecker/suggestion/SuggestionGeneratorSoftHyphens.hpp \
    spellchecker/suggestion/SuggestionGeneratorSplitWord.hpp \
    spellchecker/suggestion/SuggestionGeneratorSwap.hpp \
    spellchecker/suggestion/SuggestionType.hpp \
    spellchecker/suggestion/SuggestionGeneratorVowelChange.hpp \
    spellchecker/suggestion/SuggestionStrategy.hpp \
    spellchecker/suggestion/SuggestionStrategyOcr.hpp \
    spellchecker/suggestion/SuggestionStrategyTyping.hpp \
    spellchecker/suggestion/SuggestionStatus.hpp \
    sentence/Sentence.hpp \
    tokenizer/Tokenizer.hpp \
    grammar/cache.hpp grammar/checks.hpp grammar/error.hpp \
    grammar/cachesetup.hpp grammar/analysis.hpp \
    grammar/Token.hpp grammar/Sentence.hpp grammar/Paragraph.hpp \
    grammar/CacheEntry.hpp \
    grammar/GcCache.hpp \
    grammar/check/CapitalizationCheck.hpp \
    grammar/check/CompoundVerbCheck.hpp \
    grammar/check/MissingVerbCheck.hpp \
    grammar/check/NegativeVerbCheck.hpp \
    grammar/check/ParagraphCheck.hpp \
    grammar/check/SentenceCheck.hpp \
    grammar/check/SidesanaCheck.hpp \
    autocorrect/AutoCorrect.hpp autocorrect/TrieNode.hpp \
    utf8/core.hpp \
    utf8/checked.hpp \
    utf8/unchecked.hpp \
    utf8/utf8.hpp

EXTRA_DIST = autocorrect/triecompiler.py
BUILT_SOURCES = autocorrect/data.hpp
CLEANFILES = autocorrect/data.hpp
autocorrect/data.hpp: Makefile $(srcdir)/../data/autocorrect/fi_FI.xml $(srcdir)/autocorrect/triecompiler.py
	python $(srcdir)/autocorrect/triecompiler.py $(srcdir)/../data/autocorrect/fi_FI.xml autocorrect/data.hpp

MALAGA_CPP_FILES=morphology/MalagaAnalyzer.cpp \
    morphology/malaga/analysis.cpp \
    morphology/malaga/files.cpp \
    morphology/malaga/lexicon.cpp \
    morphology/malaga/malaga_files.cpp \
    morphology/malaga/patterns.cpp \
    morphology/malaga/rules.cpp \
    morphology/malaga/symbols.cpp \
    morphology/malaga/basic.cpp \
    morphology/malaga/libmalaga.cpp \
    morphology/malaga/malaga_lib.cpp \
    morphology/malaga/pools.cpp \
    morphology/malaga/tries.cpp \
    morphology/malaga/values.cpp \
    morphology/malaga/MalagaState.cpp

HFST_CPP_FILES=morphology/HfstAnalyzer.cpp \
			   spellchecker/HfstSpeller.cpp \
			   spellchecker/HfstSuggestion.cpp \
			   hyphenator/HfstHyphenator.cpp

LTTOOLBOX_CPP_FILES=morphology/LttoolboxAnalyzer.cpp

if HAVE_MALAGA
    libvoikko_la_SOURCES += $(MALAGA_CPP_FILES)
else
    EXTRA_DIST += $(MALAGA_CPP_FILES)
endif

if HAVE_HFST
    libvoikko_la_SOURCES += $(HFST_CPP_FILES)
    libvoikko_la_LDFLAGS += $(HFSTOSPELL_LIBS)
else
    EXTRA_DIST += $(HFST_CPP_FILES)
endif

if HAVE_LTTOOLBOX
    libvoikko_la_SOURCES += $(LTTOOLBOX_CPP_FILES)
    libvoikko_la_LDFLAGS += $(LTTOOLBOX_LIBS)
else
    EXTRA_DIST += $(LTTOOLBOX_CPP_FILES)
endif

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = libvoikko.pc
