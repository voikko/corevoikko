AUTOMAKE_OPTIONS=subdir-objects

lib_LTLIBRARIES = libvoikko.la
libvoikko_la_SOURCES = \
    spellchecker/spell.cpp spellchecker/suggestions.cpp \
    spellchecker/SpellWithPriority.cpp \
    spellchecker/SpellUtils.cpp \
    spellchecker/suggestion/Suggestion.cpp \
    spellchecker/suggestion/SuggestionStatus.cpp \
    spellchecker/suggestion/SuggestionGeneratorCaseChange.cpp \
    spellchecker/suggestion/SuggestionGeneratorDeletion.cpp \
    spellchecker/suggestion/SuggestionGeneratorInsertion.cpp \
    spellchecker/suggestion/SuggestionGeneratorInsertSpecial.cpp \
    spellchecker/suggestion/SuggestionGeneratorReplacement.cpp \
    spellchecker/suggestion/SuggestionGeneratorReplaceTwo.cpp \
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
    morphology/MalagaAnalyzer.cpp \
    morphology/malaga/analysis.cpp \
    morphology/malaga/files.cpp \
    morphology/malaga/lexicon.cpp \
    morphology/malaga/malaga_files.cpp \
    morphology/malaga/patterns.cpp \
    morphology/malaga/rules.cpp \
    morphology/malaga/symbols.cpp \
    morphology/malaga/value_parser.cpp \
    morphology/malaga/basic.cpp \
    morphology/malaga/input.cpp \
    morphology/malaga/libmalaga.cpp \
    morphology/malaga/malaga_lib.cpp \
    morphology/malaga/pools.cpp \
    morphology/malaga/scanner.cpp \
    morphology/malaga/tries.cpp \
    morphology/malaga/values.cpp \
    setup/setup.cpp setup/Dictionary.cpp setup/DictionaryLoader.cpp \
    setup/DictionaryException.cpp setup/interface.cpp \
    hyphenator/hyphenator.cpp character/charset.cpp \
    tokenizer/tokenizer.cpp sentence/sentence.cpp \
    grammar/grammar.cpp grammar/cache.cpp grammar/checks.cpp \
    grammar/cachesetup.cpp grammar/error.cpp grammar/analysis.cpp \
    grammar/Sentence.cpp grammar/Paragraph.cpp grammar/CacheEntry.cpp \
    autocorrect/AutoCorrect.cpp
libvoikko_la_LDFLAGS = @GLIB_LIBS@ -no-undefined -version-info 10:1:9 @LIBLDFLAGSWIN@

pkginclude_HEADERS = voikko.h
noinst_HEADERS = \
    utils/utils.hpp utils/StringUtils.hpp \
    morphology/Analysis.hpp \
    morphology/Analyzer.hpp \
    morphology/AnalyzerFactory.hpp \
    morphology/MalagaAnalyzer.hpp \
    morphology/malaga/analysis.hpp \
    morphology/malaga/input.hpp \
    morphology/malaga/malaga_files.hpp \
    morphology/malaga/patterns.hpp \
    morphology/malaga/rule_type.hpp \
    morphology/malaga/tries.hpp \
    morphology/malaga/basic.hpp \
    morphology/malaga/lexicon.hpp \
    morphology/malaga/malaga.hpp \
    morphology/malaga/pools.hpp \
    morphology/malaga/scanner.hpp \
    morphology/malaga/value_parser.hpp \
    morphology/malaga/files.hpp \
    morphology/malaga/libmalaga.hpp \
    morphology/malaga/malaga_lib.hpp \
    morphology/malaga/rules.hpp \
    morphology/malaga/symbols.hpp \
    morphology/malaga/values.hpp \
    setup/setup.hpp setup/Dictionary.hpp setup/DictionaryLoader.hpp \
    setup/DictionaryException.hpp \
    character/charset.hpp \
    hyphenator/hyphenator.hpp voikko_defs.h porting.h \
    spellchecker/spell.hpp \
    spellchecker/SpellWithPriority.hpp \
    spellchecker/SpellUtils.hpp \
    spellchecker/suggestion/Suggestion.hpp \
    spellchecker/suggestion/SuggestionGenerator.hpp \
    spellchecker/suggestion/SuggestionGeneratorCaseChange.hpp \
    spellchecker/suggestion/SuggestionGeneratorDeletion.hpp \
    spellchecker/suggestion/SuggestionGeneratorInsertion.hpp \
    spellchecker/suggestion/SuggestionGeneratorInsertSpecial.hpp \
    spellchecker/suggestion/SuggestionGeneratorReplacement.hpp \
    spellchecker/suggestion/SuggestionGeneratorReplaceTwo.hpp \
    spellchecker/suggestion/SuggestionGeneratorSplitWord.hpp \
    spellchecker/suggestion/SuggestionGeneratorSwap.hpp \
    spellchecker/suggestion/SuggestionGeneratorVowelChange.hpp \
    spellchecker/suggestion/SuggestionStrategy.hpp \
    spellchecker/suggestion/SuggestionStrategyOcr.hpp \
    spellchecker/suggestion/SuggestionStrategyTyping.hpp \
    spellchecker/suggestion/SuggestionStatus.hpp \
    grammar/cache.hpp grammar/checks.hpp grammar/error.hpp \
    grammar/cachesetup.hpp grammar/analysis.hpp \
    grammar/Token.hpp grammar/Sentence.hpp grammar/Paragraph.hpp \
    grammar/CacheEntry.hpp \
    autocorrect/AutoCorrect.hpp autocorrect/TrieNode.hpp

EXTRA_DIST = autocorrect/triecompiler.py
BUILT_SOURCES = autocorrect/data.hpp
CLEANFILES = autocorrect/data.hpp
autocorrect/data.hpp: Makefile $(srcdir)/../data/autocorrect/fi_FI.xml $(srcdir)/autocorrect/triecompiler.py
	python $(srcdir)/autocorrect/triecompiler.py $(srcdir)/../data/autocorrect/fi_FI.xml autocorrect/data.hpp

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = libvoikko.pc
