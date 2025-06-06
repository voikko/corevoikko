AUTOMAKE_OPTIONS=subdir-objects

lib_LTLIBRARIES = libvoikko.la
libvoikko_la_SOURCES = \
    fst/Configuration.cpp \
    fst/WeightedConfiguration.cpp \
    fst/Transducer.cpp \
    fst/UnweightedTransducer.cpp \
    fst/WeightedTransducer.cpp \
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
    spellchecker/suggestion/SuggestionGeneratorDeleteTwo.cpp \
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
    setup/setup.cpp setup/Dictionary.cpp \
    setup/BackendProperties.cpp \
    setup/DictionaryFactory.cpp \
    setup/DictionaryLoader.cpp \
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
    grammar/FinnishRuleEngine/checks.cpp \
    grammar/error.cpp grammar/Analysis.cpp \
    grammar/Sentence.cpp grammar/Paragraph.cpp grammar/CacheEntry.cpp \
    grammar/GcCache.cpp \
    grammar/VoikkoGrammarError.cpp \
    grammar/NullGrammarChecker.cpp \
    grammar/FinnishGrammarChecker.cpp \
    grammar/GrammarChecker.cpp \
    grammar/GrammarCheckerFactory.cpp \
    grammar/FinnishAnalysis.cpp \
    grammar/HfstAnalysis.cpp \
    grammar/FinnishRuleEngine.cpp \
    grammar/RuleEngine.cpp \
    grammar/FinnishRuleEngine/CapitalizationCheck.cpp \
    grammar/FinnishRuleEngine/CompoundVerbCheck.cpp \
    grammar/FinnishRuleEngine/MissingVerbCheck.cpp \
    grammar/FinnishRuleEngine/NegativeVerbCheck.cpp \
    grammar/FinnishRuleEngine/ParagraphCheck.cpp \
    grammar/FinnishRuleEngine/SentenceCheck.cpp \
    grammar/FinnishRuleEngine/SidesanaCheck.cpp \
    grammar/FinnishRuleEngine/RelativePronounCheck.cpp \
    compatibility/interface.cpp
libvoikko_la_LDFLAGS = -no-undefined -version-info 17:3:16 @LIBLDFLAGSWIN@

pkginclude_HEADERS = voikko.h voikko_enums.h voikko_defines.h voikko_deprecated.h voikko_structs.h
noinst_HEADERS = \
    fst/Configuration.hpp \
    fst/WeightedConfiguration.hpp \
    fst/Transducer.hpp \
    fst/Transition.hpp \
    fst/WeightedTransition.hpp \
    fst/UnweightedTransducer.hpp \
    fst/WeightedTransducer.hpp \
    utils/utils.hpp utils/StringUtils.hpp \
    hyphenator/Hyphenator.hpp \
    hyphenator/AnalyzerToFinnishHyphenatorAdapter.hpp \
    hyphenator/HyphenatorFactory.hpp \
    hyphenator/HfstHyphenator.hpp \
    morphology/Analysis.hpp \
    morphology/Analyzer.hpp \
    morphology/AnalyzerFactory.hpp \
    morphology/HfstAnalyzer.hpp \
    morphology/VfstAnalyzer.hpp \
    morphology/FinnishVfstAnalyzer.hpp \
    morphology/LttoolboxAnalyzer.hpp \
    morphology/NullAnalyzer.hpp \
    setup/setup.hpp setup/Dictionary.hpp \
    setup/BackendProperties.hpp \
    setup/DictionaryFactory.hpp \
    setup/DictionaryLoader.hpp \
    setup/V3DictionaryLoader.hpp \
    setup/V4DictionaryLoader.hpp \
    setup/V5DictionaryLoader.hpp \
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
    spellchecker/VfstSpeller.hpp \
    spellchecker/VfstSuggestion.hpp \
    spellchecker/suggestion/Suggestion.hpp \
    spellchecker/suggestion/SuggestionGenerator.hpp \
    spellchecker/suggestion/SuggestionGeneratorFactory.hpp \
    spellchecker/suggestion/SuggestionGeneratorCaseChange.hpp \
    spellchecker/suggestion/SuggestionGeneratorDeletion.hpp \
    spellchecker/suggestion/SuggestionGeneratorDeleteTwo.hpp \
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
    grammar/FinnishRuleEngine/checks.hpp grammar/error.hpp \
    grammar/Analysis.hpp \
    grammar/Token.hpp grammar/Sentence.hpp grammar/Paragraph.hpp \
    grammar/CacheEntry.hpp \
    grammar/GcCache.hpp \
    grammar/RuleEngine.hpp \
    grammar/NullGrammarChecker.hpp \
    grammar/FinnishGrammarChecker.hpp \
    grammar/GrammarChecker.hpp \
    grammar/GrammarCheckerFactory.hpp \
    grammar/VoikkoGrammarError.hpp \
    grammar/FinnishRuleEngine.hpp \
    grammar/FinnishAnalysis.hpp \
    grammar/HfstAnalysis.hpp \
    grammar/RuleEngine.hpp \
    grammar/FinnishRuleEngine/CapitalizationCheck.hpp \
    grammar/FinnishRuleEngine/CompoundVerbCheck.hpp \
    grammar/FinnishRuleEngine/MissingVerbCheck.hpp \
    grammar/FinnishRuleEngine/NegativeVerbCheck.hpp \
    grammar/FinnishRuleEngine/ParagraphCheck.hpp \
    grammar/FinnishRuleEngine/SentenceCheck.hpp \
    grammar/FinnishRuleEngine/SidesanaCheck.hpp \
    grammar/FinnishRuleEngine/RelativePronounCheck.hpp \
    grammar/FinnishRuleEngine/VfstAutocorrectCheck.hpp \
    utf8/core.hpp \
    utf8/checked.hpp \
    utf8/unchecked.hpp \
    utf8/utf8.hpp

EXTRA_DIST =


HFST_CPP_FILES=morphology/HfstAnalyzer.cpp \
    spellchecker/HfstSpeller.cpp \
    spellchecker/HfstSuggestion.cpp \
    hyphenator/HfstHyphenator.cpp \
    setup/V3DictionaryLoader.cpp

VISLCG3_CPP_FILES=grammar/CgRuleEngine.cpp \
    setup/V4DictionaryLoader.cpp \
    grammar/CgGrammarChecker.cpp

LTTOOLBOX_CPP_FILES=morphology/LttoolboxAnalyzer.cpp

VFST_CPP_FILES=morphology/VfstAnalyzer.cpp morphology/FinnishVfstAnalyzer.cpp \
    spellchecker/VfstSpeller.cpp spellchecker/VfstSuggestion.cpp setup/V5DictionaryLoader.cpp \
    grammar/FinnishRuleEngine/VfstAutocorrectCheck.cpp

if HAVE_VFST
    libvoikko_la_SOURCES += $(VFST_CPP_FILES)
else
    EXTRA_DIST += $(VFST_CPP_FILES)
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

if HAVE_VISLCG3
    libvoikko_la_SOURCES += $(VISLCG3_CPP_FILES)
    libvoikko_la_LDFLAGS += $(VISLCG3_LIBS) -ltinyxml2
else
    EXTRA_DIST += $(VISLCG3_CPP_FILES)
endif


pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = libvoikko.pc
