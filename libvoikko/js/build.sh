#!/bin/sh

if [ $# -eq 0 ]
then
   echo "Usage: $0 embed | preload | plain"
   echo ""
   exit 0
fi

OPTS="-O3 --closure 1"

case $1 in
   embed)
     # Closure compiler may run out of memory when large dictionary files are embedded.
     OPTS="-O1 --closure 0 --embed-file 5"
     ;;
   preload)
     OPTS="-O3 --closure 1 --preload-file 5"
     ;;
esac

emmake make

LIBFILE=`find . -name libvoikko.a`

emcc -g0 $LIBFILE $OPTS -o js/libvoikko.js --post-js js/libvoikko_api.js -s MODULARIZE="1" -s EXPORT_NAME="'Libvoikko'" -s NO_EXIT_RUNTIME="1" -s EXPORTED_FUNCTIONS="['_voikkoInit','_voikkoTerminate','_voikkoSetBooleanOption','_voikkoSetIntegerOption','_voikkoSpellCstr','_voikkoSuggestCstr','_voikkoGetAttributeValues','_voikkoHyphenateCstr','_voikkoInsertHyphensCstr','_voikkoFreeCstrArray','_voikkoFreeCstr','_voikkoNextTokenCstr','_voikkoNextSentenceStartCstr','_voikkoNextGrammarErrorCstr','_voikkoGetGrammarErrorCode','_voikkoGetGrammarErrorStartPos','_voikkoGetGrammarErrorLength','_voikkoGetGrammarErrorSuggestions','_voikkoFreeGrammarError','_voikkoGetGrammarErrorShortDescription','_voikkoFreeErrorMessageCstr','_voikko_list_dicts','_voikko_free_dicts','_voikko_dict_language','_voikko_dict_script','_voikko_dict_variant','_voikko_dict_description','_voikkoGetVersion','_voikkoAnalyzeWordCstr','_voikko_free_mor_analysis','_voikko_mor_analysis_keys','_voikko_mor_analysis_value_cstr','_voikko_free_mor_analysis_value_cstr','_emscripten_builtin_memalign','_free','_malloc']" -s EXPORTED_RUNTIME_METHODS="['cwrap']" -s ALLOW_MEMORY_GROWTH="1"
cat js/libvoikko.js js/commonjs-footer.js > js/js-libvoikko.js
