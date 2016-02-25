#!/bin/sh

emmake make
emcc src/.libs/libvoikko.so.1.14.3 --preload-file 5 -o js/libvoikko.html --post-js js/libvoikko_api.js -s MODULARIZE="1" -s EXPORT_NAME="'Libvoikko'" -s EXPORTED_FUNCTIONS="['_voikkoInit','_voikkoTerminate','_voikkoSetBooleanOption','_voikkoSetIntegerOption','_voikkoSpellCstr','_voikkoSuggestCstr','_voikkoHyphenateCstr','_voikkoFreeCstrArray','_voikkoFreeCstr']"
