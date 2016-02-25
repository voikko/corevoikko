#!/bin/sh

emmake make
emcc src/.libs/libvoikko.so.1.14.3 -o js/libvoikko.html -s EXPORTED_FUNCTIONS="['_voikkoInit','_voikkoTerminate','_voikkoSetBooleanOption','_voikkoSetIntegerOption','_voikkoSpellCstr','_voikkoSuggestCstr','_voikkoHyphenateCstr','_voikkoFreeCstrArray','_voikkoFreeCstr']"
