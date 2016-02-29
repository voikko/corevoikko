var PTR_SIZE = 4;
var c_init = Module.cwrap('voikkoInit', 'number', ['number', 'string', 'string']);
var c_spellCstr = Module.cwrap('voikkoSpellCstr', 'number', ['number', 'string']);
var c_suggestCstr = Module.cwrap('voikkoSuggestCstr', 'number', ['number', 'string']);
var c_freeCstrArray = Module.cwrap('voikkoFreeCstrArray', null, ['number']);
var c_freeCstr = Module.cwrap('voikkoFreeCstr', null, ['number']);
var c_analyzeWordCstr = Module.cwrap('voikkoAnalyzeWordCstr', 'number', ['number', 'string']);
var c_freeMorAnalysis = Module.cwrap('voikko_free_mor_analysis', null, ['number']);
var c_morAnalysisKeys = Module.cwrap('voikko_mor_analysis_keys', 'number', ['number']);
var c_morAnalysisValueCstr = Module.cwrap('voikko_mor_analysis_value_cstr', 'number', ['number', 'number']);
var c_freeMorAnalysisValueCstr = Module.cwrap('voikko_free_mor_analysis_value_cstr', null, ['number']);

var mallocPtr = function() {
	return Module._malloc(PTR_SIZE);
};

var isValidInput = function(word) {
	return word.indexOf('\0') == -1;
}

Module.init = function(lang, path) {
	var errorPtr = mallocPtr();
	var handle = c_init(errorPtr, lang, path);
	if (!handle) {
		var errorStrPtr = getValue(errorPtr, "i32*");
		var errorStr = UTF8ToString(errorStrPtr);
		Module._free(errorPtr);
		throw errorStr;
	}
	Module._free(errorPtr);
	return {
		spell: function(word) {
			return isValidInput(word) && c_spellCstr(handle, word) == 1;
		},
		
		suggest: function(word) {
			var suggestions = [];
			if (!isValidInput(word)) {
				return suggestions;
			}
			var cSuggestions = c_suggestCstr(handle, word);
			if (cSuggestions == 0) {
				return suggestions;
			}
			var cSuggestionsPtr = cSuggestions;
			var cSuggestion = getValue(cSuggestionsPtr, "i32*");
			while (cSuggestion != 0) {
				suggestions.push(UTF8ToString(cSuggestion));
				cSuggestionsPtr += PTR_SIZE;
				cSuggestion = getValue(cSuggestionsPtr, "i32*");
			}
			c_freeCstrArray(cSuggestions);
			return suggestions;
		},
		
		analyze: function(word) {
			var analysisList = [];
			if (!isValidInput(word)) {
				return analysisList;
			}
		        var cAnalysisList = c_analyzeWordCstr(handle, word);
			if (!cAnalysisList) {
				return analysisList;
			}
			var cAnalysisListPtr = cAnalysisList;
			var cAnalysis = getValue(cAnalysisListPtr, "i32*");
			while (cAnalysis != 0) {
				var cKeys = c_morAnalysisKeys(cAnalysis);
				var analysis = {};
				var cKeysPtr = cKeys;
				var cKey = getValue(cKeysPtr, "i32*");
				while (cKey != 0) {
					var key = UTF8ToString(cKey);
					var value = c_morAnalysisValueCstr(cAnalysis, cKey);
					analysis[key] = UTF8ToString(value);
					c_freeMorAnalysisValueCstr(value);
					cKeysPtr += PTR_SIZE;
					cKey = getValue(cKeysPtr, "i32*");
				}
				analysisList.push(analysis);
				cAnalysisListPtr += PTR_SIZE;
				var cAnalysis = getValue(cAnalysisListPtr, "i32*");
			}
			c_freeMorAnalysis(cAnalysisList);
			return analysisList;
		}
	};
};
