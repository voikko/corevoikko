var c_init = Module.cwrap('voikkoInit', 'number', ['number', 'string', 'string'])
var c_spellCstr = Module.cwrap('voikkoSpellCstr', 'number', ['number', 'string'])
var c_suggestCstr = Module.cwrap('voikkoSuggestCstr', 'number', ['number', 'string'])
var c_freeCstrArray = Module.cwrap('voikkoFreeCstrArray', null, ['number']);
var c_freeCstr = Module.cwrap('voikkoFreeCstr', null, ['number']);

var mallocPtr = function() {
	return Module._malloc(4);
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
			return c_spellCstr(handle, word) == 1;
		},
		suggest: function(word) {
			var cSuggestions = c_suggestCstr(handle, word);
			var suggestions = [];
			if (cSuggestions == 0) {
				return suggestions;
			}
			for (var cSuggestion = getValue(cSuggestions, "i32*"); cSuggestion != 0; cSuggestions += 4) {
				suggestions.push(UTF8ToString(cSuggestion));
				cSuggestion = getValue(cSuggestions, "i32*");
			}
			c_freeCstrArray(cSuggestions);
			return suggestions;
		}
	}
}