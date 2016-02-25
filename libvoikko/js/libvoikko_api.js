var c_init = Module.cwrap('voikkoInit', 'number', ['number', 'string', 'string'])
var c_spellCstr = Module.cwrap('voikkoSpellCstr', 'number', ['number', 'string'])
var c_freeCstr = Module.cwrap('voikkoFreeCstr', null, ['number']);

Module.init = function(lang, path) {
	var handle = c_init(0, lang, path);
	return {
		spell: function(word) {
			return c_spellCstr(handle, word);
		}
	}
}
