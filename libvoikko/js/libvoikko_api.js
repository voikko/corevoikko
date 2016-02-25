var c_init = Module.cwrap('voikkoInit', 'number', ['number', 'string', 'string'])
var c_spellCstr = Module.cwrap('voikkoSpellCstr', 'number', ['number', 'string'])
var c_freeCstr = Module.cwrap('voikkoFreeCstr', null, ['number']);
