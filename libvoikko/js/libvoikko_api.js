var PTR_SIZE = 4;
var c_init = Module.cwrap('voikkoInit', 'number', ['number', 'string', 'string']);
var c_terminate = Module.cwrap('voikkoTerminate', null, ['number']);
var c_setBooleanOption = Module.cwrap('voikkoSetBooleanOption', 'number', ['number', 'number', 'number']);
var c_setIntegerOption = Module.cwrap('voikkoSetIntegerOption', 'number', ['number', 'number', 'number']);
var c_spellCstr = Module.cwrap('voikkoSpellCstr', 'number', ['number', 'string']);
var c_suggestCstr = Module.cwrap('voikkoSuggestCstr', 'number', ['number', 'string']);
var c_hyphenateCstr = Module.cwrap('voikkoHyphenateCstr', 'number', ['number', 'string']);
var c_freeCstrArray = Module.cwrap('voikkoFreeCstrArray', null, ['number']);
var c_freeCstr = Module.cwrap('voikkoFreeCstr', null, ['number']);
var c_nextTokenCstr = Module.cwrap('voikkoNextTokenCstr', 'number', ['number', 'number', 'number', 'number']);
var c_nextSentenceStartCstr = Module.cwrap('voikkoNextSentenceStartCstr', 'number', ['number', 'number', 'number', 'number']);
var c_nextGrammarErrorCstr = Module.cwrap('voikkoNextGrammarErrorCstr', 'number', ['number', 'string', 'number', 'number', 'number']);
var c_grammarErrorCode = Module.cwrap('voikkoGetGrammarErrorCode', 'number', ['number']);
var c_grammarErrorStartPos = Module.cwrap('voikkoGetGrammarErrorStartPos', 'number', ['number']);
var c_grammarErrorLength = Module.cwrap('voikkoGetGrammarErrorLength', 'number', ['number']);
var c_grammarErrorSuggestions = Module.cwrap('voikkoGetGrammarErrorSuggestions', 'number', ['number']);
var c_freeGrammarError = Module.cwrap('voikkoFreeGrammarError', null, ['number']);
var c_grammarErrorShortDescription = Module.cwrap('voikkoGetGrammarErrorShortDescription', 'number', ['number', 'string']);
var c_freeErrorMessageCstr = Module.cwrap('voikkoFreeErrorMessageCstr', null, ['number']);
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
	
	var tokenTypes = ["NONE", "WORD", "PUNCTUATION", "WHITESPACE", "UNKNOWN"];
	var sentenceTypes = ["NONE", "NO_START", "PROBABLE", "POSSIBLE"];
	var tokensNonNull = function(text) {
		var result = [];
		var textLengthUtf8 = lengthBytesUTF8(text);
		var textBytes = Module._malloc(textLengthUtf8 + 1);
		stringToUTF8(text, textBytes, textLengthUtf8 + 1);
		var bytesStart = 0;
		var textStart = 0;
		var bytesLen = textLengthUtf8;
		var tokenLenByRef = mallocPtr();
		while (bytesLen > 0) {
			var textBytesPosition = textBytes + bytesStart;
			var tokenTypeInt = c_nextTokenCstr(handle, textBytesPosition, bytesLen, tokenLenByRef);
			var tokenLen = getValue(tokenLenByRef, "i32");
			var tokenType = tokenTypes[tokenTypeInt];
			var tokenText = text.substring(textStart, textStart + tokenLen);
			textStart += tokenText.length;
			var tokenBytes = lengthBytesUTF8(tokenText);
			result.push({type: tokenType, text: tokenText});
			bytesStart += tokenBytes;
			bytesLen -= tokenBytes;
		}
		Module._free(tokenLenByRef);
		Module._free(textBytes);
		return result;
	}
	
	var getHyphenationPattern = function(word) {
		if (!isValidInput(word)) {
			// return string of spaces
			var sb = "";
			for (var i = 0; i < word.length; i++) {
				sb += " ";
			}
			return sb;
		}
		var cPattern = c_hyphenateCstr(handle, word);
		var pattern = UTF8ToString(cPattern);
		c_freeCstr(cPattern);
		return pattern;
	};
	
	var extractSuggestions = function(cArrayPtr, targetList) {
		var cSuggestionsPtr = cArrayPtr;
		var cSuggestion = getValue(cSuggestionsPtr, "i32*");
		while (cSuggestion != 0) {
			targetList.push(UTF8ToString(cSuggestion));
			cSuggestionsPtr += PTR_SIZE;
			cSuggestion = getValue(cSuggestionsPtr, "i32*");
		}
	}
	
	var boolToInt = function(value) {
		return value ? 1 : 0;
	};
	
	var setBoolOption = function(option, value) {
		var result = c_setBooleanOption(handle, option, boolToInt(value));
		if (result == 0) {
			throw "Could not set boolean option " + option + " to value " + value + ".";
		}
	};
	
	var setIntegerOption = function(option, value) {
		var result = c_setIntegerOption(handle, option, value);
		if (result == 0) {
			throw "Could not set integer option " + option + " to value " + value + ".";
		}
	};
	
	var getGrammarError = function(cError, offset, language) {
		var errorCode = c_grammarErrorCode(cError);
		var startPos = c_grammarErrorStartPos(cError);
		var errorLength = c_grammarErrorLength(cError);
		var cSuggestions = c_grammarErrorSuggestions(cError);
		var suggestions = [];
		if (cSuggestions) {
			extractSuggestions(cSuggestions, suggestions);
		}
		var cShortDescription = c_grammarErrorShortDescription(cError, language);
		var shortDescription = UTF8ToString(cShortDescription);
		c_freeErrorMessageCstr(cShortDescription);
		return {
			errorCode: errorCode,
			startPos: offset + startPos,
			errorLen: errorLength,
			suggestions: suggestions,
			shortDescription: shortDescription
		};
	};
	var appendErrorsFromParagraph = function(errorList, paragraph, offset, language) {
		var paragraphLen = lengthBytesUTF8(paragraph);
		var skipErrors = 0;
		while (true) {
			var cError = c_nextGrammarErrorCstr(handle, paragraph, paragraphLen, 0, skipErrors);
			if (!cError) {
				return;
			}
			errorList.push(getGrammarError(cError, offset, language));
			c_freeGrammarError(cError);
			skipErrors++;
		}
	};
	
	return {
		terminate: function() {
			if (handle) {
				c_terminate(handle);
				handle = null;
			}
		},
		
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
			extractSuggestions(cSuggestions, suggestions);
			c_freeCstrArray(cSuggestions);
			return suggestions;
		},
		
		grammarErrors: function(text, language) {
			var errorList = [];
			if (!isValidInput(text)) {
				return errorList;
			}
			var offset = 0;
			var paragraphs = text.replace("\r", "\n").split("\n");
			for (var i = 0; i < paragraphs.length; i++) {
				var paragraph = paragraphs[i];
				appendErrorsFromParagraph(errorList, paragraph, offset, language);
				offset += paragraph.length + 1;
			}
			return errorList;
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
		},
		
		tokens: function(text) {
			var allTokens = [];
			var lastStart = 0;
			for (var i = text.indexOf("\0"); i != -1; i = text.indexOf("\0", i + 1)) {
				allTokens.push.apply(allTokens, tokensNonNull(text.substring(lastStart, i)));
				allTokens.push({type: "UNKNOWN", text: "\0"});
				lastStart = i + 1;
			}
			allTokens.push.apply(allTokens, tokensNonNull(text.substring(lastStart)));
			return allTokens;
		},
		
		sentences: function(text) {
			var result = [];
			if (!isValidInput(text)) {
				result.push({text: text, nextStartType: "NONE"});
				return result;
			}
			var textLen = lengthBytesUTF8(text);
			var textBytes = Module._malloc(textLen + 1);
			var textBytesPtr = textBytes;
			stringToUTF8(text, textBytes, textLen + 1);
			var sentenceLenByRef = mallocPtr();
			while (textLen > 0) {
				var sentenceTypeInt = c_nextSentenceStartCstr(handle, textBytesPtr, textLen, sentenceLenByRef);
				var sentenceLen = getValue(sentenceLenByRef, "i32");
				var sentenceType = sentenceTypes[sentenceTypeInt];
				var sentenceText = text.substring(0, sentenceLen);
				result.push({text: sentenceText, nextStartType: sentenceType});
				text = text.substring(sentenceLen);
				var sentenceLenBytes = lengthBytesUTF8(sentenceText);
				textBytesPtr += sentenceLenBytes;
				textLen -= sentenceLenBytes;
			}
			Module._free(sentenceLenByRef);
			Module._free(textBytes);
			return result;
		},
		
		getHyphenationPattern: getHyphenationPattern,
		
		hyphenate: function(word) {
			var pattern = getHyphenationPattern(word);
			var hyphenated = "";
			for (var i = 0; i < pattern.length; i++) {
				var patternC = pattern.charAt(i);
				if (patternC == ' ') {
					hyphenated += word.charAt(i);
				} else if (patternC == '-') {
					hyphenated += '-';
					hyphenated += word.charAt(i);
				} else if (patternC == '=') {
					hyphenated += '-';
				}
			}
			return hyphenated;
		},
		
		setIgnoreDot: function(value) {
			setBoolOption(0, value);
		},
		
		setIgnoreNumbers: function(value) {
			setBoolOption(1, value);
		},
		
		setIgnoreUppercase: function(value) {
			setBoolOption(3, value);
		},
		
		setAcceptFirstUppercase: function(value) {
			setBoolOption(6, value);
		},
		
		setAcceptAllUppercase: function(value) {
			setBoolOption(7, value);
		},
		
		setIgnoreNonwords: function(value) {
			setBoolOption(10, value);
		},
		
		setAcceptExtraHyphens: function(value) {
			setBoolOption(11, value);
		},
		
		setAcceptMissingHyphens: function(value) {
			setBoolOption(12, value);
		},
		
		setAcceptTitlesInGc: function(value) {
			setBoolOption(13, value);
		},
		
		setAcceptUnfinishedParagraphsInGc: function(value) {
			setBoolOption(14, value);
		},
		
		setAcceptBulletedListsInGc: function(value) {
			setBoolOption(16, value);
		},
		
		setNoUglyHyphenation: function(value) {
			setBoolOption(4, value);
		},
		
		setHyphenateUnknownWords: function(value) {
			setBoolOption(15, value);
		},
		
		setMinHyphenatedWordLength: function(value) {
			setIntegerOption(9, value);
		},
		
		setSuggestionStrategy: function(value) {
			if (value == "TYPO") {
				setBoolOption(8, false);
			}
			else if (value == "OCR") {
				setBoolOption(8, true);
			}
			else {
				throw "Unknown suggestion strategy " + value;
			}
		}
	};
};
