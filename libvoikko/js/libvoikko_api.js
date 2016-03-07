var PTR_SIZE = 4;
var c_malloc = Module["_malloc"];
var c_free = Module["_free"];
var cwrap = Module["cwrap"];
var c_init = cwrap('voikkoInit', 'number', ['number', 'string', 'string']);
var c_terminate = cwrap('voikkoTerminate', null, ['number']);
var c_setBooleanOption = cwrap('voikkoSetBooleanOption', 'number', ['number', 'number', 'number']);
var c_setIntegerOption = cwrap('voikkoSetIntegerOption', 'number', ['number', 'number', 'number']);
var c_spellCstr = cwrap('voikkoSpellCstr', 'number', ['number', 'string']);
var c_suggestCstr = cwrap('voikkoSuggestCstr', 'number', ['number', 'string']);
var c_hyphenateCstr = cwrap('voikkoHyphenateCstr', 'number', ['number', 'string']);
var c_freeCstrArray = cwrap('voikkoFreeCstrArray', null, ['number']);
var c_freeCstr = cwrap('voikkoFreeCstr', null, ['number']);
var c_nextTokenCstr = cwrap('voikkoNextTokenCstr', 'number', ['number', 'number', 'number', 'number']);
var c_nextSentenceStartCstr = cwrap('voikkoNextSentenceStartCstr', 'number', ['number', 'number', 'number', 'number']);
var c_nextGrammarErrorCstr = cwrap('voikkoNextGrammarErrorCstr', 'number', ['number', 'string', 'number', 'number', 'number']);
var c_grammarErrorCode = cwrap('voikkoGetGrammarErrorCode', 'number', ['number']);
var c_grammarErrorStartPos = cwrap('voikkoGetGrammarErrorStartPos', 'number', ['number']);
var c_grammarErrorLength = cwrap('voikkoGetGrammarErrorLength', 'number', ['number']);
var c_grammarErrorSuggestions = cwrap('voikkoGetGrammarErrorSuggestions', 'number', ['number']);
var c_freeGrammarError = cwrap('voikkoFreeGrammarError', null, ['number']);
var c_grammarErrorShortDescription = cwrap('voikkoGetGrammarErrorShortDescription', 'number', ['number', 'string']);
var c_freeErrorMessageCstr = cwrap('voikkoFreeErrorMessageCstr', null, ['number']);
var c_analyzeWordCstr = cwrap('voikkoAnalyzeWordCstr', 'number', ['number', 'string']);
var c_freeMorAnalysis = cwrap('voikko_free_mor_analysis', null, ['number']);
var c_morAnalysisKeys = cwrap('voikko_mor_analysis_keys', 'number', ['number']);
var c_morAnalysisValueCstr = cwrap('voikko_mor_analysis_value_cstr', 'number', ['number', 'number']);
var c_freeMorAnalysisValueCstr = cwrap('voikko_free_mor_analysis_value_cstr', null, ['number']);

var mallocPtr = function() {
	return c_malloc(PTR_SIZE);
};

var isValidInput = function(word) {
	return word.indexOf('\0') == -1;
}

Module["onRuntimeInitialized"] = function() {
	if ("onLoad" in Module) {
		Module["onLoad"](Module);
	}
};

Module["init"] = function(lang, path) {
	var errorPtr = mallocPtr();
	var handle = c_init(errorPtr, lang, path);
	if (!handle) {
		var errorStrPtr = getValue(errorPtr, "i32*");
		var errorStr = UTF8ToString(errorStrPtr);
		c_free(errorPtr);
		throw errorStr;
	}
	c_free(errorPtr);
	
	var tokenTypes = ["NONE", "WORD", "PUNCTUATION", "WHITESPACE", "UNKNOWN"];
	var sentenceTypes = ["NONE", "NO_START", "PROBABLE", "POSSIBLE"];
	var tokensNonNull = function(text) {
		var result = [];
		var textLengthUtf8 = lengthBytesUTF8(text);
		var textBytes = c_malloc(textLengthUtf8 + 1);
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
		c_free(tokenLenByRef);
		c_free(textBytes);
		return result;
	}
	
	var getHyphenationPattern = function(word) {
		requireValidHandle();
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
		requireValidHandle();
		var result = c_setBooleanOption(handle, option, boolToInt(value));
		if (result == 0) {
			throw "Could not set boolean option " + option + " to value " + value + ".";
		}
	};
	
	var setIntegerOption = function(option, value) {
		requireValidHandle();
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
			"errorCode": errorCode,
			"startPos": offset + startPos,
			"errorLen": errorLength,
			"suggestions": suggestions,
			"shortDescription": shortDescription
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
	
	var requireValidHandle = function() {
		if (handle == null) {
			throw "Attempt to use Voikko instance after terminate() was called";
		}
	};
	
	return {
		"terminate": function() {
			if (handle) {
				c_terminate(handle);
				handle = null;
			}
		},
		
		"spell": function(word) {
			requireValidHandle();
			return isValidInput(word) && c_spellCstr(handle, word) == 1;
		},
		
		"suggest": function(word) {
			requireValidHandle();
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
		
		"grammarErrors": function(text, language) {
			requireValidHandle();
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
		
		"analyze": function(word) {
			requireValidHandle();
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
		
		"tokens": function(text) {
			requireValidHandle();
			var allTokens = [];
			var lastStart = 0;
			for (var i = text.indexOf("\0"); i != -1; i = text.indexOf("\0", i + 1)) {
				allTokens.push.apply(allTokens, tokensNonNull(text.substring(lastStart, i)));
				allTokens.push({"type": "UNKNOWN", "text": "\0"});
				lastStart = i + 1;
			}
			allTokens.push.apply(allTokens, tokensNonNull(text.substring(lastStart)));
			return allTokens;
		},
		
		"sentences": function(text) {
			requireValidHandle();
			var result = [];
			if (!isValidInput(text)) {
				result.push({"text": text, "nextStartType": "NONE"});
				return result;
			}
			var textLen = lengthBytesUTF8(text);
			var textBytes = c_malloc(textLen + 1);
			var textBytesPtr = textBytes;
			stringToUTF8(text, textBytes, textLen + 1);
			var sentenceLenByRef = mallocPtr();
			while (textLen > 0) {
				var sentenceTypeInt = c_nextSentenceStartCstr(handle, textBytesPtr, textLen, sentenceLenByRef);
				var sentenceLen = getValue(sentenceLenByRef, "i32");
				var sentenceType = sentenceTypes[sentenceTypeInt];
				var sentenceText = text.substring(0, sentenceLen);
				result.push({"text": sentenceText, "nextStartType": sentenceType});
				text = text.substring(sentenceLen);
				var sentenceLenBytes = lengthBytesUTF8(sentenceText);
				textBytesPtr += sentenceLenBytes;
				textLen -= sentenceLenBytes;
			}
			c_free(sentenceLenByRef);
			c_free(textBytes);
			return result;
		},
		
		"getHyphenationPattern": getHyphenationPattern,
		
		"hyphenate": function(word) {
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
		
		"setIgnoreDot": function(value) {
			setBoolOption(0, value);
		},
		
		"setIgnoreNumbers": function(value) {
			setBoolOption(1, value);
		},
		
		"setIgnoreUppercase": function(value) {
			setBoolOption(3, value);
		},
		
		"setAcceptFirstUppercase": function(value) {
			setBoolOption(6, value);
		},
		
		"setAcceptAllUppercase": function(value) {
			setBoolOption(7, value);
		},
		
		"setIgnoreNonwords": function(value) {
			setBoolOption(10, value);
		},
		
		"setAcceptExtraHyphens": function(value) {
			setBoolOption(11, value);
		},
		
		"setAcceptMissingHyphens": function(value) {
			setBoolOption(12, value);
		},
		
		"setAcceptTitlesInGc": function(value) {
			setBoolOption(13, value);
		},
		
		"setAcceptUnfinishedParagraphsInGc": function(value) {
			setBoolOption(14, value);
		},
		
		"setAcceptBulletedListsInGc": function(value) {
			setBoolOption(16, value);
		},
		
		"setNoUglyHyphenation": function(value) {
			setBoolOption(4, value);
		},
		
		"setHyphenateUnknownWords": function(value) {
			setBoolOption(15, value);
		},
		
		"setMinHyphenatedWordLength": function(value) {
			setIntegerOption(9, value);
		},
		
		"setSuggestionStrategy": function(value) {
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
