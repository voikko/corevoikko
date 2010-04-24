/* Libvoikko: Library of Finnish language tools
 * Copyright (C) 2010 Harri Pitkänen <hatapitk@iki.fi>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *********************************************************************************/

#include "grammar/check/CapitalizationCheck.hpp"
#include "grammar/error.hpp"
#include "grammar/cache.hpp"
#include "character/SimpleChar.hpp"
#include "character/charset.hpp"
#include "utils/StringUtils.hpp"
#include <list>
#include <stack>

using namespace libvoikko::character;
using namespace libvoikko::utils;
using namespace std;

namespace libvoikko { namespace grammar { namespace check {

struct CapitalizationContext {
	const Paragraph * paragraph;
	size_t currentSentence; // may be one past the end of array if at the end of paragraph
	size_t currentToken;
	const Token * nextWord;
	voikko_options_t * options;
	stack<wchar_t> quotes;
};

enum CapitalizationState {
	INITIAL,
	UPPER,
	LOWER,
	DONT_CARE,
	QUOTED
};

static bool shouldSkipSentence(const Sentence * sentence) {
	// Check if the sentence is written fully in upper case letters.
	// If it is, no character case errors should be reported.
	bool onlyUpper = true;
	for (size_t i = 0; i < sentence->tokenCount; i++) {
		Token t = sentence->tokens[i];
		if (t.str[0] == L'\u201C') {
			// The presence of a foreign quotation mark makes it hard to tell
			// what is the correct capitalization.
			return true;
		}
		if (t.type != TOKEN_WORD) {
			continue;
		}
		for (size_t j = 0; j < t.tokenlen; j++) {
			if (SimpleChar::isLower(t.str[j])) {
				onlyUpper = false;
				break;
			}
		}
		if (!onlyUpper) {
			break;
		}
	}
	return onlyUpper;
}

static const Token * getTokenAndAdvance(CapitalizationContext & context) {
	const Sentence * sentence;
	while (true) {
		if (context.paragraph->sentenceCount == context.currentSentence) {
			return 0;
		}
		sentence = context.paragraph->sentences[context.currentSentence];
		if (shouldSkipSentence(sentence)) {
			context.currentSentence++;
		} else {
			break;
		}
	}
	
	const Token * token = sentence->tokens + context.currentToken;
	++context.currentToken;
	if (sentence->tokenCount == context.currentToken) {
		context.currentToken = 0;
		context.currentSentence++;
	}
	return token;
}

static list<const Token *> getTokensUntilNextWord(CapitalizationContext & context) {
	list<const Token *> tokens;
	while (true) {
		const Token * token = getTokenAndAdvance(context);
		if (!token) {
			context.nextWord = 0;
			break;
		}
		if (token->type == TOKEN_WORD) {
			context.nextWord = token;
			break;
		}
		tokens.push_back(token);
	}
	return tokens;
}

static bool containsToken(const list<const Token *> & tokens, const wchar_t * expectedText) {
	list<const Token *>::const_iterator it = tokens.begin();
	while (it != tokens.end()) {
		if (wcscmp(expectedText, (*it)->str) == 0) {
			return true;
		}
		it++;
	}
	return false;
}

static void pushAndPopQuotes(CapitalizationContext & context, const list<const Token *> & tokens) {
	list<const Token *>::const_iterator it = tokens.begin();
	while (it != tokens.end()) {
		if ((*it)->type == TOKEN_PUNCTUATION) {
			const wchar_t * text = (*it)->str;
			if (isFinnishQuotationMark(text[0])) {
				if (context.quotes.empty()) {
					context.quotes.push(text[0]);
				} else {
					wchar_t previous = context.quotes.top();
					if (previous == text[0]) {
						context.quotes.pop();
					} else {
						context.quotes.push(text[0]);
					}
				}
			} else if (text[0] == L'(') {
				context.quotes.push(text[0]);
			} else if (text[0] == L')') {
				if (context.quotes.empty()) {
					// XXX: parenthesis errors are not really related to
					// capitalization
					CacheEntry * e = new CacheEntry(0);
					e->error.error_code = GCERR_MISPLACED_CLOSING_PARENTHESIS;
					e->error.startpos = (*it)->pos;
					e->error.errorlen = 1;
					gc_cache_append_error(context.options, e);
				} else if (context.quotes.top() == L'(') {
					context.quotes.pop();
				}
			}
		}
		it++;
	}

}

static CapitalizationState inInitial(CapitalizationContext & context) {
	list<const Token *> separators = getTokensUntilNextWord(context);
	pushAndPopQuotes(context, separators);
	if (!context.quotes.empty()) {
		return QUOTED;
	}
	if (context.options->accept_bulleted_lists_in_gc) {
		return DONT_CARE;
	}
	if (containsToken(separators, L"-")) {
		// may be some sort of bulletted list
		return DONT_CARE;
	}
	return UPPER;
}

static CapitalizationState inUpper(CapitalizationContext & context) {
	const Token * word = context.nextWord;
	if (!SimpleChar::isUpper(word->str[0]) &&
	    !SimpleChar::isDigit(word->str[0]) &&
	    !word->possibleSentenceStart) {
		CacheEntry * e = new CacheEntry(1);
		e->error.error_code = GCERR_WRITE_FIRST_UPPERCASE;
		e->error.startpos = word->pos;
		e->error.errorlen = word->tokenlen;
		wchar_t * suggestion = new wchar_t[word->tokenlen];
		suggestion[0] = SimpleChar::upper(word->str[0]);
		wcsncpy(suggestion + 1, word->str + 1, word->tokenlen - 1);
		e->error.suggestions[0] = StringUtils::utf8FromUcs4(suggestion, word->tokenlen);
		delete[] suggestion;
		gc_cache_append_error(context.options, e);
	}
	list<const Token *> separators = getTokensUntilNextWord(context);
	pushAndPopQuotes(context, separators);
	if (!context.quotes.empty()) {
		return QUOTED;
	}
	if (containsToken(separators, L"\t")) {
		return DONT_CARE;
	}
	if (containsToken(separators, L".") || containsToken(separators, L"?") ||
	    containsToken(separators, L"!")) {
		return UPPER;
	}
	return LOWER;
}

static CapitalizationState inLower(CapitalizationContext & context) {
	const Token * word = context.nextWord;
	if (word->isValidWord &&
	    word->firstLetterLcase &&
	    !word->possibleSentenceStart &&
	    word->tokenlen > 1 && // Single letters are OK in upper case
	    word->str[1] != L'-' && // A-rapussa etc.
	    SimpleChar::isUpper(word->str[0])) {
		CacheEntry * e = new CacheEntry(1);
		e->error.error_code = GCERR_WRITE_FIRST_LOWERCASE;
		e->error.startpos = word->pos;
		e->error.errorlen = word->tokenlen;
		wchar_t * suggestion = new wchar_t[word->tokenlen];
		suggestion[0] = SimpleChar::lower(word->str[0]);
		wcsncpy(suggestion + 1, word->str + 1, word->tokenlen - 1);
		e->error.suggestions[0] = StringUtils::utf8FromUcs4(suggestion, word->tokenlen);
		delete[] suggestion;
		gc_cache_append_error(context.options, e);
	}
	list<const Token *> separators = getTokensUntilNextWord(context);
	pushAndPopQuotes(context, separators);
	if (!context.quotes.empty()) {
		return QUOTED;
	}
	if (containsToken(separators, L"\t")) {
		return DONT_CARE;
	}
	if (containsToken(separators, L".") || containsToken(separators, L"?") ||
	    containsToken(separators, L"!")) {
		return UPPER;
	}
	return LOWER;
}

static CapitalizationState inDontCare(CapitalizationContext & context) {
	list<const Token *> separators = getTokensUntilNextWord(context);
	pushAndPopQuotes(context, separators);
	if (!context.quotes.empty()) {
		return QUOTED;
	}
	if (containsToken(separators, L"\t")) {
		return DONT_CARE;
	}
	if (containsToken(separators, L".") || containsToken(separators, L"?") ||
	    containsToken(separators, L"!")) {
		return UPPER;
	}
	return LOWER;
}

static CapitalizationState inQuoted(CapitalizationContext & context) {
	list<const Token *> separators = getTokensUntilNextWord(context);
	pushAndPopQuotes(context, separators);
	if (!context.quotes.empty()) {
		return QUOTED;
	}
	return LOWER;
}

static CapitalizationState (*stateFunctions[])(CapitalizationContext & context) = {&inInitial, &inUpper, &inLower, &inDontCare, &inQuoted};

void CapitalizationCheck::check(voikko_options_t * options, const Paragraph * paragraph) {
	CapitalizationContext context;
	context.paragraph = paragraph;
	context.currentSentence = 0;
	context.currentToken = 0;
	context.nextWord = 0;
	context.options = options;
	
	CapitalizationState state = INITIAL;
	while (paragraph->sentenceCount >= context.currentSentence + 1 &&
	       paragraph->sentences[context.currentSentence]->tokenCount >= context.currentToken + 1) {
		state = stateFunctions[state](context);
	}
}

} } }
