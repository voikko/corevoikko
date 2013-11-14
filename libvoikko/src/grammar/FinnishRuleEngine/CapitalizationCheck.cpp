/* The contents of this file are subject to the Mozilla Public License Version 
 * 1.1 (the "License"); you may not use this file except in compliance with 
 * the License. You may obtain a copy of the License at 
 * http://www.mozilla.org/MPL/
 * 
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 * 
 * The Original Code is Libvoikko: Library of natural language processing tools.
 * The Initial Developer of the Original Code is Harri Pitkänen <hatapitk@iki.fi>.
 * Portions created by the Initial Developer are Copyright (C) 2010
 * the Initial Developer. All Rights Reserved.
 * 
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *********************************************************************************/

#include "grammar/FinnishRuleEngine/CapitalizationCheck.hpp"
#include "grammar/error.hpp"
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
	bool sentenceEnded;
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
		++it;
	}
	return false;
}

static bool lastPunctuationEndsSentence(const list<const Token *> & tokens) {
	list<const Token *>::const_reverse_iterator it = tokens.rbegin();
	while (it != tokens.rend()) {
		if ((*it)->type == TOKEN_PUNCTUATION && (*it)->str[0] != L',') {
			return std::wcschr(L".?!", (*it)->str[0]) != 0;
		}
		++it;
	}
	return false;
}

static bool placeNameInInstitutionName(const Token * word, const list<const Token *> & separators) {
	return word->isGeographicalNameInGenitive && separators.size() == 1 &&
	       (*separators.begin())->str[0] == L' ';
}

/** Return true if quote characters were found */
static bool pushAndPopQuotes(CapitalizationContext & context, const list<const Token *> & tokens) {
	list<const Token *>::const_iterator it = tokens.begin();
	bool hasQuotes = false;
	while (it != tokens.end()) {
		if ((*it)->type == TOKEN_PUNCTUATION) {
			const wchar_t * text = (*it)->str;
			if (isFinnishQuotationMark(text[0])) {
				hasQuotes = true;
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
			} else if (text[0] == L'(' || text[0] == L'[') {
				context.quotes.push(text[0]);
			} else if (text[0] == L')' || text[0] == L']') {
				if (context.quotes.empty()) {
					// XXX: parenthesis errors are not really related to
					// capitalization
					CacheEntry * e = new CacheEntry(0);
					e->error.setErrorCode(GCERR_MISPLACED_CLOSING_PARENTHESIS);
					e->error.setStartPos((*it)->pos);
					e->error.setErrorLen(1);
					context.options->grammarChecker->cache.appendError(e);
				} else if (context.quotes.top() == L'(' ||
				           context.quotes.top() == L'[') {
					// TODO: not checking for matching quote type
					context.quotes.pop();
				}
			} else if (std::wcschr(L".!?", text[0])) {
				context.sentenceEnded = true;
			}
		}
		++it;
	}
	return hasQuotes;
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

static bool isListItemAndClosingParenthesis(const Token * word, const list<const Token *> & separators) {
	if (separators.empty() || (*separators.begin())->str[0] != L')') {
		return false;
	}
	if (StringUtils::isPossibleListItem(word->str)) {
		return true;
	}
	return false;
}

static CapitalizationState inUpper(CapitalizationContext & context) {
	const Token * word = context.nextWord;
	list<const Token *> separators = getTokensUntilNextWord(context);
	if (isListItemAndClosingParenthesis(word, separators)) {
		// A) Some sentence.
		separators.pop_front();
		pushAndPopQuotes(context, separators);
		return DONT_CARE;
	}
	if (!SimpleChar::isUpper(word->str[0]) &&
	    !SimpleChar::isDigit(word->str[0]) &&
	    !word->possibleSentenceStart) {
		CacheEntry * e = new CacheEntry(1);
		e->error.setErrorCode(GCERR_WRITE_FIRST_UPPERCASE);
		e->error.setStartPos(word->pos);
		e->error.setErrorLen(word->tokenlen);
		wchar_t * suggestion = new wchar_t[word->tokenlen];
		suggestion[0] = SimpleChar::upper(word->str[0]);
		wcsncpy(suggestion + 1, word->str + 1, word->tokenlen - 1);
		e->error.getSuggestions()[0] = StringUtils::utf8FromUcs4(suggestion, word->tokenlen);
		delete[] suggestion;
		context.options->grammarChecker->cache.appendError(e);
	}
	pushAndPopQuotes(context, separators);
	if (!context.quotes.empty()) {
		return QUOTED;
	}
	if (containsToken(separators, L"\t") || placeNameInInstitutionName(word, separators)) {
		return DONT_CARE;
	}
	if (context.options->accept_titles_in_gc && StringUtils::isChapterNumber(word->str)) {
		return DONT_CARE;
	}
	if (lastPunctuationEndsSentence(separators)) {
		context.sentenceEnded = true;
		return UPPER;
	}
	return LOWER;
}

static CapitalizationState inLower(CapitalizationContext & context) {
	const Token * word = context.nextWord;
	if (word->isValidWord &&
	    word->firstLetterLcase &&
	    SimpleChar::isUpper(word->str[0]) &&
	    !word->possibleSentenceStart &&
	    word->tokenlen > 1 && // Single letters are OK in upper case
	    word->str[1] != L'-' && // A-rapussa etc.
	    word->str[1] != L':' && // A:n
	    !word->possibleGeographicalName) {
		CacheEntry * e = new CacheEntry(1);
		e->error.setErrorCode(GCERR_WRITE_FIRST_LOWERCASE);
		e->error.setStartPos(word->pos);
		e->error.setErrorLen(word->tokenlen);
		wchar_t * suggestion = new wchar_t[word->tokenlen];
		suggestion[0] = SimpleChar::lower(word->str[0]);
		wcsncpy(suggestion + 1, word->str + 1, word->tokenlen - 1);
		e->error.getSuggestions()[0] = StringUtils::utf8FromUcs4(suggestion, word->tokenlen);
		delete[] suggestion;
		context.options->grammarChecker->cache.appendError(e);
	}
	list<const Token *> separators = getTokensUntilNextWord(context);
	if (isListItemAndClosingParenthesis(word, separators)) {
		// A) Some sentence.
		separators.pop_front();
		pushAndPopQuotes(context, separators);
		return DONT_CARE;
	}
	pushAndPopQuotes(context, separators);
	if (!context.quotes.empty()) {
		return QUOTED;
	}
	if (containsToken(separators, L"\t") || placeNameInInstitutionName(word, separators)) {
		return DONT_CARE;
	}
	if (lastPunctuationEndsSentence(separators)) {
		context.sentenceEnded = true;
		return UPPER;
	}
	return LOWER;
}

static CapitalizationState inDontCare(CapitalizationContext & context) {
	const Token * word = context.nextWord;
	list<const Token *> separators = getTokensUntilNextWord(context);
	if (isListItemAndClosingParenthesis(word, separators)) {
		// A) Some sentence.
		separators.pop_front();
		pushAndPopQuotes(context, separators);
		return DONT_CARE;
	}
	pushAndPopQuotes(context, separators);
	if (!context.quotes.empty()) {
		return QUOTED;
	}
	if (containsToken(separators, L"\t")) {
		return DONT_CARE;
	}
	if (context.options->accept_titles_in_gc && StringUtils::isChapterNumber(word->str)) {
		return DONT_CARE;
	}
	if (lastPunctuationEndsSentence(separators)) {
		context.sentenceEnded = true;
		return UPPER;
	}
	return LOWER;
}

static CapitalizationState inQuoted(CapitalizationContext & context) {
	list<const Token *> separators = getTokensUntilNextWord(context);
	bool hadQuotes = pushAndPopQuotes(context, separators);
	if (!context.quotes.empty()) {
		return QUOTED;
	}
	if (lastPunctuationEndsSentence(separators)) {
		context.sentenceEnded = false;
		return UPPER;
	}
	if (hadQuotes || context.sentenceEnded) {
		context.sentenceEnded = false;
		return DONT_CARE;
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
	context.sentenceEnded = false;
	
	CapitalizationState state = INITIAL;
	while (paragraph->sentenceCount >= context.currentSentence + 1 &&
	       paragraph->sentences[context.currentSentence]->tokenCount >= context.currentToken + 1) {
		state = stateFunctions[state](context);
	}
}

} } }
