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

static const Token * getTokenAndAdvance(CapitalizationContext & context) {
	if (context.paragraph->sentenceCount == context.currentSentence) {
		return 0;
	}
	const Sentence * sentence = context.paragraph->sentences[context.currentSentence];
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

static CapitalizationState inInitial(CapitalizationContext & context) {
	//const Token * currentWord = context.nextWord;
	list<const Token *> separators = getTokensUntilNextWord(context);
	return UPPER; // FIXME
}

static CapitalizationState inUpper(CapitalizationContext & context) {
	//const Token * currentWord = context.nextWord;
	list<const Token *> separators = getTokensUntilNextWord(context);
	return LOWER; // FIXME
}

static CapitalizationState inLower(CapitalizationContext & context) {
	//const Token * currentWord = context.nextWord;
	list<const Token *> separators = getTokensUntilNextWord(context);
	return DONT_CARE; // FIXME
}

static CapitalizationState inDontCare(CapitalizationContext & context) {
	//const Token * currentWord = context.nextWord;
	list<const Token *> separators = getTokensUntilNextWord(context);
	return QUOTED; // FIXME
}

static CapitalizationState inQuoted(CapitalizationContext & context) {
	//const Token * currentWord = context.nextWord;
	list<const Token *> separators = getTokensUntilNextWord(context);
	return INITIAL; // FIXME
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
	while (false /*paragraph->sentenceCount >= context.currentSentence + 1 &&
	       paragraph->sentences[context.currentSentence]->tokenCount >= context.currentToken + 1*/) {
		state = stateFunctions[state](context);
	}
	for (size_t i = 0; i < paragraph->sentenceCount; i++) {
		check(options, paragraph->sentences[i], i == 0);
	}
}

void CapitalizationCheck::check(voikko_options_t * options, const Sentence * sentence, bool isFirstInParagraph) {
	// Check if the sentence is written fully in upper case letters.
	// If it is, no character case errors should be reported.
	bool onlyUpper = true;
	for (size_t i = 0; i < sentence->tokenCount; i++) {
		Token t = sentence->tokens[i];
		if (t.type != TOKEN_WORD) continue;
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
	if (onlyUpper) {
		return;
	}
	
	// If the sentence contains a tab character as a word separator, it is
	// likely that it is actually not a sentence. Maybe it would be best to
	// disable all checks in that case, but character case checks are more
	// likely to be wrong than others.
	// Also the presence of a foreign quotation mark makes it hard to tell
	// what is the correct capitalization.
	for (size_t i = 0; i < sentence->tokenCount; i++) {
		Token t = sentence->tokens[i];
		if (t.str[0] == L'\u201C') {
			return;
		}
		if (t.type != TOKEN_WHITESPACE) {
			continue;
		}
		for (size_t j = 0; j < t.tokenlen; j++) {
			if (t.str[j] == L'\t') {
				return;
			}
		}
	}
	
	// Sentences starting with "-" may be list items.
	bool sentenceStartsWithHyphen =
		sentence->tokenCount > 0 && sentence->tokens[0].tokenlen == 1 &&
		sentence->tokens[0].str[0] == '-';
	
	bool firstWordSeen = false;
	bool isInQuotation = false;
	for (size_t i = 0; i < sentence->tokenCount; i++) {
		Token t = sentence->tokens[i];
		if (t.type != TOKEN_WORD) {
			if (t.tokenlen == 1 && isFinnishQuotationMark(t.str[0])) {
				// TODO: quotations within quotations do not work this ways
				isInQuotation = !isInQuotation;
			}
			continue;
		}
		if (!firstWordSeen) {
			firstWordSeen = true;
			bool needCheckingOfFirstUppercase = !sentenceStartsWithHyphen &&
				(!isFirstInParagraph || !options->accept_bulleted_lists_in_gc);
			if (needCheckingOfFirstUppercase && !SimpleChar::isUpper(t.str[0]) && !SimpleChar::isDigit(t.str[0])) {
				CacheEntry * e = new CacheEntry(1);
				e->error.error_code = GCERR_WRITE_FIRST_UPPERCASE;
				e->error.startpos = t.pos;
				e->error.errorlen = t.tokenlen;
				wchar_t * suggestion = new wchar_t[t.tokenlen];
				suggestion[0] = SimpleChar::upper(t.str[0]);
				wcsncpy(suggestion + 1, t.str + 1, t.tokenlen - 1);
				e->error.suggestions[0] = StringUtils::utf8FromUcs4(suggestion, t.tokenlen);
				delete[] suggestion;
				gc_cache_append_error(options, e);
			}
			continue;
		}
		if (isInQuotation) continue;
		if (!t.isValidWord) continue;
		if (!t.firstLetterLcase) continue;
		if (t.possibleSentenceStart) continue;
		if (t.tokenlen == 1) {
			// Single letters are OK in upper case
			continue;
		}
		if (t.str[1] == L'-') {
			// A-rapussa etc.
			continue;
		}
		if (!SimpleChar::isUpper(t.str[0])) {
			continue;
		}
		CacheEntry * e = new CacheEntry(1);
		e->error.error_code = GCERR_WRITE_FIRST_LOWERCASE;
		e->error.startpos = t.pos;
		e->error.errorlen = t.tokenlen;
		wchar_t * suggestion = new wchar_t[t.tokenlen];
		suggestion[0] = SimpleChar::lower(t.str[0]);
		wcsncpy(suggestion + 1, t.str + 1, t.tokenlen - 1);
		e->error.suggestions[0] = StringUtils::utf8FromUcs4(suggestion, t.tokenlen);
		delete[] suggestion;
		gc_cache_append_error(options, e);
	}
}

} } }
