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
 * Portions created by the Initial Developer are Copyright (C) 2008 - 2010
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

#include "grammar/checks.hpp"
#include "grammar/error.hpp"
#include "grammar/cachesetup.hpp"
#include "grammar/cache.hpp"
#include "character/charset.hpp"
#include "character/SimpleChar.hpp"
#include "setup/setup.hpp"
#include "utils/utils.hpp"
#include "utils/StringUtils.hpp"
#include <cstdlib>
#include <cstring>

using namespace libvoikko::grammar;
using namespace libvoikko::character;
using namespace libvoikko::utils;

namespace libvoikko {

void gc_local_punctuation(voikko_options_t * options, const Sentence * sentence) {
	CacheEntry * e;
	for (size_t i = 0; i < sentence->tokenCount; i++) {
		Token t = sentence->tokens[i];
		switch (t.type) {
		case TOKEN_WHITESPACE:
			if (t.tokenlen > 1) {
				e = new CacheEntry(1);
				e->error.error_code = GCERR_EXTRA_WHITESPACE;
				e->error.startpos = sentence->tokens[i].pos;
				e->error.errorlen = sentence->tokens[i].tokenlen;
				e->error.suggestions[0] = new char[2];
				strcpy(e->error.suggestions[0], " ");
				gc_cache_append_error(options, e);
			}
			else if (i + 1 < sentence->tokenCount) {
				Token t2 = sentence->tokens[i+1];
				if (t2.type != TOKEN_PUNCTUATION ||
				    t2.str[0] != L',') continue;
				e = new CacheEntry(1);
				e->error.error_code = GCERR_SPACE_BEFORE_PUNCTUATION;
				e->error.startpos = sentence->tokens[i].pos;
				e->error.errorlen = 2;
				e->error.suggestions[0] = new char[2];
				e->error.suggestions[0][0] = ',';
				e->error.suggestions[0][1] = L'\0';
				gc_cache_append_error(options, e);
			}
			break;
		case TOKEN_PUNCTUATION:
			if (t.str[0] == L'[') {
				// [2] etc. can be skipped
				if (i + 2 < sentence->tokenCount && sentence->tokens[i + 2].str[0] == L']') {
					i += 2;
					continue;
				}
			}
			if (i == 0) {
				if (wcschr(L"()'-\u201C\u2013\u2014", t.str[0]) || isFinnishQuotationMark(t.str[0])) {
					continue;
				}
				e = new CacheEntry(0);
				e->error.error_code = GCERR_INVALID_SENTENCE_STARTER;
				if (sentence->tokens[i].pos == 0) {
					e->error.startpos = 0;
					e->error.errorlen = 1;
				}
				else {
					e->error.startpos = sentence->tokens[i].pos - 1;
					e->error.errorlen = 2;
				}
				gc_cache_append_error(options, e);
				continue;
			}
			if (t.str[0] == L',' && i + 1 < sentence->tokenCount) {
				Token t2 = sentence->tokens[i+1];
				if (t2.type != TOKEN_PUNCTUATION ||
				    t2.str[0] != L',') continue;
				e = new CacheEntry(1);
				e->error.error_code = GCERR_EXTRA_COMMA;
				e->error.startpos = sentence->tokens[i].pos;
				e->error.errorlen = 2;
				e->error.suggestions[0] = new char[2];
				strcpy(e->error.suggestions[0], ",");
				gc_cache_append_error(options, e);
			}
			break;
		case TOKEN_NONE:
		case TOKEN_WORD:
		case TOKEN_UNKNOWN:
			break;
		}
	}
}

void gc_punctuation_of_quotations(voikko_options_t * options, const Sentence * sentence) {
	for (size_t i = 0; i + 2 < sentence->tokenCount; i++) {
		if (sentence->tokens[i].type != TOKEN_PUNCTUATION) {
			continue;
		}
		if (sentence->tokens[i].str[0] == L'\u201C') {
			// There was a foreign quotation mark -> report an error
			// and stop processing any further.
			CacheEntry * e = new CacheEntry(1);
			e->error.error_code = GCERR_FOREIGN_QUOTATION_MARK;
			e->error.startpos = sentence->tokens[i].pos;
			e->error.errorlen = 1;
			e->error.suggestions[0] = StringUtils::utf8FromUcs4(L"\u201D", 1);
			gc_cache_append_error(options, e);
			return;
		}
		if (sentence->tokens[i + 1].type != TOKEN_PUNCTUATION) {
			continue;
		}
		if (!isFinnishQuotationMark(sentence->tokens[i + 1].str[0])) {
			continue;
		}
		if (sentence->tokens[i + 2].type != TOKEN_PUNCTUATION) {
			continue;
		}
		if (sentence->tokens[i + 2].str[0] != L',') {
			continue;
		}
		
		wchar_t quoteChar = sentence->tokens[i + 1].str[0];
		CacheEntry * e;
		switch (sentence->tokens[i].str[0]) {
		case L'.':
			e = new CacheEntry(1);
			e->error.error_code = GCERR_INVALID_PUNCTUATION_AT_END_OF_QUOTATION;
			e->error.startpos = sentence->tokens[i].pos;
			e->error.errorlen = 3;
			{
				wchar_t * suggDot = new wchar_t[e->error.errorlen];
				suggDot[0] = quoteChar;
				suggDot[1] = L',';
				suggDot[2] = L'\0';
				e->error.suggestions[0] = StringUtils::utf8FromUcs4(suggDot, e->error.errorlen);
				delete[] suggDot;
			}
			gc_cache_append_error(options, e);
			break;
		case L'!':
		case L'?':
			e = new CacheEntry(1);
			e->error.error_code = GCERR_INVALID_PUNCTUATION_AT_END_OF_QUOTATION;
			e->error.startpos = sentence->tokens[i].pos;
			e->error.errorlen = 3;
			{
				wchar_t * suggOther = new wchar_t[e->error.errorlen];
				suggOther[0] = (sentence->tokens[i].str[0] == L'!' ? L'!' : L'?');
				suggOther[1] = quoteChar;
				suggOther[2] = L'\0';
				e->error.suggestions[0] = StringUtils::utf8FromUcs4(suggOther, e->error.errorlen);
				delete[] suggOther;
			}
			gc_cache_append_error(options, e);
			break;
		}
	}
}

void gc_repeating_words(voikko_options_t * options, const Sentence * sentence) {
	for (size_t i = 0; i + 2 < sentence->tokenCount; i++) {
		if (sentence->tokens[i].type != TOKEN_WORD) continue;
		if (sentence->tokens[i + 1].type != TOKEN_WHITESPACE) {
			i++;
			continue;
		}
		if (sentence->tokens[i + 2].type != TOKEN_WORD) {
			i += 2;
			continue;
		}
		if (!SimpleChar::equalsIgnoreCase(sentence->tokens[i].str, sentence->tokens[i + 2].str)) {
			i++;
			continue;
		}
		if (wcscmp(sentence->tokens[i].str, L"ollut") == 0) {
			// "ollut" is a valid word to be repeated, maybe there are others too
			i++;
			continue;
		}
		CacheEntry * e = new CacheEntry(1);
		e->error.error_code = GCERR_REPEATING_WORD;
		e->error.startpos = sentence->tokens[i].pos;
		e->error.errorlen = sentence->tokens[i].tokenlen +
		                    sentence->tokens[i + 1].tokenlen +
		                    sentence->tokens[i + 2].tokenlen;
		e->error.suggestions[0] = StringUtils::utf8FromUcs4(sentence->tokens[i].str,
		                          sentence->tokens[i].tokenlen);
		gc_cache_append_error(options, e);
	}
}

void gc_end_punctuation(voikko_options_t * options, const Paragraph * paragraph) {
	if (options->accept_titles_in_gc && paragraph->sentenceCount == 1) return;
	if (options->accept_unfinished_paragraphs_in_gc) return;
	if (options->accept_bulleted_lists_in_gc) return;
	
	Sentence * sentence = paragraph->sentences[paragraph->sentenceCount - 1];
	Token * token = sentence->tokens + (sentence->tokenCount - 1);
	if (token->type == TOKEN_PUNCTUATION) return;
	CacheEntry * e = new CacheEntry(0);
	e->error.error_code = GCERR_TERMINATING_PUNCTUATION_MISSING;
	e->error.startpos = token->pos;
	e->error.errorlen = token->tokenlen;
	gc_cache_append_error(options, e);
}

}
