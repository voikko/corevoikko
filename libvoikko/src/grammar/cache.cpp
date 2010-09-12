/* Libvoikko: Finnish spellchecker and hyphenator library
 * Copyright (C) 2008 - 2010 Harri Pitkänen <hatapitk@iki.fi>
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

#include "utils/utils.hpp"
#include "grammar/cachesetup.hpp"
#include "grammar/cache.hpp"
#include "grammar/checks.hpp"
#include "grammar/analysis.hpp"
#include "grammar/check/CapitalizationCheck.hpp"
#include "grammar/check/NegativeVerbCheck.hpp"
#include "autocorrect/AutoCorrect.hpp"
#include <cstring>
#include <cstdlib>

using namespace libvoikko::grammar;
using namespace libvoikko::autocorrect;

namespace libvoikko {

// This will be initialized to zero meaning "no errors"
static const voikko_grammar_error no_grammar_error = voikko_grammar_error();

const voikko_grammar_error * gc_error_from_cache(voikko_options_t * voikkoOptions, const wchar_t * text,
                             size_t startpos, int skiperrors) {
	if (!voikkoOptions->gc_cache.paragraph) {
		return 0;
	}
	if (wcscmp(voikkoOptions->gc_cache.paragraph, text) != 0) {
		return 0;
	}
	CacheEntry * e = voikkoOptions->gc_cache.firstError;
	int preverrors = 0;
	while (e) {
		if (preverrors >= skiperrors &&
		    e->error.startpos >= startpos) {
			return &e->error;
		}
		preverrors++;
		e = e->nextError;
	}
	return &no_grammar_error;
}

void gc_paragraph_to_cache(voikko_options_t * voikkoOptions, const wchar_t * text, size_t textlen) {
	gc_clear_cache(voikkoOptions);
	voikkoOptions->gc_cache.paragraph = new wchar_t[textlen + 1];
	if (!voikkoOptions->gc_cache.paragraph) {
		return;
	}
	memcpy(voikkoOptions->gc_cache.paragraph, text, textlen * sizeof(wchar_t));
	voikkoOptions->gc_cache.paragraph[textlen] = L'\0';
	Paragraph * para = gc_analyze_paragraph(voikkoOptions, text, textlen);
	if (!para) {
		return;
	}
	
	// If paragraph is a single sentence without any whitespace, do not try to
	// do grammar checking on it. This could be an URL or something equally
	// strange.
	if (para->sentenceCount == 1) {
		Sentence * sentence = para->sentences[0];
		bool hasWhitespace = false;
		for (size_t i = 0; i < sentence->tokenCount; i++) {
			if (sentence->tokens[i].type == TOKEN_WHITESPACE) {
				hasWhitespace = true;
				break;
			}
		}
		if (!hasWhitespace) {
			// If this is a single word sentence, we should check it, otherwise
			// it makes no sense to try.
			if (sentence->tokenCount > 2 || sentence->tokenCount == 0 ||
			    sentence->tokens[0].type != TOKEN_WORD) {
				delete para;
				return;
			}
		}
	}
	
	check::CapitalizationCheck capitalizationCheck;
	check::NegativeVerbCheck negativeVerbCheck;
	for (size_t i = 0; i < para->sentenceCount; i++) {
		AutoCorrect::autoCorrect(voikkoOptions, para->sentences[i]);
		gc_local_punctuation(voikkoOptions, para->sentences[i]);
		gc_punctuation_of_quotations(voikkoOptions, para->sentences[i]);
		gc_repeating_words(voikkoOptions, para->sentences[i]);
		negativeVerbCheck.check(voikkoOptions, para->sentences[i]);
	}
	capitalizationCheck.check(voikkoOptions, para);
	gc_end_punctuation(voikkoOptions, para);
	delete para;
}

void gc_cache_append_error(voikko_options_t * voikkoOptions, CacheEntry * new_entry) {
	CacheEntry * entry = voikkoOptions->gc_cache.firstError;
	if (!entry) {
		voikkoOptions->gc_cache.firstError = new_entry;
		return;
	}
	if (entry->error.startpos > new_entry->error.startpos) {
		new_entry->nextError = voikkoOptions->gc_cache.firstError;
		voikkoOptions->gc_cache.firstError = new_entry;
		return;
	}
	while (1) {
		if (!entry->nextError) {
			entry->nextError = new_entry;
			return;
		}
		if (entry->error.startpos <= new_entry->error.startpos &&
		    entry->nextError->error.startpos > new_entry->error.startpos) {
			new_entry->nextError = entry->nextError;
			entry->nextError = new_entry;
			return;
		}
		entry = entry->nextError;
	}
}

}
