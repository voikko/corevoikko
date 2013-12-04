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
 * Portions created by the Initial Developer are Copyright (C) 2007 - 2013
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

#include "tokenizer/Tokenizer.hpp"
#include "utils/utils.hpp"
#include "character/charset.hpp"

namespace libvoikko { namespace tokenizer {

static size_t findUrlOrEmail(const wchar_t * text, size_t textlen) {
	// 12 is a rough lower bound for a length of a reasonable real world http URL.
	if (textlen < 12 || (wcsncmp(L"http://", text, 7) != 0 && wcsncmp(L"https://", text, 8) != 0)) {
		// try finding an email address instead
		if (textlen < 6) {
			return 0;
		}
		bool foundAt = false;
		bool foundDot = false;
		for (size_t i = 0; i < textlen; ++i) {
			switch (get_char_type(text[i])) {
				case CHAR_WHITESPACE:
					if (foundAt && foundDot) {
						return i;
					}
					return 0;
				case CHAR_UNKNOWN:
					if (text[i] == L'@') {
						if (foundAt) {
							return 0;
						}
						foundAt = true;
						break;
					}
					if (wcschr(L"#$%*+=^_`|~", text[i]) == 0) {
						if (foundAt && foundDot) {
							return i;
						}
						return 0;
					}
					break;
				case CHAR_DIGIT:
				case CHAR_LETTER:
					break;
				case CHAR_PUNCTUATION:
					if (text[i] == L'.' && foundAt) {
						if (i + 1 == textlen || get_char_type(text[i+1]) == CHAR_WHITESPACE) {
							if (foundDot) {
								return i;
							}
							return 0;
						}
						foundDot = true;
						break;
					}
					if (wcschr(L"!&'-/?{}.", text[i]) == 0) {
						if (foundAt && foundDot) {
							return i;
						}
						return 0;
					}
					break;
			}
		}
		if (foundAt && foundDot) {
			return textlen;
		}
		return 0;
	}
	for (size_t i = (text[4] == L's' ? 8 : 7); i < textlen; ++i) {
		switch (get_char_type(text[i])) {
			case CHAR_WHITESPACE:
				return i;
			case CHAR_UNKNOWN:
				if (wcschr(L"=#%", text[i]) == 0) {
					return i;
				}
				continue;
			case CHAR_DIGIT:
			case CHAR_LETTER:
				continue;
			case CHAR_PUNCTUATION:
				if (text[i] == L'.' && (i + 1 == textlen || get_char_type(text[i+1]) == CHAR_WHITESPACE)) {
					return i;
				}
		}
	}
	return textlen;
}
	
static size_t word_length(const wchar_t * text, size_t textlen, voikko_options_t * options) {
	size_t wlen = 0;
	bool processing_number = false;
	bool seenLetters = false;
	
	const size_t urlLength = findUrlOrEmail(text, textlen);
	if (urlLength != 0) {
		return urlLength;
	}
	
	size_t adot;
	if (options->ignore_dot) {
		adot = 1;
	}
	else adot = 0;
	
	while (wlen < textlen) {
		switch (get_char_type(text[wlen])) {
			case CHAR_LETTER:
				processing_number = false;
				seenLetters = true;
				wlen++;
				break;
			case CHAR_DIGIT:
				processing_number = true;
				wlen++;
				break;
			case CHAR_WHITESPACE:
			case CHAR_UNKNOWN:
				return wlen;
			case CHAR_PUNCTUATION:
				switch (text[wlen]) {
					case L'\'':
					case L'\u2019': /* RIGHT SINGLE QUOTATION MARK */
					case L':':
						if (wlen + 1 == textlen) return wlen;
						if (get_char_type(text[wlen+1]) ==
						    CHAR_LETTER) break;
						return wlen;
					case L'-':
					case L'\u00AD': /* SOFT HYPHEN */
					case L'\u2010': /* HYPHEN */
					case L'\u2011': /* NON-BREAKING HYPHEN */
						if (wlen + 1 == textlen) {
							return wlen + 1;
						}
						if (isFinnishQuotationMark(text[wlen+1])) {
							return wlen + 1;
						}
						switch (get_char_type(text[wlen+1])) {
							case CHAR_LETTER:
							case CHAR_DIGIT:
								break;
							case CHAR_WHITESPACE:
							case CHAR_UNKNOWN:
								return wlen + 1;
							case CHAR_PUNCTUATION:
								if (text[wlen+1] == L',') {
									return wlen + 1;
								}
								return wlen;
						}
						break;
					case L'.':
						if (wlen + 1 == textlen) return wlen + adot;
						switch (get_char_type(text[wlen+1])) {
							case CHAR_LETTER:
								break;
							case CHAR_DIGIT:
								if (seenLetters) {
									return wlen + adot;
								}
								break;
							case CHAR_WHITESPACE:
							case CHAR_UNKNOWN:
							case CHAR_PUNCTUATION:
								return wlen + adot;
						}
						break;
					case L',':
						if (!processing_number) return wlen;
						if (wlen + 1 == textlen) return wlen;
						if (get_char_type(text[wlen+1]) ==
						    CHAR_DIGIT) break;
						return wlen;
						
					default:
						return wlen;
				}
				wlen++;
		}
	}
	return textlen;
}

voikko_token_type Tokenizer::nextToken(voikko_options_t * options, const wchar_t * text, size_t textlen, size_t * tokenlen) {
	if (textlen == 0) {
		*tokenlen = 0;
		return TOKEN_NONE;
	}
	switch (get_char_type(text[0])) {
		case CHAR_LETTER:
		case CHAR_DIGIT:
			*tokenlen = word_length(text, textlen, options);
			return TOKEN_WORD;
		case CHAR_WHITESPACE:
			for (size_t i = 1; i < textlen; i++) {
				if (get_char_type(text[i]) != CHAR_WHITESPACE) {
					*tokenlen = i;
					return TOKEN_WHITESPACE;
				}
			}
			*tokenlen = textlen;
			return TOKEN_WHITESPACE;
		case CHAR_PUNCTUATION:
			if (wcschr(L"-\u2010\u2011", text[0])) {
				if (textlen == 1) {
					*tokenlen = 1;
					return TOKEN_PUNCTUATION;
				}
				size_t wlen = word_length(text + 1, textlen - 1, options);
				if (wlen == 0) {
					*tokenlen = 1;
					return TOKEN_PUNCTUATION;
				}
				*tokenlen = wlen + 1;
				return TOKEN_WORD;
			}
			else if (textlen >= 3 && text[0] == L'.' && text[1] == L'.'
			         && text[2] == L'.') {
				*tokenlen = 3;
				return TOKEN_PUNCTUATION;
			}
			*tokenlen = 1;
			return TOKEN_PUNCTUATION;
		case CHAR_UNKNOWN:
			*tokenlen = 1;
			return TOKEN_UNKNOWN;
	}
	return TOKEN_NONE; // unreachable
}

} }
