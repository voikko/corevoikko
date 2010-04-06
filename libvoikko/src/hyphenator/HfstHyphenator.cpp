/* Libvoikko: Library of Finnish language tools
 * Copyright (C) 2010 Flammie Pirinen <flammie@iki.fi>
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

#include "hyphenator/HfstHyphenator.hpp"
#include "setup/DictionaryException.hpp"
#include "utils/StringUtils.hpp"
#include "voikko_defines.h"
#include <fstream>
#include <hfst2/FlagDiacritics.h>
#include <hfst2/string.h>

using namespace std;
using namespace libvoikko::utils;

namespace libvoikko { namespace hyphenator {

HfstHyphenator::HfstHyphenator(const string & directoryName) throw(setup::DictionaryException) {
	keyTable = HWFST::create_key_table();
	string dictFile = directoryName + "/hyp.hwfst";
	ifstream dictStream(dictFile.c_str());
	string hypFile = directoryName + "/hyphenation.hwfst";
	ifstream hypStream(hypFile.c_str());
	if (hypStream.good() && dictStream.good()) {
		dictionary = HWFST::read_transducer(dictStream, keyTable);
		hyphenation = HWFST::read_transducer(hypStream, keyTable);
		for (HWFST::Key k = 0; k < keyTable->get_unused_key(); ++k) {
			flagTable.define_diacritic(k,
									   HWFST::get_symbol_name(HWFST::get_key_symbol(k, keyTable)));
			if (flagTable.is_diacritic(k)) {
				flags.insert(k); 
			}
		}
	}
	else {
		throw setup::DictionaryException("Failed to open hyp.hwfst or hyphenation.hwfst");
	}
}
    
char * HfstHyphenator::hyphenate(const wchar_t * word,
								 size_t wlen) {
	if (wlen > LIBVOIKKO_MAX_WORD_CHARS) {
		return 0;
	}
	char * wordUtf8 = StringUtils::utf8FromUcs4(word, wlen);
	char * result = hyphenate(wordUtf8);
	delete[] wordUtf8;
	return result;
}

char* HfstHyphenator::hyphenate(const char * word) {
	size_t wlen = strlen(word);
	if (wlen > LIBVOIKKO_MAX_WORD_CHARS) {
		return 0;
	}
	HWFST::KeyVector * wordPath = HWFST::stringUtf8ToKeyVector(word, keyTable);
	HWFST::KeyVectorVector * analysisVector = HWFST::lookup_all(dictionary, wordPath, &flags);
	HWFST::KeyVectorVector * resultVector = new HWFST::KeyVectorVector;
	if (analysisVector != NULL) {
		for (HWFST::KeyVectorVector::iterator analysisIt = analysisVector->begin();
			 analysisIt != analysisVector->end();
			 ++analysisIt) {
			HWFST::KeyVector * analysis = *analysisIt;
			KeyVector* filtlkv = flagTable.filter_diacritics(analysis);
			if (filtlkv) {
				resultVector->push_back(filtlkv);
			}
		}
		delete analysisVector;
		analysisVector = NULL;
	}
	if (resultVector->size() == 0) {
		// no results with dictionary, use algorithm
		analysisVector = HWFST::lookup_all(hyphenation, wordPath);
		if (resultVector == NULL) {
			// give up
			return 0;
		}
		for (HWFST::KeyVectorVector::iterator analysisIt = analysisVector->begin();
			 analysisIt != analysisVector->end();
			 ++analysisIt) {
			HWFST::KeyVector * analysis = *analysisIt;
			resultVector->push_back(analysis);
		}
	}
	if (resultVector->size() == 0) {
		// for the odd situation we are left with empty results
		return 0;
	}
	HWFST::Key sought = HWFST::stringToKey("-", keyTable);
	HWFST::KeyVectorVector r = *resultVector;
	vector<unsigned short> hyphenity(strlen(word), 0);
	// combine multiple suggestions
	for (HWFST::KeyVectorVector::const_iterator rkv = r.begin();
		 rkv != r.end();
		 ++rkv) {
		unsigned int i = 0;
		unsigned int j = 0;
		for (HWFST::KeyVector::const_iterator rk = (*rkv)->begin();
			 rk != (*rkv)->end();
			 ++rk) {
			if (*rk == 0) {
				continue;
			}
			else if (flags.find(*rk) != flags.end()) {
				continue;
			}
			else if ((*wordPath)[i] == sought) {
				j++;
				i++;
			}
			else if (*rk == sought) {
				hyphenity[i]++;
			}
			else {
				j++;
				i++;
			}
		}
	}
	HWFST::KeyVector rk = *r[0];
	char* voikkoHyphenationPattern = new char[strlen(word)+1];
	char* p = voikkoHyphenationPattern;
	for (vector<unsigned short>::iterator h = hyphenity.begin();
		 h != hyphenity.end();
		 ++h){
		if (*h >= r.size()) {
			*p = '-';
		}
		else {
			*p = ' ';
		}
		++p;
	}
	*p = '\0';
	delete wordPath;
	return voikkoHyphenationPattern;
}

void HfstHyphenator::terminate() {
	delete keyTable;
	keyTable = 0;
	HWFST::delete_transducer(hyphenation);
	hyphenation = 0;
	HWFST::delete_transducer(dictionary);
	dictionary = 0;
}

void HfstHyphenator::setUglyHyphenation(bool ugliness) {
	// FIXME: find ugly hyphenation transducer
	(void)ugliness;
}

void HfstHyphenator::setHyphenateUnknown(bool unknown) {
	// FIXME: currently always fall back to unknown hyphenation with algo
	(void)unknown;
}

void HfstHyphenator::setMinHyphenatedWordLength(int wlen) {
	// FIXME: not followed
	(void)wlen;
}

void HfstHyphenator::setIgnoreDot(bool dotness) {
	//FIXME: dotted will be unknown and use such fallback
	(void)dotness;
}

} }
