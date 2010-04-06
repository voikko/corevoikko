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

#include "spellchecker/HfstSuggestion.hpp"
#include "utils/StringUtils.hpp"
#include <fstream>
#include <hfst2/string.h>
#include <hfst2/FlagDiacritics.h>

using namespace std;
using namespace libvoikko::utils;

namespace libvoikko { namespace spellchecker { namespace suggestion {

HfstSuggestion::HfstSuggestion(const string & directoryName) throw(setup::DictionaryException) {
	keyTable = HWFST::create_key_table();
	string suggerFile = directoryName + "/sug.hwfst";
	ifstream suggerStream(suggerFile.c_str());
	if (suggerStream.good()) {
		// FIXME: the speller and suggestion algorithm should really be separate
		// files
		// TODO: allow multiple suggestion algorithms?
		suggestion = HWFST::read_transducer(suggerStream, keyTable);
		speller = HWFST::read_transducer(suggerStream, keyTable);
	}
	else {
		throw setup::DictionaryException("Failed to open sug.hwfst");
	}
	for (HWFST::Key k = 0; k < keyTable->get_unused_key(); ++k) {
		flagTable.define_diacritic(k,
								   HWFST::get_symbol_name(HWFST::get_key_symbol(k, keyTable)));
		if (flagTable.is_diacritic(k)) {
			flags.insert(k);
		}
	}
}

void HfstSuggestion::generate(SuggestionStatus* s) const {
	size_t wlen = s->getWordLength();
	char * wordUtf8 = StringUtils::utf8FromUcs4(s->getWord(), wlen);
	HWFST::KeyVector * wordPath = HWFST::stringUtf8ToKeyVector(wordUtf8, keyTable);
	TransducerHandle currentPaths = HWFST::create_epsilon_transducer();
	for (KeyVector::iterator k = wordPath->begin();
					k != wordPath->end();
					++k) {
		currentPaths = HWFST::minimize(HWFST::concatenate(currentPaths, HWFST::define_transducer(*k)));
	}
	vector<TransducerHandle> rulesVec(1, currentPaths);
	TransducerHandle newPaths = HWFST::intersecting_composition(
										HWFST::invert(HWFST::copy(suggestion)), &rulesVec,
										keyTable);
	if (newPaths != NULL) {
		newPaths = HWFST::minimize(newPaths);
	}
	else {
		newPaths = HWFST::create_empty_transducer();
	}
	vector<TransducerHandle> suggVec(1, newPaths);
	TransducerHandle finalPaths = HWFST::intersecting_composition(
																  HWFST::invert(HWFST::copy(speller)),
																  &suggVec,
																  keyTable);
	vector<TransducerHandle> unordered_paths = HWFST::find_all_paths(
					HWFST::minimize(HWFST::extract_input_language(finalPaths)));
	vector<float> path_weights;
	vector<TransducerHandle>* ordered_paths = new vector<TransducerHandle>;
	for (vector<TransducerHandle>::iterator t = unordered_paths.begin();
					t != unordered_paths.end();
					++t) {
			float this_weight = HWFST::get_weight(*t);
			unsigned int i = 0;
			for (i = 0; i < path_weights.size(); i++) {
					if (path_weights[i] > this_weight) {
							break;
					}
			}
			ordered_paths->insert(ordered_paths->begin() + i, *t);
			path_weights.insert(path_weights.begin() + i, this_weight);
	}
	set<KeyVector> uniqStrings;
	int i = 0;
	for (vector<TransducerHandle>::iterator t = ordered_paths->begin();
					t != ordered_paths->end();
					++t) {
			KeyVector* flagtest = new KeyVector;
			HWFST::State current_state = HWFST::get_initial_state(*t);
			while (!HWFST::is_final_state(current_state, *t)) {
				HWFST::TransitionIterator ti = HWFST::begin_ti(*t, current_state);
				HWFST::Transition tr = HWFST::get_ti_transition(ti);
				current_state = HWFST::get_transition_to(tr);
				HWFST::KeyPair* kp = HWFST::get_transition_keypair(tr);
				flagtest->push_back(HWFST::get_input_key(kp));
			}
			FlagDiacriticTable ftclone(flagTable);
			HWFST::KeyVector* filtered = ftclone.filter_diacritics(flagtest);
			if (filtered == NULL) {
					continue;
			}
			if (uniqStrings.find(*filtered) != uniqStrings.end()) {
					continue;
			}
			else {
					uniqStrings.insert(*filtered);
					string* suggestion = HWFST::keyVectorToString(filtered, keyTable);
					const char* sugUtf8 = suggestion->c_str();
					wchar_t* sugU4 = StringUtils::ucs4FromUtf8(sugUtf8,
																strlen(sugUtf8));

					s->addSuggestion(sugU4, i++);
			}
	}
	delete[] wordUtf8;
}

void HfstSuggestion::terminate() {
	delete keyTable;
	keyTable = 0;
	HWFST::delete_transducer(speller);
	speller = 0;
	HWFST::delete_transducer(suggestion);
	suggestion = 0;
}

} } }
