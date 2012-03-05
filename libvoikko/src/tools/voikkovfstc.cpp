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
 * Portions created by the Initial Developer are Copyright (C) 2012
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

#include "../fst/Transition.hpp"
#include <cassert>
#include <string>
#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>
#include <map>
#include <algorithm>

using namespace libvoikko::fst;
using namespace std;

struct Symbol {
	uint16_t code;
	std::string text;
};

struct AttState {
	vector<Transition> transitions;
	vector<uint32_t> targetStateOrds;
};

static void ensureSymbolInMap(string & symStr, vector<Symbol> & symVector, map<string, Symbol> & symMap) {
	if (symMap.find(symStr) == symMap.end()) {
		Symbol s;
		s.code = symVector.size();
		s.text = symStr;
		symVector.push_back(s);
		symMap[symStr] = s;
	}
}

static string convertSymbolNames(string input) {
	if (input == "@0@") {
		return string("");
	}
	return input;
}

struct compareSymbolsForLookupOrder {
	bool operator()(Symbol const & a, Symbol const & b) const {
		if (a.text == b.text) {
			return false;
		}
		if (a.text == "") {
			return true;
		}
		if (b.text == "") {
			return false;
		}
		if (a.text[0] == '@' && b.text[0] != '@') {
			return true;
		}
		if (b.text[0] == '@' && a.text[0] != '@') {
			return false;
		}
		if (a.text[0] == '[' && b.text[0] != '[') {
			return false;
		}
		if (b.text[0] == '[' && a.text[0] != '[') {
			return true;
		}
		if (a.text[0] == '[' || a.text[0] == '@') {
			return a.text.substr(1) < b.text.substr(1);
		}
		else {
			return a.text < b.text;
		}
	}
};

int main() {
	assert(sizeof(transinfo_t) == 4);
	assert(sizeof(Transition) == 8);
	assert(sizeof(OverflowCell) == 8);
	
	vector<Symbol> symVector;
	vector<AttState> attStateVector;
	
	string line;
	long transitionCount = 0;
	long finalStateCount = 0;
	{
		map<string, Symbol> symMap;
		ensureSymbolInMap(line, symVector, symMap); // epsilon = 0
		while (getline(std::cin, line)) {
			istringstream ss(line);
			uint32_t sourceStateOrd = 0;
			uint32_t targetStateOrd = 0;
			string symInStr;
			string symOutStr;
			ss >> sourceStateOrd;
			ss >> targetStateOrd;
			ss >> symInStr;
			ss >> symOutStr;
			if (attStateVector.size() == sourceStateOrd) {
				attStateVector.push_back(AttState());
			}
			if (line.find("\t") == string::npos) {
				finalStateCount++;
				Transition t;
				t.symIn = 0xFFFF;
				t.symOut = 0;
				attStateVector[sourceStateOrd].transitions.push_back(t);
				attStateVector[sourceStateOrd].targetStateOrds.push_back(0);
			}
			else {
				symInStr = convertSymbolNames(symInStr);
				symOutStr = convertSymbolNames(symOutStr);
				ensureSymbolInMap(symInStr, symVector, symMap);
				ensureSymbolInMap(symOutStr, symVector, symMap);
				Transition t;
				t.symIn = symMap[symInStr].code;
				t.symOut = symMap[symOutStr].code;
				attStateVector[sourceStateOrd].transitions.push_back(t);
				attStateVector[sourceStateOrd].targetStateOrds.push_back(targetStateOrd);
				transitionCount++;
			}
		}
	}
	
	cerr << "Symbols: " << symVector.size() << endl;
	cerr << "Transitions: " << transitionCount << endl;
	cerr << "Final states: " << finalStateCount << endl;
	
	// reorder symbols for faster lookup
	{
		sort(symVector.begin(), symVector.end(), compareSymbolsForLookupOrder());
		vector< pair<uint16_t, uint16_t> > oldToNewSym;
		uint16_t i = 0;
		for (vector<Symbol>::const_iterator it = symVector.begin(); it < symVector.end(); ++it) {
			oldToNewSym.push_back(pair<uint16_t, uint16_t>(it->code, i++));
		}
		sort(oldToNewSym.begin(), oldToNewSym.end());
		for (vector<AttState>::iterator sIt = attStateVector.begin(); sIt < attStateVector.end(); ++sIt) {
			for (vector<Transition>::iterator tIt = sIt->transitions.begin(); tIt < sIt->transitions.end(); ++tIt) {
				if (tIt->symIn != 0xFFFF) {
					tIt->symIn = oldToNewSym[tIt->symIn].second;
					tIt->symOut = oldToNewSym[tIt->symOut].second;
				}
			}
		}
		for (vector<Symbol>::iterator it = symVector.begin(); it < symVector.end(); ++it) {
			it->code = oldToNewSym[it->code].second;
		}
	}
	
	// Determine state offsets in binary transition table. Offsets are calculated
	// in 8 byte cells.
	vector<uint32_t> stateOrdinalToOffset;
	uint32_t currentOffset = 0;
	long overflowCells = 0;
	for (vector<AttState>::iterator it = attStateVector.begin(); it < attStateVector.end(); it++) {
		stateOrdinalToOffset.push_back(currentOffset);
		uint32_t tCount = it->transitions.size();
		if (tCount == 0) {
			cerr << "ERROR: non-final state without outgoing transitions: " << stateOrdinalToOffset.size() << endl;
			return 1;
		}
		else if (tCount <= 255) {
			currentOffset += tCount;
		}
		else {
			currentOffset += (tCount + 1);
			overflowCells++;
		}
	}
	// TODO check that currentOffset is not too large
	cerr << "Overflow cells: " << overflowCells << endl;
	
	ofstream transducerFile("transducer.vfst", ios::out | ios::binary);
	
	// Write header
	// Following two 4 byte integers can be used to determine the file type and byte order
	const uint32_t COOKIE1 = 0x00013A6E;
	const uint32_t COOKIE2 = 0x000351FA;
	transducerFile.write((char *)&COOKIE1, sizeof(uint32_t));
	transducerFile.write((char *)&COOKIE2, sizeof(uint32_t));
	// 8 bytes of reserved space for future format extensions and variants. Must be zero for now.
	transducerFile.seekp(8, ios_base::cur);
	
	// Write symbols
	uint16_t symbolCount = symVector.size();
	transducerFile.write((char *)&symbolCount, sizeof(uint16_t));
	for (vector<Symbol>::iterator it = symVector.begin(); it < symVector.end(); it++) {
		string symName = it->text;
		transducerFile.write(symName.c_str(), symName.length());
		transducerFile.put(0);
	}
	
	// Write padding so that transition table starts at 8 byte boundary
	{
		size_t partial = transducerFile.tellp() % sizeof(Transition);
		if (partial > 0) {
			transducerFile.seekp(sizeof(Transition) - partial, ios_base::cur);
		}
	}
	
	// Write state transitions
	for (vector<AttState>::iterator it = attStateVector.begin(); it < attStateVector.end(); it++) {
		uint32_t tCount = it->transitions.size();
		{
			Transition & t = it->transitions[0];
			t.transInfo.targetState = stateOrdinalToOffset[it->targetStateOrds[0]];
			t.transInfo.moreTransitions = (tCount > 255 ? 255 : tCount - 1);
			transducerFile.write((char *)&t, sizeof(Transition));
		}
		if (tCount > 255) {
			OverflowCell oc;
			oc.moreTransitions = tCount - 1;
			oc.padding = 0;
			transducerFile.write((char *)&oc, sizeof(OverflowCell));
		}
		for (uint32_t ti = 1; ti < tCount; ti++) {
			Transition & t = it->transitions[ti];
			t.transInfo.targetState = stateOrdinalToOffset[it->targetStateOrds[ti]];
			t.transInfo.moreTransitions = 0;
			transducerFile.write((char *)&t, sizeof(Transition));
		}
	}
	
	transducerFile.close();
}
