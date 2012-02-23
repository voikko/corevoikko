/* Voikkovfstc: Compiler for Varissuo Finite State Transducers
 * Copyright (C) 2012 Harri Pitkänen <hatapitk@iki.fi>
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

#include "../fst/Transition.hpp"
#include <cassert>
#include <string>
#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>
#include <map>

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

int main() {
	assert(sizeof(transinfo_t) == 4);
	assert(sizeof(Transition) == 8);
	assert(sizeof(OverflowCell) == 8);
	
	vector<Symbol> symVector;
	map<string, Symbol> symMap;
	vector<AttState> attStateVector;
	
	string line;
	long transitionCount = 0;
	long finalStateCount = 0;
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
	
	cerr << "Symbols: " << symVector.size() << endl;
	cerr << "Transitions: " << transitionCount << endl;
	cerr << "Final states: " << finalStateCount << endl;
	
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
