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

static uint16_t swap(uint16_t x) {
	return (x>>8) | (x<<8);
}

static uint16_t swapIf(bool doSwap, uint16_t x) {
	return doSwap ? swap(x) : x;
}

static void write16(ofstream & out, bool doSwap, uint16_t x) {
	uint16_t y = swapIf(doSwap, x);
	out.write((char *) &y, sizeof(uint16_t));
}

static uint32_t swap(uint32_t x) {
	return  (x>>24) | 
		((x<<8) & 0x00FF0000) |
		((x>>8) & 0x0000FF00) |
		(x<<24);
}

static uint32_t swapIf(bool doSwap, uint32_t x) {
	return doSwap ? swap(x) : x;
}

static void writeTrans(ofstream & out, bool doSwap, Transition & t) {
	if (doSwap) {
		Transition tSwapped;
		tSwapped.symIn = swap(t.symIn);
		tSwapped.symOut = swap(t.symOut);
		uint32_t ts = t.transInfo.targetState;
		tSwapped.transInfo.targetState = ((ts<<16) & 0x00FF0000) | (ts & 0x0000FF00) | ((ts>>16) & 0x000000FF);
		tSwapped.transInfo.moreTransitions = t.transInfo.moreTransitions;
		out.write((char *) &tSwapped, sizeof(Transition));
	}
	else {
		out.write((char *) &t, sizeof(Transition));
	}
}

static void writeOverflow(ofstream & out, bool doSwap, OverflowCell & oc) {
	if (doSwap) {
		OverflowCell ocSwapped;
		ocSwapped.moreTransitions = swap(oc.moreTransitions);
		ocSwapped.padding = 0;
		out.write((char *) &ocSwapped, sizeof(OverflowCell));
	}
	else {
		out.write((char *) &oc, sizeof(OverflowCell));
	}
}

static bool isLittleEndian() {
	uint16_t i = 1;
	return *((char *) &i) != 0;
}

static void ensureSymbolInMap(string & symStr, vector<Symbol> & symVector, map<string, Symbol> & symMap) {
	if (symMap.find(symStr) == symMap.end()) {
		if (symStr == "[") {
			cerr << "ERROR: '[' is not a valid VFST symbol" << endl;
			exit(1);
		}
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

static void setTarget(Transition & t, vector<uint32_t> & stateOrdinalToOffset, uint32_t targetStateOrdinal) {
	if (targetStateOrdinal >= stateOrdinalToOffset.size()) {
		cerr << "ERROR: target state not final or source for another transition: " << targetStateOrdinal << endl;
		exit(1);
	}
	t.transInfo.targetState = stateOrdinalToOffset[targetStateOrdinal];
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

int main(int argc, char ** argv) {
	assert(sizeof(transinfo_t) == 4);
	assert(sizeof(Transition) == 8);
	assert(sizeof(OverflowCell) == 8);

	string outputFile;
	string format = "le";
	for (int i = 1; i < argc; i++) {
		string args(argv[i]);
		if (args == "-o" && i + 1 < argc) {
			outputFile = string(argv[++i]);
		}
		else if (args == "-f" && i + 1 < argc) {
			format = string(argv[++i]);
		}
	}

	if (outputFile.empty()) {
		cerr << "ERROR: output file needs to be specified" << endl;
		exit(1);
	}

	bool byteSwap;
	if (format == "le") {
		byteSwap = !isLittleEndian();
	}
	else if (format == "be") {
		byteSwap = isLittleEndian();
	}
	else if (format == "native") {
		byteSwap = false;
	}
	else {
		cerr << "ERROR: output format must be one of 'le', 'be' or 'native'" << endl;
		exit(1);
	}
	
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
	
	ofstream transducerFile(outputFile.c_str(), ios::out | ios::binary);
	
	// Write header
	// Following two 4 byte integers can be used to determine the file type and byte order
	const uint32_t COOKIE1 = swapIf(byteSwap, (uint32_t)0x00013A6E);
	const uint32_t COOKIE2 = swapIf(byteSwap, (uint32_t)0x000351FA);
	transducerFile.write((char *)&COOKIE1, sizeof(uint32_t));
	transducerFile.write((char *)&COOKIE2, sizeof(uint32_t));
	// 8 bytes of reserved space for future format extensions and variants. Must be zero for now.
	transducerFile.seekp(8, ios_base::cur);
	
	// Write symbols
	uint16_t symbolCount = symVector.size();
	write16(transducerFile, byteSwap, symbolCount);
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
			setTarget(t, stateOrdinalToOffset, it->targetStateOrds[0]);
			t.transInfo.moreTransitions = (tCount > 255 ? 255 : tCount - 1);
			writeTrans(transducerFile, byteSwap, t);
		}
		if (tCount > 255) {
			OverflowCell oc;
			oc.moreTransitions = tCount - 1;
			oc.padding = 0;
			writeOverflow(transducerFile, byteSwap, oc);
		}
		for (uint32_t ti = 1; ti < tCount; ti++) {
			Transition & t = it->transitions[ti];
			setTarget(t, stateOrdinalToOffset, it->targetStateOrds[ti]);
			t.transInfo.moreTransitions = 0;
			writeTrans(transducerFile, byteSwap, t);
		}
	}
	
	transducerFile.close();
}
