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

#include "fst/Transducer.hpp"
#include "fst/Configuration.hpp"
#include "utf8/utf8.hpp"
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <cstring>

#if 0
#include <iostream>
#define DEBUG(x) cerr << x << endl;
#else
#define DEBUG(x)
#endif

using namespace std;

namespace libvoikko { namespace fst {
	
	const uint16_t FlagValueNeutral = 0;
	const uint16_t FlagValueAny = 1;
	
	static OpFeatureValue getDiacriticOperation(const string & symbol, map<string, uint16_t> & features, map<string, uint16_t> & values) {
		OpFeatureValue operation;
		switch (symbol[1]) {
			case 'P':
				operation.op = Operation_P;
				break;
			case 'C':
				operation.op = Operation_C;
				break;
			case 'U':
				operation.op = Operation_U;
				break;
			case 'R':
				operation.op = Operation_R;
				break;
			case 'D':
				operation.op = Operation_D;
				break;
			default:
				// this would be an error
				return operation;
		}
		string featureAndValue = symbol.substr(3, symbol.length() - 4);
		size_t valueStart = featureAndValue.find(".");
		{
			string feature;
			if (valueStart == string::npos) {
				feature = featureAndValue;
			}
			else {
				feature = featureAndValue.substr(0, valueStart);
			}
			map<string, uint16_t>::const_iterator it = features.find(feature);
			if (it == features.end()) {
				operation.feature = features.size();
				features[feature] = operation.feature;
			}
			else {
				operation.feature = it->second;
			}
		}
		
		{
			string value("@");
			if (valueStart != string::npos) {
				value = featureAndValue.substr(valueStart + 1);
			}
			map<string, uint16_t>::const_iterator it = values.find(value);
			if (it == values.end()) {
				operation.value = values.size();
				values[value] = operation.value;
			}
			else {
				operation.value = it->second;
			}
		}
		return operation;
	}
	
	static bool checkNeedForByteSwapping(const char * filePtr) {
		const uint32_t COOKIE1 = 0x00013A6E;
		const uint32_t COOKIE2 = 0x000351FA;
		const uint32_t COOKIE1REV = 0x6E3A0100;
		const uint32_t COOKIE2REV = 0xFA510300;
		if (memcmp(filePtr, &COOKIE1, sizeof(uint32_t)) == 0 &&
		    memcmp(filePtr + sizeof(uint32_t), &COOKIE2, sizeof(uint32_t)) == 0) {
			// native byte order
			return false;
		}
		if (memcmp(filePtr, &COOKIE1REV, sizeof(uint32_t)) == 0 &&
		    memcmp(filePtr + sizeof(uint32_t), &COOKIE2REV, sizeof(uint32_t)) == 0) {
			// reverse byte order
			return true;
		}
		// TODO
		throw "Unknown byte order or file type";
	}
	
	Transducer::Transducer(const char * filePath) {
		int fd = open(filePath, O_RDONLY);
		if (fd == -1) {
			// TODO
			throw "File could not be read";
		}
		
		struct stat st;
		fstat(fd, &st);
		fileLength = st.st_size;
		
		map = mmap(0, fileLength, PROT_READ, MAP_SHARED, fd, 0);
		
		char * filePtr = static_cast<char *>(map);
		byteSwapped = checkNeedForByteSwapping(filePtr);
		filePtr += 16; // skip header
		uint16_t symbolCount;
		memcpy(&symbolCount, filePtr, sizeof(uint16_t));
		filePtr += sizeof(uint16_t);
		
		firstNormalChar = 0;
		firstMultiChar = 0;
		std::map<string, uint16_t> features;
		std::map<string, uint16_t> values;
		values[""] = FlagValueNeutral;
		values["@"] = FlagValueAny;
		symbolToDiacritic.push_back(OpFeatureValue()); // epsilon
		for (uint16_t i = 0; i < symbolCount; i++) {
			string symbol(filePtr);
			/* TODO this does not work yet
			if (symbol == "@_SPACE_@") {
				symbol = " ";
			}
			*/
			if (firstNormalChar == 0 && i > 0 && symbol[0] != '@') {
				firstNormalChar = i;
			}
			if (firstNormalChar != 0 && firstMultiChar == 0 && symbol[0] == '[') {
				firstMultiChar = i;
			}
			symbolToString.push_back(filePtr);
			filePtr += (symbol.length() + 1);
			stringToSymbol.insert(pair<string, uint16_t>(symbol, i));
			if (firstNormalChar == 0 && i > 0) {
				symbolToDiacritic.push_back(getDiacriticOperation(symbol, features, values));
			}
		}
		flagDiacriticFeatureCount = features.size();
		{
			size_t partial = (filePtr - static_cast<char *>(map)) % sizeof(Transition);
			if (partial > 0) {
				// skip padding - transition table starts at next 8 byte boundary
				filePtr += (sizeof(Transition) - partial);
			}
		}
		transitionStart = reinterpret_cast<Transition *>(filePtr);
	}
	
	bool Transducer::prepare(Configuration * configuration, const char * input, size_t inputLen) const {
		configuration->stackDepth = 0;
		configuration->inputDepth = 0;
		configuration->stateIndexStack[0] = 0;
		configuration->currentTransitionStack[0] = 0;
		configuration->inputLength = 0;
		const char * ip = input;
		while (ip < input + inputLen) {
			const char * prevIp = ip;
			utf8::unchecked::next(ip);
			std::map<std::string,uint16_t>::const_iterator it = stringToSymbol.find(string(prevIp, ip - prevIp));
			if (it == stringToSymbol.end()) {
				// Unknown symbol
				return false;
			}
			configuration->inputSymbolStack[configuration->inputLength] = it->second;
			configuration->inputLength++;
		}
		return true;
	}
	
	static uint32_t getMaxTc(Transition * stateHead) {
		uint32_t maxTc = stateHead->transInfo.moreTransitions;
		if (maxTc == 255) {
			OverflowCell * oc = reinterpret_cast<OverflowCell *>(stateHead + 1);
			maxTc = oc->moreTransitions + 1;
		}
		return maxTc;
	}
	
	static bool flagDiacriticCheck(Configuration * configuration, const Transducer * transducer, uint16_t symbol) {
		uint16_t flagDiacriticFeatureCount = transducer->flagDiacriticFeatureCount;
		if (!flagDiacriticFeatureCount) {
			return true;
		}
		int stackDepth = configuration->stackDepth;
		size_t diacriticCell = flagDiacriticFeatureCount * sizeof(uint16_t);
		uint16_t * flagValueStack = configuration->flagValueStack;
		uint16_t * currentFlagArray = flagValueStack + stackDepth * diacriticCell;
		
		bool update = false;
		OpFeatureValue ofv;
		if (symbol != 0 && symbol < transducer->firstNormalChar) {
			ofv = transducer->symbolToDiacritic[symbol];
			uint16_t currentValue = currentFlagArray[ofv.feature];
			DEBUG("checking op " << ofv.op << " " << ofv.feature << " " << ofv.value << " current value " << currentValue)
			switch (ofv.op) {
				case Operation_P:
					update = true;
					break;
				case Operation_C:
					ofv.value = FlagValueNeutral;
					update = true;
					break;
				case Operation_U:
					if (currentValue) {
						if (currentValue != ofv.value) {
							return false;
						}
					}
					else {
						update = true;
					}
					break;
				case Operation_R:
					if (ofv.value == FlagValueAny && currentValue == FlagValueNeutral) {
						return false;
					}
					if (ofv.value != FlagValueAny && currentValue != ofv.value) {
						return false;
					}
					break;
				case Operation_D:
					if ((ofv.value == FlagValueAny && currentValue != FlagValueNeutral) || currentValue == ofv.value) {
						return false;
					}
					break;
				default:
					return false;// this would be an error
			}
			DEBUG("allowed")
		}
		
		memcpy(currentFlagArray + diacriticCell, currentFlagArray, diacriticCell);
		if (update) {
			DEBUG("updating feature " << ofv.feature << " to " << ofv.value)
			(currentFlagArray + diacriticCell)[ofv.feature] = ofv.value;
		}
		return true;
	}
	
	bool Transducer::next(Configuration * configuration, char * outputBuffer, size_t bufferLen) const {
		while (true) {
			Transition * stateHead = transitionStart + configuration->stateIndexStack[configuration->stackDepth];
			Transition * currentTransition = transitionStart + configuration->currentTransitionStack[configuration->stackDepth];
			uint32_t startTransitionIndex = currentTransition - stateHead;
			uint32_t maxTc = getMaxTc(stateHead);
			for (uint32_t tc = startTransitionIndex; tc <= maxTc; tc++) {
				if (tc == 1 && maxTc >= 255) {
					// skip overflow cell
					tc++;
					currentTransition++;
				}
				// next
				if (currentTransition->symIn == 0xFFFF) {
					// final state
					if (configuration->inputDepth == configuration->inputLength) {
						char * outputBufferPos = outputBuffer;
						for (int i = 0; i < configuration->stackDepth; i++) {
							const char * outputSym = symbolToString[configuration->outputSymbolStack[i]];
							size_t symLen = strlen(outputSym);
							strncpy(outputBufferPos, outputSym, symLen);
							outputBufferPos += symLen;
						}
						*outputBufferPos = '\0';
						configuration->currentTransitionStack[configuration->stackDepth] = currentTransition - transitionStart + 1;
						return true;
					}
				}
				else if (((configuration->inputDepth < configuration->inputLength &&
					  configuration->inputSymbolStack[configuration->inputDepth] == currentTransition->symIn) ||
					  currentTransition->symIn < firstNormalChar) &&
					  flagDiacriticCheck(configuration, this, currentTransition->symIn)) {
					// down
					DEBUG("down " << tc)
					if (configuration->stackDepth + 2 == configuration->bufferSize) {
						// max stack depth reached
						return false;
					}
					configuration->outputSymbolStack[configuration->stackDepth] = 
						(currentTransition->symOut >= firstNormalChar ? currentTransition->symOut : 0);
					configuration->currentTransitionStack[configuration->stackDepth] = currentTransition - transitionStart;
					configuration->stackDepth++;
					configuration->stateIndexStack[configuration->stackDepth] = currentTransition->transInfo.targetState;
					configuration->currentTransitionStack[configuration->stackDepth] = currentTransition->transInfo.targetState;
					if (currentTransition->symIn >= firstNormalChar) {
						configuration->inputDepth++;
					}
					goto nextInMainLoop;
				}
				currentTransition++;
			}
			if (configuration->stackDepth == 0) {
				// end
				return false;
			}
			// up
			configuration->stackDepth--;
			DEBUG("up")
			{
				uint16_t previousInputSymbol = (transitionStart + configuration->currentTransitionStack[configuration->stackDepth])->symIn;
				if (previousInputSymbol >= firstNormalChar) {
					configuration->inputDepth--;
				}
			}
			configuration->currentTransitionStack[configuration->stackDepth]++;
			nextInMainLoop:
				;
		}
	}
	
	uint16_t Transducer::getFlagDiacriticFeatureCount() const {
		return flagDiacriticFeatureCount;
	}
	
	void Transducer::terminate() {
		munmap(map, fileLength);
	}
} }
