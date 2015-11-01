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
 * Portions created by the Initial Developer are Copyright (C) 2012 - 2015
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

#include "porting.h"
#include "fst/WeightedTransducer.hpp"
#include "fst/Configuration.hpp"
#include "setup/DictionaryException.hpp"
#include "utf8/utf8.hpp"
#include <sys/types.h>
#include <cstring>

#ifdef HAVE_MMAP
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <unistd.h>
#endif

#ifdef WIN32
#include <windows.h>
#endif

#if 0
#include <iostream>
#define DEBUG(x) cerr << x << endl;
#else
#define DEBUG(x)
#endif

using namespace std;

namespace libvoikko { namespace fst {
	
	// TODO de-duplicate these
	static uint16_t swap(uint16_t x) {
		return (x>>8) | (x<<8);
	}
	
	static int16_t swap(int16_t x) {
		return (x>>8) | (x<<8);
	}
	
	static uint32_t swap(uint32_t x) {
		return  (x>>24) | 
			((x<<8) & 0x00FF0000) |
			((x>>8) & 0x0000FF00) |
			(x<<24);
	}
	
	void WeightedTransducer::byteSwapTransducer(void *& mapPtr, size_t fileLength) {
		DEBUG("Byte-swapping the transducer");
		char * newMap = new char[fileLength];
		// skip header
		char * oldMapPtr = static_cast<char *>(mapPtr) + 16;
		char * newMapPtr = newMap + 16;
		
		uint16_t symbolCount = swap(*reinterpret_cast<uint16_t *>(oldMapPtr));
		memcpy(newMapPtr, &symbolCount, sizeof(uint16_t));
		oldMapPtr += sizeof(uint16_t);
		newMapPtr += sizeof(uint16_t);
		
		for (uint16_t i = 0; i < symbolCount; i++) {
			size_t symLength = strlen(oldMapPtr) + 1;
			memcpy(newMapPtr, oldMapPtr, symLength);
			oldMapPtr += symLength;
			newMapPtr += symLength;
		}
		
		{
			size_t padding = sizeof(WeightedTransition) - (newMapPtr - newMap) % sizeof(WeightedTransition);
			if (padding < sizeof(WeightedTransition)) {
				// skip padding - transition table starts at next 16 byte boundary
				oldMapPtr += padding;
				memset(newMapPtr, 0, padding);
				newMapPtr += padding;
			}
		}
		
		bool nextIsOverflow = false;
		while (newMap + fileLength > newMapPtr) {
			if (nextIsOverflow) {
				WeightedOverflowCell oc = *reinterpret_cast<WeightedOverflowCell *>(oldMapPtr);
				oc.moreTransitions = swap(oc.moreTransitions);
				memcpy(newMapPtr, &oc, sizeof(WeightedOverflowCell));
				nextIsOverflow = false;
			}
			else {
				WeightedTransition t = *reinterpret_cast<WeightedTransition *>(oldMapPtr);
				t.symIn = swap(t.symIn);
				t.symOut = swap(t.symOut);
				t.targetState = swap(t.targetState);
				t.weight = swap(t.weight);
				nextIsOverflow = (t.moreTransitions == 0xFF);
				memcpy(newMapPtr, &t, sizeof(WeightedTransition));
			}
			oldMapPtr += sizeof(WeightedTransition);
			newMapPtr += sizeof(WeightedTransition);
		}
		
		vfstMunmap(mapPtr, fileLength);
		mapPtr = newMap;
	}
	
	WeightedTransducer::WeightedTransducer(const char * filePath) : Transducer() {
		map = vfstMmap(filePath, fileLength);
		if (!map) {
			DEBUG(filePath);
			throw setup::DictionaryException("Weighted transducer file could not be read");
		}
		byteSwapped = checkNeedForByteSwapping(static_cast<char *>(map));
		if (!isWeightedTransducerFile(static_cast<char *>(map))) {
			throw setup::DictionaryException("Expected weighted but got unweighted transducer");
		}
		if (byteSwapped) {
			byteSwapTransducer(map, fileLength);
		}
		char * filePtr = static_cast<char *>(map);
		
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
		DEBUG("Reading " << symbolCount << " symbols to symbol table");
		for (uint16_t i = 0; i < symbolCount; i++) {
			string symbol(filePtr);
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
			size_t partial = (filePtr - static_cast<char *>(map)) % sizeof(WeightedTransition);
			if (partial > 0) {
				// skip padding - transition table starts at next 8 byte boundary
				filePtr += (sizeof(WeightedTransition) - partial);
			}
		}
		transitionStart = reinterpret_cast<WeightedTransition *>(filePtr);
	}
	
	bool WeightedTransducer::prepare(WeightedConfiguration * configuration, const char * input, size_t inputLen) const {
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
	
	static uint32_t getMaxTc(WeightedTransition * stateHead) {
		uint32_t maxTc = stateHead->moreTransitions;
		if (maxTc == 255) {
			WeightedOverflowCell * oc = reinterpret_cast<WeightedOverflowCell *>(stateHead + 1);
			maxTc = oc->moreTransitions + 1;
		}
		return maxTc;
	}
	
	static bool flagDiacriticCheck(WeightedConfiguration * configuration, const Transducer * transducer, uint16_t symbol) {
		uint16_t flagDiacriticFeatureCount = transducer->flagDiacriticFeatureCount;
		if (!flagDiacriticFeatureCount) {
			return true;
		}
		int stackDepth = configuration->stackDepth;
		size_t diacriticCell = flagDiacriticFeatureCount * sizeof(uint32_t);
		uint32_t * flagValueStack = configuration->flagValueStack;
		uint32_t * currentFlagArray = flagValueStack + stackDepth * flagDiacriticFeatureCount;
		
		bool update = false;
		OpFeatureValue ofv;
		if (symbol != 0 && symbol < transducer->firstNormalChar) {
			ofv = transducer->symbolToDiacritic[symbol];
			uint32_t currentValue = currentFlagArray[ofv.feature];
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
		
		memcpy(currentFlagArray + flagDiacriticFeatureCount, currentFlagArray, diacriticCell);
		if (update) {
			DEBUG("updating feature " << ofv.feature << " to " << ofv.value)
			(currentFlagArray + flagDiacriticFeatureCount)[ofv.feature] = ofv.value;
		}
		return true;
	}
	
	bool WeightedTransducer::next(WeightedConfiguration * configuration, char * outputBuffer, size_t bufferLen) const {
		int16_t weight;
		return next(configuration, outputBuffer, bufferLen, &weight);
	}
	
	bool WeightedTransducer::next(WeightedConfiguration * configuration, char * outputBuffer, size_t bufferLen, int16_t * weight) const {
		int firstNotReachedPosition;
		return next(configuration, outputBuffer, bufferLen, weight, &firstNotReachedPosition);
	}
	
	bool WeightedTransducer::next(WeightedConfiguration * configuration, char * outputBuffer, size_t bufferLen, int16_t * weight, int * firstNotReachedPosition) const {
		uint32_t loopCounter = 0;
		*firstNotReachedPosition = configuration->inputDepth;
		while (loopCounter < MAX_LOOP_COUNT) {
			WeightedTransition * stateHead = transitionStart + configuration->stateIndexStack[configuration->stackDepth];
			WeightedTransition * currentTransition = transitionStart + configuration->currentTransitionStack[configuration->stackDepth];
			uint32_t startTransitionIndex = currentTransition - stateHead;
			uint32_t maxTc = getMaxTc(stateHead);
			for (uint32_t tc = startTransitionIndex; tc <= maxTc; tc++) {
				if (tc == 1 && maxTc >= 255) {
					// skip overflow cell
					tc++;
					currentTransition++;
				}
				// next
				if (currentTransition->symIn == 0xFFFFFFFF) {
					// final state
					if (configuration->inputDepth == configuration->inputLength) {
						char * outputBufferPos = outputBuffer;
						for (int i = 0; i < configuration->stackDepth; i++) {
							const char * outputSym = symbolToString[configuration->outputSymbolStack[i]];
							size_t symLen = strlen(outputSym);
							if ((outputBufferPos - outputBuffer) + symLen + 1 >= bufferLen) {
								DEBUG("would overflow the output buffer")
								return false;
							}
							strncpy(outputBufferPos, outputSym, symLen);
							outputBufferPos += symLen;
						}
						*outputBufferPos = '\0';
						configuration->currentTransitionStack[configuration->stackDepth] = currentTransition - transitionStart + 1;
						*weight = currentTransition->weight;
						for (int i = 0; i < configuration->stackDepth; i++) {
							*weight += (transitionStart + configuration->currentTransitionStack[i])->weight;
						}
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
						DEBUG("max stack depth reached")
						return false;
					}
					configuration->outputSymbolStack[configuration->stackDepth] = 
						(currentTransition->symOut >= firstNormalChar ? currentTransition->symOut : 0);
					configuration->currentTransitionStack[configuration->stackDepth] = currentTransition - transitionStart;
					configuration->stackDepth++;
					configuration->stateIndexStack[configuration->stackDepth] = currentTransition->targetState;
					configuration->currentTransitionStack[configuration->stackDepth] = currentTransition->targetState;
					if (currentTransition->symIn >= firstNormalChar) {
						configuration->inputDepth++;
						if (*firstNotReachedPosition < configuration->inputDepth) {
							*firstNotReachedPosition = configuration->inputDepth;
						}
					}
					goto nextInMainLoop;
				}
				currentTransition++;
			}
			if (configuration->stackDepth == 0) {
				DEBUG("end")
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
			loopCounter++;
		}
		DEBUG("maximum number of loops reached")
		return false;
	}
	
	void WeightedTransducer::backtrackToOutputDepth(WeightedConfiguration * configuration, int depth) {
		int outputDepth = 0;
		int stackIndex = 0;
		while (outputDepth < depth + 1 && stackIndex < configuration->stackDepth) {
			uint16_t outputSymbol = (transitionStart + configuration->currentTransitionStack[stackIndex])->symOut;
			if (outputSymbol >= firstNormalChar) {
				outputDepth++;
			}
			stackIndex++;
		}
		while (stackIndex + 0 < configuration->stackDepth) {
			configuration->stackDepth--;
			{
				uint16_t previousInputSymbol = (transitionStart + configuration->currentTransitionStack[configuration->stackDepth])->symIn;
				if (previousInputSymbol >= firstNormalChar) {
					configuration->inputDepth--;
				}
			}
			configuration->currentTransitionStack[configuration->stackDepth]++;
		}
	}
	
} }
