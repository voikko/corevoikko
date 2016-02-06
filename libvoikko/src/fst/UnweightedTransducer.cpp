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
#include "fst/UnweightedTransducer.hpp"
#include "fst/Configuration.hpp"
#include "setup/DictionaryException.hpp"
#include "utf8/utf8.hpp"
#include "utils/StringUtils.hpp"
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
	
	static uint16_t swap(uint16_t x) {
		return (x>>8) | (x<<8);
	}
	
	static uint32_t swap(uint32_t x) {
		return  (x>>24) | 
			((x<<8) & 0x00FF0000) |
			((x>>8) & 0x0000FF00) |
			(x<<24);
	}
	
	void UnweightedTransducer::byteSwapTransducer(void *& mapPtr, size_t fileLength) {
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
			size_t padding = sizeof(Transition) - (newMapPtr - newMap) % sizeof(Transition);
			if (padding < sizeof(Transition)) {
				// skip padding - transition table starts at next 8 byte boundary
				oldMapPtr += padding;
				memset(newMapPtr, 0, padding);
				newMapPtr += padding;
			}
		}
		
		bool nextIsOverflow = false;
		while (newMap + fileLength > newMapPtr) {
			if (nextIsOverflow) {
				OverflowCell oc = *reinterpret_cast<OverflowCell *>(oldMapPtr);
				oc.moreTransitions = swap(oc.moreTransitions);
				memcpy(newMapPtr, &oc, sizeof(OverflowCell));
				nextIsOverflow = false;
			}
			else {
				Transition t = *reinterpret_cast<Transition *>(oldMapPtr);
				t.symIn = swap(t.symIn);
				t.symOut = swap(t.symOut);
				uint32_t ts = t.transInfo.targetState;
				t.transInfo.targetState = ((ts<<16) & 0x00FF0000) | (ts & 0x0000FF00) | ((ts>>16) & 0x000000FF);
				nextIsOverflow = (t.transInfo.moreTransitions == 0xFF);
				memcpy(newMapPtr, &t, sizeof(Transition));
			}
			oldMapPtr += sizeof(Transition);
			newMapPtr += sizeof(Transition);
		}
		
		vfstMunmap(mapPtr, fileLength);
		mapPtr = newMap;
	}
	
	UnweightedTransducer::UnweightedTransducer(const char * filePath) : Transducer() {
		map = vfstMmap(filePath, fileLength);
		if (!map) {
			throw setup::DictionaryException("Unweighted transducer file could not be read");
		}
		byteSwapped = checkNeedForByteSwapping(static_cast<char *>(map));
		if (isWeightedTransducerFile(static_cast<char *>(map))) {
			throw setup::DictionaryException("Expected unweighted but got weighted transducer");
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
		DEBUG("Reading " << symbolCount << " symbols to symbol table");
		for (uint16_t i = 0; i < symbolCount; i++) {
			wchar_t * ucs4Symbol = utils::StringUtils::ucs4FromUtf8(filePtr);
			symbolToString.push_back(ucs4Symbol);
			if (i == 0) {
				symbolToDiacritic.push_back(OpFeatureValue()); // epsilon
				symbolStringLength.push_back(0);
				filePtr += 1;
			}
			else {
				string symbol(filePtr);
				if (firstNormalChar == 0) {
					if (symbol[0] == '@') {
						symbolToDiacritic.push_back(getDiacriticOperation(symbol, features, values));
					}
					else {
						firstNormalChar = i;
					}
				}
				else if (firstMultiChar == 0 && symbol[0] == '[') {
					firstMultiChar = i;
				}
				symbolStringLength.push_back(wcslen(ucs4Symbol));
				if (firstNormalChar > 0 && firstMultiChar == 0) {
					stringToSymbol.insert(pair<wchar_t, uint16_t>(ucs4Symbol[0], i));
				}
				filePtr += (symbol.length() + 1);
			}
		}
		unknownSymbolOrdinal = symbolCount;
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
	
	UnweightedTransducer::~UnweightedTransducer() {
		for (wchar_t * s : symbolToString) {
			delete[] s;
		}
	}
	
	bool UnweightedTransducer::prepare(Configuration * configuration, const wchar_t * input, size_t inputLen) const {
		configuration->stackDepth = 0;
		configuration->flagDepth = 0;
		configuration->inputDepth = 0;
		configuration->stateIndexStack[0] = 0;
		configuration->currentTransitionStack[0] = 0;
		configuration->inputLength = 0;
		bool allKnown = true;
		while ((size_t)configuration->inputLength < inputLen) {
			std::map<wchar_t,uint16_t>::const_iterator it = stringToSymbol.find(input[configuration->inputLength]);
			if (it == stringToSymbol.end()) {
				configuration->inputSymbolStack[configuration->inputLength] = unknownSymbolOrdinal;
				allKnown = false;
			}
			else {
				configuration->inputSymbolStack[configuration->inputLength] = it->second;
			}
			configuration->inputLength++;
		}
		return allKnown;
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
		if (!flagDiacriticFeatureCount || symbol == 0) {
			return true;
		}
		size_t diacriticCell = flagDiacriticFeatureCount * sizeof(uint16_t);
		uint16_t * flagValueStack = configuration->flagValueStack;
		uint16_t * currentFlagArray = flagValueStack + configuration->flagDepth * flagDiacriticFeatureCount;
		
		bool update = false;
		OpFeatureValue ofv = transducer->symbolToDiacritic[symbol];
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
		
		memcpy(currentFlagArray + flagDiacriticFeatureCount, currentFlagArray, diacriticCell);
		if (update) {
			DEBUG("updating feature " << ofv.feature << " to " << ofv.value)
			(currentFlagArray + flagDiacriticFeatureCount)[ofv.feature] = ofv.value;
		}
		configuration->flagDepth++;
		return true;
	}
	
	bool UnweightedTransducer::next(Configuration * configuration, wchar_t * outputBuffer, size_t bufferLen) const {
		return nextPrefix(configuration, outputBuffer, bufferLen, 0);
	}
	
	bool UnweightedTransducer::nextPrefix(Configuration * configuration, wchar_t * outputBuffer, size_t bufferLen, size_t * prefixLength) const {
		uint32_t loopCounter = 0;
		while (loopCounter < MAX_LOOP_COUNT) {
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
					if (configuration->inputDepth == configuration->inputLength || prefixLength) {
						wchar_t * outputBufferPos = outputBuffer;
						for (int i = 0; i < configuration->stackDepth; i++) {
							uint16_t outSymIndex = configuration->outputSymbolStack[i];
							size_t symLen = symbolStringLength[outSymIndex];
							if ((outputBufferPos - outputBuffer) + symLen + 1 >= bufferLen) {
								// would overflow the output buffer
								return false;
							}
							wcsncpy(outputBufferPos, symbolToString[outSymIndex], symLen);
							outputBufferPos += symLen;
						}
						*outputBufferPos = L'\0';
						configuration->currentTransitionStack[configuration->stackDepth] = currentTransition - transitionStart + 1;
						if (prefixLength) {
							*prefixLength = configuration->inputDepth;
						}
						return true;
					}
				}
				else if ((configuration->inputDepth < configuration->inputLength &&
					  configuration->inputSymbolStack[configuration->inputDepth] == currentTransition->symIn) ||
					 (currentTransition->symIn < firstNormalChar && flagDiacriticCheck(configuration, this, currentTransition->symIn))) {
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
				else if (flagDiacriticFeatureCount && previousInputSymbol != 0) {
					configuration->flagDepth--;
				}
			}
			configuration->currentTransitionStack[configuration->stackDepth]++;
			nextInMainLoop:
			loopCounter++;
		}
		DEBUG("maximum number of loops reached")
		return false;
	}
	
} }
