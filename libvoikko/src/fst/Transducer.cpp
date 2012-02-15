/* Libvoikko: Finnish spellchecker and hyphenator library
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

#include "fst/Transducer.hpp"
#include "fst/Configuration.hpp"
#include "utf8/utf8.hpp"
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <cstring>

using namespace std;

namespace libvoikko { namespace fst {
	
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
		uint16_t symbolCount;
		memcpy(&symbolCount, filePtr, sizeof(uint16_t));
		filePtr += sizeof(uint16_t);
		
		for (uint16_t i = 0; i < symbolCount; i++) {
			string symbol(filePtr);
			filePtr += (symbol.length() + 1);
			stringToSymbol.insert(pair<string, uint16_t>(symbol, i));
			symbolToString.push_back(symbol);
		}
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
	
	bool Transducer::next(Configuration * configuration, char * outputBuffer, size_t bufferLen) const {
		while (true) {
			Transition * stateHead = transitionStart + configuration->stateIndexStack[configuration->stackDepth];
			Transition * currentTransition = transitionStart + configuration->currentTransitionStack[configuration->stackDepth];
			uint32_t startTransitionIndex = currentTransition - stateHead;
			for (uint32_t tc = startTransitionIndex; tc <= stateHead->transInfo.moreTransitions; tc++) {
				// next
				if (currentTransition->symIn == 0xFFFF) {
					// final state
					if (configuration->inputDepth == configuration->inputLength) {
						char * outputBufferPos = outputBuffer;
						for (int i = 0; i < configuration->stackDepth; i++) {
							string outputSym = symbolToString[configuration->outputSymbolStack[i]];
							strncpy(outputBufferPos, outputSym.c_str(), outputSym.length());
							outputBufferPos += outputSym.length();
						}
						*outputBufferPos = '\0';
						configuration->currentTransitionStack[configuration->stackDepth] = currentTransition - transitionStart + 1;
						return true;
					}
				}
				else if ((configuration->inputDepth < configuration->inputLength &&
					  configuration->inputSymbolStack[configuration->inputDepth] == currentTransition->symIn) ||
					 currentTransition->symIn == 0) {
					// down
					configuration->outputSymbolStack[configuration->stackDepth] = currentTransition->symOut;
					configuration->currentTransitionStack[configuration->stackDepth] = currentTransition - transitionStart;
					configuration->stackDepth++;
					configuration->stateIndexStack[configuration->stackDepth] = currentTransition->transInfo.targetState;
					configuration->currentTransitionStack[configuration->stackDepth] = currentTransition->transInfo.targetState;
					if (currentTransition->symIn) {
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
			if ((transitionStart + configuration->currentTransitionStack[configuration->stackDepth])->symIn) {
				configuration->inputDepth--;
			}
			configuration->currentTransitionStack[configuration->stackDepth]++;
			nextInMainLoop:
				;
		}
	}
	
	void Transducer::terminate() {
		munmap(map, fileLength);
	}
} }
