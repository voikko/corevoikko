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
#include "fst/Transducer.hpp"
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
	
	Transducer::~Transducer() {
	}

	OpFeatureValue Transducer::getDiacriticOperation(const string & symbol, std::map<string, uint16_t> & features, std::map<string, uint16_t> & values) {
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
				operation.op = Operation_D;
		}
		size_t symbolLength = symbol.length();
		if (symbolLength <= 4) {
			throw setup::DictionaryException("Malformed flag diacritic");
		}
		string featureAndValue = symbol.substr(3, symbolLength - 4);
		size_t valueStart = featureAndValue.find(".");
		{
			string feature;
			if (valueStart == string::npos) {
				feature = featureAndValue;
			}
			else {
				feature = featureAndValue.substr(0, valueStart);
			}
			std::map<string, uint16_t>::const_iterator it = features.find(feature);
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
			std::map<string, uint16_t>::const_iterator it = values.find(value);
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
	
	void * Transducer::vfstMmap(const char * filePath, size_t & fileLength) {
		#ifdef HAVE_MMAP
			int fd = open(filePath, O_RDONLY);
			if (fd == -1) {
				return 0;
			}
			
			struct stat st;
			fstat(fd, &st);
			fileLength = st.st_size;
			
			void * map = mmap(0, fileLength, PROT_READ, MAP_SHARED, fd, 0);
			close(fd);
			return map;
		#endif
		#ifdef WIN32
			HANDLE fileHandle = CreateFile(filePath, GENERIC_READ, FILE_SHARE_READ,
			                    0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
			HANDLE mapHandle = CreateFileMapping(fileHandle, 0, PAGE_READONLY,
			                   0, 0, 0);
			void * map = MapViewOfFile(mapHandle, FILE_MAP_READ, 0, 0, 0);
			fileLength = GetFileSize(fileHandle, 0);
			CloseHandle(mapHandle);
			CloseHandle(fileHandle);
			return map;
		#endif
	}
	
	void Transducer::vfstMunmap(void * map, size_t fileLength) {
		#ifdef HAVE_MMAP
			munmap(map, fileLength);
		#endif
		#ifdef WIN32
			(void)(fileLength);
			UnmapViewOfFile(map);
		#endif
	}
	
	bool Transducer::checkNeedForByteSwapping(const char * filePtr) {
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
		throw setup::DictionaryException("Unknown byte order or file type");
	}
	
	bool Transducer::isWeightedTransducerFile(const char * filePtr) {
		return filePtr[8] == 0x01;
	}
	
	uint16_t Transducer::getFlagDiacriticFeatureCount() const {
		return flagDiacriticFeatureCount;
	}
	
	void Transducer::terminate() {
		if (byteSwapped) {
			delete[] (char *) map;
		}
		else {
			vfstMunmap(map, fileLength);
		}
	}
} }
