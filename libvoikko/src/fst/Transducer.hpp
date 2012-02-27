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

#ifndef LIBVOIKKO_FST_TRANSDUCER_H
#define LIBVOIKKO_FST_TRANSDUCER_H

#include <cstddef>
#include <map>
#include <vector>
#include <string>
#include <stdint.h>
#include "fst/Transition.hpp"
#include "fst/Configuration.hpp"

namespace libvoikko { namespace fst {
	
	enum Operation {
		P,
		C,
		U,
		R,
		D
	};
	
	struct OpFeatureValue {
		Operation op;
		uint16_t feature;
		uint16_t value;
	};
	
	class Transducer {
		private:
			size_t fileLength;
			void * map;
			Transition * transitionStart;
			std::map<std::string, uint16_t> stringToSymbol;
			std::vector<std::string> symbolToString;
			std::vector<OpFeatureValue> symbolToDiacritic;
			uint16_t flagDiacriticFeatureCount;
			uint16_t firstNormalChar;
			uint16_t firstMultiChar;
		public:
			Transducer(const char * filePath);
			
			bool prepare(Configuration * configuration, const char * input, size_t inputLen) const;
			
			bool next(Configuration * configuration, char * outputBuffer, size_t bufferLen) const;
			
			void terminate();
	};
} }

#endif
