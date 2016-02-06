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


#ifndef LIBVOIKKO_FST_UNWEIGHTED_TRANSDUCER_H
#define LIBVOIKKO_FST_UNWEIGHTED_TRANSDUCER_H

#include "fst/Transducer.hpp"
#include "fst/Transition.hpp"
#include "fst/Configuration.hpp"

namespace libvoikko { namespace fst {
	
	class UnweightedTransducer : public Transducer {
		static_assert(sizeof(Transition) == 8, "Size of unweighted transition must be 8 bytes");
		private:
			Transition * transitionStart;
			std::map<wchar_t, uint16_t> stringToSymbol;
			std::vector<wchar_t *> symbolToString;
			std::vector<size_t> symbolStringLength;
			uint16_t firstMultiChar;
			uint16_t unknownSymbolOrdinal;
			void byteSwapTransducer(void *& mapPtr, size_t fileLength);
		public:
			UnweightedTransducer(const char * filePath);
			~UnweightedTransducer();
			
			bool prepare(Configuration * configuration, const wchar_t * input, size_t inputLen) const;
			
			bool next(Configuration * configuration, wchar_t * outputBuffer, size_t bufferLen) const;
			
			bool nextPrefix(Configuration * configuration, wchar_t * outputBuffer, size_t bufferLen, size_t * prefixLength) const;
	};
} }

#endif
