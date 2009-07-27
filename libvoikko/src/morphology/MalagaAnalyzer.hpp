/* Libvoikko: Library of Finnish language tools
 * Copyright (C) 2009 Harri Pitkänen <hatapitk@iki.fi>
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

#ifndef VOIKKO_MORPHOLOGY_MALAGA_ANALYZER
#define VOIKKO_MORPHOLOGY_MALAGA_ANALYZER

#include "morphology/Analyzer.hpp"
#include <malaga.h>
#include <map>

namespace libvoikko { namespace morphology {

enum MalagaSymbol {
	MS_RAKENNE,
	MS_SIJAMUOTO,
	MS_CLASS,
	MS_LAST_SYMBOL
};

/**
 * Morphological analyzer that uses Malaga and Suomi-malaga
 * to analyze words. Malaga must be initialized before this
 * is used.
 */
class MalagaAnalyzer : public Analyzer {
	public:
		std::list<Analysis *> * analyze(const wchar_t * word) const;
		std::list<Analysis *> * analyze(const wchar_t * word,
		                                size_t wlen) const;
		std::list<Analysis *> * analyze(const char * word) const;
	
	private:
		void parseStructure(Analysis * &analysis, value_t &result) const;
		void parseSijamuoto(Analysis * &analysis, value_t &result) const;
		void parseClass(Analysis * &analysis, value_t &result) const;
		static void initSymbols();
		
		static bool symbolsInited;
		static symbol_t symbols[MS_LAST_SYMBOL];
		static std::map<symbol_t, const wchar_t *> sijamuotoMap;
		static std::map<symbol_t, const wchar_t *> classMap;
};

} }

#endif
