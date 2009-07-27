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

#ifndef VOIKKO_MORPHOLOGY_ANALYZER
#define VOIKKO_MORPHOLOGY_ANALYZER

#include "morphology/Analysis.hpp"
#include <list>

namespace libvoikko { namespace morphology {

/**
 * General interface for morphological analyzers.
 */
class Analyzer {
	public:
		/**
		 * Analyzes given word and returns a list of results.
		 * The results and the list must be deleted after use.
		 */
		virtual std::list<Analysis *> * analyze(const wchar_t * word) const = 0;
		virtual std::list<Analysis *> * analyze(const wchar_t * word,
		                                        size_t wlen) const = 0;
		virtual std::list<Analysis *> * analyze(const char * word) const = 0;

		/**
		 * Deletes a list of analyses returned from an analyzer.
		 */
		static void deleteAnalyses(std::list<Analysis *> * &analyses);
};

} }

#endif
