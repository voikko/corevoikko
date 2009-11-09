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

#ifndef VOIKKO_MORPHOLOGY_MALAGA_MALAGA_STATE_HPP
#define VOIKKO_MORPHOLOGY_MALAGA_MALAGA_STATE_HPP

#include "morphology/malaga/basic.hpp"
#include "morphology/malaga/pools.hpp"
#include "morphology/malaga/values.hpp"
#include "morphology/malaga/malaga_files.hpp"
#include "morphology/malaga/analysis.hpp"

namespace libvoikko { namespace morphology { namespace malaga {

enum {PATTERN_VAR_MAX = 5}; /* Maximum number of pattern variables. */

typedef struct {string_t string, pattern;} pattern_state_t;

typedef struct {
	symbol_t symbol;
	string_t name;
} symbol_and_name;

class MalagaState {
public:
	MalagaState();
	cell_t * value_heap;
	cell_t * value_heap_end; /* Pointer to first free cell in heap. */
	int_t value_heap_size; /* Size of the value heap in cells. */
	int_t value_stack_size; /* Size of the value stack. */
	int_t top; /* The index of the first unused item on VALUE_STACK. */
	value_t * value_stack; /* The value stack contains static values and local values. */

	struct { /* The run time lexicon. */
		void * lexicon_data; /* Address of lexicon file mapped into memory. */
		int_t lexicon_length; /* Length of *LEXICON_DATA. */
		int_t * trie; /* A trie with indices to FEAT_LISTS. */
		int_t trie_root; /* Index of root node in TRIE. */
		int_t * feat_lists; /* Lists of feature structures, stored in VALUES. */
		cell_t * values; /* Feature structures of lexicon entries. */
	} lexicon;

	pattern_state_t * stack; /* Stack used for backtracking. */
	int_t stack_size;
	string_t pattern_var[PATTERN_VAR_MAX]; /* Pattern variables. */

	struct { /* This is the symbol table. */
		int_t symbol_count; /* Number of symbols in this table. */
		symbol_entry_t * symbols; /* The names and atoms of all symbols. */
		symbol_t * values; /* Contains the lists of atomic symbols. */
		char_t * strings; /* Contains the symbol names. */
		symbol_and_name * symbolAndName; /* All symbols sorted by their names. */
	} symbol_table;

	analysis_t * morphologyAnalysis;
	state_t * next_result_state; /* Needed for "next_analysis_result". */
	struct { /* Information needed to generate states and tree nodes. */
		analysis_t * analysis;
		int_t rule; /* Rule just executed. */
		value_t link_feat; /* Link's feature structure. */
		int_t item_index; /* Index of item that is added. */
		string_t input; /* End of analysed input. */
	} state_info;
	rule_sys_t * morphologyRuleSystem;

};

} } }

#endif
