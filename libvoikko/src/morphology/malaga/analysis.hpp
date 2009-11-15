/* Copyright (C) 1995 Bjoern Beutel.
 *               2009 Harri Pitkänen <hatapitk@iki.fi>
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

/* Description. =============================================================*/

/* This file contains data structures and functions used for grammatical 
 * analysis. */

#ifndef LIBVOIKKO_MORPHOLOGY_MALAGA_ANALYSIS_HPP
#define LIBVOIKKO_MORPHOLOGY_MALAGA_ANALYSIS_HPP

#include "morphology/malaga/rules.hpp"

namespace libvoikko { namespace morphology { namespace malaga {

class MalagaState;

typedef struct /* The structure for morphological and syntactical analysis. */
{ 
  pool_t state_pool; /* All states are saved in STATE_POOL. */
  pool_t value_pool; /* All feature structures are saved in VALUE_POOL. */
  list_t running_states; /* States that need further analysis
			  * (in the order of their INPUT indexes). */
  list_t end_states; /* End states */
  list_t free_states; /* States that can be reused. */
} analysis_t;

typedef struct tree_node /* A rule application is stored in "tree_node". */
{ 
  int_t rule; /* Number of the executed rule. */
  value_t link_feat; /* Feature structure of the link. */
  int_t rule_set; /* Successor rules of resulting state (-1 for end state). */
  string_t input; /* The input that is not yet analysed. */
} tree_node_t;

typedef struct /* A state in morphological or syntactical analysis. */
{ 
  list_node_t *next;
  value_t feat; /* Feature structure of input read in so far. */
  string_t input; /* Pointer to input that is analysed next. */
  int_t rule_set; /* Set of rules to be applied. */
  tree_node_t *tree_node; /* Tree node of rule application that created
                           * this state (NULL if no tree). */
  int_t item_index; /* Number of items read in so far. */
} state_t;

/* Functions. ===============================================================*/

extern void init_analysis(string_t morphology_file, MalagaState * malagaState);
/* Initialise the analysis module.
 * MORPHOLOGY_FILE is the rule files to load. */

extern void terminate_analysis(MalagaState * malagaState);
/* Free analysis module. */

extern void preprocess_input(char_t *input);
/* Delete heading and trailing spaces in INPUT
 * and compress all whitespace sequences to a single space. */

extern void analyse(string_t input, MalagaState * malagaState);
/* Perform a LAG analysis of INPUT. */

extern value_t first_analysis_result(MalagaState * malagaState);
/* Return the feature structure of the first analysis result.
 * Return NULL if there are no results. */

extern value_t next_analysis_result(MalagaState * malagaState);
/* Return the feature structure of the next analysis result.
 * Return NULL if there are no more results. */

extern void add_end_state(value_t feat, const rule_t * rule, MalagaState * malagaState);
/* Add a state, consisting of feature structure FEAT, as an end state. */

extern void add_running_state(value_t feat, int_t rule_set, MalagaState * malagaState);
/* Add a running state, consisting of feature structure FEAT and RULE_SET. */

}}}

#endif
