/* Copyright (C) 1995 Bjoern Beutel. */

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

/* Variables. ===============================================================*/

extern rule_sys_t *morphologyRuleSystem; // FIXME
/*  Read only! */

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

extern value_t next_analysis_result( void );
/* Return the feature structure of the next analysis result.
 * Return NULL if there are no more results. */

extern void add_end_state(value_t feat, const rule_t * rule);
/* Add a state, consisting of feature structure FEAT, as an end state. */

extern void add_running_state( value_t feat, int_t rule_set );
/* Add a running state, consisting of feature structure FEAT and RULE_SET. */

}}}

#endif
