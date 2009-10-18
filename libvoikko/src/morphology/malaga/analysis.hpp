/* Copyright (C) 1995 Bjoern Beutel. */

/* Description. =============================================================*/

/* This file contains data structures and functions used for grammatical 
 * analysis. */

namespace libvoikko { namespace morphology { namespace malaga {

/* Variables. ===============================================================*/

extern rule_sys_t *morphologyRuleSystem; // FIXME
/*  Read only! */

/* Functions. ===============================================================*/

extern void init_analysis( string_t morphology_file );
/* Initialise the analysis module.
 * MORPHOLOGY_FILE is the rule files to load. */

extern void terminate_analysis( void );
/* Free analysis module. */

extern void preprocess_input(char_t *input);
/* Delete heading and trailing spaces in INPUT
 * and compress all whitespace sequences to a single space. */

extern void analyse( string_t input );
/* Perform a LAG analysis of INPUT. */

extern value_t first_analysis_result( void );
/* Return the feature structure of the first analysis result.
 * Return NULL if there are no results. */

extern value_t next_analysis_result( void );
/* Return the feature structure of the next analysis result.
 * Return NULL if there are no more results. */

extern void add_end_state( value_t feat );
/* Add a state, consisting of feature structure FEAT, as an end state. */

extern void add_running_state( value_t feat, int_t rule_set );
/* Add a running state, consisting of feature structure FEAT and RULE_SET. */

}}}
