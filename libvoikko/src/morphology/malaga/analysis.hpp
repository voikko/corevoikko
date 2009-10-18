/* Copyright (C) 1995 Bjoern Beutel. */

/* Description. =============================================================*/

/* This file contains data structures and functions used for grammatical 
 * analysis. */

namespace libvoikko { namespace morphology { namespace malaga {

/* Types. ===================================================================*/

typedef enum {BREAK_NODE, FINAL_NODE, UNFINAL_NODE, INTER_NODE, 
	      PRUNED_NODE} tree_node_type_t;

typedef struct /* A node of the analysis tree. */
{ 
  tree_node_type_t type;
  int_t index; /* Index of this analysis tree node's state or -1. */
  int_t parent_index; /* Index of parent analysis tree node's state or -1. */
  string_t link_surf; /* Link's surface or NULL. */
  value_t link_feat; /* Link's feature structure or NULL. */
  string_t result_surf; /* Surface of resulting state or NULL. */
  value_t result_feat; /* Feature structure of resulting state or NULL. */
} analysis_node_t;

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
