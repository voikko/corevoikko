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
  string_t rule_name; /* Name of the rule that created result or NULL. */
  string_t result_surf; /* Surface of resulting state or NULL. */
  value_t result_feat; /* Feature structure of resulting state or NULL. */
  string_t rule_set; /* Successor rules of resulting state or NULL. */
} analysis_node_t;

typedef enum 
{ROBUST_RULE_OPTION, 
 MOR_OUT_FILTER_OPTION, SYN_IN_FILTER_OPTION, SYN_OUT_FILTER_OPTION, 
 MOR_INCOMPLETE_OPTION, SYN_INCOMPLETE_OPTION,
 ANALYSIS_OPTION_COUNT} analysis_option_t;

typedef enum {STATE_SURFACE, LINK_SURFACE, RESULT_SURFACE} surface_t;
/* Kinds of surfaces that can be obtained by "get_rule_info" in "rules.h" */

/* Variables. ===============================================================*/

extern rule_sys_t *morphologyRuleSystem; // FIXME
/*  Read only! */

extern int_t state_count; /* Counts number of generated states. Read only! FIXME */

extern string_t last_analysis_input; // FIXME
/* Start of top level input string. Read only! */

/* Functions. ===============================================================*/

extern void init_analysis( string_t morphology_file );
/* Initialise the analysis module.
 * MORPHOLOGY_FILE is the rule files to load. */

extern void terminate_analysis( void );
/* Free analysis module. */

extern void preprocess_input(char_t *input);
/* Delete heading and trailing spaces in INPUT
 * and compress all whitespace sequences to a single space. */

extern bool get_analysis_option( analysis_option_t selected );
/* Return the current setting of analysis option SELECTED. */

extern void analyse( string_t input );
/* Perform a LAG analysis of INPUT. */

extern bool analysis_has_results( void );
/* Return true iff the last analysis has created results. */

extern value_t first_analysis_result( void );
/* Return the feature structure of the first analysis result.
 * Return NULL if there are no results. */

extern value_t next_analysis_result( void );
/* Return the feature structure of the next analysis result.
 * Return NULL if there are no more results. */

extern bool analysis_has_nodes( void );
/* Return true iff the last analysis has created tree nodes. */

extern analysis_node_t *get_first_analysis_node( void );
/* Return the first analysis tree node of the last analysis.
 * Return NULL if there is no node. 
 * The node must be freed with "free_analysis_node" after use. */

extern analysis_node_t *get_next_analysis_node( void );
/* Return the next analysis tree node of the last analysis.
 * Return NULL if there is no more node. 
 * The node must be freed with "free_analysis_node" after use. */

extern void free_analysis_node( analysis_node_t **node );
/* Free the memory occupied by NODE. */

}}}
