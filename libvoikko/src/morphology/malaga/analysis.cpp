/* Copyright (C) 1995 Bjoern Beutel. */

/* Description. =============================================================*/

/* This file contains data structures and functions used for grammatical 
 * analysis. */

/* Includes. ================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "morphology/malaga/basic.hpp"
#include "morphology/malaga/pools.hpp"
#include "morphology/malaga/values.hpp"
#include "morphology/malaga/rule_type.hpp"
#include "morphology/malaga/rules.hpp"
#include "morphology/malaga/lexicon.hpp"
#include "morphology/malaga/analysis.hpp"
#include "morphology/malaga/MalagaState.hpp"

namespace libvoikko { namespace morphology { namespace malaga {

/* Global variables. ========================================================*/

rule_sys_t *morphologyRuleSystem;

/* Variables. ===============================================================*/

static const int_t mor_pruning_min = 30;

/* Functions for segmentation and preprocessing. ============================*/

void 
preprocess_input( char_t *input )
/* Change input string to use lower case letters only */
{ 
  string_t input_p;
  char_t *output_p;
  u_int_t code;
  
  input_p = input;
  output_p = input;

  while (*input_p != EOS) 
  { 
    code = g_utf8_get_char( input_p );
    input_p = g_utf8_next_char( input_p );
    output_p += g_unichar_to_utf8( g_unichar_tolower(code), output_p );
  }

  *output_p = EOS;
}

/*---------------------------------------------------------------------------*/

static bool 
word_may_end_here( string_t string, const rule_t *rule )
/* Return whether there may be a word boundary between STRING-1 and STRING. */
{ 
  if (rule->type == END_RULE && rule->param_count == 2) 
    return true;
  return (*string == EOS || *string == ' ');
}

/* Functions for state list processing. =====================================*/

static void
insert_state( analysis_t *analysis,
	      list_t *state_list,
	      value_t feat,
	      string_t input,
	      int_t rule_set,
	      int_t item_index )
/* Insert a state, composed of FEAT, INPUT, RULE_SET, and ITEM_INDEX in the
 * list STATE_LIST, in front of all states with a higher INPUT index. */
{ 
  state_t *state, *prev_state, *next_state;

  state = (state_t *) remove_first_node( &analysis->free_states );
  if (state == NULL) 
    state = (state_t *) get_pool_space( analysis->state_pool, 1, NULL );

  /* Set values. */
  state->feat = feat;
  state->input = input;
  state->rule_set = rule_set;
  state->item_index = item_index;
  state->tree_node = NULL;

  /* Find position where to insert. */
  FOREACH( prev_state, *state_list, state_t ) 
  { 
    next_state = (state_t *) prev_state->next;
    if (next_state == NULL || next_state->input > input) 
      break;
  }

  insert_node( state_list, (list_node_t *) state, (list_node_t *) prev_state );
}

/*---------------------------------------------------------------------------*/

static void 
add_state( list_t *list, string_t input, value_t feat, int_t rule_set, MalagaState * malagaState)
/* Add state, consisting of INPUT, FEAT and RULE_SET, to LIST.
 * When STATE_INFO.CREATE_TREE == true, also generate a tree node. */
{ 
  /* Preserve the feature structure. */
  value_t new_feat = copy_value_to_pool(malagaState->state_info.analysis->value_pool, feat, NULL);

  /* Create a new state. */
  insert_state(malagaState->state_info.analysis, list, new_feat, input,
                        rule_set, malagaState->state_info.item_index);
}

/*---------------------------------------------------------------------------*/

void 
add_end_state(value_t feat, const rule_t * rule, MalagaState * malagaState)
/* Add a state, consisting of FEAT, as an end state. */
{ 
  /* Combi-rules and end-rules must check for word boundary. */
  if ((rule->type != COMBI_RULE && rule->type != END_RULE)
      || word_may_end_here(malagaState->state_info.input, rule ))
  { 
    add_state(&(malagaState->state_info.analysis->end_states),
	       malagaState->state_info.input, feat, -1, malagaState);
  }
}

/*---------------------------------------------------------------------------*/

void 
add_running_state(value_t feat, int_t rule_set, MalagaState * malagaState)
/* Add a running state, consisting of FEAT and RULE_SET. */
{ 
  add_state(&(malagaState->state_info.analysis->running_states), 
	    malagaState->state_info.input, feat, rule_set, malagaState);
}

/* Analysis functions. ======================================================*/

static analysis_t *
new_analysis( void )
/* Create a new analysis structure. */
{ 
  analysis_t *analysis;

  analysis = (analysis_t *) new_mem( sizeof( analysis_t ) );
  analysis->state_pool = new_pool( sizeof( state_t ) );
  analysis->value_pool = new_pool( sizeof( cell_t ) );
  clear_list( &analysis->running_states );
  clear_list( &analysis->end_states );
  clear_list( &analysis->free_states );
  return analysis;
}

/*---------------------------------------------------------------------------*/

static void 
free_analysis( analysis_t **analysis )
/* Destroy an analysis structure. */
{ 
  if (*analysis != NULL) 
  { 
    free_pool( &(*analysis)->state_pool );
    free_pool( &(*analysis)->value_pool );
    free_mem( analysis );
  }
}

/*---------------------------------------------------------------------------*/

void 
init_analysis(string_t morphology_file, MalagaState * malagaState)
/* Initialise the analysis module.
 * MORPHOLOGY_FILE is the rule files to load. */
{ 
  /* Read rule files. */
  morphologyRuleSystem = read_rule_sys( morphology_file );

  /* Init analysis structure. */
  malagaState->morphologyAnalysis = new_analysis();
}

/*---------------------------------------------------------------------------*/

void 
terminate_analysis(MalagaState * malagaState)
/* Terminate the analysis module. */
{ 
  free_rule_sys( &morphologyRuleSystem );
  free_analysis(&(malagaState->morphologyAnalysis));
}

/*---------------------------------------------------------------------------*/

value_t
first_analysis_result(MalagaState * malagaState)
/* Return the feature structure of the first analysis result.
 * Return NULL if there are no results. */
{ 
  malagaState->next_result_state = (state_t *) malagaState->morphologyAnalysis->end_states.first;
  return next_analysis_result(malagaState);
}

/*---------------------------------------------------------------------------*/

value_t 
next_analysis_result(MalagaState * malagaState)
/* Return the feature structure of the next analysis result.
 * Return NULL if there are no more results. */
{ 
  if (malagaState->next_result_state == NULL) 
    return NULL;
  value_t result = malagaState->next_result_state->feat;
  malagaState->next_result_state = (state_t *) malagaState->next_result_state->next;
  return result;
}

/*---------------------------------------------------------------------------*/

static void 
execute_filter_rule( analysis_t *analysis, 
                     rule_sys_t *rule_sys,
                     int_t filter_rule,
                     MalagaState * malagaState)
/* Execute FILTER_RULE in RULE_SYS for ANALYSIS. */
{
  list_t old_end_states;
  state_t *state;
  string_t input;

  /* Go through all results with the same length. */
  old_end_states = analysis->end_states;
  clear_list( &analysis->end_states );
  while (old_end_states.first != NULL) 
  { 
    state = (state_t *) old_end_states.first;
    input = state->input;

    /* Create a list with the results of all states and remove states. */
    malagaState->top = 0;
    while (old_end_states.first != NULL 
	   && ((state_t *) old_end_states.first)->input == input) 
    { 
      state = (state_t *) remove_first_node( &old_end_states );
      add_node( &analysis->free_states, (list_node_t *) state, LIST_END );
      push_value(state->feat, malagaState);
    }
    build_list(malagaState->top, malagaState);

    /* Execute filter rule. */
    malagaState->state_info.analysis = analysis;
    malagaState->state_info.item_index = 0;
    malagaState->state_info.input = input;
    execute_rule(rule_sys, filter_rule, malagaState);
  }
}

/*---------------------------------------------------------------------------*/

static void 
execute_pruning_rule(analysis_t *analysis, MalagaState * malagaState)
/* Execute pruning_rule in GRAMMAR for the running states in ANALYSIS. */
{ 
  int_t result_count, i;
  state_t *state, *next_state;
  string_t input;
  rule_sys_t *rule_sys;
  value_t list;
  symbol_t symbol;

  state = (state_t *) analysis->running_states.first;
  input = state->input;

  /* Create a list that contains the results. */
  malagaState->top = 0;
  result_count = 0;
  FOREACH( state, analysis->running_states, state_t ) 
  { 
    if (state->input != input) 
      break;
    result_count++;
    push_value(state->feat, malagaState);
  }
  /* Don't execute if number of states is too low. */
  if (result_count < mor_pruning_min)
    return;
  build_list(result_count, malagaState);

  rule_sys = morphologyRuleSystem;
  execute_rule(rule_sys, rule_sys->pruning_rule, malagaState); /* Execute pruning rule. */

  /* Interprete the result. */
  list = malagaState->value_stack[ malagaState->top - 1 ];
  state = (state_t *) analysis->running_states.first;
  for (i = 0; i < result_count; i++) 
  { 
    next_state = (state_t *) state->next;
    symbol = value_to_symbol( get_element( list, i + 1 ) );
    if (symbol == NO_SYMBOL) 
    { 
      remove_node( &analysis->running_states, (list_node_t *) state );
      add_node( &analysis->free_states, (list_node_t *) state, LIST_END );
    } 
    state = next_state;
  }
}

/*---------------------------------------------------------------------------*/

static void 
execute_rules( analysis_t *analysis, 
               rule_sys_t *rule_sys, 
               state_t *state, 
               value_t link_feat,
               string_t link_surf,
               string_t link_surf_end,
               rule_type_t rule_type,
               MalagaState * malagaState)
/* Execute the successor rules of RULE_TYPE in RULE_SYS for STATE in ANALYSIS.
 * Consume the segment from LINK_SURF to LINK_SURF_END with feature structure
 * LINK_FEAT. */
{ 
  int_t *rule_p;
  bool rules_successful, rules_executed;
  rule_t *rule;

  /* Setup STATE_INFO. */
  malagaState->state_info.analysis = analysis;
  malagaState->state_info.link_feat = link_feat;
  malagaState->state_info.item_index = state->item_index + 1;
  malagaState->state_info.input = link_surf_end;

  /* Execute rules in rule set. */
  rules_executed = rules_successful = false;
  for (rule_p = rule_sys->rule_sets + state->rule_set; *rule_p != -1; rule_p++)
  { 
    if (*rule_p == -2) 
    { 
      if (rule_type == END_RULE || rules_successful) 
	break;
    } 
    else 
    { 
      rule = rule_sys->rules + *rule_p;
      if (rule->type == rule_type
	  && (rule->type == COMBI_RULE 
	      || word_may_end_here( link_surf, rule )))
      { 
	malagaState->state_info.rule = *rule_p;
	malagaState->top = 0;
	push_value(state->feat, malagaState);
	if (rule->type == COMBI_RULE) 
	{ 
	  push_value(link_feat, malagaState);
	  if (rule->param_count >= 3) 
	    push_string_value(link_surf, link_surf_end, malagaState);
	  if (rule->param_count >= 4) 
	    push_number_value(malagaState->state_info.item_index, malagaState);
	} 
	else /* rule->type == END_RULE */
	{ 
	  if (rule->param_count >= 2) 
	    push_string_value(link_surf, NULL, malagaState);
	}
	rules_successful |= execute_rule(rule_sys, *rule_p, malagaState);
	rules_executed = true;
      }
    }
  }
}

/*---------------------------------------------------------------------------*/

static void
check_end_states( analysis_t *analysis )
{ 
  state_t *state;

  while (true) 
  { 
    state = (state_t *) analysis->end_states.first;
    if (state == NULL || *state->input == EOS) 
      break;
    remove_first_node( &analysis->end_states );
    add_node( &analysis->free_states, (list_node_t *) state, LIST_END );
  }
}

/*---------------------------------------------------------------------------*/

void 
analyse(string_t input, MalagaState * malagaState)
/* Perform a LAG analysis of INPUT using.
 * An analysis tree will be built if CREATE_TREE == true. */
{ 
  state_t *state;
  string_t current_input;
  value_t link_feat;
  string_t link_surf_end; /* End of the link's surface. */
  analysis_t *analysis = malagaState->morphologyAnalysis;

  rule_sys_t * rule_sys = morphologyRuleSystem;

  /* Reset the analysis data structures */
  clear_list( &analysis->running_states );
  clear_list( &analysis->end_states );
  clear_list( &analysis->free_states );
  clear_pool( analysis->state_pool );
  clear_pool( analysis->value_pool );

  /* Enter the initial state. */
  insert_state( analysis, &analysis->running_states,
                                rule_sys->values + rule_sys->initial_feat,
                                input, rule_sys->initial_rule_set, 0 );


  /* Analyse while there are running states. */
  while (analysis->running_states.first != NULL) 
  { 
    state = (state_t *) analysis->running_states.first;
    current_input = state->input;
    if ((mor_pruning_min > 0) && current_input > input && rule_sys->pruning_rule != -1) 
    {
      execute_pruning_rule(analysis, malagaState);
    }
    if (current_input > input) /* Apply end_rules if any input was parsed. */
    {
      /* Apply all end_rules to states at CURRENT_INPUT. */
      FOREACH( state, analysis->running_states, state_t ) 
      {
	if (state->input != current_input) 
	  break;
        execute_rules( analysis, rule_sys, state, NULL, current_input,
                       current_input, END_RULE, malagaState);
      }
    }
    if (*current_input == EOS) 
      break; /* If analysis ate all input, leave. */

    /* Look for prefixes of increasing length
     * that match the string at CURRENT_INPUT. */
    trie_search_state searchState;
    search_for_prefix(current_input, searchState, malagaState);
    while (get_next_prefix(&link_surf_end, &link_feat, searchState, malagaState)) 
    { 
      /* Combine that link with all morphological states. */
      FOREACH( state, analysis->running_states, state_t ) 
      { 
        if (state->input != current_input) 
          break;
        execute_rules(analysis, rule_sys, state, link_feat, current_input,
                      link_surf_end, COMBI_RULE, malagaState);
      }
    }

    /* We have combined all analyses at CURRENT_INPUT with all states
     * that were at CURRENT_INPUT, so we can kill these states. */
    while (true) 
    { 
      state = (state_t *) analysis->running_states.first;
      if (state == NULL || state->input != current_input) 
	break;
      remove_first_node( &analysis->running_states );
      add_node( &analysis->free_states, (list_node_t *) state, LIST_END );
    }
  } /* End of loop that consumes all running states. */

  check_end_states(analysis);

  execute_filter_rule(analysis, morphologyRuleSystem,
                      morphologyRuleSystem->output_filter, malagaState);
}

}}}
