/* Copyright (C) 1995 Bjoern Beutel. */

/* Description. =============================================================*/

/* This module contains the Malaga rule interpreter. */

namespace libvoikko { namespace morphology { namespace malaga {

/* Types. ===================================================================*/

typedef struct 
/* Contains a rule system of a compiled rule file.
 * A "..._count" or "..._size" variable contains the number of elements
 * in the following table. */
{ 
  int_t initial_rule_set; /* Rules index of the initial rule set. */
  int_t initial_feat; /* Values index of initial feature structure. */

  int_t robust_rule; /* Number of robust_rule or -1. */
  int_t pruning_rule; /* Number of pruning_rule or -1. */
  int_t allo_rule; /* Number of allo_rule or -1. */
  int_t input_filter; /* Number of input filter rule or -1. */
  int_t output_filter; /* Number of output filter rule or -1. */

  rule_t *rules; /* Name and code of every rule. */
  int_t rule_count;

  int_t *rule_sets; /* A collection of lists. Each list is a series of rules,
                     * followed by -1. A list may be subdivided into sublists,
                     * which are separated by -2. The rules of a sublist are
                     * executed if none of the rules of the preceding sublist
                     * has been successful. */
  int_t rule_sets_size;

  instr_t *instrs; /* The actual rule instructions. */
  int_t instr_count;

  cell_t *values; /* All constant Malaga values. */
  int_t values_size;

  char_t *strings; /* Names of files, variables, rules, patterns. */
  int_t strings_size;
  
  src_line_t *src_lines; /* Correspondence between source lines
                          * and rule instructions. */
  int_t src_line_count;

  var_t *vars; /* Variable names. */
  int_t var_count;

  var_scope_t *var_scopes; /* Variable scopes. */
  int_t var_scope_count;

  constant_t *constants; /* Named constants. */
  int_t constant_count;
} rule_sys_t;

/* Variables. ===============================================================*/

extern bool rule_successful; // FIXME
/* Indicator for execution of result, accept, or allo statement. Read only! */

/* Functions. ===============================================================*/

extern void execute_rule( rule_sys_t *rule_sys, int_t rule_number );
/* Execute rule RULE_NUMBER in the rule system RULE_SYS.
 * Any parameters must be on the value stack. */

extern rule_sys_t *read_rule_sys( string_t file_name );
/* Read rule system from file FILE_NAME.
 * A symbol file must have already been loaded. */

extern void free_rule_sys( rule_sys_t **rule_sys );
/* Free all memory used by *RULE_SYS. */

}}}
