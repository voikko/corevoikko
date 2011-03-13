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

/* This module contains the Malaga rule interpreter. */

#ifndef LIBVOIKKO_MORPHOLOGY_MALAGA_RULES_HPP
#define LIBVOIKKO_MORPHOLOGY_MALAGA_RULES_HPP

#include "morphology/malaga/rule_type.hpp"

namespace libvoikko { namespace morphology { namespace malaga {

class MalagaState;

/* Types. ===================================================================*/

typedef struct 
/* Contains a rule system of a compiled rule file.
 * A "..._count" or "..._size" variable contains the number of elements
 * in the following table. */
{ 
  int_t initial_rule_set; /* Rules index of the initial rule set. */
  int_t initial_feat; /* Values index of initial feature structure. */

  int_t pruning_rule; /* Number of pruning_rule or -1. */
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
} rule_sys_t;

/* Variables. ===============================================================*/

/* Functions. ===============================================================*/

extern bool execute_rule(rule_sys_t *rule_sys, int_t rule_number, MalagaState * malagaState);
/* Execute rule RULE_NUMBER in the rule system RULE_SYS.
 * Any parameters must be on the value stack.
 * Returns true if executed rule was successful */

extern rule_sys_t *read_rule_sys( string_t file_name );
/* Read rule system from file FILE_NAME.
 * A symbol file must have already been loaded. */

extern void free_rule_sys( rule_sys_t **rule_sys );
/* Free all memory used by *RULE_SYS. */

}}}

#endif
