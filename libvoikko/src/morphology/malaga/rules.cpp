/* Copyright (C) 1995 Bjoern Beutel.
 *               2010 Harri Pitkänen <hatapitk@iki.fi>
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

/* Includes. ================================================================*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include "setup/DictionaryException.hpp"
#include "morphology/malaga/basic.hpp"
#include "morphology/malaga/pools.hpp"
#include "morphology/malaga/values.hpp"
#include "morphology/malaga/symbols.hpp"
#include "morphology/malaga/patterns.hpp"
#include "morphology/malaga/files.hpp"
#include "morphology/malaga/malaga_files.hpp"
#include "morphology/malaga/rule_type.hpp"
#include "morphology/malaga/rules.hpp"
#include "morphology/malaga/analysis.hpp"
#include "morphology/malaga/MalagaState.hpp"
#include "utf8/utf8.hpp"

namespace libvoikko { namespace morphology { namespace malaga {

/* Types. ===================================================================*/

typedef struct 
{ 
  int_t pc;
  int_t nested_subrules;
  int_t base; 
  int_t bottom;
} path_node_t;

/* Rule execution. ==========================================================*/

static void
standard_function(int_t function, MalagaState * malagaState)
/* Stack effect: VALUE -> NEW_VALUE.
 * Perform function FUNCTION on VALUE yielding NEW_VALUE. */
{ 

  switch (function) 
  {
  case FUNC_GET_LENGTH:
    if (get_value_type( malagaState->value_stack[ malagaState->top - 1 ] ) == STRING_SYMBOL)
    {
      const char * stringVal = value_to_string(malagaState->value_stack[--(malagaState->top)]);
      push_number_value(utf8::unchecked::distance(stringVal, stringVal + strlen(stringVal)),
                        malagaState);
    }
    else 
      push_number_value(get_list_length(malagaState->value_stack[--(malagaState->top)]), malagaState);
    break;
  case FUNC_SUBSTRING:
    string_t string = value_to_string(malagaState->value_stack[malagaState->top - 3]);
    int_t start = value_to_int(malagaState->value_stack[malagaState->top - 2]);
    int_t end;
    if (malagaState->value_stack[malagaState->top - 1] == NULL) 
      end = start;
    else 
      end = value_to_int(malagaState->value_stack[malagaState->top - 1]);
    // Support for negative "start" or "end" has been removed
    if (end < start) {
      push_string_value("", NULL, malagaState);
    }
    else {
      const char * startPos = string;
      utf8::unchecked::advance(startPos, start - 1);
      const char * endPos = startPos;
      utf8::unchecked::advance(endPos, end - (start - 1));
      push_string_value(startPos, endPos, malagaState);
    }
    malagaState->value_stack[malagaState->top - 4] = malagaState->value_stack[malagaState->top - 1];
    malagaState->top -= 3;
    break;
  }
}

/*---------------------------------------------------------------------------*/

bool 
execute_rule(rule_sys_t *rule_sys, int_t rule_number, MalagaState * malagaState)
/* Execute rule RULE_NUMBER in the rule system RULE_SYS.
 * Any parameters must be on the value stack. */
{ 
  static symbol_t nil = NIL_SYMBOL; /* The "nil" symbol. FIXME */
  path_node_t *path;
          
  /* Initialise the value stack. */
  malagaState->top = rule_sys->rules[ rule_number ].param_count;
  int_t base = 0;
  int_t bottom = 0;

  /* Reset nesting and alternative paths. */
  while (malagaState->path_list.first != NULL) 
    free_first_node(&(malagaState->path_list));

  int_t pc = rule_sys->rules[ rule_number ].first_instr;
  { 
    bool rule_successful = false;
    bool terminate = false;
    int_t nested_subrules = 0;
    int_t path_count = 0;
    while (! terminate) 
    { 
      instr_t instruction = rule_sys->instrs[ pc ];
      int_t new_pc = pc + 1;
      int_t info = INSTR_INFO( instruction );
      switch (OPCODE( instruction ) ) 
      {
      case INS_TERMINATE:
        terminate = true;
        break;
      case INS_NOP:
        break;
      case INS_TERMINATE_IF_NULL:
        if (malagaState->value_stack[--(malagaState->top)] == NULL) 
	  terminate = true;
        break;
      case INS_ADD_END_STATE:
        add_end_state(malagaState->value_stack[--(malagaState->top)], rule_sys->rules + rule_number, malagaState);
        rule_successful = true;
        break;
      case INS_ADD_STATE:
        add_running_state(malagaState->value_stack[--(malagaState->top)], info, malagaState);
        rule_successful = true;
        break;
      case INS_ACCEPT:
        path_count = 0;
        while (malagaState->path_list.first != NULL) 
	  free_first_node(&(malagaState->path_list));
        terminate = true;
        break;
      case INS_PUSH_NULL:
        for (int_t i = 0; i < info; i++) 
	  push_value(NULL, malagaState);
        break;
      case INS_PUSH_VAR:
        push_value(malagaState->value_stack[ base + info ], malagaState);
        break;
      case INS_PUSH_CONST:
        push_value(rule_sys->values + info, malagaState);
        break;
      case INS_PUSH_SYMBOL:
        push_symbol_value(info, malagaState);
        break;
      case INS_PUSH_PATTERN_VAR:
        push_string_value(malagaState->pattern_var[info], NULL, malagaState);
        break;
      case INS_POP:
        malagaState->top -= info;
        break;
      case INS_POP_TO:
        malagaState->top = base + info;
        break;
      case INS_BUILD_LIST:
        build_list(info, malagaState);
        break;
      case INS_BUILD_RECORD:
        build_record(info, malagaState);
        break;
      case INS_BUILD_PATH:
        build_path(info, malagaState);
        break;
      case INS_DOT_OPERATION:
        dot_operation(malagaState);
        if (malagaState->value_stack[malagaState->top - 1] == NULL) 
	  malagaState->value_stack[malagaState->top - 1] = &nil;
        break;
      case INS_PLUS_OPERATION:
        plus_operation(malagaState);
        break;
      case INS_MINUS_OPERATION:
        minus_operation(malagaState);
        break;
      case INS_GET_ATTRIBUTE:
        malagaState->value_stack[malagaState->top - 1] = get_attribute(malagaState->value_stack[malagaState->top - 1], 
						(symbol_t) info );
        if (malagaState->value_stack[malagaState->top - 1 ] == NULL) 
	  malagaState->value_stack[malagaState->top - 1 ] = &nil;
        break;
      case INS_REMOVE_ATTRIBUTE:
        remove_attribute((symbol_t) info, malagaState);
        break;
      case INS_STD_FUNCTION:
        standard_function(info, malagaState);
        break;
      case INS_MATCH:
        if (match_pattern(value_to_string(malagaState->value_stack[--(malagaState->top)]),
                           rule_sys->strings + info, malagaState)) {
          push_symbol_value(YES_SYMBOL, malagaState);
        }
        else {
          push_symbol_value(NO_SYMBOL, malagaState);
        }
        break;
      case INS_SET_VAR:
        malagaState->value_stack[ base + info ] = malagaState->value_stack[--(malagaState->top)];
        break;
      case INS_PLUS_VAR:
        insert_value(1, malagaState->value_stack[ base + info ], malagaState);
        plus_operation(malagaState);
        malagaState->value_stack[ base + info ] = malagaState->value_stack[--(malagaState->top)];
        break;
      case INS_MINUS_VAR:
        insert_value(1, malagaState->value_stack[ base + info ], malagaState);
        minus_operation(malagaState);
        malagaState->value_stack[ base + info ] = malagaState->value_stack[--(malagaState->top)];
        break;
      case INS_SET_VAR_PATH:
        insert_value(2, malagaState->value_stack[ base + info ], malagaState);
        modify_value_part(right_value, malagaState);
        malagaState->value_stack[ base + info ] = malagaState->value_stack[--(malagaState->top)];
        break;
      case INS_PLUS_VAR_PATH:
        insert_value(2, malagaState->value_stack[ base + info ], malagaState);
        modify_value_part(plus_operation, malagaState);
        malagaState->value_stack[ base + info ] = malagaState->value_stack[--(malagaState->top)];
        break;
      case INS_MINUS_VAR_PATH:
        insert_value(2, malagaState->value_stack[ base + info ], malagaState);
        modify_value_part(minus_operation, malagaState);
        malagaState->value_stack[ base + info ] = malagaState->value_stack[--(malagaState->top)];
        break;
      case INS_GET_1ST_ELEMENT:
        get_first_element(malagaState);
        break;
      case INS_ITERATE:
        get_next_element(base + info, malagaState);
        break;
      case INS_JUMP:
        new_pc = info;
        break;
      case INS_JUMP_IF_EQUAL:
	if (values_equal(malagaState->value_stack[malagaState->top - 2], malagaState->value_stack[malagaState->top - 1]))
	  new_pc = info;
        malagaState->top -= 2;
        break;
      case INS_JUMP_IF_NOT_EQUAL:
        if (! values_equal(malagaState->value_stack[malagaState->top - 2], malagaState->value_stack[malagaState->top - 1] )) 
	  new_pc = info;
        malagaState->top -= 2;
        break;
      case INS_JUMP_IF_IN:
        if (value_in_value(malagaState->value_stack[malagaState->top - 2], malagaState->value_stack[malagaState->top - 1] )) 
	  new_pc = info;
        malagaState->top -= 2;
        break;
      case INS_JUMP_IF_NOT_IN:
        if (! value_in_value(malagaState->value_stack[malagaState->top - 2], malagaState->value_stack[malagaState->top - 1] )) 
	  new_pc = info;
        malagaState->top -= 2;
        break;
      case INS_JUMP_IF_LESS:
        if (value_to_double(malagaState->value_stack[malagaState->top - 2] ) 
            < value_to_double(malagaState->value_stack[malagaState->top - 1] )) 
	{ 
	  new_pc = info; 
	}
        malagaState->top -= 2;
        break;
      case INS_JUMP_IF_NOT_LESS:
        if (! (value_to_double(malagaState->value_stack[malagaState->top - 2] )
               < value_to_double(malagaState->value_stack[malagaState->top - 1] ))) 
	{ 
	  new_pc = info; 
	}
        malagaState->top -= 2;
        break;
      case INS_JUMP_IF_GREATER:
        if (value_to_double(malagaState->value_stack[malagaState->top - 2] )
            > value_to_double(malagaState->value_stack[malagaState->top - 1] )) 
	{ 
	  new_pc = info; 
	}
        malagaState->top -= 2;
        break;
      case INS_JUMP_IF_NOT_GREATER:
        if (! (value_to_double(malagaState->value_stack[malagaState->top - 2] )
               > value_to_double(malagaState->value_stack[malagaState->top - 1] ))) 
	{ 
	  new_pc = info; 
	}
        malagaState->top -= 2;
        break;
      case INS_JUMP_IF_NULL:
        if (malagaState->value_stack[--(malagaState->top)] == NULL) 
	  new_pc = info;
        break;
      case INS_JUMP_IF_NOT_NULL:
        if (malagaState->value_stack[--(malagaState->top)] != NULL) 
	  new_pc = info;
        break;
      case INS_JUMP_IF_YES:
        {
        symbol_t symbol = value_to_symbol(malagaState->value_stack[--(malagaState->top)] );
        if (symbol == YES_SYMBOL) 
	  new_pc = info;
	}
        break;
      case INS_JUMP_IF_NO:
        {
        symbol_t symbol = value_to_symbol(malagaState->value_stack[--(malagaState->top)] );
        if (symbol == NO_SYMBOL) 
	  new_pc = info;
	}
        break;
      case INS_JUMP_NOW:
        {
        int_t old_top = malagaState->top;
        path = (path_node_t *) new_node(&(malagaState->path_list), sizeof( path_node_t ), LIST_START );
        path->pc = new_pc;
        path->nested_subrules = nested_subrules;
        path->base = base;
        path->bottom = bottom;
        while (bottom < old_top)
	  push_value(malagaState->value_stack[ bottom++ ], malagaState);
        base += (malagaState->top - old_top);
        path_count++;
        new_pc = info;
	}
        break;
      case INS_JUMP_LATER:
        {
        int_t old_top = malagaState->top;
        path = (path_node_t *) new_node(&(malagaState->path_list), sizeof( path_node_t ), LIST_START );
        path->pc = info;
        path->nested_subrules = nested_subrules;
        path->base = base;
        path->bottom = bottom;
        while (bottom < old_top) 
	  push_value(malagaState->value_stack[ bottom++ ], malagaState);
        base += (malagaState->top - old_top);
        path_count++;
	}
        break;
      case INS_JUMP_SUBRULE:
        push_number_value(base - bottom, malagaState);
        push_number_value(new_pc, malagaState);
        base = malagaState->top;
        new_pc = rule_sys->rules[ info ].first_instr;
        nested_subrules++;
        break;
      case INS_RETURN:
        {
        int_t old_base = bottom + value_to_int(malagaState->value_stack[ base - 2 ] );
        new_pc = value_to_int(malagaState->value_stack[ base - 1 ] );
        malagaState->value_stack[ base - info - 2 ] = malagaState->value_stack[malagaState->top - 1]; /* Result. */
        malagaState->top = base - (info + 1);
        base = old_base;
        nested_subrules--;
        }
        break;
      default:
        throw setup::DictionaryException("Unknown instruction type in malaga rule.");
      }
      if (! terminate) 
	pc = new_pc;
      else if (malagaState->path_list.first != NULL) 
      { 
	/* Load a previously saved rule-internal path and continue. */
        path_count--;
        path = (path_node_t *) malagaState->path_list.first;
        malagaState->top = bottom;
        base = path->base;
        bottom = path->bottom;
        pc = path->pc;
        nested_subrules = path->nested_subrules;
        free_first_node(&(malagaState->path_list));
        terminate = false;
      }
    }
    return rule_successful;
  }
}

/*---------------------------------------------------------------------------*/

rule_sys_t *
read_rule_sys( string_t file_name )
/* Read rule system from file FILE_NAME.
 * A symbol file must have already been loaded. */
{ 
  FILE *stream;
  rule_header_t header;
  rule_sys_t *rule_sys;

  stream = open_stream( file_name, "rb" );
  read_vector(&header, sizeof( header ), 1, stream);
  check_header( &header.common_header, 
                RULE_FILE, MIN_RULE_CODE_VERSION, RULE_CODE_VERSION );

  rule_sys = (rule_sys_t *) new_mem( sizeof( rule_sys_t ) );
  rule_sys->initial_rule_set = header.initial_rule_set;
  rule_sys->initial_feat = header.initial_feat;
  rule_sys->robust_rule = header.robust_rule;
  rule_sys->allo_rule = header.allo_rule;
  rule_sys->pruning_rule = header.pruning_rule;
  rule_sys->input_filter = header.input_filter;
  rule_sys->output_filter = header.output_filter;
  rule_sys->rule_count = header.rule_count;
  rule_sys->rules = (rule_t *) read_new_vector( sizeof( rule_t ), header.rule_count,
                                     stream);
  rule_sys->rule_sets_size = header.rule_sets_size;
  rule_sys->rule_sets = (int_t *) read_new_vector( sizeof( int_t ), 
					 header.rule_sets_size,
                                         stream);
  rule_sys->instr_count = header.instr_count;
  rule_sys->instrs = (instr_t *) read_new_vector( sizeof( instr_t ), header.instr_count, 
                                      stream);
  rule_sys->values_size = header.values_size;
  rule_sys->values = (cell_t *) read_new_vector( sizeof( cell_t ), header.values_size, 
                                      stream);
  rule_sys->src_line_count = header.src_line_count;
  rule_sys->src_lines = (src_line_t *) read_new_vector( sizeof( src_line_t ), 
                                         header.src_line_count, 
                                         stream);
  rule_sys->var_count = header.var_count;
  rule_sys->vars = (var_t *) read_new_vector( sizeof( var_t ), header.var_count,
                                    stream);
  rule_sys->var_scope_count = header.var_scope_count;
  rule_sys->var_scopes = (var_scope_t *) read_new_vector( sizeof( var_scope_t ), 
                                          header.var_scope_count,
                                          stream);
  rule_sys->constant_count = header.constant_count;
  rule_sys->constants = (constant_t *) read_new_vector( sizeof( constant_t ), 
					 header.constant_count, 
					 stream);
  rule_sys->strings_size = header.strings_size;
  rule_sys->strings = (char_t *) read_new_vector( sizeof( char_t ), header.strings_size, 
                                       stream);

  close_stream(&stream);
  return rule_sys;
}

/*---------------------------------------------------------------------------*/

void 
free_rule_sys( rule_sys_t **rule_sys )
/* Free all memory used by *RULE_SYS. */
{ 
  if (*rule_sys != NULL) 
  { 
    free_mem( &(*rule_sys)->rules );
    free_mem( &(*rule_sys)->rule_sets );
    free_mem( &(*rule_sys)->instrs );
    free_mem( &(*rule_sys)->values );
    free_mem( &(*rule_sys)->src_lines );
    free_mem( &(*rule_sys)->vars );
    free_mem( &(*rule_sys)->var_scopes );
    free_mem( &(*rule_sys)->constants );
    free_mem( &(*rule_sys)->strings );
    free_mem( rule_sys );
  }
}

}}}
