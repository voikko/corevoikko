/* Copyright (C) 1995 Bjoern Beutel. */

/* Description. =============================================================*/

/* This module contains the Malaga rule interpreter. */

/* Includes. ================================================================*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <setjmp.h>
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

namespace libvoikko { namespace morphology { namespace malaga {

/* Types. ===================================================================*/

typedef struct /* Used to hold the value for a "switch". */
{
  list_node_t *next;
  symbol_t key; 
  value_t value; 
} switch_node_t;

typedef struct 
{ 
  list_node_t *next;
  int_t pc;
  int_t nested_subrules;
  int_t base; 
  int_t bottom;
} path_node_t;

/* Global variables. ========================================================*/

static int_t pc = -1; /* Current instruction index. FIXME */
static int_t base; /* Current frame base. FIXME */
static int_t nested_subrules; /* Current nesting level. FIXME */
static int_t path_count; /* Current number of alternative paths. FIXME */

bool rule_successful;

/* Variables. ===============================================================*/

static int_t bottom; /* Index of first stack element used in this branch. */

static list_t path_list; /* List of nodes for alternative paths. */

/* Rule execution. ==========================================================*/

static void 
standard_function( int_t function )
/* Stack effect: VALUE -> NEW_VALUE.
 * Perform function FUNCTION on VALUE yielding NEW_VALUE. */
{ 

  switch (function) 
  {
  case FUNC_GET_LENGTH:
    if (get_value_type( value_stack[ top - 1 ] ) == STRING_SYMBOL)
    {
      push_number_value( 
	g_utf8_strlen( value_to_string( value_stack[ --top ] ), -1 ) );
    }
    else 
      push_number_value( get_list_length( value_stack[ --top ] ) );
    break;
  case FUNC_GET_VALUE_TYPE:
    push_symbol_value( get_value_type( value_stack[ --top ] ) );
    break;
  case FUNC_SUBSTRING:
    string_t string = value_to_string( value_stack[ top - 3 ] );
    int_t start = value_to_int( value_stack[ top - 2 ] );
    int_t end;
    if (value_stack[ top - 1 ] == NULL) 
      end = start;
    else 
      end = value_to_int( value_stack[ top - 1 ] );
    int_t len = g_utf8_strlen( string, -1 );
    if (start < 0) 
      start += len + 1;
    if (end < 0) 
      end += len + 1;
    if (end < start) 
      push_string_value( "", NULL );
    else 
      push_string_value( g_utf8_offset_to_pointer( string, start - 1 ), 
			 g_utf8_offset_to_pointer( string, end ) );
    value_stack[ top - 4 ] = value_stack[ top - 1 ];
    top -= 3;
    break;
  }
}

/*---------------------------------------------------------------------------*/

void 
execute_rule( rule_sys_t *rule_sys, int_t rule_number )
/* Execute rule RULE_NUMBER in the rule system RULE_SYS.
 * Any parameters must be on the value stack. */
{ 
  static symbol_t nil = NIL_SYMBOL; /* The "nil" symbol. */
  path_node_t *path;
          
  /* Initialise the value stack. */
  top = rule_sys->rules[ rule_number ].param_count;
  base = bottom = 0;

  /* Reset nesting and alternative paths. */
  nested_subrules = path_count = 0;
  while (path_list.first != NULL) 
    free_first_node( &path_list );

  pc = rule_sys->rules[ rule_number ].first_instr;
  { 
    rule_successful = false;
    bool terminate = false;
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
        if (value_stack[ --top ] == NULL) 
	  terminate = true;
        break;
      case INS_ADD_END_STATE:
        add_end_state(value_stack[ --top ], rule_sys->rules + rule_number);
        rule_successful = true;
        break;
      case INS_ADD_STATE:
        add_running_state( value_stack[ --top ], info );
        rule_successful = true;
        break;
      case INS_ACCEPT:
        path_count = 0;
        while (path_list.first != NULL) 
	  free_first_node( &path_list );
        terminate = true;
        break;
      case INS_PUSH_NULL:
        for (int_t i = 0; i < info; i++) 
	  push_value( NULL );
        break;
      case INS_PUSH_VAR:
        push_value( value_stack[ base + info ] );
        break;
      case INS_PUSH_CONST:
        push_value( rule_sys->values + info );
        break;
      case INS_PUSH_SYMBOL:
        push_symbol_value( info );
        break;
      case INS_PUSH_PATTERN_VAR:
        push_string_value( pattern_var[ info ], NULL );
        break;
      case INS_POP:
        top -= info;
        break;
      case INS_POP_TO:
        top = base + info;
        break;
      case INS_BUILD_LIST:
        build_list( info );
        break;
      case INS_BUILD_RECORD:
        build_record( info );
        break;
      case INS_BUILD_PATH:
        build_path( info );
        break;
      case INS_DOT_OPERATION:
        dot_operation();
        if (value_stack[ top - 1 ] == NULL) 
	  value_stack[ top - 1 ] = &nil;
        break;
      case INS_PLUS_OPERATION:
        plus_operation();
        break;
      case INS_MINUS_OPERATION:
        minus_operation();
        break;
      case INS_GET_ATTRIBUTE:
        value_stack[ top - 1 ] = get_attribute( value_stack[ top - 1 ], 
						(symbol_t) info );
        if (value_stack[ top - 1 ] == NULL) 
	  value_stack[ top - 1 ] = &nil;
        break;
      case INS_REMOVE_ATTRIBUTE:
        remove_attribute( (symbol_t) info );
        break;
      case INS_STD_FUNCTION:
        standard_function( info );
        break;
      case INS_MATCH:
        if (match_pattern( value_to_string( value_stack[ --top ] ), 
                           rule_sys->strings + info )) 
	{ 
	  push_symbol_value( YES_SYMBOL ); 
	} 
	else 
	  push_symbol_value( NO_SYMBOL );
        break;
      case INS_SET_VAR:
        value_stack[ base + info ] = value_stack[ --top ];
        break;
      case INS_PLUS_VAR:
        insert_value( 1, value_stack[ base + info ] );
        plus_operation();
        value_stack[ base + info ] = value_stack[ --top ];
        break;
      case INS_MINUS_VAR:
        insert_value( 1, value_stack[ base + info ]);
        minus_operation();
        value_stack[ base + info ] = value_stack[ --top ];
        break;
      case INS_SET_VAR_PATH:
        insert_value( 2, value_stack[ base + info ] );
        modify_value_part( right_value );
        value_stack[ base + info ] = value_stack[ --top ];
        break;
      case INS_PLUS_VAR_PATH:
        insert_value( 2, value_stack[ base + info ] );
        modify_value_part( plus_operation );
        value_stack[ base + info ] = value_stack[ --top ];
        break;
      case INS_MINUS_VAR_PATH:
        insert_value( 2, value_stack[ base + info ] );
        modify_value_part( minus_operation );
        value_stack[ base + info ] = value_stack[ --top ];
        break;
      case INS_GET_1ST_ELEMENT:
        get_first_element();
        break;
      case INS_ITERATE:
        get_next_element( base + info );
        break;
      case INS_JUMP:
        new_pc = info;
        break;
      case INS_JUMP_IF_EQUAL:
	if (values_equal( value_stack[top - 2], value_stack[top - 1] )) 
	  new_pc = info;
        top -= 2;
        break;
      case INS_JUMP_IF_NOT_EQUAL:
        if (! values_equal( value_stack[top - 2], value_stack[top - 1] )) 
	  new_pc = info;
        top -= 2;
        break;
      case INS_JUMP_IF_IN:
        if (value_in_value( value_stack[top - 2], value_stack[top - 1] )) 
	  new_pc = info;
        top -= 2;
        break;
      case INS_JUMP_IF_NOT_IN:
        if (! value_in_value( value_stack[top - 2], value_stack[top - 1] )) 
	  new_pc = info;
        top -= 2;
        break;
      case INS_JUMP_IF_LESS:
        if (value_to_double( value_stack[top - 2] ) 
            < value_to_double( value_stack[top - 1] )) 
	{ 
	  new_pc = info; 
	}
        top -= 2;
        break;
      case INS_JUMP_IF_NOT_LESS:
        if (! (value_to_double( value_stack[top - 2] )
               < value_to_double( value_stack[top - 1] ))) 
	{ 
	  new_pc = info; 
	}
        top -= 2;
        break;
      case INS_JUMP_IF_GREATER:
        if (value_to_double( value_stack[top - 2] )
            > value_to_double( value_stack[top - 1] )) 
	{ 
	  new_pc = info; 
	}
        top -= 2;
        break;
      case INS_JUMP_IF_NOT_GREATER:
        if (! (value_to_double( value_stack[top - 2] )
               > value_to_double( value_stack[top - 1] ))) 
	{ 
	  new_pc = info; 
	}
        top -= 2;
        break;
      case INS_JUMP_IF_NULL:
        if (value_stack[ --top ] == NULL) 
	  new_pc = info;
        break;
      case INS_JUMP_IF_NOT_NULL:
        if (value_stack[ --top ] != NULL) 
	  new_pc = info;
        break;
      case INS_JUMP_IF_YES:
        {
        symbol_t symbol = value_to_symbol( value_stack[ --top ] );
        if (symbol == YES_SYMBOL) 
	  new_pc = info;
	}
        break;
      case INS_JUMP_IF_NO:
        {
        symbol_t symbol = value_to_symbol( value_stack[ --top ] );
        if (symbol == NO_SYMBOL) 
	  new_pc = info;
	}
        break;
      case INS_JUMP_NOW:
        {
        int_t old_top = top;
        path = (path_node_t *) new_node( &path_list, sizeof( path_node_t ), LIST_START );
        path->pc = new_pc;
        path->nested_subrules = nested_subrules;
        path->base = base;
        path->bottom = bottom;
        while (bottom < old_top)
	  push_value( value_stack[ bottom++ ] );
        base += (top - old_top);
        path_count++;
        new_pc = info;
	}
        break;
      case INS_JUMP_LATER:
        {
        int_t old_top = top;
        path = (path_node_t *) new_node( &path_list, sizeof( path_node_t ), LIST_START );
        path->pc = info;
        path->nested_subrules = nested_subrules;
        path->base = base;
        path->bottom = bottom;
        while (bottom < old_top) 
	  push_value( value_stack[ bottom++ ] );
        base += (top - old_top);
        path_count++;
	}
        break;
      case INS_JUMP_SUBRULE:
        push_number_value( base - bottom );
        push_number_value( new_pc );
        base = top;
        new_pc = rule_sys->rules[ info ].first_instr;
        nested_subrules++;
        break;
      case INS_RETURN:
        {
        int_t old_base = bottom + value_to_int( value_stack[ base - 2 ] );
        new_pc = value_to_int( value_stack[ base - 1 ] );
        value_stack[ base - info - 2 ] = value_stack[ top - 1 ]; /* Result. */
        top = base - (info + 1);
        base = old_base;
        nested_subrules--;
        }
        break;
      default:
        malaga_throw();
        break;
      }
      if (! terminate) 
	pc = new_pc;
      else if (path_list.first != NULL) 
      { 
	/* Load a previously saved rule-internal path and continue. */
        path_count--;
        path = (path_node_t *) path_list.first;
        top = bottom;
        base = path->base;
        bottom = path->bottom;
        pc = path->pc;
        nested_subrules = path->nested_subrules;
        free_first_node( &path_list );
        terminate = false;
      }
    }
  } 

  pc = -1;
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
  read_vector( &header, sizeof( header ), 1, stream, file_name );
  check_header( &header.common_header, file_name, 
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
                                     stream, file_name );
  rule_sys->rule_sets_size = header.rule_sets_size;
  rule_sys->rule_sets = (int_t *) read_new_vector( sizeof( int_t ), 
					 header.rule_sets_size,
                                         stream, file_name );
  rule_sys->instr_count = header.instr_count;
  rule_sys->instrs = (instr_t *) read_new_vector( sizeof( instr_t ), header.instr_count, 
                                      stream, file_name );
  rule_sys->values_size = header.values_size;
  rule_sys->values = (cell_t *) read_new_vector( sizeof( cell_t ), header.values_size, 
                                      stream, file_name );
  rule_sys->src_line_count = header.src_line_count;
  rule_sys->src_lines = (src_line_t *) read_new_vector( sizeof( src_line_t ), 
                                         header.src_line_count, 
                                         stream, file_name );
  rule_sys->var_count = header.var_count;
  rule_sys->vars = (var_t *) read_new_vector( sizeof( var_t ), header.var_count,
                                    stream, file_name );
  rule_sys->var_scope_count = header.var_scope_count;
  rule_sys->var_scopes = (var_scope_t *) read_new_vector( sizeof( var_scope_t ), 
                                          header.var_scope_count,
                                          stream, file_name );
  rule_sys->constant_count = header.constant_count;
  rule_sys->constants = (constant_t *) read_new_vector( sizeof( constant_t ), 
					 header.constant_count, 
					 stream, file_name );
  rule_sys->strings_size = header.strings_size;
  rule_sys->strings = (char_t *) read_new_vector( sizeof( char_t ), header.strings_size, 
                                       stream, file_name );

  close_stream( &stream, file_name );
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
