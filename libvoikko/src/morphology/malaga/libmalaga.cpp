/* Copyright (C) 1997 Bjoern Beutel. */

/* Description. =============================================================*/

/* This module defines a Malaga library to analyse words and sentences. */

/* Includes. ================================================================*/

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <stdlib.h>
#include <setjmp.h>
#include "morphology/malaga/basic.hpp"
#include "morphology/malaga/pools.hpp"
#include "morphology/malaga/values.hpp"
#include "morphology/malaga/symbols.hpp"
#include "morphology/malaga/files.hpp"
#include "morphology/malaga/rule_type.hpp"
#include "morphology/malaga/rules.hpp"
#include "morphology/malaga/analysis.hpp"
#include "morphology/malaga/input.hpp"
#include "morphology/malaga/malaga_lib.hpp"
#include "morphology/malaga/scanner.hpp"
#include "morphology/malaga/value_parser.hpp"
#include "morphology/malaga/libmalaga.hpp"

namespace libvoikko { namespace morphology { namespace malaga {

/* Variables. ===============================================================*/

string_t malaga_error; 
/* If one of the functions below has created an error, this variable
 * contains an error message. If a function did its job, it is NULL. */

/* Functions. ===============================================================*/

void 
init_libmalaga( string_t project_file )
/* Initialise this module. */
{ 
  string_t project_file_absolute;

  malaga_error = NULL;
  init_basic( "libmalaga" );
  project_file_absolute = absolute_path( project_file, NULL );
  TRY 
    init_malaga( project_file_absolute );
  IF_ERROR 
  { 
    malaga_error = error_text->buffer;
    RESUME;
  }
  END_TRY;
  free_mem( &project_file_absolute );
}

/*---------------------------------------------------------------------------*/

void
terminate_libmalaga( void )
/* Terminate this module. */
{ 
  terminate_malaga();
  terminate_basic();
}

/*---------------------------------------------------------------------------*/

char_t *
get_value_string( value_t string )
/* Return the value of STRING as a C-style string. 
 * The string must be freed after use. */
{
  char_t *s;
  
  s = new_string( value_to_string( string ), NULL );
  return s;
}

/*---------------------------------------------------------------------------*/

void
analyse_item( string_t item )
/* Analyse ITEM */
{ 
  char_t *analysis_input;

  analysis_input = NULL;
  malaga_error = NULL;
  TRY 
  { 
    analysis_input = new_string( item, NULL );
    preprocess_input( analysis_input );
    analyse( analysis_input, false, true );
  }
  IF_ERROR 
  { 
    malaga_error = error_text->buffer;
    RESUME;
  }
  END_TRY;
  free_mem( &analysis_input );
}

/*---------------------------------------------------------------------------*/

string_t
get_info( void )
/* Get info about the current grammar. */
{ 
  return grammar_info->buffer;
}

/*---------------------------------------------------------------------------*/

value_t
parse_malaga_value( string_t string )
/* Convert STRING to a Malaga value and return it.
 * The value must be freed after use.
 * This function sets "malaga_error". */
{
  volatile value_t value;

  malaga_error = NULL;
  set_scanner_input( string );
  TRY
  {
    parse_a_value();
    parse_token( EOF );
    value = new_value( value_stack[ --top ] );
  }
  IF_ERROR
  {
    malaga_error = error_text->buffer;
    value = NULL;
    RESUME;
  }
  END_TRY;
  set_scanner_input( NULL );
  return value;
}

}}}
