/* Copyright (C) 2002 Bjoern Beutel. */

/* Description. =============================================================*/

/* This module parses Malaga values. */

/* Includes. ================================================================*/

#include <stdio.h>
#include <setjmp.h>
#include "morphology/malaga/basic.hpp"
#include "morphology/malaga/pools.hpp"
#include "morphology/malaga/values.hpp"
#include "morphology/malaga/symbols.hpp"
#include "morphology/malaga/scanner.hpp"
#include "morphology/malaga/value_parser.hpp"

/* Functions. ===============================================================*/

static void 
parse_symbol( void )
/* Parse a symbol and push it on the VALUE_STACK. */
{
  test_token( TOK_IDENT );
  push_symbol_value( find_symbol( token_name ) );
  read_next_token();
}

/*---------------------------------------------------------------------------*/

void 
parse_a_value( void )
/* Parse a value (use scanner input) and leave it on the VALUE_STACK. */
{
  int_t n; /* Number of values in list or record. */
  
  switch (next_token) 
  {
  case '<': 
    /* Parse a list. */
    read_next_token();
    n = 0;
    if (next_token != '>') 
    { 
      parse_a_value();
      n++;
      while (next_token == ',') 
      { 
	read_next_token();
        parse_a_value();
        n++;
      }
    }
    parse_token( '>' );
    build_list(n);
    break;
  case '[': 
    /* Parse a record. */
    read_next_token();
    n = 0;
    if (next_token != ']') 
    { 
      parse_symbol();
      parse_token( ':' );
      parse_a_value();
      n++;
      while (next_token == ',') 
      { 
	read_next_token();
        parse_symbol();
        parse_token( ':' );
        parse_a_value();
        n++;
      }
    }
    parse_token( ']' );
    build_record(n);
    break;
  case TOK_IDENT: 
    /* Parse a symbol. */
    parse_symbol();
    break;
  case TOK_STRING: 
    /* Parse a string. */
    push_string_value( token_string, NULL );
    read_next_token();
    break;
  case TOK_NUMBER: 
    /* Parse a number. */
    push_number_value( token_number );
    read_next_token();
    break;
  case '-':
    /* Parse a negative number. */
    read_next_token();
    test_token( TOK_NUMBER );
    push_number_value( -token_number );
    read_next_token();
    break;
  default:
    complain( "Value expected, not %s.", token_as_text( next_token ) );
  }
}

/* End of file. =============================================================*/
