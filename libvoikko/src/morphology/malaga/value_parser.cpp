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

namespace libvoikko { namespace morphology { namespace malaga {

/* Functions. ===============================================================*/

static void 
parse_symbol(string_t & scanner_input)
/* Parse a symbol and push it on the VALUE_STACK. */
{
  test_token( TOK_IDENT );
  push_symbol_value( find_symbol( token_name ) );
  read_next_token(scanner_input);
}

/*---------------------------------------------------------------------------*/

void 
parse_a_value(string_t & scanner_input)
/* Parse a value (use scanner input) and leave it on the VALUE_STACK. */
{
  int_t n; /* Number of values in list or record. */
  
  switch (next_token) 
  {
  case '<': 
    /* Parse a list. */
    read_next_token(scanner_input);
    n = 0;
    if (next_token != '>') 
    { 
      parse_a_value(scanner_input);
      n++;
      while (next_token == ',') 
      { 
	read_next_token(scanner_input);
        parse_a_value(scanner_input);
        n++;
      }
    }
    parse_token('>', scanner_input);
    build_list(n);
    break;
  case '[': 
    /* Parse a record. */
    read_next_token(scanner_input);
    n = 0;
    if (next_token != ']') 
    { 
      parse_symbol(scanner_input);
      parse_token(':', scanner_input);
      parse_a_value(scanner_input);
      n++;
      while (next_token == ',') 
      { 
	read_next_token(scanner_input);
        parse_symbol(scanner_input);
        parse_token(':', scanner_input);
        parse_a_value(scanner_input);
        n++;
      }
    }
    parse_token(']', scanner_input);
    build_record(n);
    break;
  case TOK_IDENT: 
    /* Parse a symbol. */
    parse_symbol(scanner_input);
    break;
  case TOK_STRING: 
    /* Parse a string. */
    push_string_value( token_string, NULL );
    read_next_token(scanner_input);
    break;
  case TOK_NUMBER: 
    /* Parse a number. */
    push_number_value( token_number );
    read_next_token(scanner_input);
    break;
  case '-':
    /* Parse a negative number. */
    read_next_token(scanner_input);
    test_token( TOK_NUMBER );
    push_number_value( -token_number );
    read_next_token(scanner_input);
    break;
  default:
    complain( "Value expected, not %s.", token_as_text( next_token ) );
  }
}

}}}
