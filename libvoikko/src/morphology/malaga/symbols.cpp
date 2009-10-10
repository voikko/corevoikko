/* Copyright (C) 1995 Bjoern Beutel. */

/* Description. =============================================================*/

/* This module administrates the name and atoms for each symbol. */

/* Includes. ================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <setjmp.h>
#include <string.h>
#include "morphology/malaga/basic.hpp"
#include "morphology/malaga/pools.hpp"
#include "morphology/malaga/values.hpp"
#include "morphology/malaga/files.hpp"
#include "morphology/malaga/malaga_files.hpp"
#include "morphology/malaga/symbols.hpp"

namespace libvoikko { namespace morphology { namespace malaga {

static struct /* This is the symbol table. FIXME */
{ 
  int_t symbol_count; /* Number of symbols in this table. */

  symbol_entry_t *symbols; /* The names and atoms of all symbols. */
  symbol_t *symbols_by_name; /* All symbols sorted by their names. */
  symbol_t *symbols_by_atoms; /* All symbols sorted by their atom lists. */

  int_t values_size;
  symbol_t *values; /* Contains the lists of atomic symbols. */

  int_t strings_size;     
  char_t *strings; /* Contains the symbol names. */
} symbol_table;

/* Functions. ===============================================================*/

string_t 
get_symbol_name( symbol_t symbol )
/* Return the name of SYMBOL. */
{
  return symbol_table.strings + symbol_table.symbols[ symbol ].name;
}

/*---------------------------------------------------------------------------*/

value_t 
get_atoms( symbol_t symbol )
/* Return the atom list of SYMBOL. */
{
  return symbol_table.values + symbol_table.symbols[ symbol ].atoms;
}

/*---------------------------------------------------------------------------*/

symbol_t 
find_symbol( string_t name )
/* Find a symbol NAME in the symbol table and return its code.
 * If there is no symbol NAME, report an error. */
{
  int_t lower, upper, middle, result;
  symbol_t symbol;

  /* We do a binary search in SYMBOLS_BY_NAME. */
  lower = 0;
  upper = symbol_table.symbol_count - 1;
  while (lower <= upper) 
  { 
    middle = (lower + upper) / 2;
    symbol = symbol_table.symbols_by_name[ middle ];
    result = strcmp( name, get_symbol_name( symbol ) );
    if (result < 0) 
      upper = middle - 1;
    else if (result > 0) 
      lower = middle + 1;
    else 
      return symbol;
  }
  complain( "Unknown symbol \"%s\".", name );
}

/*---------------------------------------------------------------------------*/

symbol_t 
find_multi_symbol( value_t atoms )
/* Find a symbol by its atoms in the symbol table and return its code.
 * If there is no multi-symbol for ATOMS, report an error. */
{
  int_t lower, upper, middle, result;
  symbol_t symbol;

  /* We do a binary search in SYMBOLS_BY_ATOMS. */
  lower = 0;
  upper = symbol_table.symbol_count - 1;
  while (lower <= upper) 
  { 
    middle = (lower + upper) / 2;
    symbol = symbol_table.symbols_by_atoms[ middle ];
    result = compare_atom_lists( atoms, get_atoms( symbol ) );
    if (result < 0) 
      upper = middle - 1;
    else if (result > 0) 
      lower = middle + 1;
    else 
      return symbol;
  }
  complain( "No multi symbol for this atom list." );
}

/*---------------------------------------------------------------------------*/

int_t 
symbol_count( void )
/* Return the number of symbols defined. */
{
  return symbol_table.symbol_count;
}

/*---------------------------------------------------------------------------*/

static int 
compare_symbols_by_name( const void *symbol1, const void *symbol2 )
/* Return -1 if name( SYMBOL1 ) < name( SYMBOL2 )
 *         0 if name( SYMBOL1 ) == name( SYMBOL2 )
 *         1 if name( SYMBOL1 ) > name( SYMBOL2 ). */
{
  return strcmp( get_symbol_name( *(symbol_t *) symbol1 ), 
                         get_symbol_name( *(symbol_t *) symbol2 ) );
}

/*---------------------------------------------------------------------------*/

static int 
compare_symbols_by_atoms( const void *symbol1, const void *symbol2 )
/* Return -1 if atoms( SYMBOL1 ) < atoms( SYMBOL2 )
 *         0 if atoms( SYMBOL1 ) == atoms( SYMBOL2 )
 *         1 if atoms( SYMBOL1 ) > atoms( SYMBOL2 ). */
{
  return compare_atom_lists( get_atoms( *(symbol_t *) symbol1 ), 
                             get_atoms( *(symbol_t *) symbol2 ) );
}

/*---------------------------------------------------------------------------*/

void 
init_symbols( string_t file_name )
/* Initialise this module. Read SYMBOL_TABLE from file FILE_NAME. */
{
  FILE *stream;
  symbol_header_t header;
  int_t i;
  
  stream = open_stream( file_name, "rb" );
  read_vector( &header, sizeof( header ), 1, stream, file_name );
  check_header( &header.common_header, file_name, 
                SYMBOL_FILE, MIN_SYMBOL_CODE_VERSION, SYMBOL_CODE_VERSION );
  
  symbol_table.symbol_count = header.symbol_count;
  symbol_table.symbols = (symbol_entry_t *) read_new_vector( sizeof( symbol_entry_t ), 
                                          header.symbol_count, 
                                          stream, file_name );
  symbol_table.values_size = header.values_size;
  symbol_table.values = (symbol_t *) read_new_vector( sizeof( cell_t ), header.values_size, 
                                         stream, file_name );
  symbol_table.strings_size = header.strings_size;
  symbol_table.strings = (char_t *) read_new_vector( sizeof( char_t ), 
					  header.strings_size,
					  stream, file_name );
  close_stream( &stream, file_name );
  
  /* Build a list of all symbols sorted by their names
   * and a list of all symbols sorted by their atom lists. */
  symbol_table.symbols_by_name = (symbol_t *) new_vector( sizeof( symbol_t ),
					     header.symbol_count );
  symbol_table.symbols_by_atoms = (symbol_t *) new_vector( sizeof( symbol_t ),
					      header.symbol_count );
  for (i = 0; i < header.symbol_count; i++) 
  { 
    symbol_table.symbols_by_name[i] = i;
    symbol_table.symbols_by_atoms[i] = i;
  }
  qsort( symbol_table.symbols_by_name, header.symbol_count, 
         sizeof( symbol_t ), compare_symbols_by_name );
  qsort( symbol_table.symbols_by_atoms, header.symbol_count, 
         sizeof( symbol_t ), compare_symbols_by_atoms );
  
  values_get_symbol_name = get_symbol_name;
  values_get_atoms = get_atoms;
}

/*---------------------------------------------------------------------------*/

void 
terminate_symbols( void )
/* Terminate this module. */
{
  free_mem( &symbol_table.symbols );
  free_mem( &symbol_table.symbols_by_name );
  free_mem( &symbol_table.symbols_by_atoms );
  free_mem( &symbol_table.values );
  free_mem( &symbol_table.strings );
}

}}}
