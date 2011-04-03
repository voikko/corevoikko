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

/* This module administrates the name and atoms for each symbol. */

/* Includes. ================================================================*/

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include "setup/DictionaryException.hpp"
#include "morphology/malaga/basic.hpp"
#include "morphology/malaga/pools.hpp"
#include "morphology/malaga/values.hpp"
#include "morphology/malaga/files.hpp"
#include "morphology/malaga/malaga_files.hpp"
#include "morphology/malaga/symbols.hpp"
#include "morphology/malaga/MalagaState.hpp"

namespace libvoikko { namespace morphology { namespace malaga {

/* Functions. ===============================================================*/

static string_t get_symbol_name(symbol_t symbol, MalagaState * malagaState)
/* Return the name of SYMBOL. */
{
  return malagaState->symbol_table.strings + malagaState->symbol_table.symbols[symbol].name;
}

/*---------------------------------------------------------------------------*/

symbol_t 
find_symbol(string_t name, MalagaState * malagaState)
/* Find a symbol NAME in the symbol table and return its code.
 * If there is no symbol NAME, report an error. */
{
  /* We do a binary search in SYMBOLS_BY_NAME. */
  int_t lower = 0;
  int_t upper = malagaState->symbol_table.symbol_count - 1;
  while (lower <= upper) { 
    int_t middle = (lower + upper) / 2;
    int_t result = strcmp(name, malagaState->symbol_table.symbolAndName[middle].name);
    if (result < 0) {
      upper = middle - 1;
    } else if (result > 0) {
      lower = middle + 1;
    } else {
      return malagaState->symbol_table.symbolAndName[middle].symbol;
    }
  }
  throw setup::DictionaryException("Unknown symbol");
}

/*---------------------------------------------------------------------------*/

static int compareSymbolsByName(const void * symbol1, const void * symbol2) {
  return strcmp(((symbol_and_name *) symbol1)->name, ((symbol_and_name *) symbol2)->name);
}

/*---------------------------------------------------------------------------*/

void 
init_symbols(string_t file_name, MalagaState * malagaState)
/* Initialise this module. Read SYMBOL_TABLE from file FILE_NAME. */
{
  symbol_header_t header;
  FILE * stream = open_stream( file_name, "rb" );
  read_vector( &header, sizeof( header ), 1, stream);
  check_header( &header.common_header, 
                SYMBOL_FILE, MIN_SYMBOL_CODE_VERSION, SYMBOL_CODE_VERSION );
  
  malagaState->symbol_table.symbol_count = header.symbol_count;
  malagaState->symbol_table.symbols = (symbol_entry_t *) read_new_vector( sizeof( symbol_entry_t ), 
                                          header.symbol_count, 
                                          stream);
  malagaState->symbol_table.values = (symbol_t *) read_new_vector( sizeof( cell_t ), header.values_size, 
                                         stream);
  malagaState->symbol_table.strings = (char_t *) read_new_vector( sizeof( char_t ), 
					  header.strings_size,
					  stream);
  close_stream(&stream);
  
  /* Build a list of all symbols sorted by their names. */
  malagaState->symbol_table.symbolAndName = (symbol_and_name *) new_vector(sizeof(symbol_and_name),
                               header.symbol_count);
  for (int_t i = 0; i < header.symbol_count; i++) { 
    malagaState->symbol_table.symbolAndName[i].symbol = i;
    malagaState->symbol_table.symbolAndName[i].name = get_symbol_name(i, malagaState);
  }
  qsort(malagaState->symbol_table.symbolAndName, header.symbol_count, sizeof(symbol_and_name), compareSymbolsByName);
}

/*---------------------------------------------------------------------------*/

void 
terminate_symbols(MalagaState * malagaState)
/* Terminate this module. */
{
  free_mem(&(malagaState->symbol_table.symbols));
  free_mem(&(malagaState->symbol_table.values));
  free_mem(&(malagaState->symbol_table.strings));
  free_mem(&(malagaState->symbol_table.symbolAndName));
}

}}}
