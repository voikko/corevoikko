/* Copyright (C) 1997 Bjoern Beutel.
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

/* This module defines a Malaga library to analyse words and sentences. */

/* Includes. ================================================================*/

#include <cstdio>
#include <cstdarg>
#include <cstring>
#include <cstdlib>
#include "morphology/malaga/basic.hpp"
#include "morphology/malaga/pools.hpp"
#include "morphology/malaga/values.hpp"
#include "morphology/malaga/symbols.hpp"
#include "morphology/malaga/files.hpp"
#include "morphology/malaga/rule_type.hpp"
#include "morphology/malaga/rules.hpp"
#include "morphology/malaga/analysis.hpp"
#include "morphology/malaga/malaga_lib.hpp"
#include "morphology/malaga/libmalaga.hpp"
#include "morphology/malaga/MalagaState.hpp"

namespace libvoikko { namespace morphology { namespace malaga {

/* Variables. ===============================================================*/

/* Functions. ===============================================================*/

void 
init_libmalaga(string_t project_directory, MalagaState * malagaState) throw(setup::DictionaryException)
/* Initialise this module. */
{ 
  char * project_directory_absolute = absolute_path(project_directory, NULL);
  try {
    init_malaga(project_directory_absolute, malagaState);
    free_mem(&project_directory_absolute);
  }
  catch (setup::DictionaryException & e) {
    free_mem(&project_directory_absolute);
    throw;
  }  
}

/*---------------------------------------------------------------------------*/

void
terminate_libmalaga(MalagaState * malagaState)
/* Terminate this module. */
{ 
  terminate_malaga(malagaState);
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
analyse_item(string_t item, MalagaState * malagaState) throw(setup::DictionaryException)
/* Analyse ITEM */
{ 
  char_t * analysis_input = NULL;
  try {
    analysis_input = new_string( item, NULL );
    preprocess_input( analysis_input );
    analyse(analysis_input, malagaState);
    free_mem(&analysis_input);
    // Clear value heap. There is nothing but garbage on it and old entries will affect
    // carbage collector triggering during subsequent analysis operations, making it
    // harder to reproduce bugs.
    malagaState->value_heap_end = malagaState->value_heap;
  }
  catch (setup::DictionaryException & e) {
    free_mem(&analysis_input);
    throw;
  }
}

/*---------------------------------------------------------------------------*/

value_t
parse_malaga_symbol(string_t string, MalagaState * malagaState)
/* Convert STRING to a Malaga value and return it.
 * The value must be freed after use. */
{
  push_symbol_value(find_symbol(string, malagaState), malagaState);
  return new_value(malagaState->value_stack[--(malagaState->top)] );
}

}}}
