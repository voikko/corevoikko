/* Copyright (C) 1997 Bjoern Beutel. */

/* Description. =============================================================*/

/* This module defines a Malaga library to analyse words and sentences. */

/* Includes. ================================================================*/

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <stdlib.h>
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
  string_t project_directory_absolute = absolute_path(project_directory, NULL);
  try {
    init_malaga(project_directory_absolute, malagaState);
    free_mem(&project_directory_absolute);
  }
  catch (setup::DictionaryException e) {
    free_mem(&project_directory_absolute);
    throw e;
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
  }
  catch (setup::DictionaryException e) {
    free_mem(&analysis_input);
    throw e;
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
