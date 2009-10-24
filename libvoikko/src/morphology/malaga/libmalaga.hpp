/* Copyright (C) 1997 Bjoern Beutel. */

/* Description. =============================================================*/

/* This module defines a Malaga library to analyse words and sentences. */

#include "setup/DictionaryException.hpp"

namespace libvoikko { namespace morphology { namespace malaga {

/* Functions. ===============================================================*/

extern void terminate_libmalaga( void );
/* Terminate this module. */

extern void analyse_item( string_t item ) throw(setup::DictionaryException);
/* Analyse ITEM.
 * This function sets "malaga_error". */

extern char_t *get_value_string( value_t string );
/* Return the value of STRING as a C-style string in external coding. 
 * The string must be freed after use. */

extern value_t parse_malaga_symbol( string_t string );
/* Convert STRING to a Malaga value and return it.
 * The value must be freed after use.
 * This function sets "malaga_error". */

}}}
