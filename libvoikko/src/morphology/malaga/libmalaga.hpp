/* Copyright (C) 1997 Bjoern Beutel. */

/* Description. =============================================================*/

/* This module defines a Malaga library to analyse words and sentences. */

namespace libvoikko { namespace morphology { namespace malaga {

/* Variables. ===============================================================*/

extern string_t malaga_error; 
/* In case of an error, some of the functions below may set this variable to 
 * the error message. If they worked correctly, they set it to NULL. */

/* Functions. ===============================================================*/

extern void init_libmalaga( string_t project_file );
/* Initialise this module.
 * This function sets "malaga_error". */

extern void terminate_libmalaga( void );
/* Terminate this module. */

extern void analyse_item( string_t item, grammar_t grammar );
/* Analyse ITEM according to GRAMMAR.
 * GRAMMAR must be MORPHOLOGY or SYNTAX.
 * This function sets "malaga_error". */

extern string_t get_info( void );
/* Get info about the current grammar. */

extern char_t *get_value_string( value_t string );
/* Return the value of STRING as a C-style string in external coding. 
 * The string must be freed after use. */

extern value_t parse_malaga_value( string_t string );
/* Convert STRING to a Malaga value and return it.
 * The value must be freed after use.
 * This function sets "malaga_error". */

}}}
