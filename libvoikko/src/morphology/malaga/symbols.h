/* Copyright (C) 1995 Bjoern Beutel. */

/* Description. =============================================================*/

/* This module administrates the name and atoms for each symbol. */

/* Functions. ===============================================================*/

extern string_t get_symbol_name( symbol_t symbol );
/* Return the name of SYMBOL. */

extern value_t get_atoms( symbol_t symbol );
/* Return the atom list of SYMBOL. */

extern symbol_t find_symbol( string_t name );
/* Find a symbol by NAME in the symbol table and return its code.
 * If there is no symbol NAME, report an error. */

extern symbol_t find_multi_symbol( value_t atoms );
/* Find the multi-symbol for ATOMS and return its code. 
 * If there is no multi-symbol for ATOMS, report an error. */

extern int_t symbol_count( void );
/* Return the number of symbols defined. */

extern void init_symbols( string_t file_name );
/* Initialise this module; Read symbol table from file FILE_NAME. */

extern void terminate_symbols( void );
/* Terminate this module. */

/* End of file. =============================================================*/
