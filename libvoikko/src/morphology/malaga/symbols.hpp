/* Copyright (C) 1995 Bjoern Beutel. */

/* Description. =============================================================*/

/* This module administrates the name and atoms for each symbol. */

namespace libvoikko { namespace morphology { namespace malaga {

class MalagaState;

/* Functions. ===============================================================*/

extern symbol_t find_symbol(string_t name, MalagaState * malagaState);
/* Find a symbol by NAME in the symbol table and return its code.
 * If there is no symbol NAME, report an error. */

extern void init_symbols(string_t file_name, MalagaState * malagaState);
/* Initialise this module; Read symbol table from file FILE_NAME. */

extern void terminate_symbols(MalagaState * malagaState);
/* Terminate this module. */

}}}
