/* Copyright (C) 1995 Bjoern Beutel. */

/* Description. =============================================================*/

/* This is the header file for "libmalaga". */

/*===========================================================================*/

namespace libvoikko { namespace morphology { namespace malaga {

/* Basic types. =============================================================*/

/* Numeric types. */
typedef unsigned short int u_short_t; /* Unsigned 16 bits. */

/* Character types. */
typedef char char_t; /* A single byte of a char. */
typedef const char_t *string_t; /* An EOS-terminated C-style string. */

typedef u_short_t cell_t;
/* A value is stored in one or more cells.
 * Use this type if you want to allocate memory (pools etc.) for values. */ 

typedef cell_t *value_t; /* Reference to a Malaga values by this type. */
typedef cell_t symbol_t;

/* Variables. ===============================================================*/

extern string_t malaga_error; // FIXME
/* If one of the functions below has created an error, this variable
 * contains an error message. If a function did its job, it is NULL. */

/* Value functions. =========================================================*/

extern symbol_t value_to_symbol( value_t value );
/* Return VALUE as a symbol. It is an error if VALUE is no symbol. */

extern char_t *get_value_string( value_t string );
/* Return the value of STRING as a C-style string in external encoding. 
 * The string must be freed after use. */

extern value_t get_attribute( value_t record, symbol_t attribute );
/* Return the value of ATTRIBUTE in RECORD or NULL if it doesn't exist. */

extern value_t parse_malaga_symbol( string_t string );
/* Convert the STRING to a Malaga value and return it.
 * STRING must be a valid UTF-8 string.
 * The value must be freed after use.
 * This function sets "malaga_error". */

/* Functions. ===============================================================*/

extern void analyse_item( string_t item );
/* Analyse ITEM.
 * ITEM must be a valid UTF-8 string.
 * This function sets "malaga_error". */

extern value_t first_analysis_result( void );
/* Get the first result of the last call of "analyse_item".
 * Return NULL if there is no result. */

extern value_t next_analysis_result( void );
/* Get the next result of the last call of "analyse_item".
 * Return NULL if there is no more result. */

extern void init_libmalaga(string_t project_directory);
/* Initialise this module.
 * This function sets "malaga_error". */

extern void terminate_libmalaga( void );
/* Terminate this module. */

/*===========================================================================*/

}}}
