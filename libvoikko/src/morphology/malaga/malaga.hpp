/* Copyright (C) 1995 Bjoern Beutel. */

/* Description. =============================================================*/

/* This is the header file for "libmalaga". */

/*===========================================================================*/

namespace libvoikko { namespace morphology { namespace malaga {

/* Constants. ===============================================================*/

/* Current version of libmalaga interface. */
#define LIBMALAGA_VERSION 8

/* Minimum version of libmalaga interface that is still fully supported. */
#define MIN_LIBMALAGA_VERSION 7

#undef NULL
#define NULL 0 /* Null pointer. */

/* Some standard symbols. */
enum {NIL_SYMBOL, YES_SYMBOL, NO_SYMBOL, 
      SYMBOL_SYMBOL, STRING_SYMBOL, NUMBER_SYMBOL, LIST_SYMBOL, RECORD_SYMBOL};

/* Basic types. =============================================================*/

/* Numeric types. */
typedef signed char byte_t; /* Signed 8 bits. */
typedef unsigned char u_byte_t; /* Unsigned 8 bits. */
typedef signed short int short_t; /* Signed 16 bits. */
typedef unsigned short int u_short_t; /* Unsigned 16 bits. */
typedef signed int int_t; /* Signed 32 bits. */
typedef unsigned int u_int_t; /* Unsigned 32 bits. */

/* Character types. */
typedef char char_t; /* A single byte of a char. */
typedef const char_t *string_t; /* An EOS-terminated C-style string. */
enum {EOS= '\0'}; /* End-Of-String control character. */
#define ORD(c) ((u_byte_t) (c)) /* The ordinal number of character C. */

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

extern value_t new_value( value_t value );
/* Allocate space for VALUE and copy it.
 * Free the space occupied by this value with "free" after use. */

extern int_t length_of_value( value_t value );
/* Return the length of VALUE in cells. */

extern symbol_t get_value_type( value_t value );
/* Return the type of VALUE. Depending of the type, the result value may be
 * SYMBOL_SYMBOL, STRING_SYMBOL, NUMBER_SYMBOL, LIST_SYMBOL, RECORD_SYMBOL. */

extern symbol_t value_to_symbol( value_t value );
/* Return VALUE as a symbol. It is an error if VALUE is no symbol. */

extern string_t get_symbol_name( symbol_t symbol );
/* Return the name of SYMBOL. */

extern char_t *get_value_string( value_t string );
/* Return the value of STRING as a C-style string in external encoding. 
 * The string must be freed after use. */

extern value_t get_attribute( value_t record, symbol_t attribute );
/* Return the value of ATTRIBUTE in RECORD or NULL if it doesn't exist. */

extern int_t get_list_length( value_t list );
/* Return the number of elements in the list LIST. */

extern value_t get_element( value_t list, int_t n );
/* Return the N-th element of the list LIST, or NULL if it doesn't exist.
 * If N is positive, elements will be counted from the left border.
 * If N is negative, elements will be counted from the right border. */

extern double value_to_double( value_t value );
/* Return the value of VALUE which must be a number value. */

extern int_t value_to_int( value_t value );
/* Return the value of VALUE which must be an integral number value. */

extern value_t get_value_part( value_t value, value_t path );
/* Return the value part of VALUE that is specified by the path PATH. 
 * If that value part does not exist, return NULL. */

extern value_t get_first_item( value_t value );
/* If VALUE is a list, then return its first element (or NULL).
 * If VALUE is a record, then return its first attribute (or NULL). */

extern value_t get_next_item( value_t value, value_t item );
/* If VALUE is a list, and ELEMENT one of its elements,
 * then NEW_ELEMENT is the successor of ELEMENT (or NULL).
 * If VALUE is a record, and ELEMENT one of its attributes,
 * then NEW_ELEMENT is the next attribute in VALUE (or NULL). */

extern bool values_equal( value_t value1, value_t value2 );
/* Return a truth value indicating whether VALUE1 and VALUE2 are equal.
 * VALUE1 an VALUE2 must be of same type or one of them must be nil.
 * Refer to documentation to see what "equal" in Malaga really means. */

extern bool value_in_value( value_t value1, value_t value2 );
/* Return bool value saying if VALUE1 is element or attribute of VALUE2.
 * VALUE2 must be a list or a record.
 * If VALUE2 is a record, then VALUE1 must be a symbol. */

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
