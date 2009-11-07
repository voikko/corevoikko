/* Copyright (C) 1995 Bjoern Beutel. */

/* Description. =============================================================*/

/* This module defines the data type "value_t", and many
 * operations to build, modify, and print such values.
 * There are six different types of values:
 * symbol, string, list, record, number and index. */

#ifndef LIBVOIKKO_MORPHOLOGY_MALAGA_VALUES_HPP
#define LIBVOIKKO_MORPHOLOGY_MALAGA_VALUES_HPP

namespace libvoikko { namespace morphology { namespace malaga {

class MalagaState;

/* Constants. ===============================================================*/

/* Some standard symbols. */
enum {NIL_SYMBOL, YES_SYMBOL, NO_SYMBOL, 
      SYMBOL_SYMBOL, STRING_SYMBOL, NUMBER_SYMBOL, LIST_SYMBOL, RECORD_SYMBOL};

enum {SYMBOL_MAX = 8192}; /* Symbols are in the range of 0..SYMBOL_MAX - 1. */

/* Types. ===================================================================*/

typedef u_short_t cell_t;
/* A value is stored in one or more cells.
 * Use this type if you want to allocate memory (pools etc.) for values. */ 

typedef cell_t *value_t; /* Reference to a Malaga values by this type. */
typedef cell_t symbol_t;

/* Module initialisation. ===================================================*/

extern void init_values(MalagaState * malagaState);
/* Initialise this module. */

extern void terminate_values(MalagaState * malagaState);
/* Terminate this module. */

/* Value operations. ========================================================*/

extern value_t new_value( value_t value );
/* Allocate space for VALUE and copy it.
 * Use "free" to free the space occupied by this value. */

extern value_t copy_value_to_pool( pool_t value_pool, 
                                   value_t value, 
                                   int_t *index );
/* Copy VALUE to the pool VALUE_POOL and store its index in *INDEX. */

extern int_t length_of_value( value_t value );
/* Return the length of VALUE in cells. */

extern symbol_t get_value_type( value_t value );
/* Return the type of VALUE. Depending of the type, the result value may be
 * SYMBOL_SYMBOL, STRING_SYMBOL, NUMBER_SYMBOL, LIST_SYMBOL, RECORD_SYMBOL. */

extern void push_value(value_t value, MalagaState * malagaState);
/* Stack effects: (nothing) -> VALUE. */

extern void insert_value(int_t n, value_t value, MalagaState * malagaState);
/* Stack effects: VALUE1...VALUE_N -> VALUE VALUE1...VALUE_N. */

/* Symbol operations. =======================================================*/

extern symbol_t value_to_symbol( value_t value );
/* VALUE must be a symbol.
 * Return VALUE as a symbol. */

extern void push_symbol_value(symbol_t symbol, MalagaState * malagaState);
/* Stack effects: (nothing) -> NEW_SYMBOL.
 * NEW_SYMBOL is SYMBOL converted to a Malaga value. */

/* String operations. =======================================================*/

extern string_t value_to_string( value_t value );
/* VALUE must be a string value.
 * Return the value of VALUE as a C style string. */

extern void push_string_value(string_t string_start, string_t string_end, MalagaState * malagaState);
/* Stack effects: (nothing) -> NEW_STRING.
 * NEW_STRING is the string starting at STRING_START as a Malaga value.
 * If STRING_END != NULL, it marks the end of the string. */

extern void concat_string_values(MalagaState * malagaState);
/* Stack effects: STRING1 STRING2 -> NEW_STRING.
 * NEW_STRING is the concatenation of STRING1 and STRING2. */

/* Record operations. =======================================================*/

extern value_t get_attribute( value_t record, symbol_t attribute );
/* Return the value of ATTRIBUTE in the record RECORD 
 * or NULL if it doesn't exist. */

extern void build_record(int_t n, MalagaState * malagaState);
/* Stack effects: ATTR1 VALUE1 ... ATTR_N VALUE_N -> NEW_RECORD.
 * NEW_RECORD looks like [ATTR1: VALUE1, ..., ATTR_N: VALUE_N]. */

extern void join_records(MalagaState * malagaState);
/* Stack effects: RECORD1 RECORD2 -> NEW_RECORD.
 * NEW_RECORD contains all attributes of RECORD1 and RECORD2, and 
 * their associated values. If an attribute has different values in RECORD1
 * and RECORD2, the value in RECORD2 will be taken. */

extern void remove_attribute(symbol_t attribute, MalagaState * malagaState);
/* Stack effects: RECORD -> NEW_RECORD.
 * NEW_RECORD contains all attribute-value pairs of RECORD but the one with
 * attribute ATTRIBUTE. */

extern void remove_attributes(MalagaState * malagaState);
/* Stack effects: RECORD LIST -> NEW_RECORD.
 * NEW_RECORD contains all attribute-value pairs of RECORD but the ones
 * whose attributes are in LIST. */

extern void replace_attribute(symbol_t attribute, MalagaState * malagaState);
/* Stack effects: RECORD VALUE -> NEW_RECORD.
 * NEW_RECORD is equal to RECORD, only the value of ATTRIBUTE is replaced
 * by VALUE. RECORD must contain ATTRIBUTE. */

/* List operations. =========================================================*/

extern int_t get_list_length( value_t list );
/* Return the number of elements in the list LIST. */

extern value_t get_element( value_t list, int_t n );
/* Return the N-th element of the list LIST,
 * or NULL, if that element doesn't exist.
 * If N is positive, elements will be counted from the left border.
 * If it's negative, elements will be counted from the right border. */

extern void build_list(int_t n, MalagaState * malagaState);
/* Stack effects: VALUE1 ... VALUE_N -> NEW_LIST.
 * NEW_LIST looks like <VALUE1, ..., VALUE_N>. */

extern void concat_lists(MalagaState * malagaState);
/* Stack effects: LIST1 LIST2 -> NEW_LIST.
 * NEW_LIST is the concatenation of LIST1 and LIST2. */

extern void get_list_difference(MalagaState * malagaState);
/* Stack effects: LIST1 LIST2 -> NEW_LIST.
 * NEW_LIST contains the list difference of LIST1 and LIST2:
 * An element that appears M times in LIST1 and N times in LIST2 
 * appears M - N times in NEW_LIST. */

extern void remove_element(int_t n, MalagaState * malagaState);
/* Stack effects: LIST -> NEW_LIST.
 * NEW_LIST is LIST without element at index N.
 * If N is positive, the elements will be counted from the left border;
 * if N is negative, they will be counted from the right border.
 * If LIST contains less than abs(N) elements, then NEW_LIST = LIST. */

extern void replace_element(int_t n, MalagaState * malagaState);
/* Stack effects: LIST VALUE -> NEW_LIST.
 * NEW_LIST is LIST, but its N-th element is replaced by VALUE.
 * If N is negative, count from the right end.
 * LIST must contain at least N elements. */

/* Number operations. =======================================================*/

extern double value_to_double( value_t value );
/* Return the value of VALUE which must be a number value. */

extern int_t value_to_int( value_t value );
/* Return the value of VALUE which must be an integral number value. */

extern void push_number_value(double number, MalagaState * malagaState);
/* Stack effects: (nothing) -> NEW_NUMBER.
 * NEW_NUMBER is NUMBER as a Malaga value. */

/* Type dependent Malaga operations. ========================================*/

extern void dot_operation(MalagaState * malagaState);
/* Stack effects: VALUE1 VALUE2 -> NEW_VALUE.
 * NEW_VALUE is VALUE1 "." VALUE2 or NULL, if that value doesn't exist.
 * The actual operation depends on the type of the values. */

extern void plus_operation(MalagaState * malagaState);
/* Stack effects: VALUE1 VALUE2 -> NEW_VALUE.
 * NEW_VALUE is VALUE1 "+" VALUE2. 
 * The actual operation depends on the type of the values. */

extern void minus_operation(MalagaState * malagaState);
/* Stack effects: VALUE1 VALUE2 -> NEW_VALUE.
 * NEW_VALUE is VALUE1 "-" VALUE2. 
 * The actual operation depends on the type of the values. */

/* Functions for value paths. ===============================================*/

extern void build_path(int_t n, MalagaState * malagaState);
/* Stack effects: VALUE1 ... VALUE_N -> NEW_LIST.
 * NEW_LIST is a path which contains VALUE1, ..., VALUE_N. 
 * VALUE1, ..., VALUE_N must be numbers, symbols or lists of numbers and 
 * symbols. If a value is a list, the elements of this list are inserted into
 * NEW_LIST instead of the value itself. */

extern void modify_value_part(void (*modifier)(MalagaState *), MalagaState * malagaState);
/* Stack effects: VALUE PATH MOD_VALUE -> NEW_VALUE.
 * NEW_VALUE is VALUE, but the part that is described by PATH is 
 * modified. PATH must be a list of symbols and numbers <E1, E2, .. , E_N>.
 * They will be used as nested attributes and indexes, so the part of VALUE
 * that is actually modified is OLD_VALUE := VALUE.E1.E2..E_N. 
 * If this part does not exist, an error will be reported. Else the function 
 * MODIFIER will be called on OLD_VALUE and MOD_VALUE. 
 * The value returned by MODIFIER will be entered in VALUE in place of
 * OLD_VALUE. */

extern void right_value(MalagaState * malagaState);
/* Stack effects: LEFT_VALUE RIGHT_VALUE -> RIGHT_VALUE.
 * A modifier for "modify_value_part". */

/* Functions for list/record iteration. =====================================*/

extern void get_first_element(MalagaState * malagaState);
/* Stack effects: VALUE -> NEW_VALUE.
 * If VALUE is a list, then NEW_VALUE is its first element (or NULL).
 * If VALUE is a record, then NEW_VALUE is its first attribute (or NULL).
 * If VALUE is a number, then NEW_VALUE is NULL (if VALUE == 0),
 * 1 (if VALUE > 0) or -1 (if VALUE < 0). */

extern void get_next_element(int_t stack_index, MalagaState * malagaState);
/* Stack effects: (nothing) -> (nothing).
 * VALUE1 is VALUE_STACK[ INDEX - 1 ], VALUE2 is VALUE_STACK[ INDEX ].
 * VALUE_STACK[ INDEX ] will be set to NEW_VALUE.
 * VALUE2 must be the result of an application of "get_first_element" or 
 * "get_next_element" on VALUE1.
 * If VALUE1 is a list, and VALUE2 one of its elements,
 * then NEW_VALUE is the successor of VALUE2 (or NULL).
 * If VALUE1 is a record, and VALUE2 one of its attributes,
 * then NEW_VALUE is the next attribute in VALUE1 (or NULL).
 * If VALUE1 is a positive number, and VALUE2 a number smaller than
 * VALUE1, then NEW_VALUE is VALUE2 + 1.
 * If VALUE1 is a negative number, and VALUE2 a number greater than
 * VALUE1, then NEW_VALUE is VALUE2 - 1. */

/* Functions to compare values. =============================================*/

extern bool values_equal( value_t value1, value_t value2 );
/* Return a truth value indicating whether VALUE1 and VALUE2 are equal.
 * VALUE1 an VALUE2 must be of same type or one of them must be nil.
 * Refer to documentation to see what "equal" in Malaga really means. */

extern bool value_in_value( value_t value1, value_t value2 );
/* Return bool value saying if VALUE1 is element or attribute of VALUE2.
 * VALUE2 must be a list or a record.
 * If VALUE2 is a record, then VALUE1 must be a symbol. */

}}}

#endif
