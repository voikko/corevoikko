/* Copyright (C) 1995 Bjoern Beutel. */

/* Description. =============================================================*/

/* This file contains basic types, macros and functions used everywhere. */

#include <glib.h>

namespace libvoikko { namespace morphology { namespace malaga {

/* Constants. ===============================================================*/

#undef NULL
#define NULL 0 /* Null pointer. */

enum {BITS_PER_BYTE = 8};

/* Basic types. =============================================================*/

/* Numeric types. */
typedef signed char byte_t; /* Signed 8 bits. */
typedef unsigned char u_byte_t; /* Unsigned 8 bits. */
typedef signed short short_t; /* Signed 16 bits. */
typedef unsigned short u_short_t; /* Unsigned 16 bits. */
typedef signed int int_t; /* Signed 32 bits. */
typedef unsigned int u_int_t; /* Unsigned 32 bits. */
typedef unsigned long ptr_t; /* Pointer in arithmetic expressions. */

/* Character types. */
typedef char char_t; /* A single char. */
typedef const char_t *string_t; /* A constant EOS-terminated C string. */
enum {EOS= '\0'}; /* End-Of-String control character. */


/* Forward-linked lists. ====================================================*/
                                              
typedef struct list_node /* A node in a list of nodes. */
{ 
  struct list_node *next; /* Next list node. */
} list_node_t;

typedef struct /* A list of nodes. */
{ 
  list_node_t *first, *last; /* The first and last element in the list. */
} list_t;

typedef enum {LIST_START, LIST_END} position_t;
/* The position where a new element is added to a list. */

extern void clear_list( list_t *list );
/* Initialise LIST to be an empty list */

extern void add_node( list_t *list, list_node_t *node, position_t position );
/* Add NODE to LIST.
 * If POSITION = LIST_START, add it at the start of the list;
 * If POSITION = LIST_END, add it at the end. */

extern void insert_node( list_t *list, list_node_t *node, list_node_t *prev );
/* Insert NODE in LIST, behind PREV.
 * If PREV == NULL, insert NODE at the beginning of the list. */

extern void *remove_first_node( list_t *list );
/* Remove the first node in LIST and return it. */

extern void remove_node( list_t *list, list_node_t *node );
/* Remove NODE in LIST. */

extern void *new_node( list_t *list, int_t size, position_t position );
/* Add a node of size SIZE to LIST.
 * If POSITION = LIST_START, add the element at the start of the list;
 * If POSITION = LIST_END, add the element at the end. 
 * Return the newly created node. */

extern void free_first_node( list_t *list );
/* Remove first node in LIST and free it. */

/* Iterate through a "list_t". */
#define FOREACH(var, list, type) \
  for ((var) = (type *) (list).first; \
       (var) != NULL; \
       (var) = (type *) (var)->next)

/* Memory functions. ========================================================*/

extern void *new_mem( int_t item_size );
/* Allocate a memory block of ITEM_SIZE bytes, clear it and return it.
 * If memory is out, call the function "error". */

extern void *new_vector( int_t item_size, int_t item_count );
/* Allocate a memory block to contain ITEM_COUNT items of size ITEM_SIZE,
 * clear it and return it.
 * If memory is out, call the function "error". */

extern int_t renew_vector( void *block_p, int_t item_size, int_t item_count );
/* Realloc *BLOCK_P to contain ITEM_COUNT items of ITEM_SIZE bytes each.
 * Return ITEM_COUNT.
 * If memory is out, call the function "error". */

extern void free_mem( void *pointer );
/* Free memory *POINTER points to, and set *POINTER to NULL. */

/* String functions. ========================================================*/

extern char_t *new_string( string_t string, string_t end );
/* Allocate memory and copy STRING into it.
 * If END != NULL, it marks the end of the string.
 * The result string must be freed after use. */

extern char_t *concat_strings( string_t first_string, ... );
/* Concatenate a list of strings and return the result string.
 * Must have NULL-terminated list of strings as parameters.
 * The result string must be freed after use. */

/* Text functions. ==========================================================*/

/* A data structure that contains a string that may grow indefinitely. */
typedef struct
{
  char_t *buffer;
  int_t buffer_size;
  int_t string_size;
} text_t;

extern text_t *new_text( void );
/* Return a new text structure. */

extern void free_text( text_t **text_p );
/* Free the content of *TEXT_P. */

extern void add_to_text( text_t *text, string_t string );
/* Add STRING to TEXT. */

extern void add_char_to_text( text_t *text, char_t character );
/* Add CHARACTER to TEXT. */

extern char_t *text_to_string( text_t **text_p );
/* Return content of *TEXT_P as a string and delete *TEXT_P.
 * The string must be freed after use. */

}}}
