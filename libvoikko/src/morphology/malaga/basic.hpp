/* Copyright (C) 1995 Bjoern Beutel. */

/* Description. =============================================================*/

/* This file contains basic types, macros and functions used everywhere. */

#include <glib.h>

namespace libvoikko { namespace morphology { namespace malaga {

/* Constants. ===============================================================*/

#undef NULL
#define NULL 0 /* Null pointer. */

enum {BITS_PER_BYTE = 8};

/* Attribute for a function that never returns. */
#ifdef __GNUC__
#define NO_RETURN __attribute__((noreturn))
#else
#define NO_RETURN
#endif

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
#define ORD(c) ((u_byte_t) (c)) /* The ordinal number of character C. */

/* Macros. ==================================================================*/

#undef MIN
#define MIN(a,b) ((a) < (b) ? (a) : (b)) /* Minimum of A and B. */
#undef MAX
#define MAX(a,b) ((a) > (b) ? (a) : (b)) /* Maximum of A and B. */
#undef ABS
#define ABS(a) ((a) >= 0 ? (a) : (-a)) /* Absolute value of A. */

#define ARRAY_LENGTH(a) (sizeof(a) / sizeof( (a)[0] ))

/* Read-only variables. =====================================================*/

extern char_t malaga_version[];
extern string_t program_name; /* This is set by "init_basic". */

/* Program message. =========================================================*/

extern void program_message( void );
/* Print some information about the program. */

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

extern void combine_lists( list_t *list1, list_t *list2 );
/* Append LIST2 to LIST1.
 * LIST1 will contain the concatenation; LIST2 will be empty. */

extern void *new_node( list_t *list, int_t size, position_t position );
/* Add a node of size SIZE to LIST.
 * If POSITION = LIST_START, add the element at the start of the list;
 * If POSITION = LIST_END, add the element at the end. 
 * Return the newly created node. */

extern void free_first_node( list_t *list );
/* Remove first node in LIST and free it. */

extern void free_node( list_t *list, list_node_t *node );
/* Remove NODE from LIST and free it. */

/* Iterate through a "list_t". */
#define FOREACH(var, list, type) \
  for ((var) = (type *) (list).first; \
       (var) != NULL; \
       (var) = (type *) (var)->next)

/* Iterate through a "list_t" and free every node in it. */
#define FOREACH_FREE(var, list, type) \
  for ((var) = (type *) (list).first; \
       (var) != NULL; \
       (var) = (type *) (var)->next, free_first_node( &(list) ))

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

extern char_t *new_string_readable( string_t from, string_t from_end );
/* Like "new_string", but copy a "\" in front of quotes
 * and copy any control chars in octal code: "\000". 
 * If FROM_END != NULL, it marks the end of the string. 
 * The result string must be freed after use. */

extern char_t *concat_strings( string_t first_string, ... );
/* Concatenate a list of strings and return the result string.
 * Must have NULL-terminated list of strings as parameters.
 * The result string must be freed after use. */

extern string_t next_non_space( string_t string );
/* Return STRING, but without leading spaces. */

extern char_t *double_to_string( double number );
/* Convert NUMBER to a string. The string must be freed after use. */

extern char_t *int_to_string( int_t number );
/* Convert NUMBER to a string. The string must be freed after use. */

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

extern void clear_text( text_t *text );
/* Initialize TEXT to an empty string. */

extern void free_text( text_t **text_p );
/* Free the content of *TEXT_P. */

extern void add_to_text( text_t *text, string_t string );
/* Add STRING to TEXT. */

extern void add_char_to_text( text_t *text, char_t character );
/* Add CHARACTER to TEXT. */

void add_unichar_to_text( text_t *text, gunichar c );
/* Add C to TEXT. */

extern void insert_in_text( text_t *text, string_t string, int_t position );
/* Insert STRING at POSITION in TEXT (position counts from 0 onward). */

extern void insert_char_in_text( text_t *text, 
				 char_t character, 
                                 int_t position );
/* Insert CHARACTER at POSITION in TEXT. */

extern char_t *text_to_string( text_t **text_p );
/* Return content of *TEXT_P as a string and delete *TEXT_P.
 * The string must be freed after use. */

/* Add CHARACTER to TEXT (macro version). */
#define ADD_CHAR_TO_TEXT( text, character ) \
do { \
  if (text->buffer_size < text->string_size + 2) \
  { \
    text->buffer_size = renew_vector( &text->buffer, sizeof( char_t ), \
				      2 * (text->string_size + 1) ); \
  } \
  text->buffer[ text->string_size++ ] = character; \
  text->buffer[ text->string_size ] = EOS; \
} while (false)

/* Error handlers. ==========================================================*/

/* The syntax of an error handler is:
 * TRY STATEMENT1
 * IF_ERROR STATEMENT2
 * FINALLY STATEMENT3
 * END_TRY;
 *
 * The parts "IF_ERROR STATEMENT2" and "FINALLY STATEMENT3" are optional.
 *
 * First, STATEMENT1 is executed.
 * If the function "error" is called in STATEMENT1 or in a function 
 * called from there, STATEMENT2 and then STATEMENT3 will be executed
 * (if they exist).
 * If the function "error" is not called, STATEMENT3 will executed.
 * In STATEMENT2, you can use the statement "RESUME" to leave the error
 * state. */

#define TRY \
do { \
  jmp_buf error_handler, *older_error_handler = current_error_handler; \
  volatile bool rethrow; \
  current_error_handler = &error_handler; \
  if (! (rethrow = setjmp( error_handler )))

#define IF_ERROR else
#define FINALLY /* Nothing. */

#define END_TRY \
  current_error_handler = older_error_handler; \
  if (rethrow) \
    malaga_throw(); \
} while (false)

#define RESUME rethrow = false

extern text_t *error_text; /* The text of the last error. */

extern jmp_buf *current_error_handler; /* The active innermost error handler */

extern void malaga_throw( void ) NO_RETURN;
/* Call the current error handler. 
 * If there is no current error handler, print error and exit. */

extern void complain( string_t message, ... ) NO_RETURN;
/* Save the error MESSAGE in ERROR_TEXT.
 * Additional arguments to "complain" are inserted where 
 * "%s" (string_t ARGUMENT), "%c" (char_t ARGUMENT), "%u" (gunichar ARGUMENT),
 * or "%d" (int_t ARGUMENT) is part of MESSAGE. */

/* Module initialisation. ===================================================*/

extern void init_basic( string_t prog_name );
/* Initialise this module. PROG_NAME should be the name of the program. */

extern void terminate_basic( void );
/* Terminate this module. */

}}}
