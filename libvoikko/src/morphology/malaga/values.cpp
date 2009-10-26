/* Copyright (C) 1995 Bjoern Beutel. */

/* Description. =============================================================*/

/* This module defines the data type "value_t", and many
 * operations to build, modify, and print such values.
 * The first cell of a value, its type cell, is used to store the value's type
 * together with some type dependent information, which is an unsigned number
 * less than INFO_MAX.
 * Use the macro TYPE to get the type of a value, and INFO to get the type 
 * dependent information. Use TYPE_CELL to create a type-cell.
 * There are five different types of values:
 * symbol, string, list, record and number. */

/* Includes. ================================================================*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "setup/DictionaryException.hpp"
#include "morphology/malaga/basic.hpp"
#include "morphology/malaga/pools.hpp"
#include "morphology/malaga/values.hpp"
#include "morphology/malaga/symbols.hpp"

namespace libvoikko { namespace morphology { namespace malaga {

/* Constants. ===============================================================*/

#define CELL_BITS BITS_PER_BYTE * sizeof( cell_t )
#define TYPE_BITS 3
#define INFO_BITS (CELL_BITS - TYPE_BITS)
#define TYPE_MAX ((cell_t) 1 << TYPE_BITS)
#define INFO_MAX ((cell_t) 1 << INFO_BITS)
#define TYPE_MASK ((TYPE_MAX - 1) << INFO_BITS)
#define INFO_MASK (INFO_MAX - 1)

#define SYMBOL_TYPE ((cell_t) 0 << INFO_BITS) 
/* A value of type SYMBOL_TYPE consists only of a type cell. 
 * Its INFO-value is the code for the symbol. */

#define STRING_TYPE ((cell_t) 1 << INFO_BITS) 
/* A value of type STRING_TYPE consists of its type cell,
 * followed by the actual string. Its INFO-value is the
 * number of characters in the string. The actual string
 * is stored in the subsequent cells, two characters
 * paired in a cell, and terminated by one or two
 * NUL-characters, so that the total number of chars is
 * even. The NUL-chars are NOT part of the string. */

#define LIST_TYPE ((cell_t) 2 << INFO_BITS)
/* A value of type LIST_TYPE consists of its type cell and a subsequent cell,
 * which hold the type and the length of the value, followed by 0 or more
 * values of any type, namely the values that form the list. 
 * The length of a list VALUE, that is the number of cells needed to store 
 * all the list's values, is (INFO( value[0] ) << CELL_BITS) + value[1]. */

#define RECORD_TYPE ((cell_t) 3 << INFO_BITS)
/* A value of type RECORD_TYPE consists of its type cell and a subsequent cell,
 * which hold the type and the length of the value, followed by 0 or more 
 * pairs of values. 
 * In a pair of values, the first value must be a symbol and is considered as 
 * an attribute of that record. The second value is the value of that
 * attribute, and it can be of any type.
 * The length of a record VALUE, that is the number of cells
 * needed to store all the record's value pairs, is computed as
 * (INFO( value ) << CELL_BITS) + value[1]. */

#define NUMBER_TYPE ((cell_t) 4 << INFO_BITS)
/* A value of type NUMBER_TYPE consists of its type cell,
 * followed by a implementation-dependent number of cells
 * that contain a C "double" value.
 * Its INFO-value is 0. */

/* Macros. ==================================================================*/

#define TYPE(value) ((*(value)) & TYPE_MASK)
#define INFO(value) ((*(value)) & INFO_MASK)
#define TYPE_CELL(type,info) ((type) | (info))

/* Use one of the following predicates to test a value
 * against a specific type. */
#define IS_SYMBOL(value) (TYPE( value ) == SYMBOL_TYPE)
#define IS_STRING(value) (TYPE( value ) == STRING_TYPE)
#define IS_RECORD(value) (TYPE( value ) == RECORD_TYPE)
#define IS_LIST(value) (TYPE( value ) == LIST_TYPE)
#define IS_NUMBER(value) (TYPE( value ) == NUMBER_TYPE)

#define NEXT_VALUE(value) ((value) + length_of_value( value ))
/* Return end of VALUE.
 * This may also be the beginning of the next value in a list. */

#define NEXT_ATTRIB(attrib) ((attrib) + 1 + length_of_value( (attrib) + 1 ))
/* Return the next attribute in a record. */

#define CELLS_PER_NUMBER ((int_t) (sizeof( double ) / sizeof( cell_t )))
/* The number of cells needed to contain a number value.
 * sizeof( double ) *must* be a multiple of sizeof( cell_t ). */

/* Types. ===================================================================*/

typedef struct /* An element in a list of hidden attributes. */
{
  list_node_t *next;
  symbol_t symbol;
} attribute_t;

/* Global variables. ========================================================*/

value_t *value_stack;
int_t top;

/* Variables. ===============================================================*/

static cell_t *value_heap; /* The actual heap. FIXME */
static cell_t *value_heap_end; /* Pointer to first free cell in heap. FIXME */
static int_t value_heap_size; /* Size of the value heap in cells. FIXME */

static int_t value_stack_size; /* Size of the value stack. FIXME */

/* Support functions. =======================================================*/

static void 
copy_cells( value_t destination, value_t source, int_t n )
/* Copy N cells of value SOURCE to DESTINATION. */
{
  memcpy( destination, source, n * sizeof( cell_t ) );
}

/*---------------------------------------------------------------------------*/

static void 
copy_value( value_t destination, value_t source )
/* Copy all cells of value SOURCE to DESTINATION. */
{
  memcpy( destination, source, length_of_value( source ) * sizeof( cell_t ) );
}

/*---------------------------------------------------------------------------*/

static int 
compare_value_pointers( const void *key1, const void *key2 )
/* Return -1/0/1 when the value VALUE1_P points to is stored on a
 * lower/same/higher address than the value VALUE2_P points to. */
{
  value_t *value1_p;
  value_t *value2_p;

  value1_p = * (value_t **) key1;
  value2_p = * (value_t **) key2;

  if (*value1_p < *value2_p) 
    return -1;
  else if (*value1_p > *value2_p) 
    return 1;
  else
    return 0;
}

/*---------------------------------------------------------------------------*/

static void 
collect_garbage( void )
/* Make sure the value heap only contains values that are on the value stack.
 * Compactify the heap, i.e. move all values on the heap to the beginning. */
{
  value_t old_value, new_value;
  value_t **value_pointer;

  new_value = value_heap;

  /* Copy values if there is at least one value to save. */
  if (top > 0) 
  { 
    /* Create a table of pointers to the values. */
    value_pointer = (cell_t ***) new_vector( sizeof( value_t * ), top );
    for (int_t i = 0; i < top; i++) { 
      value_pointer[i] = value_stack + i;
    }

    /* Sort pointers according to the address of the value they point to. */
    qsort( value_pointer, top, sizeof( value_t * ), compare_value_pointers );

    /* Find the first index I whose value is on the heap. */
    int_t i;
    for (i = 0; i < top; i++) { 
      if (*value_pointer[i] >= value_heap) 
	break;
    }

    /* Work on all values on the heap. */
    while (i < top && *value_pointer[i] < value_heap_end) 
    { 
      /* Copy the value. */
      old_value = *value_pointer[i];
      int_t value_len = length_of_value( old_value );
      memmove( new_value, old_value, value_len * sizeof( cell_t ) );

      /* Adjust the value address and the addresses of all values
       * that are part of that value. */
      while (i < top && *value_pointer[i] < old_value + value_len) 
      { 
	*value_pointer[i] -= (old_value - new_value);
        i++;
      }
      new_value += value_len;
    }
    free_mem( &value_pointer );
  }
  value_heap_end = new_value;
}

/*---------------------------------------------------------------------------*/

static value_t 
space_for_value( int_t size )
/* Get SIZE adjacent free cells on the value heap. */
{
  value_t pointer, old_heap, old_heap_end;
  int_t i;

  if ((value_heap_end - value_heap) + size > value_heap_size) 
  { 
    collect_garbage();
    if ((value_heap_end - value_heap) + size > value_heap_size) 
    { 
      old_heap = value_heap;
      old_heap_end = value_heap_end;

      /* Enlarge the value heap. */
      value_heap_size = renew_vector( &value_heap, sizeof( cell_t ),
                                      2 * (size + (old_heap_end - old_heap)) );
      value_heap_end = value_heap + (old_heap_end - old_heap);

      /* Adapt the value stack pointers. */
      for (i = 0; i < top; i++) 
      { 
	if (value_stack[i] >= old_heap && value_stack[i] < old_heap_end) 
	  value_stack[i] = value_heap + (value_stack[i] - old_heap);
      }
    }
  }
  pointer = value_heap_end;
  value_heap_end += size;
  return pointer;
}

/*---------------------------------------------------------------------------*/

static value_t 
space_for_composed_value( int_t type, int_t length )
/* Allocate LENGTH cells for a composed value of TYPE, set its type cell
 * and return the value. */
{
  value_t value;
  int_t content_size;

  value = space_for_value( length );
  content_size = length - 2;
  value[0] = TYPE_CELL( type, content_size >> CELL_BITS );
  value[1] = content_size & ((1L << CELL_BITS) - 1);
  return value;
}

/* Module initialisation. ===================================================*/

void 
init_values( void )
/* Initialise this module. */
{
  value_heap_size = 1000;
  value_heap = (cell_t *) new_vector( sizeof( cell_t ), value_heap_size );
  value_heap_end = value_heap;
  value_stack_size = 100;
  value_stack = (cell_t **) new_vector( sizeof( value_t ), value_stack_size );
  top = 0;
}

/*---------------------------------------------------------------------------*/

void 
terminate_values( void )
/* Terminate this module. */
{
  free_mem( &value_heap );
  free_mem( &value_stack );
}

/* Value operations. ========================================================*/
 
value_t 
new_value( value_t value )
/* Allocate space for VALUE and copy it.
 * Free the value space after use. */
{
  value_t value2;

  value2 = (cell_t *) new_vector( sizeof( cell_t ), length_of_value( value ) );
  copy_value( value2, value );
  return value2;
}

/*---------------------------------------------------------------------------*/

value_t 
copy_value_to_pool( pool_t value_pool, value_t value, int_t *index )
/* Copy VALUE to the pool VALUE_POOL and store its index in *INDEX. */
{
  value_t value2;

  value2 = (cell_t *) get_pool_space( value_pool, length_of_value( value ), index );
  copy_value( value2, value );
  return value2;
}

/*---------------------------------------------------------------------------*/

int_t 
length_of_value( value_t value )
/* Return the length of VALUE in cells. */
{
  switch (TYPE( value )) 
  {
  case LIST_TYPE: 
  case RECORD_TYPE:
    return 2 + ((int_t) INFO( value ) << CELL_BITS) + value[1];
  case SYMBOL_TYPE: 
    return 1;
  case STRING_TYPE: 
    return 2 + INFO( value ) / sizeof( cell_t );
  case NUMBER_TYPE: 
    return 1 + CELLS_PER_NUMBER;
  default:
    throw setup::DictionaryException("Unexpected value type in length_of_value");
  }
}

/*---------------------------------------------------------------------------*/

symbol_t 
get_value_type( value_t value )
/* Return the type of VALUE. Depending of the type, the result value may be
 * SYMBOL_SYMBOL, STRING_SYMBOL, NUMBER_SYMBOL, LIST_SYMBOL, RECORD_SYMBOL. */
{
  switch (TYPE( value )) 
  {
  case SYMBOL_TYPE: 
    return SYMBOL_SYMBOL;
  case STRING_TYPE: 
    return STRING_SYMBOL;
  case NUMBER_TYPE: 
    return NUMBER_SYMBOL;
  case LIST_TYPE: 
    return LIST_SYMBOL;
  case RECORD_TYPE: 
    return RECORD_SYMBOL;
  default:
    throw setup::DictionaryException("Unexpected value type in get_value_type");
  }
}

/*---------------------------------------------------------------------------*/

void 
push_value( value_t value )
/* Stack effects: (nothing) -> VALUE. */
{
  if (top + 1 > value_stack_size) 
  { 
    value_stack_size = renew_vector( &value_stack, sizeof( value_t ), 
				     2 * (top + 1) );
  }
  value_stack[ top++ ] = value;
}

/*---------------------------------------------------------------------------*/

void 
insert_value( int_t n, value_t value )
/* Stack effects: VALUE1...VALUE_N -> VALUE VALUE1...VALUE_N. */
{
  int_t i;

  push_value( NULL );
  for (i = 0; i < n; i++) 
    value_stack[ top - i - 1 ] = value_stack[ top - i - 2 ];
  value_stack[ top - n - 1 ] = value;
}

/* Symbol operations. =======================================================*/

symbol_t 
value_to_symbol( value_t value )
/* Return VALUE as a symbol. It is an error if VALUE is no symbol. */
{
  return *value;
}

/*---------------------------------------------------------------------------*/

void 
push_symbol_value( symbol_t symbol )
/* Stack effects: (nothing) -> NEW_SYMBOL.
 * NEW_SYMBOL is SYMBOL converted to a Malaga value. */
{
  value_t value;

  value = space_for_value(1);
  *value = TYPE_CELL( SYMBOL_TYPE, symbol );
  push_value( value );
}

/* String operations. =======================================================*/

string_t 
value_to_string( value_t value )
/* Return the value of STRING as a C style string. */
{
  return (string_t) (value + 1);
}

/*---------------------------------------------------------------------------*/

void 
push_string_value( string_t string_start, string_t string_end )
/* Stack effects: (nothing) -> NEW_STRING.
 * NEW_STRING is the string starting at STRING_START as a Malaga value.
 * If STRING_END != NULL, it marks the end of the string. */
{
  value_t value, value_end;
  int_t length;
  string_t source_p;
  char_t *target_p;

  if (string_end == NULL) 
    string_end = string_start + strlen( string_start );
  length = string_end - string_start;
  value = space_for_value( 2 + length / sizeof( cell_t ) );
  *value = TYPE_CELL( STRING_TYPE, length );

  /* Copy the string content. */
  source_p = string_start;
  target_p = (char_t *) (value + 1);
  value_end = NEXT_VALUE( value );
  while (source_p < string_end) 
    *target_p++ = *source_p++;

  /* Pad with EOS. */
  while (target_p < (string_t) value_end) 
    *target_p++ = EOS;

  push_value( value );
}

/*---------------------------------------------------------------------------*/

void 
concat_string_values( void )
/* Stack effects: STRING1 STRING2 -> NEW_STRING.
 * NEW_STRING is the concatenation of STRING1 and STRING2. */
{
  int_t new_length;
  string_t old_string, string_end;
  char_t *string;
  value_t string_value;

  new_length = ((int_t) INFO( value_stack[ top - 2 ] ) 
                + (int_t) INFO( value_stack[ top - 1 ] ));
  string_value = space_for_value( 2 + new_length / sizeof( cell_t ) );
  *string_value = TYPE_CELL( STRING_TYPE, new_length );

  /* Join the strings. We do it by hand so it's easier to align. */
  string = (char_t *) (string_value + 1);
  old_string = (string_t) (value_stack[ top - 2 ] + 1);
  while (*old_string != '\0') 
    *string++ = *old_string++;
  old_string = (string_t) (value_stack[ top - 1 ] + 1);
  while (*old_string != '\0') 
    *string++ = *old_string++;
  string_end = (string_t) NEXT_VALUE( string_value );
  while (string < string_end) 
    *string++ = '\0';

  top--;
  value_stack[ top - 1 ] = string_value;
}

/* Record operations. =======================================================*/

value_t 
get_attribute( value_t record, symbol_t attribute )
/* Return the value of ATTRIBUTE in the record RECORD 
 * or NULL if it doesn't exist. */
{
  value_t record_end, v;
 
  /* No error when getting an attribute from "nil". */
  if (*record == NIL_SYMBOL) 
    return NULL;
  record_end = NEXT_VALUE( record );
  for (v = record + 2; v < record_end; v = NEXT_ATTRIB(v)) 
  { 
    if (*v == attribute) 
      return v + 1;
  }
  return NULL;
}

/*---------------------------------------------------------------------------*/

void 
build_record( int_t n )
/* Stack effects: ATTR1 VALUE1 ... ATTR_N VALUE_N -> NEW_RECORD.
 * NEW_RECORD looks like [ATTR1: VALUE1, ..., ATTR_N: VALUE_N]. */
{
  value_t new_record, v;
  int_t i, new_record_length;
  value_t *values;

  values = value_stack + top - 2 * n;

  /* Compute record length. */
  new_record_length = 2;
  for (i = 0; i < n; i++) 
    new_record_length += 1 + length_of_value( values[ 2 * i + 1 ] );

  /* Allocate new record and copy content. */
  new_record = space_for_composed_value( RECORD_TYPE, new_record_length );
  v = new_record + 2;
  for (i = 0; i < n; i++) 
  { 
    *v++ = *values[ 2 * i ];
    copy_value( v, values[ 2 * i + 1 ] );
    v = NEXT_VALUE(v);
  }

  top -= 2 * n;
  push_value( new_record );
}

/*---------------------------------------------------------------------------*/

void 
join_records( void )
/* Stack effects: RECORD1 RECORD2 -> NEW_RECORD.
 * NEW_RECORD contains all attributes of RECORD1 and RECORD2, and 
 * their associated values. If an attribute has different values in RECORD1
 * and RECORD2, the value in RECORD2 will be taken. */
{
  value_t record1, record2, record1_end, record2_end, new_record, v, v1, v2;
  int_t new_record_length;

  record1 = value_stack[ top - 2 ];
  record2 = value_stack[ top - 1 ];
  record1_end = NEXT_VALUE( record1 );
  record2_end = NEXT_VALUE( record2 );

  /* Calculate the space needed. This is the length of the
   * first record plus the length of the second record minus the
   * sum of the length of all attribute-value-pairs in RECORD1 whose
   * attributes are also in RECORD2. */
  new_record_length
    = length_of_value( record1 ) + length_of_value( record2 ) - 2;
  for (v1 = record1 + 2; v1 < record1_end; v1 = NEXT_ATTRIB( v1 )) 
  { 
    for (v2 = record2 + 2; v2 < record2_end; v2 = NEXT_ATTRIB( v2 )) 
    { 
      if (*v1 == *v2) /* We've discovered two identical attributes */
      { 
	new_record_length -= (1 + length_of_value( v1 + 1 ));
        break;
      }
    }
  }

  /* Allocate a new record value. */
  new_record = space_for_composed_value( RECORD_TYPE, new_record_length );

  /* The values on stack may have moved if garbage collection was called. */
  record1 = value_stack[ top - 2 ];
  record2 = value_stack[ top - 1 ];
  record1_end = NEXT_VALUE( record1 );
  record2_end = NEXT_VALUE( record2 );

  /* Copy the attributes of the first record. If an attribut
   * belongs to both VALUE1 and VALUE2, don't copy its value. */
  v = new_record + 2;
  for (v1 = record1 + 2; v1 < record1_end; v1 = NEXT_ATTRIB( v1 )) 
  { 
    /* Go through RECORD2 until we reach end or find same attribute. */
    for (v2 = record2 + 2; v2 < record2_end; v2 = NEXT_ATTRIB( v2 )) 
    { 
      if (*v1 == *v2) 
	break;
    }
    if (v2 >= record2_end) 
    { 
      /* If attrib not in RECORD2, copy the value of RECORD1. */
      *v = *v1;
      copy_value( v + 1, v1 + 1 );
      v = NEXT_ATTRIB(v);
    }
  }

  /* Append the attributes of the second record. */
  copy_cells( v, record2 + 2, length_of_value( record2 ) - 2 );

  /* Push new record on stack. */
  top--;
  value_stack[ top - 1 ] = new_record;
}

/*---------------------------------------------------------------------------*/

void 
remove_attribute( symbol_t attribute )
/* Stack effects: RECORD -> NEW_RECORD.
 * NEW_RECORD contains all attribute-value pairs of RECORD but the one with
 * attribute ATTRIBUTE. */
{
  value_t record, new_record, record_end, v, v1;
  int_t new_record_length;

  record = value_stack[ top - 1 ];
  record_end = NEXT_VALUE( record );

  /* Find the attribute that is to be deleted. */
  for (v1 = record + 2; v1 < record_end; v1 = NEXT_ATTRIB( v1 )) 
  { 
    if (*v1 == attribute) 
      break;
  }

  /* Check if we have an attribute to delete. */
  if (v1 == record_end) 
    new_record = record;
  else 
  { 
    /* Compute its length and get space for the new record. */
    new_record_length 
      = length_of_value( record ) - (length_of_value( v1 + 1 ) + 1);
    new_record = space_for_composed_value( RECORD_TYPE, new_record_length );

    /* Get the original record. */
    record = value_stack[ top - 1 ];
    record_end = NEXT_VALUE( record );

    /* Copy the record. */
    v = new_record + 2;
    for (v1 = record + 2; v1 < record_end; v1 = NEXT_ATTRIB( v1 )) 
    { 
      if (*v1 != attribute) 
      { 
	*v = *v1;
        copy_value( v + 1, v1 + 1 );
        v = NEXT_ATTRIB(v);
      }
    }
  }
  value_stack[ top - 1 ] = new_record;
}

/*---------------------------------------------------------------------------*/

void 
remove_attributes( void )
/* Stack effects: RECORD LIST -> NEW_RECORD.
 * NEW_RECORD contains all attribute-value pairs of RECORD but the ones
 * whose attributes are in LIST. */
{
  value_t v, v1, v2, record, list, record_end, list_end, new_record;
  int_t new_record_length;

  record = value_stack[ top - 2 ];
  list = value_stack[ top - 1 ];
  record_end = NEXT_VALUE( record );
  list_end = NEXT_VALUE( list );

  /* Compute the length of the new record. */
  new_record_length = 2;
  for (v1 = record + 2; v1 < record_end; v1 = NEXT_ATTRIB( v1 )) 
  { 
    for (v2 = list + 2; v2 < list_end; v2++) 
    { 
      if (*v1 == *v2) 
	break;
    }
    if (v2 == list_end) 
      new_record_length += 1 + length_of_value( v1 + 1 );
  }

  /* We don't create a new record if no attributes will be deleted. */
  if (new_record_length == length_of_value( record )) 
    new_record = record;
  else 
  { 
    new_record = space_for_composed_value( RECORD_TYPE, new_record_length );

    /* Get the values, since they may have moved by garbage collection. */
    record = value_stack[ top - 2 ];
    list = value_stack[ top - 1 ];
    record_end = NEXT_VALUE( record );
    list_end = NEXT_VALUE( list );

    /* Copy the other attributes. */
    v = new_record + 2;
    for (v1 = record + 2; v1 < record_end; v1 = NEXT_ATTRIB( v1 )) 
    { 
      for (v2 = list + 2; v2 < list_end; v2++) 
      { 
	if (*v1 == *v2) 
	  break;
      }
      if (v2 == list_end) 
      { 
	*v = *v1;
	copy_value( v + 1, v1 + 1 );
	v = NEXT_ATTRIB(v);
      }
    }
  }
  top--;
  value_stack[top - 1] = new_record;
}

/*---------------------------------------------------------------------------*/

void 
replace_attribute( symbol_t attribute )
/* Stack effects: RECORD VALUE -> NEW_RECORD.
 * NEW_RECORD is equal to RECORD, only the value of ATTRIBUTE is replaced
 * by VALUE. RECORD must contain ATTRIBUTE. */
{
  value_t record, value, record_end, new_record, v, nv;
  int_t new_record_length;

  record = value_stack[ top - 2 ];
  value = value_stack[ top - 1 ];
  record_end = NEXT_VALUE( record );

  /* Find the attribute to replace. */
  for (v = record + 2; v < record_end; v = NEXT_ATTRIB(v)) 
  { 
    if (*v == attribute) 
      break;
  }

  new_record_length = (length_of_value( record ) 
		       + length_of_value( value ) - length_of_value( v + 1 ));
  new_record = space_for_composed_value( RECORD_TYPE, new_record_length );

  record = value_stack[ top - 2 ];
  value = value_stack[ top - 1 ];
  record_end = NEXT_VALUE( record );

  nv = new_record + 2;
  for (v = record + 2; v < record_end; v = NEXT_ATTRIB(v)) 
  { 
    *nv = *v;
    if (*v == attribute) 
      copy_value( nv + 1, value );
    else 
      copy_value( nv + 1, v + 1 );
    nv = NEXT_ATTRIB( nv );
  }

  top--;
  value_stack[ top - 1 ] = new_record;
}

/* List operations. =========================================================*/

int_t 
get_list_length( value_t list )
/* Return the number of elements in LIST. 
 * LIST must be a list. */
{
  int_t elements;
  value_t list_end, v;

  list_end = NEXT_VALUE( list );
  elements = 0;
  for (v = list + 2; v < list_end; v = NEXT_VALUE(v)) 
    elements++;
  return elements;
}

/*---------------------------------------------------------------------------*/

value_t 
get_element( value_t list, int_t n )
/* Return the N-th element of the list LIST,
 * or NULL, if that element doesn't exist.
 * If N is positive, elements will be counted from the left border.
 * If it's negative, elements will be counted from the right border. */
{
  value_t list_end, v;
  
  /* No error when getting an element from "nil". */
  if (*list == NIL_SYMBOL) 
    return NULL;

  if (n < 0) 
    n = get_list_length( list ) + n + 1;
  if (n <= 0) 
    return NULL;
  list_end = NEXT_VALUE( list );
  for (v = list + 2; v < list_end; v = NEXT_VALUE(v)) 
  { 
    n--;
    if (n == 0) 
      return v;
  }
  return NULL;
}

/*---------------------------------------------------------------------------*/

void 
build_list( int_t n )
/* Stack effects: VALUE1 ... VALUE_N -> NEW_LIST.
 * NEW_LIST looks like <VALUE1, ..., VALUE_N>. */
{
  value_t new_list, v;
  int_t i, new_list_length;
  value_t *elements;

  elements = value_stack + top - n;
  new_list_length = 2;
  for (i = 0; i < n; i++) 
    new_list_length += length_of_value( elements[i] );
  new_list = space_for_composed_value( LIST_TYPE, new_list_length );
  v = new_list + 2;
  for (i = 0; i < n; i++) 
  { 
    copy_value( v, elements[i] );
    v = NEXT_VALUE( v );
  }
  top -= n;
  push_value( new_list );
}

/*---------------------------------------------------------------------------*/

void 
concat_lists( void )
/* Stack effects: LIST1 LIST2 -> NEW_LIST.
 * NEW_LIST is the concatenation of LIST1 and LIST2. */
{
  int_t list1_length, list2_length, new_list_length;
  value_t list1, list2, new_list;

  list1 = value_stack[ top - 2 ];
  list2 = value_stack[ top - 1 ];

  list1_length = length_of_value( list1 );
  list2_length = length_of_value( list2 );
  new_list_length = list1_length + list2_length - 2;
  new_list = space_for_composed_value( LIST_TYPE, new_list_length );

  list1 = value_stack[ top - 2 ];
  list2 = value_stack[ top - 1 ];
    
  /* Copy all elements of the first and the second list. */
  copy_cells( new_list + 2, list1 + 2, list1_length - 2 );
  copy_cells( new_list + list1_length, list2 + 2, list2_length - 2 );

  top--;
  value_stack[ top - 1 ] = new_list;
}

/*---------------------------------------------------------------------------*/

void 
get_list_difference( void )
/* Stack effects: LIST1 LIST2 -> NEW_LIST.
 * NEW_LIST contains the list difference of LIST1 and LIST2:
 * An element that appears M times in LIST1 and N times in LIST2 
 * appears M - N times in NEW_LIST. */
{
  value_t list1, list2, list1_end, list2_end, new_list, v, v1, v2;
  int_t new_list_length, appearances;

  list1 = value_stack[ top - 2 ];
  list2 = value_stack[ top - 1 ];
  list1_end = NEXT_VALUE( list1 );
  list2_end = NEXT_VALUE( list2 );

  /* Calculate the size of the new value. */
  new_list_length = 2;
  for (v1 = list1 + 2; v1 < list1_end; v1 = NEXT_VALUE( v1 )) 
  { /* Check whether V1 will be included in the list.
     * It will be included if the ordinal number of its appearance is 
     * higher than the number of appearances in LIST2. */
    
    /* Count appearences in LIST1 up to (including) V1. */
    appearances = 1;
    for (v2 = list1 + 2; v2 < v1; v2 = NEXT_VALUE( v2 )) 
    { 
      if (values_equal( v1, v2 )) 
	appearances++;
    }
    
    /* Subtract appearences in VALUE2. */
    for (v2 = list2 + 2; v2 < list2_end; v2 = NEXT_VALUE( v2 )) 
    { 
      if (values_equal( v1, v2 )) 
	appearances--;
    }
    
    if (appearances > 0) 
      new_list_length += length_of_value( v1 );
  }
  
  /* We don't create a new list if no elements will be deleted. */
  if (new_list_length == length_of_value( list1 )) 
    new_list = list1;
  else
  { 
    new_list = space_for_composed_value( LIST_TYPE, new_list_length );

    list1 = value_stack[ top - 2 ];
    list2 = value_stack[ top - 1 ];
    list1_end = NEXT_VALUE( list1 );
    list2_end = NEXT_VALUE( list2 );
  
    v = new_list + 2;
    for (v1 = list1 + 2; v1 < list1_end; v1 = NEXT_VALUE( v1 )) 
    { /* Check whether V1 will be included in the list.
       * It will be included if the ordinal number of its appearance is 
       * higher than the number of appearances in VALUE2. */
      
      /* Count appearences in VALUE1 up to (including) V1. */
      appearances = 1;
      for (v2 = list1 + 2; v2 < v1; v2 = NEXT_VALUE( v2 )) 
      { 
	if (values_equal( v1, v2 )) 
	  appearances++;
      }
      
      /* Subtract appearences in VALUE2. */
      for (v2 = list2 + 2; v2 < list2_end; v2 = NEXT_VALUE( v2 )) 
      { 
	if (values_equal( v1, v2 )) 
	  appearances--;
      }
      
      if (appearances > 0) 
      { 
	copy_value( v, v1 );
        v = NEXT_VALUE(v);
      }
    }
  }
  top--;
  value_stack[ top - 1 ] = new_list;
}

/*---------------------------------------------------------------------------*/

void 
remove_element( int_t n )
/* Stack effects: LIST -> NEW_LIST.
 * NEW_LIST is LIST without element at index N.
 * If N is positive, the elements will be counted from the left border;
 * if N is negative, they will be counted from the right border.
 * If LIST contains less than abs(N) elements, then NEW_LIST = LIST. */
{
  value_t list, list_end, new_list, element, v;
  int_t new_list_length;

  list = value_stack[ top - 1 ];

  /* Find the first/last value in the list that will/won't be copied. */
  element = get_element( list, n );
  if (element == NULL) 
    new_list = list;
  else
  { 
    new_list_length = length_of_value( list ) - length_of_value( element );
    new_list = space_for_composed_value( LIST_TYPE, new_list_length );

    /* Get the values again, since they may have moved. */
    list = value_stack[ top - 1 ];
    list_end = NEXT_VALUE( list );
    element = get_element( list, n );

    /* Copy the list. */
    v = new_list + 2;
    copy_cells( v, list + 2, element - (list + 2) );
    v += element - (list + 2);
    copy_cells( v, NEXT_VALUE( element ), list_end - NEXT_VALUE( element ) );
  }
  value_stack[ top - 1 ] = new_list;
}

/*---------------------------------------------------------------------------*/

void 
replace_element( int_t n )
/* Stack effects: LIST VALUE -> NEW_LIST.
 * NEW_LIST is LIST, but its N-th element is replaced by VALUE.
 * If N is negative, count from the right end.
 * LIST must contain at least N elements. */
{
  value_t list, value, new_list, element, nv;
  int_t new_list_length;

  /* Get arguments. */
  list = value_stack[ top - 2 ];
  value = value_stack[ top - 1 ];

  /* Check arguments. */
  element = get_element( list, n );
  new_list_length = (length_of_value( list ) +
                     length_of_value( value ) - length_of_value( element ));
  new_list = space_for_composed_value( LIST_TYPE, new_list_length );

  /* Get arguments again: they may have been moved by garbage collection. */
  list = value_stack[ top - 2 ];
  value = value_stack[ top - 1 ];
  element = get_element( list, n );

  /* Copy left part */
  nv = new_list + 2;
  copy_cells( nv, list + 2, element - (list + 2) );

  /* Copy changed element. */
  nv += element - (list + 2);
  copy_value( nv, value );

  /* Copy right part. */
  nv = NEXT_VALUE( nv );
  copy_cells( nv, NEXT_VALUE( element ), 
	      NEXT_VALUE( list ) - NEXT_VALUE( element ) );

  /* Push result on stack. */
  top--;
  value_stack[ top - 1 ] = new_list;
}

/* Number operations. =======================================================*/

double 
value_to_double( value_t value )
/* Return the value of VALUE which must be a number value. */
{
  int_t i;
  union
  {
    double number;
    cell_t cells[CELLS_PER_NUMBER];
  } v;

  for (i = 0; i < CELLS_PER_NUMBER; i++) 
    v.cells[i] = value[ i + 1 ];
  return v.number;
}

/*---------------------------------------------------------------------------*/

int_t 
value_to_int( value_t value )
/* Return the value of VALUE which must be an integral number value. */
{
  double number;
  int_t result;

  number = value_to_double( value );
  result = (int_t) number;
  return result;
}

/*---------------------------------------------------------------------------*/

void 
push_number_value( double number )
/* Stack effects: (nothing) -> NEW_NUMBER.
 * NEW_NUMBER is NUMBER as a Malaga value. */
{
  int_t i;
  value_t value;
  union
  {
    double number;
    cell_t cells[CELLS_PER_NUMBER];
  } v;

  v.number = number;
  value = space_for_value( 1 + CELLS_PER_NUMBER );
  *value = TYPE_CELL( NUMBER_TYPE, 0 );
  for (i = 0; i < CELLS_PER_NUMBER; i++) 
    value[ i + 1 ] = v.cells[i];
  push_value( value );
}

/* Type dependent Malaga operations. ========================================*/

static value_t 
get_value_part( value_t value, value_t path )
/* Return the value part of VALUE that is specified by the path PATH. 
 * If that value part does not exist, return NULL. */
{
  value_t part, path_end;
  
  path_end = NEXT_VALUE( path );
  for (part = path + 2; part < path_end; part = NEXT_VALUE( part )) 
  { 
    if (IS_SYMBOL( part )) 
      value = get_attribute( value, *part );
    else if (IS_NUMBER( part )) 
      value = get_element( value, value_to_int( part ) );
    if (value == NULL) 
      return NULL;
  }
  return value;
}

/*---------------------------------------------------------------------------*/

void 
dot_operation( void )
/* Stack effects: VALUE1 VALUE2 -> NEW_VALUE.
 * NEW_VALUE is VALUE1 "." VALUE2 or NULL, if that value doesn't exist.
 * The actual operation depends on the type of the values. */
{
  value_t value1, value2;

  value1 = value_stack[ top - 2 ];
  value2 = value_stack[ top - 1 ];
  switch (TYPE( value2 )) 
  {
  case SYMBOL_TYPE:
    top--;
    value_stack[ top - 1 ] = get_attribute( value1, 
					    value_to_symbol( value2 ) );
    break;
  case NUMBER_TYPE:
    top--;
    value_stack[ top - 1 ] = get_element( value1, value_to_int( value2 ) );
    break;
  case LIST_TYPE:
    top--;
    value_stack[ top - 1 ] = get_value_part( value1, value2 );
    break;
  }
}

/*---------------------------------------------------------------------------*/

void 
plus_operation( void )
/* Stack effects: VALUE1 VALUE2 -> NEW_VALUE.
 * NEW_VALUE is VALUE1 "+" VALUE2. 
 * The actual operation depends on the type of the values. */
{
  value_t value1, value2;

  value1 = value_stack[ top - 2 ];
  value2 = value_stack[ top - 1 ];
  switch (TYPE( value1 )) 
  {
  case STRING_TYPE:
    concat_string_values();
    break;
  case LIST_TYPE:
    concat_lists();
    break;
  case RECORD_TYPE:
    join_records();
    break;
  case NUMBER_TYPE:
    top -= 2;
    push_number_value( value_to_double( value1 ) + value_to_double( value2 ) );
    break;
  }
}

/*---------------------------------------------------------------------------*/

void 
minus_operation( void )
/* Stack effects: VALUE1 VALUE2 -> NEW_VALUE.
 * NEW_VALUE is VALUE1 "-" VALUE2. 
 * The actual operation depends on the type of the values. */
{
  value_t value1, value2;

  value1 = value_stack[ top - 2 ];
  value2 = value_stack[ top - 1 ];
  switch (TYPE( value1 )) 
  {
  case LIST_TYPE:
    switch (TYPE( value2 )) 
    {
    case NUMBER_TYPE:
      top--;
      remove_element( value_to_int( value2 ) ); 
      break;
    case LIST_TYPE:
      get_list_difference();
      break;
    }
    break;
  case RECORD_TYPE:
    switch (TYPE( value2 )) 
    {
    case SYMBOL_TYPE:
      top--;
      remove_attribute( value_to_symbol( value2 ) );
      break;
    case LIST_TYPE:
      remove_attributes();
      break;
    }
    break;
  case NUMBER_TYPE:
    top -= 2;
    push_number_value( value_to_double( value1 ) - value_to_double( value2 ) );
    break;
  }
}

/* Attribute path functions. ================================================*/

void 
build_path( int_t n )
/* Stack effects: VALUE1 ... VALUE_N -> NEW_LIST.
 * NEW_LIST is a path which contains VALUE1, ..., VALUE_N. 
 * VALUE1, ..., VALUE_N must be numbers, symbols or lists of numbers and 
 * symbols. If a value is a list, the elements of this list are inserted into
 * NEW_LIST instead of the value itself. */
{
  value_t new_list, v, element_end;
  int_t i, new_list_length;
  value_t *elements;

  elements = value_stack + top - n;
  new_list_length = 2;
  for (i = 0; i < n; i++) 
  { 
    switch (TYPE( elements[i] )) 
    {
    case LIST_TYPE:
      element_end = NEXT_VALUE( elements[i] );
      new_list_length += length_of_value( elements[i] ) - 2;
      break;
    case SYMBOL_TYPE:
    case NUMBER_TYPE:
      new_list_length += length_of_value( elements[i] );
      break;
    }
  }
  new_list = space_for_composed_value( LIST_TYPE, new_list_length );
  v = new_list + 2;
  for (i = 0; i < n; i++) 
  { 
    if (IS_LIST( elements[i] )) 
    { 
      copy_cells( v, elements[i] + 2, length_of_value( elements[i] ) - 2 );
      v += length_of_value( elements[i] ) - 2;
    } 
    else 
    { 
      copy_value( v, elements[i] );
      v = NEXT_VALUE(v);
    }
  }
  top -= n;
  push_value( new_list );
}

/*---------------------------------------------------------------------------*/

static void 
modify_value_part_local( void (*modifier)( void ), int_t value_index, 
                         int_t path_index )
/* Stack effects: VALUE PATH MOD_VALUE -> VALUE PATH NEW_VALUE.
 * NEW_VALUE is VALUE, but the part that is described by PATH is 
 * modified. PATH must be a list of symbols and numbers <E1, E2, .. , E_N>.
 * They will be used as nested attributes and indexes, so the part of VALUE
 * that is actually modified is OLD_VALUE := VALUE.E1.E2..E_N. 
 * If this part does not exist, an error will be reported. Else the function 
 * MODIFIER will be called on OLD_VALUE and MOD_VALUE. 
 * The value returned by MODIFIER will be entered in VALUE in place of
 * OLD_VALUE. */
{
  value_t value, subvalue, selector;
  int_t subvalue_index, index;
  symbol_t symbol;
  
  value = value_stack[ top - 3 ] + value_index;
  selector = get_element( value_stack[ top - 2 ], path_index );
  if (selector == NULL) /* No more selectors. */
  { 
    insert_value( 1, value );
    modifier();
  } 
  else /* Find attribute in VALUE. */
  { 
    if (IS_SYMBOL( selector ) ) 
    { 
      symbol = value_to_symbol( selector );
      subvalue = get_attribute( value, symbol );
    } 
    else if (IS_NUMBER( selector )) 
    { 
      index = value_to_int( selector );
      subvalue = get_element( value, index );
    }
    else {
      throw setup::DictionaryException("Unexpected selector type");
    }
    subvalue_index = subvalue - value_stack[ top - 3 ];

    /* Go down recursively */
    modify_value_part_local( modifier, subvalue_index, path_index + 1 );
    subvalue = value_stack[ top - 3 ] + subvalue_index;
    value = value_stack[ top - 3 ] + value_index;
    selector = get_element( value_stack[ top - 2 ], path_index );
    if (value_stack[ top - 1 ] == subvalue) 
      value_stack[ top - 1 ] = value;
    else if (IS_SYMBOL( selector )) 
    { 
      insert_value( 1, value );
      replace_attribute( value_to_symbol( selector ) );
    } 
    else 
    { 
      insert_value( 1, value );
      replace_element( value_to_int( selector ) );
    }
  }
}

/*---------------------------------------------------------------------------*/

void 
modify_value_part( void (*modifier)( void ) )
/* Stack effects: VALUE PATH MOD_VALUE -> NEW_VALUE.
 * NEW_VALUE is VALUE, but the part that is described by PATH is 
 * modified. PATH must be a list of symbols and numbers <E1, E2, .. , E_N>.
 * They will be used as nested attributes and indexes, so the part of VALUE
 * that is actually modified is OLD_VALUE := VALUE.E1.E2..E_N. 
 * If this part does not exist, an error will be reported. Else the function 
 * MODIFIER will be called on OLD_VALUE and MOD_VALUE. 
 * The value returned by MODIFIER will be entered in VALUE in place of
 * OLD_VALUE. */
{
  modify_value_part_local( modifier, 0, 1 );
  value_stack[ top - 3 ] = value_stack[ top - 1 ];
  top -= 2;
}

/*---------------------------------------------------------------------------*/

void 
right_value( void )
/* Stack effects: LEFT_VALUE RIGHT_VALUE -> RIGHT_VALUE.
 * A modifier for "modify_value_part". */
{
  top--;
  value_stack[ top - 1 ] = value_stack[ top ];
}

/* Functions for list/record iteration. =====================================*/

void 
get_first_element( void )
/* Stack effects: VALUE -> NEW_VALUE.
 * If VALUE is a list, then NEW_VALUE is its first element (or NULL).
 * If VALUE is a record, then NEW_VALUE is its first attribute (or NULL).
 * If VALUE is a number, then NEW_VALUE is NULL (if VALUE == 0),
 * 1 (if VALUE > 0) or -1 (if VALUE < 0). */
{
  value_t value;
  int_t limit;

  value = value_stack[ top - 1 ];
  top--;
  if (*value == NIL_SYMBOL) 
    push_value( NULL );
  else 
  { 
    switch (TYPE( value )) 
    {
    case RECORD_TYPE: 
    case LIST_TYPE:
      /* Return NULL if list or record is empty. */
      if (length_of_value( value ) == 2) 
	push_value( NULL );
      else 
	push_value( value + 2 );
      break;
    case NUMBER_TYPE:
      limit = value_to_int( value );
      if (limit > 0) 
	push_number_value( 1.0 );
      else if (limit < 0) 
	push_number_value( -1.0 );
      else 
	push_value( NULL );
      break;
    }
  }
}

/*---------------------------------------------------------------------------*/

void 
get_next_element( int_t index )
/* Stack effects: (nothing) -> (nothing).
 * VALUE is VALUE_STACK[ INDEX - 1 ], ELEMENT is VALUE_STACK[ INDEX ].
 * VALUE_STACK[ INDEX ] will be set to NEW_ELEMENT.
 * ELEMENT must be the result of an application of "get_first_element()" or 
 * "get_next_element()" on VALUE.
 * If VALUE is a list, and ELEMENT one of its elements,
 * then NEW_ELEMENT is the successor of ELEMENT (or NULL).
 * If VALUE is a record, and ELEMENT one of its attributes,
 * then NEW_ELEMENT is the next attribute in VALUE (or NULL).
 * If VALUE is a positive number, and ELEMENT a number smaller than
 * VALUE, then NEW_ELEMENT is ELEMENT + 1.
 * If VALUE is a negative number, and ELEMENT a number greater than
 * VALUE, then NEW_ELEMENT is ELEMENT - 1. */
{
  value_t value, element;
  int_t number, limit;

  value = value_stack[ index - 1 ];
  element = value_stack[ index ];
  if (element == NULL) 
    return;
  switch (TYPE( value )) 
  {
  case RECORD_TYPE:
    element = NEXT_ATTRIB( element );
    if (element >= NEXT_VALUE( value )) 
      element = NULL;
    break;
  case LIST_TYPE:
    element = NEXT_VALUE( element );
    if (element >= NEXT_VALUE( value )) 
      element = NULL;
    break;
  case NUMBER_TYPE:
    limit = value_to_int( value );
    number = value_to_int( element );
    if (limit > 0 && number < limit) 
    { 
      push_number_value( number + 1 );
      element = value_stack[ --top ];
    } 
    else if (limit < 0 && number > limit) 
    { 
      push_number_value( number - 1 );
      element = value_stack[ --top ];
    } 
    else 
      element = NULL;
    break;
  }
  value_stack[ index ] = element;
}

/* Functions to compare values. =============================================*/

bool 
values_equal( value_t value1, value_t value2 )
/* Return a truth value indicating whether VALUE1 and VALUE2 are equal.
 * VALUE1 an VALUE2 must be of same type or one of them must be nil.
 * Refer to documentation to see what "equal" in Malaga really means. */
{
  value_t value1_end, value2_end, v1, v2;

  if (TYPE( value1 ) != TYPE( value2 )) 
  {
    return false;
  }
  switch (TYPE( value1 )) 
  {
  case SYMBOL_TYPE:
    return (*value1 == *value2);
  case STRING_TYPE:
    return (strcmp( (string_t) (value1 + 1), (string_t) (value2 + 1) ) 
	    == 0);
  case LIST_TYPE:
    /* Look for each value pair if they are equal. */ 
    value1_end = NEXT_VALUE( value1 );
    value2_end = NEXT_VALUE( value2 );
    for (v1 = value1 + 2, v2 = value2 + 2; 
         v1 < value1_end && v2 < value2_end; 
         v1 = NEXT_VALUE( v1 ), v2 = NEXT_VALUE( v2 )) 
    { 
      if (! values_equal( v1, v2 )) 
	return false;
    }
    return (v1 == value1_end && v2 == value2_end);
  case RECORD_TYPE:
    value1_end = NEXT_VALUE( value1 );
    value2_end = NEXT_VALUE( value2 );

    /* Do the records have the same length? */
    if (value1_end - value1 != value2_end - value2) 
      return false;

    /* Check whether for every attribute in VALUE1, there is one
     * in VALUE2 and that their values are equal. */
    for (v1 = value1 + 2; v1 < value1_end; v1 = NEXT_ATTRIB( v1 )) 
    { 
      /* Look for the same attribute in VALUE2. */
      for (v2 = value2 + 2; v2 < value2_end; v2 = NEXT_ATTRIB( v2 )) 
      { 
	if (*v1 == *v2) 
	  break;
      }

      /* Return if we looked 'till end of value2 and didn't the find attribute,
       * or if they don't have the same values. */
      if (v2 == value2_end || ! values_equal( v1 + 1, v2 + 1 )) 
	return false;
    }
    return true;
  case NUMBER_TYPE:
    return (value_to_double( value1 ) == value_to_double( value2 ));
  default:
    throw setup::DictionaryException("Unexpected value type in values_equal");
  }
}

/*---------------------------------------------------------------------------*/

bool 
value_in_value( value_t value1, value_t value2 )
/* Return bool value saying if VALUE1 is element or attribute of VALUE2.
 * VALUE2 must be a list or a record.
 * If VALUE2 is a record, then VALUE1 must be a symbol. */
{
  value_t value2_end;

  value2_end = NEXT_VALUE( value2 );
  if (IS_LIST( value2 )) 
  { 
    for (value2 += 2; value2 < value2_end; value2 = NEXT_VALUE( value2 )) 
    { 
      if (values_equal( value1, value2 )) 
	return true;
    }
  } 
  else if (IS_RECORD( value2 )) 
  { 
    for (value2 += 2; value2 < value2_end; value2 = NEXT_ATTRIB( value2 )) 
    { 
      if (*value1 == *value2) 
	return true;
    }
  }
  return false;
}

}}}
