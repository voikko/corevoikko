/* Copyright (C) 1995 Bjoern Beutel. */

/* Description. =============================================================*/

/* This file contains basic types, macros and functions used everywhere. */

/* Includes. ================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <setjmp.h>
#include "morphology/malaga/basic.hpp"

namespace libvoikko { namespace morphology { namespace malaga {

/* Global variables. ========================================================*/

text_t *error_text;
jmp_buf *current_error_handler;

/* List functions. ==========================================================*/

void 
clear_list( list_t *list )
/* Initialise LIST to be an list */
{ 
  list->first = list->last = NULL; 
}

/*---------------------------------------------------------------------------*/

void add_node( list_t *list, list_node_t *node, position_t position )
/* Add NODE to LIST.
 * If POSITION = LIST_START, add it at the start of the list;
 * If POSITION = LIST_END, add it at the end. */
{ 
  if (list->first == NULL) 
  {
    node->next = NULL;
    list->first = list->last = node;
  } 
  else if (position == LIST_START) 
  { 
    node->next = list->first;
    list->first = node;
  } 
  else /* position == LIST_END */
  { 
    node->next = NULL;
    list->last->next = node;
    list->last = node;
  }
}

/*---------------------------------------------------------------------------*/

void insert_node( list_t *list, list_node_t *node, list_node_t *prev )
/* Insert NODE in LIST, behind PREV.
 * If PREV == NULL, insert NODE at the beginning of the list. */
{ 
  if (prev == NULL) 
  {
    node->next = list->first;
    list->first = node;
  } 
  else 
  { 
    node->next = prev->next;
    prev->next = node;
  }
  if (node->next == NULL) 
    list->last = node;
}

/*---------------------------------------------------------------------------*/

void *
remove_first_node( list_t *list )
/* Remove the first node in LIST and return it.
 * Return NULL if LIST is empty. */
{
  list_node_t *node;
    
  if (list->first == NULL) 
    return NULL;
  node = list->first;
  list->first = node->next;
  if (node == list->last) 
    list->last = NULL;
  return node;
}

/*---------------------------------------------------------------------------*/

void
remove_node( list_t *list, list_node_t *node )
/* Remove NODE in LIST. */
{ 
  list_node_t *prev_node;

  if (list->first == node) 
  { 
    list->first = node->next;
    prev_node = NULL;
  } 
  else 
  { 
    prev_node = list->first;
    while (prev_node->next != node) 
      prev_node = prev_node->next;
    prev_node->next = node->next;
  }
  if (node->next == NULL) 
    list->last = prev_node;
}

/*---------------------------------------------------------------------------*/

void
combine_lists( list_t *list1, list_t *list2 )
/* Append LIST2 to LIST1.
 * LIST1 will contain the concatenation; LIST2 will be empty. */
{ 
  if (list1->first == NULL) 
    list1->first = list2->first;
  else 
    list1->last->next = list2->first;
  if (list2->first != NULL) 
  {
    list1->last = list2->last;
    list2->first = list2->last = NULL;
  }
}

/*---------------------------------------------------------------------------*/

void *
new_node( list_t *list, int_t size, position_t position )
/* Add a node of size SIZE to LIST.
 * If POSITION = LIST_START, add the element at the start of the list;
 * If POSITION = LIST_END, add the element at the end. 
 * Return the newly created node. */
{ 
  list_node_t *node;

  node = (list_node_t *) new_mem( size );
  add_node( list, node, position );
  return node;
}

/*---------------------------------------------------------------------------*/

void 
free_first_node( list_t *list )
/* Remove first node in LIST and free it. */
{ 
  list_node_t *node;

  node = (list_node_t *) remove_first_node( list );
  if (node != NULL) 
    free_mem( &node );
}

/*---------------------------------------------------------------------------*/

void 
free_node( list_t *list, list_node_t *node )
/* Remove NODE from LIST and free it. */
{ 
  remove_node( list, node );
  free_mem( &node );
}

/* Memory functions. ========================================================*/

void *
new_mem( int_t item_size )
/* Allocate a memory block of ITEM_SIZE bytes, clear it and return it.
 * If memory is out, call the function "complain". */
{ 
  if (item_size == 0) 
    return NULL;
  return calloc( 1, item_size );
}

/*---------------------------------------------------------------------------*/

void *
new_vector( int_t item_size, int_t item_count )
/* Allocate a memory block to contain ITEM_COUNT items of size ITEM_SIZE,
 * clear it and return it.
 * If memory is out, call the function "complain". */
{ 
  if (item_size == 0 || item_count == 0) 
    return NULL;
  return calloc( item_count, item_size );
}

/*---------------------------------------------------------------------------*/

int_t 
renew_vector( void *block_p, int_t item_size, int_t item_count )
/* Realloc *BLOCK_P to contain ITEM_COUNT items of ITEM_SIZE bytes each.
 * Return ITEM_COUNT.
 * If memory is out, call the function "complain". */
{ 
  void *block;

  block = *((void **) block_p);
  block = realloc( block, item_count * item_size );
  *((void **) block_p) = block;
  return item_count;
}

/*---------------------------------------------------------------------------*/

void 
free_mem( void *pointer )
/* Free memory *POINTER points to, and set *POINTER to NULL. */
{ 
  free( *((void **) pointer) );
  *((void **) pointer) = NULL;
}

/* Functions for text (indefinitely growing strings). =======================*/

text_t *
new_text( void )
/* Return a new text structure. */
{ 
  text_t *text;
  
  text = (text_t *) new_mem( sizeof( text_t ) );
  text->buffer_size = 100;
  text->buffer = (char_t *) new_vector( sizeof( char_t ), text->buffer_size + 1 );
  /* text->buffer[0] is set to EOS. */
  text->string_size = 0;

  return text;
}

/*---------------------------------------------------------------------------*/

void 
clear_text( text_t *text )
/* Initialize TEXT to an empty string. */
{ 
  text->buffer[0] = EOS;
  text->string_size = 0;
}

/*---------------------------------------------------------------------------*/

void 
free_text( text_t **text_p )
/* Free the content of *TEXT_P. */
{ 
  if (*text_p != NULL) 
  { 
    free_mem( &(*text_p)->buffer );
    free_mem( text_p );
  }
}

/*---------------------------------------------------------------------------*/

void 
add_to_text( text_t *text, string_t string )
/* Add STRING to TEXT. */
{ 
  int_t string_len;

  string_len = strlen( string );
  if (text->buffer_size < text->string_size + string_len + 1) 
  { 
    text->buffer_size = renew_vector( &text->buffer, sizeof( char_t ), 
                                      2 * (text->string_size + string_len) );
  }
  strcpy( text->buffer + text->string_size, string );
  text->string_size += string_len;
}

/*---------------------------------------------------------------------------*/

void 
add_char_to_text( text_t *text, char_t character )
/* Add CHARACTER to TEXT. */
{ 
  if (text->buffer_size < text->string_size + 2)
  {
    text->buffer_size = renew_vector( &text->buffer, sizeof( char_t ), 
				      2 * (text->string_size + 1) );
  }
  text->buffer[ text->string_size++ ] = character;
  text->buffer[ text->string_size ] = EOS;
}

/*---------------------------------------------------------------------------*/

void 
add_unichar_to_text( text_t *text, gunichar c )
/* Add C to TEXT. */
{ 
  char buf[7];
  int_t n;

  n = g_unichar_to_utf8( c, buf );
  buf[n] = EOS;
  if (text->buffer_size < text->string_size + n + 1) 
  { 
    text->buffer_size = renew_vector( &text->buffer, sizeof( char_t ), 
                                      2 * (text->string_size + n) );
  }
  strcpy( text->buffer + text->string_size, buf );
  text->string_size += n;
}

/*---------------------------------------------------------------------------*/

void 
insert_in_text( text_t *text, string_t string, int_t position )
/* Insert STRING at POSITION in TEXT (position counts bytes from 0 onward). */
{ 
  int_t string_len;

  string_len = strlen( string );
  if (text->buffer_size < text->string_size + string_len + 1) 
  { 
    text->buffer_size = renew_vector( &text->buffer, sizeof( char_t ), 
                                      2 * (text->string_size + string_len) );
  }
  if (position < 0) 
    position = 0;
  if (position > text->string_size) 
    position = text->string_size;
  memmove( text->buffer + position + string_len, text->buffer + position,
           sizeof( char_t ) * (text->string_size + 1 - position) );
  memcpy( text->buffer + position, string, sizeof( char_t ) * string_len );
  text->string_size += string_len;
}

/*---------------------------------------------------------------------------*/

void 
insert_char_in_text( text_t *text, char_t character, int_t position )
/* Insert CHARACTER at POSITION in TEXT. */
{ 
  if (text->buffer_size < text->string_size + 2)
  {
    text->buffer_size = renew_vector( &text->buffer, sizeof( char_t ),
				      2 * (text->string_size + 1) );
  }
  if (position < 0) 
    position = 0;
  if (position > text->string_size) 
    position = text->string_size;
  memmove( text->buffer + position + 1, text->buffer + position,
           sizeof( char_t ) * (text->string_size + 1 - position) );
  text->buffer[ position ] = character;
  text->string_size++;
}

/*---------------------------------------------------------------------------*/

char_t * 
text_to_string( text_t **text_p )
/* Return content of *TEXT_P as a string and delete *TEXT_P.
 * The string must be freed after use. */
{
  char_t *string;

  string = new_string( (*text_p)->buffer, NULL );
  free_text( text_p );
  return string;
}

/* String functions. ========================================================*/

char_t * 
new_string( string_t string, string_t end )
/* Allocate memory and copy STRING into it.
 * If END != NULL, it marks the end of the string.
 * The result string must be freed after use. */
{
  char_t *new_str;
  char_t *new_str_p;

  if (end == NULL) 
    end = string + strlen( string );
  new_str = new_str_p = (char_t *) new_vector( sizeof( char_t ), end - string + 1 );
  while (string < end) 
    *new_str_p++ = *string++;
  *new_str_p = EOS;
  return new_str;
}

/*---------------------------------------------------------------------------*/

char_t * 
new_string_readable( string_t from, string_t from_end )
/* Like "new_string", but enclose the string in double quotes, copy a "\" in 
 * front of quotes and backslashed, and copy control chars in "\uxxxx" format.
 * If FROM_END != NULL, it marks the end of the string. 
 * The result string must be freed after use. */
{
  text_t *text;
  int_t i, code, position;

  text = new_text();
  if (from_end == NULL) 
    from_end = from + strlen( from );
  add_char_to_text( text, '\"' );
  while (from < from_end)
  { 
    if (*from == '\"' || *from == '\\')  /* Prepend a backslash. */
    { 
      add_char_to_text( text, '\\' );
      add_char_to_text( text, *from++ );
    } 
    else if ((*from >= 0 && *from < 32) || *from == 127)
    { 
      /* Convert control chars to octal "\xxx" format. */
      add_char_to_text( text, '\\' );
      position = text->string_size;
      code = *from++;
      for (i = 0; i < 3; i++) 
      { 
	insert_char_in_text( text, code % 8 + '0', position );
        code = code / 8;  
      }
    } 
    else
    {
      add_unichar_to_text( text, g_utf8_get_char( from ) );
      from = g_utf8_next_char( from );
    }
  }
  add_char_to_text( text, '\"' );
  return text_to_string( &text );
}

/*---------------------------------------------------------------------------*/

char_t *
concat_strings( string_t first_string, ... )
/* Concatenate a list of strings and return the result string.
 * Must have NULL-terminated list of strings as parameters.
 * The result string must be freed after use. */
{
  va_list args;
  size_t length;
  string_t next_string;
  char_t *string;
  char_t *string_p;

  /* Compute length of the result string. */
  va_start( args, first_string );
  length = strlen( first_string );
  for (next_string = va_arg( args, string_t ); 
       next_string != NULL;
       next_string = va_arg( args, string_t )) 
  {
    length += strlen( next_string ); 
  }
  va_end( args );

  /* Concatenate strings. */
  va_start( args, first_string );
  string = (char_t *) new_vector( sizeof( char_t ), length + 1 );
  strcpy( string, first_string );
  string_p = string + strlen( first_string );
  for (next_string = va_arg( args, string_t ); 
       next_string != NULL; 
       next_string = va_arg( args, string_t )) 
  {
    strcpy( string_p, next_string );
    string_p += strlen( next_string );
  }
  va_end( args );

  return string;
}

/*---------------------------------------------------------------------------*/

string_t 
next_non_space( string_t string )
/* Return STRING, but without leading spaces. */
{ 
  while (g_unichar_isspace( g_utf8_get_char( string ) ))
    string = g_utf8_next_char( string );
  return string;
}

/*---------------------------------------------------------------------------*/

char_t *
double_to_string( double number )
/* Convert NUMBER to a string. It must be freed after use. */
{ 
  char_t buffer[30];

  sprintf( buffer, "%.11G", number );
  return new_string( buffer, NULL );
}

/*---------------------------------------------------------------------------*/

char_t * 
int_to_string( int_t number )
/* Convert NUMBER to a string. It must be freed after use. */
{
  char_t buffer[12];
  
  sprintf( buffer, "%d", number );
  return new_string( buffer, NULL );
}

/* Error handling. ==========================================================*/

void 
malaga_throw( void )
/* Call the current error handler. 
 * If there is no current error handler, print error and exit. */
{ 
  if (current_error_handler != NULL) 
    longjmp( *current_error_handler, 1 );
  else 
  { 
    fprintf( stderr, "libmalaga: %s\n", error_text->buffer );
    exit( 1 );
  }
}

/*---------------------------------------------------------------------------*/

void 
complain( string_t message, ... )
{ 
  malaga_throw();
}

/* Module initialisation. ===================================================*/

void 
init_basic()
/* Initialise this module. */
{
  error_text = new_text();
}

/*---------------------------------------------------------------------------*/

void 
terminate_basic( void )
/* Terminate this module. */
{ 
  free_text( &error_text );
}

}}}
